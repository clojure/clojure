(ns tojs
    (:import (clojure.lang Compiler Compiler$C))
    (:require [clojure.contrib.duck-streams :as ds]))

(defn vstr [v]
  (let [sb (StringBuilder.)
        lvl (fn lvl [v]
              (doseq i v
                (if (vector? i)
                  (lvl i)
                  (.append sb (str i)))))]
    (lvl v)
    (str sb)))

(def *debug-fn-names* true)
(def *debug-comments* true)

(def *has-recur*) ; used internally

(defmulti tojs (fn [e ctx] (class e)))

(defn fnmethod [fm maxm ctx]
  (let [lm (into {} (for [[lb lb] (.locals fm)]
                      [lb (str (.name lb) "_" (.idx lb))]))
        thisfn (first (filter #(= 0 (.idx %)) (keys lm)))
        [body has-recur] (binding [*has-recur* false]
                           [(tojs (.body fm)
                                  (merge-with merge ctx {:localmap lm}))
                            *has-recur*])
        mparm (into {} (for [p (.reqParms maxm)] [(.idx p) p]))
        inits (concat
                (when has-recur ["_cnt" "_rtn"])
                (vals (reduce dissoc lm (cons thisfn (.reqParms fm))))
                (when (:fnname ctx) [(str (lm thisfn) "=arguments.callee")])
                (when (not= fm maxm)
                  (for [lb (.reqParms fm)
                        :when (not= (.name lb) (.name (mparm (.idx lb))))]
                    [(lm lb) "=arguments[" (dec (.idx lb)) "]"]))
                (when-let lb (.restParm fm)
                  [(str (lm lb) "=clojure.JS.rest_args(this,arguments,"
                        (count (.reqParms fm)) ")")]))]
    (.reqParms maxm)
    (vstr [(when (seq inits)
             [(apply vector "var " (interpose "," inits)) ";\n"])
           (if has-recur
             ["do{_cnt=0;_rtn="
              body
              "\n}while(_cnt);return _rtn;"]
             ["return (" body ")"])])))

(defmethod tojs clojure.lang.Compiler$FnExpr [e ctx]
  (let [maxm (or (.variadicMethod e)
                 (-> (into (sorted-map)
                           (for [fm (.methods e)
                                 :when (not= fm (.variadicMethod e))]
                             [(count (.reqParms fm)) fm]))
                     last val))
        manym (< 1 (count (.methods e)))
        newctx (assoc ctx :fnname (.thisName e))]
    (vstr [(when (.variadicMethod e)
             ["clojure.JS.variatic(" (count (.reqParms maxm)) ","])
           "(function"
           (when *debug-fn-names*
             [" __" (.replaceAll (.name e) "[\\W_]+" "_")])
           "("
           (vec (interpose "," (for [lb (.reqParms maxm)]
                                 [(.name lb) "_" (.idx lb)])))
           "){"
           (when manym
             ["switch(arguments.length){"
              (vec (for [fm (.methods e) :when (not= fm maxm)]
                     ["\ncase " (count (.reqParms fm)) ":"
                      (fnmethod fm maxm newctx)]))
              "}"])
           "\n"
           (fnmethod maxm maxm newctx) "})"
           (when (.variadicMethod e)
             ")")
           ])))

(defmethod tojs clojure.lang.Compiler$BodyExpr [e ctx]
   (apply str (interpose ",\n" (map #(tojs % ctx) (.exprs e)))))

(defmethod tojs clojure.lang.Compiler$LetExpr [e ctx]
  (let [inits (vec (interpose ",\n" (for [bi (.bindingInits e)]
                                    ["(" ((:localmap ctx) (.binding bi))
                                     "=" (tojs (.init bi) ctx) ")"])))]
    (if (.isLoop e)
      (binding [*has-recur* false]
        (vstr ["((function"
               (when *debug-fn-names* " __loop")
               "(){var _rtn,_cnt;"
               inits ";"
               "do{_cnt=0;\n_rtn=" (tojs (.body e) ctx)
               "}while(_cnt);return _rtn;})())"]))
      (vstr ["(" inits ",\n" (tojs (.body e) ctx) ")"]))))

(defmethod tojs clojure.lang.Compiler$VectorExpr [e ctx]
  (vstr ["clojure.JS.lit_vector(["
         (vec (interpose "," (map #(tojs % ctx) (.args e))))
         "])"]))

(defn const-str [c]
  (cond
    (or (instance? Character c)
        (keyword? c)
        (string?  c)) (pr-str (str c))
    (symbol?  c) (str \" \' c \")
    (class?   c) (.getCanonicalName c)
    (list?    c) (vstr ["clojure.JS.lit_list(["
                        (vec (interpose "," (map const-str c)))
                        "])"])
    (fn?      c) (str \" c \")
    :else (str c)))

(defmethod tojs clojure.lang.Compiler$ConstantExpr [e ctx]
  (const-str (.v e)))

(def js-reserved '#{import boolean short byte char})

(defn var-munge [x]
  (let [n (-> x str Compiler/munge (.replace "." "_DOT_"))]
    (if (js-reserved (symbol n))
      (str n "_")
      n)))

(defn var-parts [e]
  (let [{:keys [name ns]} ^(.var e)]
    [(Compiler/munge (str (.getName ns))) (var-munge name)]))

(defmethod tojs clojure.lang.Compiler$UnresolvedVarExpr [e ctx]
  (vstr ["clojure.JS.resolveVar(\""
         (var-munge (name (.symbol e))) "\","
         (Compiler/munge (name (.name *ns*))) ")"]))

(defmethod tojs clojure.lang.Compiler$VarExpr [e ctx]
  (let [[vns vname] (var-parts e)]
    (if (and (= vns "clojurescript.js") (#{"this"} vname))
      vname
      (str vns "." vname))))

(defmethod tojs clojure.lang.Compiler$TheVarExpr [e ctx]
  (let [[vns vname] (var-parts e)]
    (str vns "._var_" vname)))

(defmethod tojs clojure.lang.Compiler$AssignExpr [e ctx]
  (let [[vns vname] (var-parts (.target e))]
    (str vns "._var_" vname ".set(" (tojs (.val e) ctx) ")")))

(defmethod tojs clojure.lang.Compiler$DefExpr [e ctx]
  (let [[vns vname] (var-parts e)]
    (str "clojure.JS.def(" vns ",\"" vname "\"," (tojs (.init e) ctx) ")")))


(defmethod tojs clojure.lang.Compiler$InvokeExpr [e ctx]
  (vstr [(tojs (.fexpr e) ctx)
         "("
         (vec (interpose "," (map #(tojs % ctx) (.args e))))
         ")"]))

(defmethod tojs clojure.lang.Compiler$LocalBindingExpr [e ctx]
  ((:localmap ctx) (.b e)))

(defmethod tojs clojure.lang.Compiler$NilExpr [e ctx]
  "null")

(defmethod tojs clojure.lang.Compiler$EmptyExpr [e ctx]
  (str (.getCanonicalName (class (.coll e))) ".EMPTY"))

(defmethod tojs clojure.lang.Compiler$StringExpr [e ctx]
  (const-str (.str e)))

(defmethod tojs clojure.lang.Compiler$KeywordExpr [e ctx]
  (const-str (.k e)))

(defmethod tojs clojure.lang.Compiler$StaticFieldExpr [e ctx]
  (str (.getCanonicalName (.c e)) "." (.fieldName e)))

(defmethod tojs clojure.lang.Compiler$StaticMethodExpr [e ctx]
  (vstr [(.getCanonicalName (.c e)) "." (.methodName e) "("
         (vec (interpose "," (map #(tojs % ctx) (.args e))))
         ")"]))

(defmethod tojs clojure.lang.Compiler$NewExpr [e ctx]
  (vstr ["(new " (.getCanonicalName (.c e)) "("
         (vec (interpose "," (map #(tojs % ctx) (.args e))))
         "))"]))

(defmethod tojs clojure.lang.Compiler$InstanceMethodExpr [e ctx]
  (vstr ["(" (tojs (.target e) ctx) ")." (.methodName e)
         "(" (vec (interpose "," (map #(tojs % ctx) (.args e)))) ")"]))

(defmethod tojs clojure.lang.Compiler$InstanceFieldExpr [e ctx]
  (vstr ["(" (tojs (.target e) ctx) ")." (.fieldName e)]))

(defmethod tojs clojure.lang.Compiler$IfExpr [e ctx]
  (str "((" (tojs (.testExpr e) ctx)
       ")?(" (tojs (.thenExpr e) ctx)
       "):(" (tojs (.elseExpr e) ctx) "))"))

(defmethod tojs clojure.lang.Compiler$RecurExpr [e ctx]
  (set! *has-recur* true)
  (vstr ["(_cnt=1"
         (vec (map #(str ",_t" %2 "=" (tojs %1 ctx)) (.args e) (iterate inc 0)))
         (vec (map #(str "," ((:localmap ctx) %1) "=_t" %2)
                   (.loopLocals e) (iterate inc 0)))
         ")"]))

(defmethod tojs clojure.lang.Compiler$MapExpr [e ctx]
  (vstr ["clojure.lang.HashMap.create(["
         (vec (interpose "," (map #(tojs % ctx) (.keyvals e))))
         "])"]))

(defmethod tojs clojure.lang.Compiler$SetExpr [e ctx]
  (vstr ["clojure.lang.HashSet.create(["
         (vec (interpose "," (map #(tojs % ctx) (.keys e))))
         "])"]))

(defmethod tojs clojure.lang.Compiler$BooleanExpr [e ctx]
  (if (.val e) "true" "false"))

(defmethod tojs clojure.lang.Compiler$ThrowExpr [e ctx]
  (vstr ["(function"
         (when *debug-fn-names* " __throw")
         "(){throw " (tojs (.excExpr e) ctx) "})()"]))

(defmethod tojs clojure.lang.Compiler$TryExpr [e ctx]
  (vstr ["(function"
         (when *debug-fn-names* " __try")
         "(){try{var _rtn=("
         (tojs (.tryExpr e) ctx)
         ")}"
         (when (seq (.catchExprs e))
           (when (not= 1 (count (.catchExprs e)))
             (throw (Exception. "tojs only supports one catch clause per try")))
           (let [cc (first (.catchExprs e))]
             ["\ncatch(" ((:localmap ctx) (.lb cc)) "){_rtn="
              (tojs (.handler cc) ctx)
              "}"]))
         (when (.finallyExpr e)
           ["\nfinally{"
            (tojs (.finallyExpr e) ctx)
            "}"])
         "})()"]))


(def skip-set '#{seq instance? assoc floats doubles ints longs
                 global-hierarchy apply refer first rest import hash-map
                 count find})

(defn skip-defs [expr]
  (let [m ^(.var expr)]
    (or (:macro m) (skip-set (:name m)))))

(defn formtojs [f]
  (binding [*allow-unresolved-vars* true]
    (let [expr (Compiler/analyze Compiler$C/STATEMENT `((fn [] ~f)))
          mainexpr (-> expr .fexpr .methods first .body .exprs first)]
      ;(when (instance? clojure.lang.Compiler$InvokeExpr mainexpr) (prn :invoke f))
      (when-not (or (and (instance? clojure.lang.Compiler$DefExpr mainexpr)
                         (skip-defs mainexpr))
                    (instance? clojure.lang.Compiler$InstanceMethodExpr mainexpr)
                    (and (instance? clojure.lang.Compiler$BodyExpr mainexpr)
                         (instance? clojure.lang.Compiler$DefExpr (first (.exprs mainexpr)))
                         (skip-defs (first (.exprs mainexpr)))))
        (str (tojs expr {:localmap {}}) ";")))))

(defn filetojs [filename]
  (let [reader (java.io.PushbackReader. (ds/reader filename))]
    (binding [*ns* (create-ns 'tmp)]
      (loop []
        (when-let f (try (read reader) (catch Exception e nil))
          (when-let js (formtojs f)
            (when *debug-comments*
              (println "\n//======")
              (print "//")
              (prn f)
              (println "//---"))
            (println (formtojs f))
            (when (or (= 'ns (first f))
                      (= 'in-ns (first f)))
              (eval f)))
          (recur))))))

(defn simple-tests []
  (println (formtojs
    '(defn foo
      ([a b c & d] (prn 3 a b c))
      ([c]
        ;(String/asd "hello")
        ;(.foo 55)
        (let [[a b] [1 2]]
          (prn a b c)
          "hi")))))

  (println (formtojs
    '(defn foo [a]
      (prn "hi")
      (let [a 5]
        (let [a 10]
          (prn "yo")
          (prn a))
        (prn a))
      (prn a))))

  (println (formtojs
    '(defn x [] (conj [] (loop [i 5] (if (pos? i) (recur (- i 2)) i))))))

  ;(println (formtojs '(binding [*out* 5] (set! *out* 10))))
  (println (formtojs '(.replace "a/b/c" "/" ".")))
  (println (formtojs '(list '(1 "str" 'sym :key) 4 "str2" 6 #{:set 9 8})))
  (println (formtojs '(fn forever[] (forever))))
  (println (formtojs '(fn forever[] (loop [] (recur))))))

;(simple-tests)

(filetojs (first *command-line-args*))
