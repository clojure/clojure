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

(def *arity-check* false)

(defmulti tojs (fn [e ctx] (class e)))

(defmethod tojs clojure.lang.Var [e ctx]
  (let [{:keys [name ns]} ^e]
    (str (Compiler/munge (str (.getName ns))) "." (Compiler/munge (str name)))))

(defmethod tojs clojure.lang.Compiler$DefExpr [e ctx]
  (str (tojs (.var e) ctx) "=" (tojs (.init e) ctx) ";\n"))

(defn fnmethod [fm maxm ctx]
  (let [lm (into {} (map (fn [[lb lb] i] [lb (str (.name lb) "_" i)])
                         (.locals fm) (iterate inc 0)))
        thisfn (some #(when (= (.name %) (:fnname ctx)) %) (keys lm))
        mainvars (vals (reduce dissoc lm (cons thisfn (when (= fm maxm)
                                                        (.reqParms fm)))))]
    (.reqParms maxm)
    (vstr [(when mainvars ["var " (vec (interpose "," mainvars)) ";\n"])
           (when thisfn ["var " (lm thisfn) "=arguments.callee;\n"])
           (when (not= fm maxm)
             (vec (for [lb (.reqParms fm)]
                    [(lm lb) "=arguments[" (dec (.idx lb)) "];\n"])))
           (when-let lb (.restParm fm)
             ["var " (lm lb) "=clojure.JS.rest_args(arguments,"
              (count (.reqParms fm)) ");\n"])
           "var _rtn,_cnt;do{_cnt=0;\n_rtn="
           (tojs (.body fm) (merge-with merge ctx {:localmap lm}))
           ";\n}while(_cnt);return _rtn;"])))

(defmethod tojs clojure.lang.Compiler$FnExpr [e ctx]
  (let [maxm (or (.variadicMethod e)
                 (-> (into (sorted-map)
                           (for [fm (.methods e)
                                 :when (not= fm (.variadicMethod e))]
                             [(count (.reqParms fm)) fm]))
                     last val))
        manym (< 1 (count (.methods e)))
        newctx (assoc ctx :fnname (.thisName e))]
    (vstr ["(function("
           (vec (interpose "," (map #(vector (.name %) "_" (.idx %))
                                    (.reqParms maxm))))
           "){\n"
           (vec (for [fm (.methods e) :when (not= fm (.variadicMethod e))]
                  [(when manym
                     ["if(arguments.length==" (count (.reqParms fm)) "){\n"])
                   (fnmethod fm maxm newctx)
                   (when manym "}\n")]))
           (if (.variadicMethod e)
             [(fnmethod (.variadicMethod e) maxm newctx) "\n"]
             (when *arity-check*
               ["throw \"Wrong number of args passed to: "
                (.thisName e) "\";\n"]))
           "})"])))

(defmethod tojs clojure.lang.Compiler$BodyExpr [e ctx]
   (apply str (interpose ",\n" (map #(tojs % ctx) (.exprs e)))))

(defmethod tojs clojure.lang.Compiler$LetExpr [e ctx]
  (let [inits (vec (for [bi (.bindingInits e)]
                     ["(" ((:localmap ctx) (.binding bi))
                      "=" (tojs (.init bi) ctx) "),\n"]))]
    (if (.isLoop e)
      (vstr ["((function(){var _rtn,_cnt;"
             inits "0;"
             "do{_cnt=0;\n_rtn=" (tojs (.body e) ctx)
             "}while(_cnt);return _rtn;})())"])
      (vstr ["(" inits (tojs (.body e) ctx) ")"]))))

(defmethod tojs clojure.lang.Compiler$VectorExpr [e ctx]
  (vstr ["clojure.JS.lit_vector(["
         (vec (interpose "," (map #(tojs % ctx) (.args e))))
         "])"]))

(defn const-str [c]
  (cond
    (string?  c) (str \" c \")
    (keyword? c) (str \" c \")
    (symbol?  c) (str \" \' c \")
    (class?   c) (.getCanonicalName c)
    (list?    c) (vstr ["clojure.JS.lit_list(["
                        (vec (interpose "," (map const-str c)))
                        "])"])
    :else (str c)))

(defmethod tojs clojure.lang.Compiler$ConstantExpr [e ctx]
  (const-str (.v e)))

(defmethod tojs clojure.lang.Compiler$UnresolvedVarExpr [e ctx]
  (vstr ["clojure.JS.resolveVar("
         (if-let ns (namespace (.symbol e))
                 [\" (Compiler/munge ns) \"]
                 "null")
         ",\"" (Compiler/munge (name (.symbol e)))
         "\"," (Compiler/munge (name (.name *ns*)))
         ")"]))

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

(defmethod tojs clojure.lang.Compiler$VarExpr [e ctx]
  (tojs (.var e) ctx))

(defmethod tojs clojure.lang.Compiler$TheVarExpr [e ctx]
  ; XXX not really right
  (tojs (.var e) ctx))

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

(defmethod tojs clojure.lang.Compiler$AssignExpr [e ctx]
  (vstr ["(" (tojs (.target e) ctx) "=" (tojs (.val e) ctx) ")"]))

(defmethod tojs clojure.lang.Compiler$ThrowExpr [e ctx]
  (vstr ["(function(){throw " (tojs (.excExpr e) ctx) "})()"]))

(defmethod tojs clojure.lang.Compiler$TryExpr [e ctx]
  (vstr ["(function(){try{var _rtn="
         (tojs (.tryExpr e) ctx)
         "}"
         (when (seq (.catchExprs e))
           (when (not= 1 (count (.catchExprs e)))
             (throw (Exception. "tojs only supports one catch clause per try")))
           (let [cc (first (.catchExprs e))]
             ["\ncatch(" ((:localmap ctx) (.lb cc)) "){_rtn="
              (tojs (.handler e) ctx)
              "}"]))
         (when (.finallyExpr e)
           ["\nfinally{"
            (tojs (.finallyExpr e) ctx)
            "}"])
         "})()"]))


(defn formtojs [f]
  (binding [*allow-unresolved-vars* true]
    (str (tojs (Compiler/analyze Compiler$C/STATEMENT `((fn [] ~f)))
               {:localmap {}})
         ";\n")))

(defn testboot []
  (let [boot "/home/chouser/build/clojure/src/clj/clojure/boot.clj"
        bootreader (java.io.PushbackReader. (ds/reader boot))
        tmpns (create-ns 'tmp)]
    (binding [*ns* tmpns]
      (eval '(def identical? clojure/identical?))
      (eval '(def *ns* nil))
      (eval '(def *in* nil))
      (eval '(def *out* nil))
      (eval '(def *flush-on-newline* nil))
      (eval '(def *print-readably* nil))
      (eval '(def *agent* nil)))
    (loop []
      (when-let f (read bootreader)
        (println "======")
        (prn f)
        (println "---")
        (binding [*ns* tmpns]
          (println (formtojs f))
          (eval f))
        (recur)))))

(defn filetojs [filename]
  (let [reader (java.io.PushbackReader. (ds/reader filename))]
    (binding [*ns* (create-ns 'tmp)]
      (loop []
        (when-let f (try (read reader) (catch Exception e nil))
          (println "//======")
          (print "//")
          (prn f)
          (println "//---")
          (println (formtojs f))
          (when (= 'ns (first f))
            (eval f))
          (recur))))))

(defn simple-tests []
  (println (formtojs
    '(defn foo
      ([a b c & d] (prn 3 a b c))
      ([c];
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
  (println (formtojs '(fn forever[] (forever)))))

;(simple-tests)
;(testboot)

(filetojs "t01.cljs")
