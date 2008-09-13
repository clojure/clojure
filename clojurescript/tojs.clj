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

(defmulti tojs (fn [e ctx] (class e)))

(defmethod tojs clojure.lang.Var [e ctx]
  (let [{:keys [name ns]} ^e]
    (str (Compiler/munge (str (.getName ns))) "." (Compiler/munge (str name)))))

(defmethod tojs clojure.lang.Compiler$DefExpr [e ctx]
  (str (tojs (.var e) ctx) "=" (tojs (.init e) ctx) ";\n"))

(defn fnmethod [fm ctx]
  (let [lm (into {} (map (fn [[lb lb] i] [lb (str (.name lb) "_" i)])
                         (.locals fm) (iterate inc 0)))]
    (vstr [ "var " (vec (interpose "," (vals lm))) ";\n"
           (vec (for [lb (.reqParms fm)]
                  [(lm lb) "=arguments[" (dec (.idx lb)) "];\n"]))
           (when-let lb (.restParm fm)
             ["var " (lm lb) "=clojure.RT.rest_args(arguments,"
              (count (.reqParms fm)) ");\n"])
           "var _rtn,_cnt;do{_cnt=0;\n_rtn="
           (tojs (.body fm) (assoc ctx :localmap lm))
           ";\n}while(_cnt);return _rtn;"])))

(defmethod tojs clojure.lang.Compiler$FnExpr [e ctx]
  (vstr ["(function " (.simpleName e) "(){\n"
         (vec (for [fm (.methods e) :when (not= fm (.variadicMethod e))]
                ["if(arguments.length==" (count (.argLocals fm)) "){\n"
                  (fnmethod fm ctx)
                 "}\n"]))
         (if (.variadicMethod e)
           [(fnmethod (.variadicMethod e) ctx) "\n"]
           ["throw \"Wrong number of args passed to: " (.thisName e) "\";\n"])
         "})"]))

(defmethod tojs clojure.lang.Compiler$BodyExpr [e ctx]
   (apply str (interpose ",\n" (map #(tojs % ctx) (.exprs e)))))

(defmethod tojs clojure.lang.Compiler$LetExpr [e ctx]
  (vstr ["("
         (when (.isLoop e)
           "(function(){var _rtn,_cnt;do{_cnt=0;\n_rtn=")
         (vec (for [bi (.bindingInits e)]
                ["(" ((:localmap ctx) (.binding bi))
                 "=" (tojs (.init bi) ctx) "),\n"]))
         (tojs (.body e) ctx)
         (when (.isLoop e)
           "}while(_cnt);return _rtn;})()")
         ")"]))

(defmethod tojs clojure.lang.Compiler$VectorExpr [e ctx]
  (vstr ["clojure.RT.lit_vector(["
         (vec (interpose "," (map #(tojs % ctx) (.args e))))
         "])"]))

(defn const-str [c]
  (cond
    (string?  c) (str \" c \")
    (keyword? c) (str \" c \")
    (symbol?  c) (str \" \' c \")
    (class?   c) (.getCanonicalName c)
    (list?    c) (vstr ["clojure.RT.lit_list(["
                        (vec (interpose "," (map const-str c)))
                        "])"])
    :else (str c)))

(defmethod tojs clojure.lang.Compiler$ConstantExpr [e ctx]
  (const-str (.v e)))

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
  (str "(" (tojs (.testExpr e) ctx)
       "?" (tojs (.thenExpr e) ctx)
       ":" (tojs (.elseExpr e) ctx) ")"))

(defmethod tojs clojure.lang.Compiler$RecurExpr [e ctx]
  (vstr ["(_cnt=0"
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
  (tojs (Compiler/analyze Compiler$C/STATEMENT f) {}))

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

(println (formtojs '(binding [*out* 5] (set! *out* 10))))
(println (formtojs '(.replace "a/b/c" "/" ".")))
(println (formtojs '(list '(1 "str" 'sym :key) 4 "str2" 6 #{:set 9 8})))

(testboot)
