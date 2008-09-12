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
  (-> e str (.substring 2) (.replace "/" ".")))

(defmethod tojs clojure.lang.Compiler$DefExpr [e ctx]
  (str (tojs (.var e) ctx) "=" (tojs (.init e) ctx) ";\n"))

(defn fnmethod [fm ctx]
  (let [lm (into {} (map (fn [[lb lb] i] [lb (str (.name lb) "_" i)])
                         (.locals fm) (iterate inc 0)))]
    (vstr
      [
       "var " (vec (interpose "," (vals lm))) ";\n"
       (vec (for [lb (.reqParms fm)]
              [(lm lb) "=arguments[" (dec (.idx lb)) "];\n"]))
       (when-let lb (.restParm fm)
         ["var " (lm lb) "=clojure.lang.ArraySeq.create(arguments).drop("
          (count (.reqParms fm)) ");\n"])
       "return (" (tojs (.body fm) (assoc ctx :localmap lm)) ")"])))

(defmethod tojs clojure.lang.Compiler$FnExpr [e ctx]
  (vstr ["function " (.thisName e) "(){\n"
         (vec (for [fm (.methods e) :when (not= fm (.variadicMethod e))]
                ["if(arguments.length=" (count (.argLocals fm)) "){\n"
                  (fnmethod fm ctx)
                 "}\n"]))
         (if (.variadicMethod e)
           [(fnmethod (.variadicMethod e) ctx) "\n"]
           ["throw \"Wrong number of args passed to: " (.thisName e) "\";\n"])
         "}"]))

(defmethod tojs clojure.lang.Compiler$BodyExpr [e ctx]
   (apply str (interpose ",\n" (map #(tojs % ctx) (.exprs e)))))

(defmethod tojs clojure.lang.Compiler$LetExpr [e ctx]
  (vstr ["("
         (vec (for [bi (.bindingInits e)]
                ["(" ((:localmap ctx) (.binding bi))
                 "=" (tojs (.init bi) ctx) "),\n"]))
         (tojs (.body e) ctx)
         ")"]))

;(defmethod tojs clojure.lang.Compiler$LetExpr [e ctx]
;  (let [names (map #(.name (.binding %)) (.bindingInits e))
;        inits (map #(.init %) (.bindingInits e))]
;    (vstr ["(function("
;           (vec (interpose ",\n" names))
;           "){\n" (tojs (.body e) ctx) "})(\n  "
;           (vec (interpose ",\n  " (map tojs inits)))
;           ");\n"])))

(defmethod tojs clojure.lang.Compiler$VectorExpr [e ctx]
  (vstr ["clojure.lang.PersistentVector.create(["
         (vec (interpose "," (map #(tojs % ctx) (.args e))))
         "])"]))

(defmethod tojs clojure.lang.Compiler$ConstantExpr [e ctx]
  (if (symbol? (.v e))
    (str \" (.v e) \")
    (str (.v e))))

(defmethod tojs clojure.lang.Compiler$InvokeExpr [e ctx]
  (vstr [(tojs (.fexpr e) ctx)
         "("
         (vec (interpose "," (map #(tojs % ctx) (.args e))))
         ")"]))

(defmethod tojs clojure.lang.Compiler$LocalBindingExpr [e ctx]
  ((:localmap ctx) (.b e)))

(defmethod tojs clojure.lang.Compiler$NilExpr [e ctx]
  "null")

(defmethod tojs clojure.lang.Compiler$StringExpr [e ctx]
  (str \" (.str e) \"))

(defmethod tojs clojure.lang.Compiler$VarExpr [e ctx]
  (tojs (.var e) ctx))

(defmethod tojs clojure.lang.Compiler$StaticFieldExpr [e ctx]
  (str (.getCanonicalName (.c e)) "." (.fieldName e)))

(defmethod tojs clojure.lang.Compiler$StaticMethodExpr [e ctx]
  (vstr [(.getCanonicalName (.c e)) "." (.methodName e) "("
         (vec (interpose "," (map #(tojs % ctx) (.args e))))
         ")"]))

(defmethod tojs clojure.lang.Compiler$IfExpr [e ctx]
  (str "(" (tojs (.testExpr e) ctx)
       "?" (tojs (.thenExpr e) ctx)
       ":" (tojs (.elseExpr e) ctx) ")"))

(defn formtojs [f]
  (tojs (Compiler/analyze Compiler$C/STATEMENT f) {}))

(defn testboot []
  (let [boot "/home/chouser/build/clojure/src/clj/clojure/boot.clj"
        bootreader (java.io.PushbackReader. (ds/reader boot))
        tmpns (create-ns 'tmp)]
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

(testboot)
