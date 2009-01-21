;   Copyright (c) Chris Houser, Sep 2008-Jan 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Reads Clojure code and emits equivalent JavaScript

(ns clojure.contrib.clojurescript
  (:import (clojure.lang Compiler Compiler$C Compiler$BodyExpr
                         Compiler$DefExpr Compiler$InstanceMethodExpr))
  (:require [clojure.contrib.duck-streams :as ds]))

(defn- vstr [v]
  (let [sb (StringBuilder.)
        lvl (fn lvl [v]
              (doseq [i v]
                (if (vector? i)
                  (lvl i)
                  (.append sb (str i)))))]
    (lvl v)
    (str sb)))

(def *debug-fn-names* true)
(def *debug-comments* true)
(def *eval-defmacro* true)

; used internally
(def *has-recur*)
(def *local-names* {})

(defmulti #^{:private true} tojs (fn [e ctx] (class e)))

(defn- fnmethod [fm maxm ctx]
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
                (vals (reduce dissoc lm
                              (conj (.reqParms fm) thisfn (.restParm fm))))
                (when (:fnname ctx) [(str (lm thisfn) "=arguments.callee")])
                (when (not= fm maxm)
                  (for [lb (.reqParms fm)
                        :when (not= (.name lb) (.name (mparm (.idx lb))))]
                    [(lm lb) "=arguments[" (dec (.idx lb)) "]"]))
                (when-let [lb (.restParm fm)]
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
        newctx (assoc ctx :fnname (.thisName e))
        [methods local-names] (binding [*local-names* *local-names*]
                                [(into {} (for [fm (.methods e)]
                                             [fm (fnmethod fm maxm newctx)]))
                                 *local-names*])]
    (vstr [(when (.variadicMethod e)
             ["clojure.JS.variadic(" (count (.reqParms maxm)) ","])
           "(function"
           (when *debug-fn-names*
             [" __" (.replaceAll (.name e) "[\\W_]+" "_")])
           "("
           (vec (interpose "," (for [lb (.reqParms maxm)]
                                 [(.name lb) "_" (.idx lb)])))
           "){"
           ;"\n//" (vec (interpose "," (vals local-names))) "\n"
           (when manym
             ["switch(arguments.length){"
              (vec (for [[fm body] methods :when (not= fm maxm)]
                     ["\ncase " (count (.reqParms fm)) ":" body]))
              "}"])
           "\n"
           (methods maxm) "})"
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

(defn- const-str [c]
  (cond
    (or (instance? Character c)
        (string?  c)) (pr-str (str c))
    (keyword? c) (str "clojure.core.keyword(\"" (namespace c) "\",\"" (name c) "\")")
    (symbol?  c) (str "clojure.core.symbol(\"" c "\")")
    (class?   c) (.getCanonicalName c)
    (list?    c) (vstr ["clojure.JS.lit_list(["
                        (vec (interpose "," (map const-str c)))
                        "])"])
    (fn?      c) (str \" c \")
    (instance? java.util.regex.Pattern c) (str "(/"
                                               (.replace (str c) "/" "\\/")
                                               "/)")
    :else (str "(" c ")")))

(defmethod tojs clojure.lang.Compiler$ConstantExpr [e ctx]
  (const-str (.v e)))

(def js-reserved '#{import boolean short byte char class})

(defn- var-munge [x]
  (let [n (-> x str Compiler/munge (.replace "." "_DOT_"))]
    (if (js-reserved (symbol n))
      (str n "_")
      n)))

(defn- var-parts [e]
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
  (let [target (.target e)]
    (if (instance? clojure.lang.Compiler$InstanceFieldExpr target)
      (vstr ["(" (tojs (.target target) ctx) "."
             (var-munge (.fieldName target)) "=" (tojs (.val e) ctx) ")"])
      (let [[vns vname] (var-parts target)]
        (str vns "._var_" vname ".set(" (tojs (.val e) ctx) ")")))))

(defmethod tojs clojure.lang.Compiler$DefExpr [e ctx]
  (let [[vns vname] (var-parts e)]
    (str "clojure.JS.def(" vns ",\"" vname "\"," (tojs (.init e) ctx) ")")))


(defmethod tojs clojure.lang.Compiler$InvokeExpr [e ctx]
  (vstr [(tojs (.fexpr e) ctx)
         ".apply(null,["
         (vec (interpose "," (map #(tojs % ctx) (.args e))))
         "])"]))

(defmethod tojs clojure.lang.Compiler$LocalBindingExpr [e ctx]
  (let [local-name ((:localmap ctx) (.b e))]
    (set! *local-names* (assoc *local-names* (.b e) local-name))
    local-name))

(defmethod tojs clojure.lang.Compiler$NilExpr [e ctx]
  "null")

(defmethod tojs clojure.lang.Compiler$EmptyExpr [e ctx]
  (str (.getCanonicalName (class (.coll e))) ".EMPTY"))

(defmethod tojs clojure.lang.Compiler$StringExpr [e ctx]
  (const-str (.str e)))

(defmethod tojs clojure.lang.Compiler$KeywordExpr [e ctx]
  (const-str (.k e)))

(defmethod tojs clojure.lang.Compiler$StaticFieldExpr [e ctx]
  (str "clojure.JS.getOrRun(" (.getCanonicalName (.c e)) ",\""
       (var-munge (.fieldName e)) "\")"))

(defmethod tojs clojure.lang.Compiler$StaticMethodExpr [e ctx]
  (vstr [(.getCanonicalName (.c e)) "." (.methodName e) "("
         (vec (interpose "," (map #(tojs % ctx) (.args e))))
         ")"]))

(defmethod tojs clojure.lang.Compiler$NewExpr [e ctx]
  (vstr ["(new " (.getCanonicalName (.c e)) "("
         (vec (interpose "," (map #(tojs % ctx) (.args e))))
         "))"]))

(defmethod tojs clojure.lang.Compiler$InstanceMethodExpr [e ctx]
  (vstr ["(" (tojs (.target e) ctx) ")." (var-munge (.methodName e))
         "(" (vec (interpose "," (map #(tojs % ctx) (.args e)))) ")"]))

(defmethod tojs clojure.lang.Compiler$InstanceFieldExpr [e ctx]
  (vstr ["clojure.JS.getOrRun(" (tojs (.target e) ctx) ",\""
         (var-munge (.fieldName e)) "\")"]))

(defmethod tojs clojure.lang.Compiler$IfExpr [e ctx]
  (str "((" (tojs (.testExpr e) ctx)
       ")?(" (tojs (.thenExpr e) ctx)
       "):(" (tojs (.elseExpr e) ctx) "))"))

(defmethod tojs clojure.lang.Compiler$RecurExpr [e ctx]
  (set! *has-recur* true)
  (vstr ["(_cnt=1,_rtn=["
         (vec (interpose "," (map #(tojs % ctx) (.args e))))
         "]"
         (vec (map #(str "," ((:localmap ctx) %1) "=_rtn[" %2 "]")
                   (.loopLocals e) (iterate inc 0)))
         ")"]))

(defmethod tojs clojure.lang.Compiler$MapExpr [e ctx]
  (vstr ["clojure.core.hash_map("
         (vec (interpose "," (map #(tojs % ctx) (.keyvals e))))
         ")"]))

(defmethod tojs clojure.lang.Compiler$SetExpr [e ctx]
  (vstr ["clojure.core.hash_set("
         (vec (interpose "," (map #(tojs % ctx) (.keys e))))
         ")"]))

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
         "return _rtn})()"]))


(defmulti toclj class)
(defmethod toclj clojure.lang.Compiler$KeywordExpr  [e] (.k e))
(defmethod toclj clojure.lang.Compiler$StringExpr   [e] (.str e))
(defmethod toclj clojure.lang.Compiler$ConstantExpr [e] (.v e))


(def skip-def '#{;-- implemented directly in clj.js
                 seq instance? assoc apply refer first rest import
                 hash-map count find keys vals get class contains?
                 print-method class? number? string? integer? nth
                 to-array cons keyword symbol load
                 ;-- not supported yet
                 make-array to-array-2d re-pattern re-matcher re-groups
                 re-seq re-matches re-find format
                 ;-- macros defined without using defmacro
                 let loop fn defn defmacro
                 ;-- will probably never be supported in clojurescript
                 eval resolve ns-resolve await await-for macroexpand
                 macroexpand-1 load-reader load-string special-symbol?
                 bigint bigdec floats doubles ints longs float-array
                 double-array int-array long-array aset-int
                 aset-long aset-boolean aset-float aset-double
                 aset-short aset-char aset-byte slurp seque
                 decimal? float? pmap primitives-classnames})

(def skip-method #{"java.lang.Class"})

(defn formtojs [f]
  (when-not (and (coll? f) (= 'definline (first f)))
    (let [expr (binding [*allow-unresolved-vars* true
                         *compiler-analyze-only* true
                         *private-compiler-loader* (clojure.lang.RT/baseLoader)]
                 (Compiler/analyze Compiler$C/STATEMENT `((fn [] ~f))))
          mainexpr (-> expr .fexpr .methods first .body .exprs first)
          defmacro?  (and (instance? Compiler$BodyExpr mainexpr)
                          (instance? Compiler$DefExpr (first (.exprs mainexpr)))
                          (instance? Compiler$InstanceMethodExpr (second (.exprs mainexpr)))
                          (= "setMacro" (.methodName (second (.exprs mainexpr)))))]
      (if defmacro?
        (when *eval-defmacro*
          (eval f)
          nil)
        (when-not (or (and (instance? Compiler$DefExpr mainexpr)
                          (skip-def (:name ^(.var mainexpr))))
                      (and (instance? Compiler$InstanceMethodExpr mainexpr)
                          (or (= "setMacro" (.methodName mainexpr))
                              (and (= "addMethod" (.methodName mainexpr))
                                    (skip-method (tojs (first (.args mainexpr))
                                                      nil)))))
                      (and (instance? Compiler$BodyExpr mainexpr)
                          (instance? Compiler$DefExpr (first (.exprs mainexpr)))
                          (instance? Compiler$InstanceMethodExpr (second (.exprs mainexpr)))
                          (= "setMacro" (.methodName (second (.exprs mainexpr))))))
          (tojs expr {:localmap {}}))))))

(defn filetojs [filename & optseq]
  (let [reader (java.io.PushbackReader. (ds/reader filename))
        opts (apply array-map optseq)]
    (binding [*ns* (create-ns 'user)
              *debug-fn-names* (:debug-fn-names opts true)
              *debug-comments* (:debug-comments opts true)
              *eval-defmacro*  (:eval-defmacro opts true)]
      (loop []
        (let [f (read reader false reader false)]
          (when-not (identical? f reader)
            (if-let [js (formtojs f)]
              (do
                (when *debug-comments*
                  (println "\n//======")
                  (print "//")
                  (prn f)
                  (println "//---"))
                (println (str js ";"))
                (when (and (coll? f)
                           (or (= 'ns (first f))
                               (= 'in-ns (first f))))
                  (eval f)))
              (when *debug-comments*
                (print "// Skipping: ")
                (prn f)))
            (recur)))))))
