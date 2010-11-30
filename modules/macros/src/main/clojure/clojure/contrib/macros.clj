;; Various useful macros
;;
;; Everybody is invited to add their own little macros here!
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution. By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license. You must not
;; remove this notice, or any other, from this software.

(ns
  ^{:author "Konrad Hinsen"
     :doc "Various small macros"}
  clojure.contrib.macros)

;; By Konrad Hinsen
(defmacro const
  "Evaluate the constant expression expr at compile time."
  [expr]
  (eval expr))

;; By Konrad Hinsen
; This macro is made obsolete by Clojure's built-in letfn. I renamed it to
; letfn- (to avoid a name clash) but leave it in for a while, since its
; syntax is not quite the same as Clojure's. Expect this to disappear
; in the long run!
(defmacro letfn-
  "OBSOLETE: use clojure.core/letfn
   A variant of let for local function definitions. fn-bindings consists
   of name/args/body triples, with (letfn [name args body] ...)
   being equivalent to (let [name (fn name args body)] ...)."
  [fn-bindings & exprs]
  (let [makefn (fn [[name args body]] (list name (list 'fn name args body)))
	fns (vec (apply concat (map makefn (partition 3 fn-bindings))))]
  `(let ~fns ~@exprs)))

 ;; By Konrad Hinsen

 (defn- unqualified-symbol
  [s]
  (let [s-str (str s)]
    (symbol (subs s-str (inc (.indexOf s-str (int \/)))))))
 
(defn- bound-var?
  [var]
  (try
    (do (deref var) true)
    (catch java.lang.IllegalStateException e false)))

(defn- fns-from-ns
  [ns ns-symbol]
  (apply concat
    (for [[k v] (ns-publics ns)
          :when (and (bound-var? v)
                     (fn? @v)
                     (not (:macro (meta v))))]
       [k (symbol (str ns-symbol) (str k))])))

(defn- expand-symbol
  [ns-or-var-sym]
  (if (= ns-or-var-sym '*ns*)
    (fns-from-ns *ns* (ns-name *ns*))
    (if-let [ns (find-ns ns-or-var-sym)]
      (fns-from-ns ns ns-or-var-sym)
      (list (unqualified-symbol ns-or-var-sym) ns-or-var-sym))))

(defmacro with-direct-linking
  "EXPERIMENTAL!
   Compiles the functions in body with direct links to the functions
   named in symbols, i.e. without a var lookup for each invocation.
   Symbols is a vector of symbols that name either vars or namespaces.
   A namespace reference is replaced by the list of all symbols in the
   namespace that are bound to functions. If symbols is not provided,
   the default value ['clojure.core] is used. The symbol *ns* can be
   used to refer to the current namespace."
  {:arglists '([symbols? & body])}
  [& body]
  (let [[symbols body] (if (vector? (first body))
                         [(first body) (rest body)]
                         [['clojure.core] body])
  			bindings (vec (mapcat expand-symbol symbols))]
    `(let ~bindings ~@body)))
 