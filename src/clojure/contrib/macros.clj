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
  #^{:author "Konrad Hinsen"
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
