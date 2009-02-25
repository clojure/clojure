;; Algebraic data types

;; by Konrad Hinsen
;; last updated February 25, 2009

;; WARNING: This is a proof-of-concept implementation of algebratic data
;; types. Expect this module to change in the near future!

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.types
  "Algebraic data types

   NOTE: This library is experimental. It may change significantly
   with future release.
   
   NOTE: The major limitation of this implementation of algebraic data
   types is that the objects created with it are compared for equality
   by identity, not by value. Equality tests are therefore almost useless.")

(defn- constructor-code
  [constructor]
  (if (symbol? constructor)
    `(def ~constructor (~'make (quote ~constructor)))
    (let [[name & args] constructor]
      (if (empty? args)
        (throw (IllegalArgumentException. "zero argument constructor"))
        `(defn ~name ~(vec args) (~'make (quote ~name) ~@args))))))

(defmacro deftype
  "Define the algebraic data type name by an exhaustive list of constructors.
   Each constructor can be a symbol (argument-free constructor) or a
   list consisting of a tag symbol followed by the argument symbols.
   The data type itself is a class object, argument-free constructors
   become singleton instances of that class, and constructors with arguments
   are functions that return a new object."
  [name & constructors]
  `(let [~'make (fn [~'tag & ~'values]
                  (fn ~name []
		    (if (nil? ~'values) ~'tag (cons ~'tag ~'values))))]
     (def ~name (class (~'make (quote ~name))))
     ~@(map constructor-code constructors)
     (defmethod clojure.core/print-method ~name [~'o ~'w]
       (clojure.core/print-method (~'o) ~'w))
     (defmethod clojure.core/print-dup ~name [~'o ~'w]
       (clojure.core/print-dup (~'o) ~'w))))

(defn- tests-and-bindings
  [template vsymbol]
  (if (symbol? template)
    [`(= (quote ~template) ~vsymbol)
     []]
    (let [[tag & values] template
          enum-values (map list values (range 1 (inc (count values))))
          ; Non-symbols in the template create an equality test with the
	  ; corresponding value in the object's value list
	  tests (map (fn [[v i]] `(= ~v (nth ~vsymbol ~i)))
		     (filter (complement #(symbol? (first %))) enum-values))
          ; Symbols in the template become bindings to the corresponding
          ; value in the object. However, if a symbol occurs more than once,
          ; only one binding is generated, and equality tests are added
          ; for the other values.
	  bindings (reduce (fn [map [symbol index]]
			     (assoc map symbol
				    (conj (get map symbol []) index)))
			   {}
			   (filter #(symbol? (first %)) enum-values))
	  tests (concat tests
			(map (fn [[symbol indices]]
			       (cons `= (map #(list `nth vsymbol %) indices)))
			     (filter #(> (count (second %)) 1) bindings)))
	  bindings (apply concat
			  (map (fn [[symbol indices]]
				 [symbol (list `nth vsymbol (first indices))])
			       bindings))]
      [(cons `and
	     (cons `(seq? ~vsymbol)
		   (cons `(= (quote ~tag) (first ~vsymbol)) tests)))
       (vec bindings)])))

(defmacro match
  "Given a value and a list of template-expr clauses, evaluate the first
   expr whose template matches the value. Templates have the same form
   as constructors. The arguments can be expressions, which must be equal
   to the corresponding elements of value for the template to match, or
   symbols, which will be bound to the corresponding elements of value
   in the evaluation of expr. If the same symbol occurs more than once,
   the corresponding elements of value must be equal for the template
   to match."
  [value & clauses]
  (let [vsymbol (gensym)
	terms (apply concat
		     (map (fn [[template expr]]
			    (if (= template :else)
			      [template expr]
			      (let [[tests bindings]
				    (tests-and-bindings template vsymbol)]
				[tests
				 (if (empty? bindings)
				   expr
				   `(let ~bindings ~expr))])))
			  (partition 2 clauses)))]
    `(let [~vsymbol (~value)]
       (cond ~@terms))))
