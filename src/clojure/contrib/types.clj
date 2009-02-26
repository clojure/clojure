;; Algebraic data types

;; by Konrad Hinsen
;; last updated February 26, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.types
  "Algebraic data types")

;
; Defining algebraic types
;
(defn- qualified-symbol
  [s]
  (symbol (str *ns*) (str s)))

(defn- qualified-keyword
  [s]
  (keyword (str *ns*) (str s)))

(defn- constructor-code
  [meta-map-symbol constructor]
  (if (symbol? constructor)
    `(def ~constructor
	  (with-meta (quote ~(qualified-symbol constructor))
		     ~meta-map-symbol))
    (let [[name & args] constructor]
      (if (empty? args)
	(throw (IllegalArgumentException. "zero argument constructor"))
	`(defn ~name ~(vec args)
	   (with-meta [(quote ~(qualified-symbol name)) ~@args]
		      ~meta-map-symbol))))))

(defmacro deftype
  "Define the algebraic data type name by an exhaustive list of constructors.
   Each constructor can be a symbol (argument-free constructor) or a
   list consisting of a tag symbol followed by the argument symbols.
   The data type itself is a keyword declared as deriving from
   :clojure.contrib.types/adt."
  [name & constructors]
  (let [type-tag (qualified-keyword name)
	meta-map-symbol (gensym "mm")
	accessor1 (if (and (= 1 (count constructors))
			   (seq? (first constructors))
			   (= 2 (count (first constructors))))
		    `(defmethod get-value ~type-tag [v#]
		       (second v#))
		    nil)
	accessor2 (if (and (= 1 (count constructors))
			   (seq? (first constructors)))
		    `(defmethod get-values ~type-tag [v#]
		       (subvec v# 1))
		    nil)]
    `(let [~meta-map-symbol {:type ~type-tag}]
       (derive ~type-tag ::adt)
       (def ~name ~type-tag)
       ~@(map (partial constructor-code meta-map-symbol) constructors)
       ~accessor1
       ~accessor2
       )))

;
; Printing
;
(defn- unqualified-symbol
  [s]
  (let [s-str (str s)]
    (symbol (.substring s-str (inc (.indexOf s-str (int \/)))))))

(defmethod print-method ::adt [o w]
  (if (symbol? o)
    (let [tag (unqualified-symbol o)]
      (print-method tag w))
    (let [[tag & values] o
	  tag (unqualified-symbol tag)]
      (print-method (cons tag values) w))))

;
; Value access
;
(defmulti get-value
  "Obtain the value stored in an object of an algebraic type.
   Defined only for types with a single constructor that has
   one argument."
  {:arglists '([v])}
  type)

(defmethod get-value ::adt
  [v]
  (throw (Exception.
	  "defined for types with a single constructor of one argument")))

(defmulti get-values
  "Obtain the values stored in an object of an algebraic type.
   Defined only for types with a single constructor that must have
   at least one argument."
  {:arglists '([v])}
  type)

(defmethod get-values ::adt
  [v]
  (throw
   (Exception.
    "defined for types with a single constructor of at least one argument")))

;
;  Matching templates
;
(defn- resolve-symbol
  [s]
  (if-let [var (resolve s)]
    (symbol (str (.ns var)) (str (.sym var)))
    s))

(defn- tests-and-bindings
  [template vsymbol]
  (if (symbol? template)
    [`(= (quote ~(resolve-symbol template)) ~vsymbol)
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
	     (cons `(vector? ~vsymbol)
		   (cons `(= (quote ~(resolve-symbol tag))
			     (first ~vsymbol))
			 tests)))
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
   (when (odd? (count clauses))
     (throw (Exception. "Odd number of elements in match expression")))
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
    `(let [~vsymbol ~value]
       (cond ~@terms))))
 
