;; Data types

;; by Konrad Hinsen
;; last updated March 12, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.types
  "General and algebraic data types

   NOTE: This library is experimental. It may change significantly
   with future release.")

;
; Data type definition
;
(defmulti deconstruct type)
(defmethod deconstruct :default [x] (list x))

(defmacro deftype
  "Define a data type by a type tag (a namespace-qualified keyword)
  and a symbol naming the constructor function. Optionally, a pair
  of constructor and deconstructor functions can be given as well,
  the defaults are clojure.core/identity and clojure.core/list.
  The full constructor associated with constructor-name calls the
  constructor function and attaches the type tag to its result
  as metadata. The deconstructor function must return the arguments
  to be passed to the constructor in order to create an equivalent
  object. It is used for printing and matching."
  ([type-tag constructor-name]
   `(deftype ~type-tag ~constructor-name identity list))
  ([type-tag constructor-name constructor deconstructor]
   `(do
      (derive ~type-tag ::type)
      (let [meta-map# {:type ~type-tag ::constructor (quote ~constructor-name)}]
        (def ~constructor-name
	  (comp (fn [~'x] (with-meta ~'x meta-map#)) ~constructor))
	(defmethod deconstruct ~type-tag [~'x]
	  (~deconstructor (with-meta ~'x {})))))))

(defmethod print-method ::type [o w]
  (print-method (cons (::constructor ^o) (deconstruct o)) w))

;
; Defining algebraic types
;
(derive ::adt ::type)

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

(defmacro defadt
  "Define an algebraic data type name by an exhaustive list of constructors.
   Each constructor can be a symbol (argument-free constructor) or a
   list consisting of a tag symbol followed by the argument symbols.
   The data type tag must be a keyword."
  [type-tag & constructors]
  (let [meta-map-symbol (gensym "mm")
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
    (symbol (subs s-str (inc (.indexOf s-str (int \/)))))))

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
 
