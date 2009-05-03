;; Data types

;; by Konrad Hinsen
;; last updated May 3, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns
  #^{:author "Konrad Hinsen"
     :doc "General and algebraic data types"}
  clojure.contrib.types
  (:use [clojure.contrib.def :only (name-with-attributes)]))

;
; Utility functions
;
(defn- qualified-symbol
  [s]
  (symbol (str *ns*) (str s)))

(defn- qualified-keyword
  [s]
  (keyword (str *ns*) (str s)))

(defn- unqualified-symbol
  [s]
  (let [s-str (str s)]
    (symbol (subs s-str (inc (.indexOf s-str (int \/)))))))

(defn- resolve-symbol
  [s]
  (if-let [var (resolve s)]
    (symbol (str (.ns var)) (str (.sym var)))
    s))

;
; Data type definition
;
(defmulti deconstruct type)

(defmulti constructor-form type)
(defmethod constructor-form :default
  [o] nil)
(defmethod constructor-form ::type
  [o] (cons (::constructor ^o) (deconstruct o)))

(defmacro deftype
  "Define a data type by a type tag (a namespace-qualified keyword)
   and a symbol naming the constructor function. Optionally, a
   constructor and a deconstructor function can be given as well,
   the defaults being clojure.core/identity and clojure.core/list.
   The full constructor associated with constructor-name calls the
   constructor function and attaches the type tag to its result
   as metadata. The deconstructor function must return the arguments
   to be passed to the constructor in order to create an equivalent
   object. It is used for printing and matching."
  {:arglists
  '([type-tag constructor-name docstring? attr-map?]
    [type-tag constructor-name docstring? attr-map? constructor]
    [type-tag constructor-name docstring? attr-map? constructor deconstructor])}
  [type-tag constructor-name & options]
  (let [[constructor-name options]  (name-with-attributes
				      constructor-name options)
	[constructor deconstructor] options
	constructor   		    (if (nil? constructor)
		      		      'clojure.core/identity
		      		      constructor)
	deconstructor 		    (if (nil? deconstructor)
		      		     'clojure.core/list
		      		     deconstructor)]
    `(do
       (derive ~type-tag ::type)
       (let [meta-map# {:type ~type-tag
			::constructor
			    (quote ~(qualified-symbol constructor-name))}]
	 (def ~constructor-name
	      (comp (fn [~'x] (with-meta ~'x meta-map#)) ~constructor))
	 (defmethod deconstruct ~type-tag [~'x]
	   (~deconstructor (with-meta ~'x {})))))))

(defmacro deftype-
  "Same as deftype but the constructor is private."
  [type-tag constructor-name & optional]
  `(deftype ~type-tag
     ~(vary-meta constructor-name assoc :private true)
     ~@optional))

(defmethod print-method ::type [o w]
  (let [cf (constructor-form o)]
    (if (symbol? cf)
      (print-method (unqualified-symbol cf) w)
      (print-method (cons (unqualified-symbol (first cf)) (rest cf)) w))))

;
; Algebraic types
;
(derive ::adt ::type)

(defmethod constructor-form ::adt
  [o]
  (let [v (vals o)]
    (if (= 1 (count v))
      (first v)
      v)))

(defn- constructor-code
  [meta-map-symbol constructor]
  (if (symbol? constructor)
    `(def ~constructor
	  (with-meta {::tag (quote ~(qualified-symbol constructor))}
		     ~meta-map-symbol))
    (let [[name & args] constructor
	  keys (cons ::tag (map (comp keyword str) args))]
      (if (empty? args)
	(throw (IllegalArgumentException. "zero argument constructor"))
	`(let [~'basis (create-struct ~@keys)]
	   (defn ~name ~(vec args)
	     (with-meta (struct ~'basis (quote ~(qualified-symbol name)) ~@args)
			~meta-map-symbol)))))))

(defmacro defadt
  "Define an algebraic data type name by an exhaustive list of constructors.
   Each constructor can be a symbol (argument-free constructor) or a
   list consisting of a tag symbol followed by the argument symbols.
   The data type tag must be a keyword."
  [type-tag & constructors]
  (let [meta-map-symbol (gensym "mm")]
    `(let [~meta-map-symbol {:type ~type-tag}]
       (derive ~type-tag ::adt)
       ~@(map (partial constructor-code meta-map-symbol) constructors)
       )))

;
;  Matching templates
;
(defn- symbol-tests-and-bindings
  [template vsymbol]
  [`(= (quote ~(resolve-symbol template)) ~vsymbol)
   []])

(defn- sequential-tests-and-bindings
  [template vsymbol]
  (let [enum-values (map list template (range (count template)))
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
	bindings (mapcat (fn [[symbol indices]]
			   [symbol (list `nth vsymbol (first indices))])
			 bindings)]
    [tests (vec bindings)]))

(defn- constr-tests-and-bindings
  [template cfsymbol]
  (let [[tag & values] template
	cfasymbol (gensym)
	[tests bindings] (sequential-tests-and-bindings values cfasymbol)
	argtests (if (empty? tests)
		   tests
		   `((let [~cfasymbol (rest ~cfsymbol)] ~@tests)))]
    [`(and (seq? ~cfsymbol)
	   (= (quote ~(resolve-symbol tag)) (first ~cfsymbol))
	   ~@argtests)
     `[~cfasymbol (rest ~cfsymbol) ~@bindings]]))

(defn- list-tests-and-bindings
  [template vsymbol]
  (let [[tests bindings] (sequential-tests-and-bindings template vsymbol)]
    [`(and (list? ~vsymbol) ~@tests)
     bindings]))

(defn- vector-tests-and-bindings
  [template vsymbol]
  (let [[tests bindings] (sequential-tests-and-bindings template vsymbol)]
    [`(and (vector? ~vsymbol) ~@tests)
     bindings]))

(defn- map-tests-and-bindings
  [template vsymbol]
  (let [; First test if the given keys are all present.
	tests (map (fn [[k v]] `(contains? ~vsymbol ~k)) template)
        ; Non-symbols in the template create an equality test with the
	; corresponding value in the object's value list.
	tests (concat tests
	        (map (fn [[k v]] `(= ~v (~k ~vsymbol)))
		     (filter (complement #(symbol? (second %))) template)))
        ; Symbols in the template become bindings to the corresponding
        ; value in the object. However, if a symbol occurs more than once,
        ; only one binding is generated, and equality tests are added
        ; for the other values.
	bindings (reduce (fn [map [key symbol]]
			   (assoc map symbol
				  (conj (get map symbol []) key)))
			 {}
			 (filter #(symbol? (second %)) template))
	tests (concat tests
	        (map (fn [[symbol keys]]
		       (cons `= (map #(list % vsymbol) keys)))
		     (filter #(> (count (second %)) 1) bindings)))
	bindings (mapcat (fn [[symbol keys]]
			   [symbol (list (first keys) vsymbol)])
			 bindings)]
    [`(and (map? ~vsymbol) ~@tests)
     (vec bindings)]))

(defn- tests-and-bindings
  [template vsymbol cfsymbol]
  (cond (symbol? template)
	  (symbol-tests-and-bindings template cfsymbol)
	(seq? template)
	  (if (= (first template) 'quote)
	    (list-tests-and-bindings (second template) vsymbol)
	    (constr-tests-and-bindings template cfsymbol))
	(vector? template)
	  (vector-tests-and-bindings template vsymbol)
	(map? template)
	  (map-tests-and-bindings template vsymbol)
	:else
	  (throw (IllegalArgumentException. "illegal template for match"))))

(defmacro match
  "Given a value and a list of template-expr clauses, evaluate the first
   expr whose template matches the value. There are four kinds of templates:
   1) Lists of the form (tag x1 x2 ...) match instances of types
      whose constructor has the same form as the list.
   2) Quoted lists of the form '(x1 x2 ...) match lists of the same
      length.
   3) Vectors of the form [x1 x2 ...] match vectors of the same length.
   4) Maps of the form {:key1 x1 :key2 x2 ...} match maps that have
      the same keys as the template, but which can have additional keys
      that are not part of the template.
   The values x1, x2, ... can be symbols or non-symbol values. Non-symbols
   must be equal to the corresponding values in the object to be matched.
   Symbols will be bound to the corresponding value in the object in the
   evaluation of expr. If the same symbol occurs more than once in a,
   template the corresponding elements of the object must be equal
   for the template to match."
  [value & clauses]
   (when (odd? (count clauses))
     (throw (Exception. "Odd number of elements in match expression")))
  (let [vsymbol (gensym)
	cfsymbol (gensym)
	terms (mapcat (fn [[template expr]]
			(if (= template :else)
			  [template expr]
			  (let [[tests bindings]
				(tests-and-bindings template vsymbol cfsymbol)]
			    [tests
			     (if (empty? bindings)
			       expr
			       `(let ~bindings ~expr))])))
		      (partition 2 clauses))]
    `(let [~vsymbol ~value
	   ~cfsymbol (constructor-form ~vsymbol)]
       (cond ~@terms))))
