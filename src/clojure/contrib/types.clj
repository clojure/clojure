;; Data types

;; by Konrad Hinsen
;; last updated March 15, 2009

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
  ([type-tag constructor-name]
   `(deftype ~type-tag ~constructor-name
      clojure.core/identity clojure.core/list))
  ([type-tag constructor-name constructor]
   `(deftype ~type-tag ~constructor-name
      ~constructor clojure.core/list))
  ([type-tag constructor-name constructor deconstructor]
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
  (let [[name & args] (constructor-form o)]
    (print-method (cons (unqualified-symbol name) args) w)))

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
	     (cons `(seq? ~vsymbol)
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
	cfsymbol (gensym)
	terms (apply concat
		     (map (fn [[template expr]]
			    (if (= template :else)
			      [template expr]
			      (let [[tests bindings]
				    (tests-and-bindings template cfsymbol)]
				[tests
				 (if (empty? bindings)
				   expr
				   `(let ~bindings ~expr))])))
			  (partition 2 clauses)))]
    `(let [~vsymbol ~value
	   ~cfsymbol (constructor-form ~vsymbol)]
       (cond ~@terms))))
 