;; Generic interfaces for arithmetic operations

;; by Konrad Hinsen
;; last updated March 13, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.generic.arithmetic
  "Generic arithmetic interface

   NOTE: This library is VERY experimental. It WILL change significantly
   with future release.

   This library defines generic versions of + - * / as multimethods
   that can be defined for any type. The minimal required implementations
   for a type are binary + and * plus unary - and /. Everything else
   is derived from these automatically. Explicit binary definitions
   for - and / can be provided for efficiency reasons."
  (:use [clojure.contrib.types :only (defadt)])
  (:refer-clojure :exclude [+ - * /]))

;
; A dispatch function that separates nulary, unary, binary, and
; higher arity calls and also selects on type for unary and binary
; calls.
;
(defn- nary-dispatch
  ([] ::nulary)
  ([x] (type x))
  ([x y]
     [(type x) (type y)])
  ([x y & more] ::n-ary))

;
; Universal zero and one values
;
(defadt ::zero zero)
(defadt ::one one)

;
; We can't use [::binary :default], so we need to define a root type
; of the type hierarcy. The derivation for Object covers all classes,
; but all non-class types will need an explicit derive clause.
; Ultimately, a macro might take care of this.
;
(derive Object ::any)
(derive ::zero ::any)
(derive ::one ::any)

;
; Addition
;
; The minimal implementation is for [::binary my-type]. It is possible
; in principle to implement [::unary my-type] as well, though this
; doesn't make any sense.
;
(defmulti + nary-dispatch)

(defmethod + ::nulary
  []
  zero)

(defmethod + ::any
  [x] x)

(defmethod + [::any ::zero]
  [x y] x)

(defmethod + [::zero ::any]
  [x y] y)

(defmethod + ::n-ary
  [x y & more]
  (if more
    (recur (+ x y) (first more) (next more))
    (+ x y)))

;
; Subtraction
;
; The minimal implementation is for [::unary my-type]. A default binary
; implementation is provided as (+ x (- y)), but it is possible to
; implement [::unary my-type] explicitly for efficiency reasons.
;
(defmulti - nary-dispatch)

(defmethod - ::nulary
  []
  (throw (java.lang.IllegalArgumentException.
	  "Wrong number of arguments passed")))

(defmethod - [::any ::zero]
  [x y] x)

(defmethod - [::zero ::any]
  [x y] (- y))

(defmethod - [::any ::any]
  [x y] (+ x (- y)))

(defmethod - ::n-ary
  [x y & more]
  (if more
    (recur (- x y) (first more) (next more))
    (- x y)))

;
; Multiplication
;
; The minimal implementation is for [::binary my-type]. It is possible
; in principle to implement [::unary my-type] as well, though this
; doesn't make any sense.
;
(defmulti * nary-dispatch)

(defmethod * ::nulary
  []
  one)

(defmethod * ::any
  [x] x)

(defmethod * [::any ::one]
  [x y] x)

(defmethod * [::one ::any]
  [x y] y)

(defmethod * ::n-ary
  [x y & more]
  (if more
    (recur (* x y) (first more) (next more))
    (* x y)))

;
; Division
;
; The minimal implementation is for [::unary my-type]. A default binary
; implementation is provided as (* x (/ y)), but it is possible to
; implement [::unary my-type] explicitly for efficiency reasons.
;
(defmulti / nary-dispatch)

(defmethod / ::nulary
  []
  (throw (java.lang.IllegalArgumentException.
	  "Wrong number of arguments passed")))

(defmethod / [::any ::one]
  [x y] x)

(defmethod / [::one ::any]
  [x y] (/ y))

(defmethod / [::any ::any]
  [x y] (* x (/ y)))

(defmethod / ::n-ary
  [x y & more]
  (if more
    (recur (/ x y) (first more) (next more))
    (/ x y)))

;
; Minimal implementations for java.lang.Number
;
(defmethod + [java.lang.Number java.lang.Number]
  [x y] (clojure.core/+ x y))

(defmethod - java.lang.Number
  [x] (clojure.core/- x))

(defmethod * [java.lang.Number java.lang.Number]
  [x y] (clojure.core/* x y))

(defmethod / java.lang.Number
  [x] (clojure.core// x))

