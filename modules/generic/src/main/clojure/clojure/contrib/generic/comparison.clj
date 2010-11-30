;; Generic interfaces for comparison operations

;; by Konrad Hinsen
;; last updated May 25, 2010

;; Copyright (c) Konrad Hinsen, 2009-2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns
  ^{:author "Konrad Hinsen"
     :doc "Generic comparison interface
           This library defines generic versions of = < > <= >= zero?
           as multimethods that can be defined for any type. Of the
           greater/less-than relations, types must minimally implement >."}
  clojure.contrib.generic.comparison
  (:refer-clojure :exclude [= < > <= >= zero? pos? neg? min max])
  (:use [clojure.contrib.generic
	 :only (root-type nulary-type nary-type nary-dispatch)]))

;
; zero? pos? neg?
;
(defmulti zero?
  "Return true of x is zero."
  {:arglists '([x])}
  type)

(defmulti pos?
  "Return true of x is positive."
  {:arglists '([x])}
  type)

(defmulti neg?
  "Return true of x is negative."
  {:arglists '([x])}
  type)

;
; Equality
;
(defmulti =
  "Return true if all arguments are equal. The minimal implementation for type
   ::my-type is the binary form with dispatch value [::my-type ::my-type]."
  {:arglists '([x] [x y] [x y & more])}
  nary-dispatch)

(defmethod = root-type
  [x] true)

(defmethod = nary-type
  [x y & more]
  (if (= x y)
    (if (next more)
      (recur y (first more) (next more))
      (= y (first more)))
    false))

;
; Greater-than
;
(defmulti >
  "Return true if each argument is larger than the following ones.
   The minimal implementation for type ::my-type is the binary form
   with dispatch value [::my-type ::my-type]."
  {:arglists '([x] [x y] [x y & more])}
  nary-dispatch)

(defmethod > root-type
  [x] true)

(defmethod > nary-type
  [x y & more]
  (if (> x y)
    (if (next more)
      (recur y (first more) (next more))
      (> y (first more)))
    false))

;
; Less-than defaults to greater-than with arguments inversed
;
(defmulti <
  "Return true if each argument is smaller than the following ones.
   The minimal implementation for type ::my-type is the binary form
   with dispatch value [::my-type ::my-type]. A default implementation
   is provided in terms of >."
  {:arglists '([x] [x y] [x y & more])}
  nary-dispatch)

(defmethod < root-type
  [x] true)

(defmethod < [root-type root-type]
  [x y]
  (> y x))

(defmethod < nary-type
  [x y & more]
  (if (< x y)
    (if (next more)
      (recur y (first more) (next more))
      (< y (first more)))
    false))

;
; Greater-or-equal defaults to (complement <)
;
(defmulti >=
  "Return true if each argument is larger than or equal to the following
   ones. The minimal implementation for type ::my-type is the binary form
   with dispatch value [::my-type ::my-type]. A default implementation
   is provided in terms of <."
  {:arglists '([x] [x y] [x y & more])}
  nary-dispatch)

(defmethod >= root-type
  [x] true)

(defmethod >= [root-type root-type]
  [x y]
  (not (< x y)))

(defmethod >= nary-type
  [x y & more]
  (if (>= x y)
    (if (next more)
      (recur y (first more) (next more))
      (>= y (first more)))
    false))

;
; Less-than defaults to (complement >)
;
(defmulti <=
  "Return true if each arguments is smaller than or equal to the following
   ones. The minimal implementation for type ::my-type is the binary form
   with dispatch value [::my-type ::my-type]. A default implementation
   is provided in terms of >."
  {:arglists '([x] [x y] [x y & more])}
  nary-dispatch)

(defmethod <= root-type
  [x] true)

(defmethod <= [root-type root-type]
  [x y]
  (not (> x y)))

(defmethod <= nary-type
  [x y & more]
  (if (<= x y)
    (if (next more)
      (recur y (first more) (next more))
      (<= y (first more)))
    false))

;
; Implementations for Clojure's built-in types
;
(defmethod zero? java.lang.Number
  [x]
  (clojure.core/zero? x))

(defmethod pos? java.lang.Number
  [x]
  (clojure.core/pos? x))

(defmethod neg? java.lang.Number
  [x]
  (clojure.core/neg? x))

(defmethod = [Object Object]
  [x y]
  (clojure.core/= x y))

(defmethod > [java.lang.Number java.lang.Number]
  [x y]
  (clojure.core/> x y))

(defmethod < [java.lang.Number java.lang.Number]
  [x y]
  (clojure.core/< x y))

(defmethod >= [java.lang.Number java.lang.Number]
  [x y]
  (clojure.core/>= x y))

(defmethod <= [java.lang.Number java.lang.Number]
  [x y]
  (clojure.core/<= x y))

;
; Functions defined in terms of the comparison operators
;
(defn max
  "Returns the greatest of its arguments. Like clojure.core/max except that
   is uses generic comparison functions implementable for any data type."
  ([x] x)
  ([x y] (if (> x y) x y))
  ([x y & more]
   (reduce max (max x y) more)))

(defn min
  "Returns the least of its arguments. Like clojure.core/min except that
   is uses generic comparison functions implementable for any data type."
  ([x] x)
  ([x y] (if (< x y) x y))
  ([x y & more]
   (reduce min (min x y) more)))
