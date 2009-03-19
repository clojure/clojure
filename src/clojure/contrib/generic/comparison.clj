;; Generic interfaces for comparison operations

;; by Konrad Hinsen
;; last updated March 19, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.generic.comparison
  "Generic comparison interface

   NOTE: This library is VERY experimental. It WILL change significantly
   with future release.

   This library defines generic versions of = < > <= >= zero? as multimethods
   that can be defined for any type."
  (:refer-clojure :exclude [= < > <= >= zero?])
  (:use [clojure.contrib.generic
	 :only (root-type nulary-type nary-type nary-dispatch)]))

;
; zero?
;
(defmulti zero? type)

;
; Equality
;
(defmulti = nary-dispatch)

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
; Implementations for Clojure's built-in types
;
(defmethod zero? java.lang.Number
  [x]
  (clojure.core/zero? x))

(defmethod = [Object Object]
  [x y]
  (clojure.core/= x y))
