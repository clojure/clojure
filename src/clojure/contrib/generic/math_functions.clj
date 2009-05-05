;; Generic interfaces for mathematical functions

;; by Konrad Hinsen
;; last updated May 5, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns
  #^{:author "Konrad Hinsen"
     :doc "Generic math function interface
           This library defines generic versions of common mathematical
           functions such as sqrt or sin as multimethods that can be
           defined for any type."}
  clojure.contrib.generic.math-functions
  (:use [clojure.contrib.def :only (defmacro-)])
  (:require [clojure.contrib.generic.arithmetic :as ga]
	    [clojure.contrib.generic.comparison :as gc]))

(defmacro- defmathfn-1
  [name]
  (let [java-symbol (symbol "java.lang.Math" (str name))]
    `(do
       (defmulti ~name
	 ~(str "Return the " name " of x.")
	 {:arglists '([~'x])}
	 type)
       (defmethod ~name java.lang.Number
	 [~'x]
	 (~java-symbol ~'x)))))

(defn- two-types [x y] [(type x) (type y)])

(defmacro- defmathfn-2
  [name]
  (let [java-symbol (symbol "java.lang.Math" (str name))]
    `(do
       (defmulti ~name
	 ~(str "Return the " name " of x and y.")
	 {:arglists '([~'x ~'y])}
	 two-types)
       (defmethod ~name [java.lang.Number java.lang.Number]
	 [~'x ~'y]
	 (~java-symbol ~'x ~'y)))))

; List of math functions taken from
; http://java.sun.com/j2se/1.4.2/docs/api/java/lang/Math.html
(defmathfn-1 abs)
(defmathfn-1 acos)
(defmathfn-1 asin)
(defmathfn-1 atan)
(defmathfn-2 atan2)
(defmathfn-1 ceil)
(defmathfn-1 cos)
(defmathfn-1 exp)
(defmathfn-1 floor)
(defmathfn-1 log)
(defmathfn-2 pow)
(defmathfn-1 rint)
(defmathfn-1 round)
(defmathfn-1 sin)
(defmathfn-1 sqrt)
(defmathfn-1 tan)

;
; Sign
;
(defmulti sgn
  "Return the sign of x (-1, 0, or 1)."
  {:arglists '([x])}
  type)

(defmethod sgn :default
  [x]
  (cond (gc/zero? x) 0
	(gc/> x 0) 1
	:else -1))

;
; Conjugation
;
(defmulti conjugate
  "Return the conjugate of x."
  {:arglists '([x])}
  type)

(defmethod conjugate :default
  [x] x)

;
; Square
;
(defmulti sqr
  "Return the square of x."
  {:arglists '([x])}
  type)

(defmethod sqr :default
  [x]
  (ga/* x x))

;
; Approximate equality for use with floating point types
;
(defn approx=
  "Return true if the absolute value of the difference between x and y
   is less than eps."
  [x y eps]
  (gc/< (abs (ga/- x y)) eps))
