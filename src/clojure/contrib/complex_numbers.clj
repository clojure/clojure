;; Complex numbers

;; by Konrad Hinsen
;; last updated March 19, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.complex-numbers
  "Complex numbers

   NOTE: This library is in evolution. It may change with future releases."
  (:use [clojure.contrib.types :only (deftype)]
	[clojure.contrib.generic :only (root-type)])
  (:require [clojure.contrib.generic.arithmetic :as ga]
	    [clojure.contrib.generic.comparison :as gc]
	    [clojure.contrib.generic.math-functions :as gm]))

;
; Complex numbers are represented as struct maps. The real and imaginary
; parts can be of any type for which arithmetic and maths functions
; are defined.
;
(defstruct complex-struct :real :imag)

;
; The general complex number type
;
(deftype ::complex complex
  (fn [real imag] (struct complex-struct real imag))
  (fn [c] (vals c)))

(derive ::complex root-type)

;
; A specialized subtype for pure imaginary numbers. Introducing this type
; reduces the number of operations by eliminating additions with and
; multiplications by zero.
;
(deftype ::pure-imaginary imaginary
  (fn [imag] (struct complex-struct 0 imag))
  (fn [c] (list (:imag c))))

(derive ::pure-imaginary ::complex)

;
; Extraction of real and imaginary parts
;
(def real (accessor complex-struct :real))
(def imag (accessor complex-struct :imag))

;
; Equality and zero test
;
(defmethod gc/zero? ::complex
  [x]
  (let [[rx ix] (vals x)]
    (and (zero? rx) (zero? ix))))

(defmethod gc/= [::complex ::complex]
  [x y]
  (let [[rx ix] (vals x)
	[ry iy] (vals y)]
    (and (gc/= rx ry) (gc/= ix iy))))

(defmethod gc/= [::pure-imaginary ::pure-imaginary]
  [x y]
  (gc/= (imag x) (imag y)))

(defmethod gc/= [::complex ::pure-imaginary]
  [x y]
  (let [[rx ix] (vals x)]
    (and (gc/zero? rx) (gc/= ix (imag y)))))

(defmethod gc/= [::pure-imaginary ::complex]
  [x y]
  (let [[ry iy] (vals y)]
    (and (gc/zero? ry) (gc/= (imag x) iy))))

(defmethod gc/= [::complex root-type]
  [x y]
  (let [[rx ix] (vals x)]
    (and (gc/zero? ix) (gc/= rx y))))

(defmethod gc/= [root-type ::complex]
  [x y]
  (let [[ry iy] (vals y)]
    (and (gc/zero? iy) (gc/= x ry))))

(defmethod gc/= [::pure-imaginary root-type]
  [x y]
  (and (gc/zero? (imag x)) (gc/zero? y)))

(defmethod gc/= [root-type ::pure-imaginary]
  [x y]
  (and (gc/zero? x) (gc/zero? (imag y))))

;
; Addition
;
(defmethod ga/+ [::complex ::complex]
  [x y]
  (let [[rx ix] (vals x)
	[ry iy] (vals y)]
    (complex (ga/+ rx ry) (ga/+ ix iy))))

(defmethod ga/+ [::pure-imaginary ::pure-imaginary]
  [x y]
  (imaginary (ga/+ (imag x) (imag y))))

(defmethod ga/+ [::complex ::pure-imaginary]
  [x y]
  (let [[rx ix] (vals x)]
    (complex rx (ga/+ ix (imag y)))))

(defmethod ga/+ [::pure-imaginary ::complex]
  [x y]
  (let [[ry iy] (vals y)]
    (complex ry (ga/+ (imag x) iy))))

(defmethod ga/+ [::complex root-type]
  [x y]
  (let [[rx ix] (vals x)]
    (complex (ga/+ rx y) ix)))

(defmethod ga/+ [root-type ::complex]
  [x y]
  (let [[ry iy] (vals y)]
    (complex (ga/+ x ry) iy)))

(defmethod ga/+ [::pure-imaginary root-type]
  [x y]
  (complex y (imag x)))

(defmethod ga/+ [root-type ::pure-imaginary]
  [x y]
  (complex x (imag y)))

;
; Negation
;
(defmethod ga/- ::complex
  [x]
  (let [[rx ix] (vals x)]
    (complex (ga/- rx) (ga/- ix))))

(defmethod ga/- ::pure-imaginary
  [x]
  (imaginary (ga/- (imag x))))

;
; Subtraction is automatically supplied by ga/-, optimized implementations
; can be added later...
;

;
; Multiplication
;
(defmethod ga/* [::complex ::complex]
  [x y]
  (let [[rx ix] (vals x)
	[ry iy] (vals y)]
    (complex (ga/- (ga/* rx ry) (ga/* ix iy))
	     (ga/+ (ga/* rx iy) (ga/* ix ry)))))

(defmethod ga/* [::pure-imaginary ::pure-imaginary]
  [x y]
  (ga/- (ga/* (imag x) (imag y))))

(defmethod ga/* [::complex ::pure-imaginary]
  [x y]
  (let [[rx ix] (vals x)
	iy (imag y)]
    (complex (ga/- (ga/* ix iy))
	     (ga/* rx iy))))

(defmethod ga/* [::pure-imaginary ::complex]
  [x y]
  (let [ix (imag x)
	[ry iy] (vals y)]
    (complex (ga/- (ga/* ix iy))
	     (ga/* ix ry))))

(defmethod ga/* [::complex root-type]
  [x y]
  (let [[rx ix] (vals x)]
    (complex (ga/* rx y) (ga/* ix y))))

(defmethod ga/* [root-type ::complex]
  [x y]
  (let [[ry iy] (vals y)]
    (complex (ga/* x ry) (ga/* x iy))))

(defmethod ga/* [::pure-imaginary root-type]
  [x y]
  (imaginary (ga/* (imag x) y)))

(defmethod ga/* [root-type ::pure-imaginary]
  [x y]
  (imaginary (ga/* x (imag y))))

;
; Inversion
;
(ga/defmethod* ga / ::complex
  [x]
  (let [[rx ix] (vals x)
	den ((ga/qsym ga /) (ga/+ (ga/* rx rx) (ga/* ix ix)))]
    (complex (ga/* rx den) (ga/- (ga/* ix den)))))

(ga/defmethod* ga / ::pure-imaginary
  [x]
  (imaginary (ga/- ((ga/qsym ga /) (imag x)))))

;
; Division is automatically supplied by ga//, optimized implementations
; can be added later...
;

