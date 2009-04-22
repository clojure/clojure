;; Monte-Carlo algorithms

;; by Konrad Hinsen
;; last updated April 21, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.probabilities.monte-carlo
  "Monte-Carlo method support

   Monte-Carlo methods transform an input random number stream
   (usually having a continuous uniform distribution in the interval [0, 1))
   into a random number stream whose distribution satisfies certain
   conditions (usually the expectation value is equal to some desired
   quantity). They are thus transformations from one probability distribution
   to another one.

   This library represents a Monte-Carlo method by a function that takes
   as input the state of a random number stream with uniform distribution
   (see clojure.contrib.probabilities.random-numbers) and returns a
   vector containing one sample value of the desired output distribution 
   and the final state of the input random number stream. Such functions
   are state monad values and can be composed using operations defined
   in clojure.contrib.monads.
   "
  (:use [clojure.contrib.macros :only (const)])
  (:use [clojure.contrib.types :only (deftype)])
  (:use [clojure.contrib.stream-utils :only (defstream stream-next)])
  (:use [clojure.contrib.monads
	 :only (with-monad state-m m-lift m-seq m-fmap)])
  (:require [clojure.contrib.generic.arithmetic :as ga])
  (:require [clojure.contrib.accumulators :as acc]))

;; Random number transformers and random streams
;;
;; A random number transformer is a function that takes a random stream
;; state as input and returns the next value from the transformed stream
;; plus the new state of the input stream. Random number transformers
;; are thus state monad values.
;;
;; Distributions are implemented as random number transformers that
;; transform a uniform distribution in the interval [0, 1) to the
;; desired distribution. Composition of such distributions allows
;; the realization of any kind of Monte-Carlo algorithm. The result
;; of such a composition is always again a distribution.
;;
;; Random streams are defined by a random number transformer and an
;; input random number stream. If the randon number transformer represents
;; a distribution, the input stream must have a uniform distribution
;; in the interval [0, 1).

; Random stream definition
(deftype ::random-stream random-stream
  "Define a random stream by a distribution and the state of a
   random number stream with uniform distribution in [0, 1)."
  {:arglists '([distribution random-stream-state])}
  (fn [d rs] (list d rs)))

(defstream ::random-stream
  [[d rs]]
  (let [[r nrs] (d rs)]
    [r (random-stream d nrs)]))

; Rejection of values is used in the construction of distributions
(defn reject
  "Return the distribution that results from rejecting the values from
   dist that do not satisfy predicate p."
  [p dist]
  (fn [rs]
    (let [[r nrs] (dist rs)]
      (if (p r)
	(recur nrs)
	[r nrs]))))

; Draw a value from a discrete distribution given as a map from
; values to probabilities.
; (see clojure.contrib.probabilities.finite-distributions)
(with-monad state-m
  (defn discrete
    "A discrete distribution, defined by a map dist mapping values
     to probabilities. The sum of probabilities must be one."
    [dist]
    (letfn [(pick-at-level [l dist-items]
	      (let [[[x p] & rest-dist] dist-items]
		(if (> p l)
		  x
		  (recur (- l p) rest-dist))))]
      (m-fmap #(pick-at-level % (seq dist)) stream-next))))

; Uniform distribution in an finite half-open interval
(with-monad state-m
  (defn interval
    [a b]
    "Transform a sequence of uniform random numbers in the interval [0, 1)
     into a sequence of uniform random numbers in the interval [a, b)."
    (let [d (- b a)
	  f (if (zero? a)
	      (if (= d 1)
		identity
		(fn [r] (* d r)))
	      (if (= d 1)
		(fn [r] (+ a r))
		(fn [r] (+ a (* d r)))))]
      (m-fmap f stream-next))))

; Normal (Gaussian) distribution
(defn normal
  "Transform a sequence urs of uniform random number in the interval [0, 1)
   into a sequence of normal random numbers with mean mu and standard
   deviation sigma."
  [mu sigma]
  ; This function implements the Kinderman-Monahan ratio method:
  ;  A.J. Kinderman & J.F. Monahan
  ;  Computer Generation of Random Variables Using the Ratio of Uniform Deviates
  ;  ACM Transactions on Mathematical Software 3(3) 257-260, 1977
  (fn [rs]
    (let [[u1  rs] (stream-next rs)
	  [u2* rs] (stream-next rs)
	  u2 (- 1. u2*)
	  s (const (* 4 (/ (. Math exp (- 0.5)) (. Math sqrt 2.))))
	  z (* s (/ (- u1 0.5) u2))
	  zz (+ (* 0.25 z z) (. Math log u2))]
      (if (> zz 0)
	(recur rs)
	[(+ mu (* sigma z)) rs]))))

; Lognormal distribution
(with-monad state-m
  (defn lognormal
    "Transform a sequence of uniform random numbesr in the interval [0, 1)
     into a sequence of lognormal random numbers with mean mu and standard
     deviation sigma."
    [mu sigma]
    (m-fmap #(. Math exp %) (normal mu sigma))))

; Exponential distribution
(with-monad state-m
  (defn exponential
    "Transform a sequence of uniform random numbers in the interval [0, 1)
     into a sequence of exponential random numbers with parameter lambda."
    [lambda]
    (when (<= lambda 0)
      (throw (IllegalArgumentException.
  	    "exponential distribution requires a positive argument")))
    (let [neg-inv-lambda (- (/ lambda))
	  ; remove very small numbers to prevent log from returning -Infinity
	  not-too-small  (reject #(< % 1e-323) stream-next)]
      (m-fmap #(* (. Math log %) neg-inv-lambda) not-too-small))))

; Another implementation of the normal distribution. It uses the
; Box-Muller transform, but discards one of the two result values
; at each cycle because the random number transformer interface cannot
; handle two outputs at the same time.
(defn normal-box-muller
  "Transform a sequence of uniform random numbers in the interval [0, 1)
   into a sequence of normal random numbers with mean mu and standard
   deviation sigma."
  [mu sigma]
  (fn [rs]
    (let [[u1 rs] (stream-next rs)
	  [u2 rs] (stream-next rs)
	   v1 (- (* 2.0 u1) 1.0)
	   v2 (- (* 2.0 u2) 1.0)
	   s  (+ (* v1 v1) (* v2 v2))
	   ls (. Math sqrt (/ (* -2.0 (. Math log s)) s))
	   x1 (* v1 ls)
	   x2 (* v2 ls)]
	  (if (or (>= s 1) (= s 0))
	    (recur rs)
	    [x1 rs]))))

; Finite samples from a distribution
(with-monad state-m

    (defn sample
      "Return the distribution of samples of length n from the
       distribution dist"
      [n dist]
      (m-seq (replicate n dist)))

    (defn sample-reduce
      "Returns the distribution of the reduction of f over n samples from the
       distribution dist."
      ([f n dist]
	 (if (zero? n)
	   (m-result (f))
	   (let [m-f    (m-lift 2 f)
		 sample (replicate n dist)]
	     (reduce m-f sample))))
      ([f val n dist]
	 (let [m-f    (m-lift 2 f)
	       m-val  (m-result val)
	       sample (replicate n dist)]
	   (reduce m-f m-val sample))))

    (defn sample-sum
      "Return the distribution of the sum over n samples from the
       distribution dist."
      [n dist]
      (sample-reduce ga/+ n dist))

    (defn sample-mean
      "Return the distribution of the mean over n samples from the
       distribution dist"
      [n dist]
      (let [div-by-n (m-lift 1 #(ga/* % (/ n)))]
	(div-by-n (sample-sum n dist))))

    (defn sample-mean-variance
      "Return the distribution of the mean-and-variance (a vector containing
       the mean and the variance) over n samples from the distribution dist"
      [n dist]
      (let [extract (m-lift 1 (fn [mv] [(:mean mv) (:variance mv)]))]
	(extract (sample-reduce acc/add acc/empty-mean-variance n dist))))

)

; Uniform distribution inside an n-sphere
(with-monad state-m
  (defn n-sphere
    "Return a uniform distribution of n-dimensional vectors inside an
     n-sphere of radius r."
    [n r]
    (let [box-dist    (sample n (interval (- r) r))
	  sq          #(* % %)
	  r-sq        (sq r)
	  vec-sq      #(apply + (map sq %))
	  sphere-dist (reject #(> (vec-sq %) r-sq) box-dist)
	  as-vectors  (m-lift 1 vec)]
      (as-vectors sphere-dist))))

