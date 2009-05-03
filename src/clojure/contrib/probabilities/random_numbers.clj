;; Random number generators

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
     :doc "Random number streams

           This library provides random number generators with a common
           stream interface. They all produce pseudo-random numbers that are
           uniformly distributed in the interval [0, 1), i.e. 0 is a
           possible value but 1 isn't. For transformations to other
           distributions, see clojure.contrib.probabilities.monte-carlo.

           At the moment, the only generator provided is a rather simple
           linear congruential generator."}
  clojure.contrib.probabilities.random-numbers
  (:use [clojure.contrib.types :only (deftype)])
  (:use [clojure.contrib.stream-utils :only (defstream)])
  (:use [clojure.contrib.def :only (defvar)]))

;; Linear congruential generator
;; http://en.wikipedia.org/wiki/Linear_congruential_generator

(deftype ::lcg lcg
  "Create a linear congruential generator"
  {:arglists '([modulus multiplier increment seed])}
  (fn [modulus multiplier increment seed]
    {:m modulus :a multiplier :c increment :seed seed})
  (fn [s] (map s (list :m :a :c :seed))))

(defstream ::lcg
  [lcg-state]
  (let [{m :m a :a c :c seed :seed} lcg-state
	value (/ (float seed) (float m))
	new-seed (rem (+ c (* a seed)) m)]
    [value (assoc lcg-state :seed new-seed)]))

;; A generator based on Clojure's built-in rand function
;; (and thus random from java.lang.Math)
;; Note that this generator uses an internal mutable state.
;;
;; The state is *not* stored in the stream object and can thus
;; *not* be restored!

(defvar rand-stream (with-meta 'rand {:type ::rand-stream})
  "A random number stream based on clojure.core/rand. Note that this
   generator uses an internal mutable state. The state is thus not stored
   in the stream object and cannot be restored.")

(defstream ::rand-stream
  [dummy-state]
  [(rand) dummy-state])
