;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Monte-Carlo application examples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns
  #^{:author "Konrad Hinsen"
     :skip-wiki true
     :doc "Examples for monte carlo methods"}
  clojure.contrib.probabilities.random.examples-monte-carlo
  (:require [clojure.contrib.generic.collection :as gc])
  (:use [clojure.contrib.probabilities.random-numbers
	 :only (lcg rand-stream)])
  (:use [clojure.contrib.probabilities.finite-distributions
	 :only (uniform)])
  (:use [clojure.contrib.probabilities.monte-carlo
	 :only (random-stream discrete interval normal lognormal exponential
		n-sphere
		sample sample-sum sample-mean sample-mean-variance)]
	:reload)
  (:use [clojure.contrib.monads
	:only (domonad state-m)]))

; Create a linear congruential generator
(def urng (lcg 259200 7141 54773 1))

;; Use Clojure's built-in random number generator
;(def urng rand-stream)

; Sample transformed distributions
(defn sample-distribution
  [n rt]
  (take n (gc/seq (random-stream rt urng))))

; Interval [-2, 2)
(sample-distribution 10 (interval -2 2))
; Compare with a direct transformation
(= (sample-distribution 10 (interval -2 2))
   (map (fn [x] (- (* 4 x) 2)) (take 10 (gc/seq urng))))

; Normal distribution
(sample-distribution 10 (normal 0 1))

; Log-Normal distribution
(sample-distribution 10 (lognormal 0 1))

; Exponential distribution
(sample-distribution 10 (exponential 1))

; n-sphere distribution
(sample-distribution 10 (n-sphere 2 1))

; Discrete distribution
(sample-distribution 10 (discrete (uniform (range 1 7))))

; Compose distributions in the state monad
(def sum-two-dists
  (domonad state-m
    [r1 (interval -2 2)
     r2 (normal 0 1)]
    (+ r1 r2)))

(sample-distribution 10 sum-two-dists)

; Distribution transformations
(sample-distribution  5 (sample 2 (interval -2 2)))
(sample-distribution 10 (sample-sum 10 (interval -2 2)))
(sample-distribution 10 (sample-mean 10 (interval -2 2)))
(sample-distribution 10 (sample-mean-variance 10 (interval -2 2)))

