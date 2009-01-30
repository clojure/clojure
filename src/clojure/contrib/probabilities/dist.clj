;; Finite probability distributions

;; by Konrad Hinsen
;; last updated January 30, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.probabilities.dist
  (:use clojure.contrib.monads clojure.contrib.macros
	clojure.contrib.monads clojure.contrib.def))

; The probability distribution monad. It is limited to finite probability
; distributions (e.g. there is a finite number of possible value), which
; are represented as maps from values to probabilities.

(defmonad dist
  "Monad describing computations on fuzzy quantities, represented by a finite
   probability distribution for the possible values. A distribution is
   represented by a map from values to probabilities."
  [m-result (fn m-result-dist [v]
       {v 1})
   m-bind   (fn m-bind-dist [mv f]
	      (letfn [add-prob [dist [x p]]
		        (assoc dist x (+ (get dist x 0) p))]
	        (reduce add-prob {}
		        (for [[x p] mv  [y q] (f x)]
			  [y (* q p)]))))
   ])

; Applying the monad transformer maybe-t to the basic dist monad results
; in the cond-dist monad that can handle invalid values. The total probability
; for invalid values ends up as the probability of m-zero (which is nil).
; The function normalize takes this probability out of the distribution and
; re-distributes its weight over the valid values.

(defvar cond-dist
  (maybe-t dist)
  "Variant of the dist monad that can handle undefined values.")

; Normalization

(defn- scale-by
  "Multiply each entry in dist by the scale factor s and remove zero entries."
  [dist s]
  (into {}
	(for [[val p] dist :when (> p 0)]
	  [val (* p s)])))

(defn normalize-cond [cdist]
  "Normalize a probability distribution resulting from a computation in
   the cond-dist monad by re-distributing the weight of the invalid values
   over the valid ones."
  (let [missing (get cdist nil 0)
	dist    (dissoc cdist nil)]
    (cond (zero? missing) dist
	  (= 1 missing)   {}
	  :else (let [scale  (/ 1 (- 1 missing))]
		  (scale-by dist scale)))))

(defn normalize
  "Convert a weight map (e.g. a map of counter values) to a distribution
   by multiplying with a normalization factor. If the map has a key
   :total, its value is assumed to be the sum over all the other values and
   it is used for normalization. Otherwise, the sum is calculated
   explicitly. The :total key is removed from the resulting distribution."
  [weights]
  (let [total (:total weights)
	w (dissoc weights :total)
	s (/ 1 (if (nil? total) (reduce + (vals w)) total))]
    (scale-by w s)))

; Functions that construct distributions

(defn uniform
  "Return a distribution in which each of the elements of coll
   has the same probability."
  [coll]
  (let [n (count coll)
	p (/ 1 n)]
    (into {} (for [x (seq coll)] [x p]))))

(defn choose
  "Construct a distribution from an explicit list of probabilities
   and values. They are given in the form of a vector of probability-value
   pairs. In the last pair, the probability can be given by the keyword
   :else, which stands for 1 minus the total of the other probabilities."
  [& choices]
  (letfn [add-choice [dist [p v]]
	    (cond (nil? p) dist
		  (= p :else)
		       (let [total-p (reduce + (vals dist))]
		         (assoc dist v (- 1 total-p)))
		  :else (assoc dist v p))]
    (reduce add-choice {} (partition 2 choices))))

(with-monad dist

  (defn certainly
    "Returns a distribution in which the single value v has probability 1."
    [v]
    (m-result v))

  (defn join-with
    "Returns the distribution of (f x y) with x from dist1 and y from dist2."
    [f dist1 dist2]
    ((m-lift 2 f) dist1 dist2))

)

(defn cond-prob
  "Returns the conditional probability for the values in dist that satisfy
   the predicate pred."
  [pred dist]
  (normalize-cond
    (with-monad cond-dist
      (m-bind dist (fn [v] (m-result (when (pred v) v)))))))

; Select (with equal probability) N items from a sequence

(defn- nth-and-rest [n xs]
  "Return a list containing the n-th value of xs and the sequence
   obtained by removing the n-th value from xs."
  (let [[h t] (split-at n xs)]
    (list (first t) (concat h (rest t)))))

(with-monad dist

  (defn- select-n [n xs]
    (letfn [select-1 [[s xs]]
	      (uniform (for [i (range (count xs))]
			 (let [[nth rest] (nth-and-rest i xs)]
			   (list (cons nth s) rest))))]
      ((m-chain (replicate n select-1)) (list '() xs))))

  (defn select [n xs]
    "Return the distribution for all possible ordered selections of n elements
     out of xs."
    ((m-lift 1 first) (select-n n xs)))

)

; Find the probability that a given predicate is satisfied

(defn prob
  "Return the probability that the predicate pred is satisfied in the
   distribution dist, i.e. the sum of the probabilities of the values
   that satisfy pred."
  [pred dist]
  (apply + (for [[x p] dist :when (pred x)] p)))

