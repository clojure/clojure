;; Test routines for monads.clj

;; by Konrad Hinsen
;; last updated March 28, 2009

;; Copyright (c) Konrad Hinsen, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.test-monads
  (:use [clojure.test :only (deftest is are run-tests)]
	[clojure.contrib.monads
	 :only (with-monad domonad m-lift m-seq m-chain
		sequence-m maybe-m state-m maybe-t sequence-t)]))

(deftest sequence-monad
  (with-monad sequence-m
    (are [a b] (= a b)
      (domonad [x (range 3) y (range 2)] (+ x y))
        '(0 1 1 2 2 3)
      (domonad [x (range 5) y (range (+ 1 x)) :when  (= (+ x y) 2)] (list x y))
        '((1 1) (2 0))
      ((m-lift 2 #(list %1 %2)) (range 3) (range 2))
        '((0 0) (0 1) (1 0) (1 1) (2 0) (2 1))
      (m-seq (replicate 3 (range 2)))
        '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))
      ((m-chain (replicate 3 range)) 5)
        '(0 0 0 1 0 0 1 0 1 2)
      (m-plus (range 3) (range 2))
        '(0 1 2 0 1))))

(deftest maybe-monad
  (with-monad maybe-m
    (let [m+ (m-lift 2 +)
          mdiv (fn [x y] (domonad [a x  b y  :when (not (zero? b))] (/ a b)))]
      (are [a b] (= a b)
        (m+ (m-result 1) (m-result 3))
	  (m-result 4)
        (mdiv (m-result 1) (m-result 3))
	  (m-result (/ 1 3))
        (m+ 1 (mdiv (m-result 1) (m-result 0)))
	  m-zero
	(m-plus m-zero (m-result 1) m-zero (m-result 2))
	  (m-result 1)))))

(deftest seq-maybe-monad
  (with-monad (maybe-t sequence-m)
    (letfn [(pairs [xs] ((m-lift 2 #(list %1 %2)) xs xs))]
      (are [a b] (= a b)
        ((m-lift 1 inc) (for [n (range 10)] (when (odd? n) n)))
          '(nil 2 nil 4 nil 6 nil 8 nil 10)
        (pairs (for [n (range 5)] (when (odd? n) n)))
          '(nil nil (1 1) nil (1 3) nil nil nil (3 1) nil (3 3) nil nil)))))

(deftest state-maybe-monad
  (with-monad (maybe-t state-m)
    (is (= (for [[a b c d] (list [1 2 3 4] [nil 2 3 4] [ 1 nil 3 4]
				 [nil nil 3 4] [1 2 nil nil])]
	     (let [f (domonad
		       [x (m-plus (m-result a) (m-result b))
			y (m-plus (m-result c) (m-result d))]
		       (+ x y))]
	       (f :state)))
	   (list [4 :state] [5 :state] [4 :state] [nil :state] [nil :state])))))

(deftest state-seq-monad
  (with-monad (sequence-t state-m)
    (is (= (let [[a b c d] [1 2 10 20]
		 f (domonad
		     [x (m-plus (m-result a) (m-result b))
		      y (m-plus (m-result c) (m-result d))]
		     (+ x y))]
	     (f :state)))
	(list [(list 11 21 12 22) :state]))))
