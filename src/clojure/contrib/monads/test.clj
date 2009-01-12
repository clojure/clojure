;; Test routines for monads.clj

;; by Konrad Hinsen
;; last updated January 12, 2009

;; Copyright (c) Konrad Hinsen, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.test-monads
  (:use clojure.contrib.test-is
	clojure.contrib.monads
	clojure.contrib.macros))

(deftest sequence-monad
  (with-monad sequence 
    (are (= _1 _2)
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
  (with-monad maybe
    (let [m+ (m-lift 2 +)
          mdiv (fn [x y] (domonad [a x  b y  :when (not (zero? b))] (/ a b)))]
      (are (= _1 _2)
        (m+ (m-result 1) (m-result 3))
	  (m-result 4)
        (mdiv (m-result 1) (m-result 3))
	  (m-result (/ 1 3))
        (m+ 1 (mdiv (m-result 1) (m-result 0)))
	  m-zero
	(m-plus m-zero (m-result 1) m-zero (m-result 2))
	  (m-result 1)))))

(deftest seq-maybe-monad
  (with-monad (maybe-t sequence)
    (letfn [pairs [xs] ((m-lift 2 #(list %1 %2)) xs xs)]
      (are (= _1 _2)
        ((m-lift 1 inc) (for [n (range 10)] (when (odd? n) n)))
          '(nil 2 nil 4 nil 6 nil 8 nil 10)
        (pairs (for [n (range 5)] (when (odd? n) n)))
          '(nil nil (1 1) nil (1 3) nil nil nil (3 1) nil (3 3) nil nil)))))

(run-tests)
