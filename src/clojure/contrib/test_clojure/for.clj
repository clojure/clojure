;;  Copyright (c) Chris Houser. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  Tests for the Clojure 'for' macro
;;
;;  by Chouser, http://chouser.n01se.net
;;  Created Dec 2008

(ns clojure.contrib.test-clojure.for
  (:use clojure.contrib.test-is))

(deftest Docstring-Example
  (is (= (take 100 (for [x (range 100000000)
                         y (range 1000000) :while (< y x)]
                     [x y]))
         '([1 0] [2 0] [2 1] [3 0] [3 1] [3 2] [4 0] [4 1] [4 2] [4 3]
           [5 0] [5 1] [5 2] [5 3] [5 4]
           [6 0] [6 1] [6 2] [6 3] [6 4] [6 5]
           [7 0] [7 1] [7 2] [7 3] [7 4] [7 5] [7 6]
           [8 0] [8 1] [8 2] [8 3] [8 4] [8 5] [8 6] [8 7]
           [9 0] [9 1] [9 2] [9 3] [9 4] [9 5] [9 6] [9 7] [9 8]
           [10 0] [10 1] [10 2] [10 3] [10 4] [10 5] [10 6] [10 7] [10 8] [10 9]
           [11 0] [11 1] [11 2] [11 3] [11 4] [11 5] [11 6] [11 7] [11 8] [11 9]
             [11 10]
           [12 0] [12 1] [12 2] [12 3] [12 4] [12 5] [12 6] [12 7] [12 8] [12 9]
             [12 10] [12 11]
           [13 0] [13 1] [13 2] [13 3] [13 4] [13 5] [13 6] [13 7] [13 8] [13 9]
             [13 10] [13 11] [13 12]
           [14 0] [14 1] [14 2] [14 3] [14 4] [14 5] [14 6] [14 7] [14 8]))))

(defmacro deftest-both [txt & ises]
  `(do
     (deftest ~(symbol (str "For-" txt)) ~@ises)
     (deftest ~(symbol (str "Doseq-" txt))
              ~@(map (fn [[x-is [x-= [x-for binds body] value]]]
                       (when (and (= x-is 'is) (= x-= '=) (= x-for 'for))
                         `(is (= (let [acc# (atom [])]
                                   (doseq ~binds (swap! acc# conj ~body))
                                   @acc#)
                                 ~value))))
                     ises))))

(deftest-both When
  (is (= (for [x (range 10) :when (odd? x)] x) '(1 3 5 7 9)))
  (is (= (for [x (range 4) y (range 4) :when (odd? y)] [x y])
         '([0 1] [0 3] [1 1] [1 3] [2 1] [2 3] [3 1] [3 3])))
  (is (= (for [x (range 4) y (range 4) :when (odd? x)] [x y])
         '([1 0] [1 1] [1 2] [1 3] [3 0] [3 1] [3 2] [3 3])))
  (is (= (for [x (range 4) :when (odd? x) y (range 4)] [x y])
         '([1 0] [1 1] [1 2] [1 3] [3 0] [3 1] [3 2] [3 3])))
  (is (= (for [x (range 5) y (range 5) :when (< x y)] [x y])
         '([0 1] [0 2] [0 3] [0 4] [1 2] [1 3] [1 4] [2 3] [2 4] [3 4]))))

(defn only
  "Returns a lazy seq of increasing ints starting at 0.  Trying to get
  the nth+1 value of the seq throws an exception.  This is meant to
  help detecting over-eagerness in lazy seq consumers."
  [n]
  (lazy-cat (range n)
            (throw (Exception. "consumer went too far in lazy seq"))))

(deftest-both While
  (is (= (for [x (only 6) :while (< x 5)] x) '(0 1 2 3 4)))
  (is (= (for [x (range 4) y (only 4) :while (< y 3)] [x y])
         '([0 0] [0 1] [0 2] [1 0] [1 1] [1 2]
           [2 0] [2 1] [2 2] [3 0] [3 1] [3 2])))
  (is (= (for [x (range 4) y (range 4) :while (< x 3)] [x y])
         '([0 0] [0 1] [0 2] [0 3] [1 0] [1 1] [1 2] [1 3]
           [2 0] [2 1] [2 2] [2 3])))
  (is (= (for [x (only 4) :while (< x 3) y (range 4)] [x y])
         '([0 0] [0 1] [0 2] [0 3] [1 0] [1 1] [1 2] [1 3]
           [2 0] [2 1] [2 2] [2 3])))
  (is (= (for [x (range 4) y (range 4) :while (even? x)] [x y])
         '([0 0] [0 1] [0 2] [0 3] [2 0] [2 1] [2 2] [2 3])))
  (is (= (for [x (only 2) :while (even? x) y (range 4)] [x y])
         '([0 0] [0 1] [0 2] [0 3])))
  (is (= (for [x (range 4) y (only 4) :while (< y x)] [x y])
         '([1 0] [2 0] [2 1] [3 0] [3 1] [3 2]))))

(deftest-both While-and-When
  (is (= (for [x (only 6) :while (< x 5) y (range 4) :when (odd? y)] [x y])
         '([0 1] [0 3] [1 1] [1 3] [2 1] [2 3] [3 1] [3 3] [4 1] [4 3])))
  (is (= (for [x (range 4) :when (odd? x) y (only 6) :while (< y 5)] [x y])
         '([1 0] [1 1] [1 2] [1 3] [1 4] [3 0] [3 1] [3 2] [3 3] [3 4])))
  (is (= (for [x (only 6) :while (< x 5) y (range 4) :when (odd? (+ x y))]
           [x y])
         '([0 1] [0 3] [1 0] [1 2] [2 1] [2 3] [3 0] [3 2] [4 1] [4 3])))
  (is (= (for [x (range 4) :when (odd? x) y (only 2) :while (odd? (+ x y))]
           [x y])
         '([1 0] [3 0]))))

(deftest-both While-and-When-Same-Binding
  (is (= (for [x (only 6) :while (< x 5) :when (odd? x)] x) '(1 3)))
  (is (= (for [x (only 6)
               :while (< x 5) ; if :while is false, :when should not be evaled
               :when (do (if (< x 5) (odd? x)))] x) '(1 3)))
  (is (= (for [a (range -2 5)
               :when (not= a 0) ; :when may guard :while
               :while (> (Math/abs (/ 1.0 a)) 1/3)] a) '(-2 -1 1 2))))

(deftest-both Nesting
  (is (= (for [x '(a b) y (interpose x '(1 2)) z (list x y)] [x y z])
         '([a 1 a] [a 1 1] [a a a] [a a a] [a 2 a] [a 2 2]
           [b 1 b] [b 1 1] [b b b] [b b b] [b 2 b] [b 2 2])))
  (is (= (for [x ['a nil] y [x 'b]] [x y])
         '([a a] [a b] [nil nil] [nil b]))))

(deftest-both Destructuring
  (is (= (for [{:syms [a b c]} (map #(zipmap '(a b c) (range % 5)) (range 3))
               x [a b c]]
           (Integer. (str a b c x)))
         '(120 121 122 1231 1232 1233 2342 2343 2344))))

(deftest-both Let
  (is (= (for [x (range 3) y (range 3) :let [z (+ x y)] :when (odd? z)] [x y z])
         '([0 1 1] [1 0 1] [1 2 3] [2 1 3])))
  (is (= (for [x (range 6) :let [y (rem x 2)] :when (even? y) z [8 9]] [x z])
         '([0 8] [0 9] [2 8] [2 9] [4 8] [4 9]))))
