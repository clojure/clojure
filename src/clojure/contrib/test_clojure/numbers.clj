;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  clojure.contrib.test-clojure.numbers
;;
;;  scgilardi (gmail)
;;  Created 30 October 2008

(ns clojure.contrib.test-clojure.numbers
  (:use clojure.contrib.test-is))

(deftest Coerced-Byte
  (let [v (byte 3)]
    (are _
     (instance? Byte v)
     (number? v)
     (integer? v)
     (rational? v))))

(deftest Coerced-Short
  (let [v (short 3)]
    (are _
     (instance? Short v)
     (number? v)
     (integer? v)
     (rational? v))))

(deftest Coerced-Integer
  (let [v (int 3)]
    (are _
     (instance? Integer v)
     (number? v)
     (integer? v)
     (rational? v))))

(deftest Coerced-Long
  (let [v (long 3)]
    (are _
     (instance? Long v)
     (number? v)
     (integer? v)
     (rational? v))))

(deftest Coerced-BigInteger
  (let [v (bigint 3)]
    (are _
     (instance? BigInteger v)
     (number? v)
     (integer? v)
     (rational? v))))

(deftest Coerced-Float
  (let [v (float 3)]
    (are _
     (instance? Float v)
     (number? v)
     (float? v))))

(deftest Coerced-Double
  (let [v (double 3)]
    (are _
     (instance? Double v)
     (number? v)
     (float? v))))

(deftest Coerced-BigDecimal
  (let [v (bigdec 3)]
    (are _
     (instance? BigDecimal v)
     (number? v)
     (decimal? v)
     (not (float? v)))))


;; *** Number predicates ***

;; pos? zero? neg?

(deftest test-pos?-zero?-neg?
  (let [nums [[(byte 2) (byte 0) (byte -2)]
              [(short 3) (short 0) (short -3)]
              [(int 4) (int 0) (int -4)]
              [(long 5) (long 0) (long -5)]
              [(bigint 6) (bigint 0) (bigint -6)]
              [(float 7) (float 0) (float -7)]
              [(double 8) (double 0) (double -8)]
              [(bigdec 9) (bigdec 0) (bigdec -9)]
              [2/3 0 -2/3]]
        pred-result [[pos?  [true false false]]
                     [zero? [false true false]]
                     [neg?  [false false true]]] ]
    (doseq [pr pred-result]
      (doseq [n nums]
        (is (= (map (first pr) n) (second pr))
          (pr-str (first pr) n))))))


;; even? odd?

(deftest test-even?
  (are _
    (even? -4)
    (not (even? -3))
    (even? 0)
    (not (even? 5))
    (even? 8))
  (is (thrown? ArithmeticException (even? 1/2)))
  (is (thrown? ArithmeticException (even? (double 10)))))

(deftest test-odd?
  (are _
    (not (odd? -4))
    (odd? -3)
    (not (odd? 0))
    (odd? 5)
    (not (odd? 8)))
  (is (thrown? ArithmeticException (odd? 1/2)))
  (is (thrown? ArithmeticException (odd? (double 10)))))

