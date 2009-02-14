;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  scgilardi (gmail)
;;  Created 30 October 2008
;;

(ns clojure.contrib.test-clojure.numbers
  (:use clojure.contrib.test-is))


;; *** Types ***

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


;; *** Functions ***

;; mod
;; http://en.wikipedia.org/wiki/Modulo_operation
;; http://mathforum.org/library/drmath/view/52343.html
;;
;; is mod correct?
;; http://groups.google.com/group/clojure/browse_frm/thread/2a0ee4d248f3d131#
;;
;; Issue 23: mod (modulo) operator
;; http://code.google.com/p/clojure/issues/detail?id=23

(deftest test-mod
  ; wrong number of args
  (is (thrown? IllegalArgumentException (mod)))
  (is (thrown? IllegalArgumentException (mod 1)))
  (is (thrown? IllegalArgumentException (mod 3 2 1)))

  ; divide by zero
  (is (thrown? ArithmeticException (mod 9 0)))
  (is (thrown? ArithmeticException (mod 0 0)))

  (are (= _1 _2)
    (mod 4 2) 0
    (mod 3 2) 1
    (mod 6 4) 2
    (mod 0 5) 0

;; Removed non-integer tests for Clojure SVN r1227+

;;  (mod 2 1/2) 0
;;  (mod 2/3 1/2) 1/6
;;  (mod 1 2/3) 1/3

;;  (mod 4.0 2.0) 0.0
;;  (mod 4.5 2.0) 0.5

    ; |num| > |div|, num != k * div
    (mod 42 5) 2      ; (42 / 5) * 5 + (42 mod 5)        = 8 * 5 + 2        = 42
    (mod 42 -5) -3    ; (42 / -5) * (-5) + (42 mod -5)   = -9 * (-5) + (-3) = 42
    (mod -42 5) 3     ; (-42 / 5) * 5 + (-42 mod 5)      = -9 * 5 + 3       = -42
    (mod -42 -5) -2   ; (-42 / -5) * (-5) + (-42 mod -5) = 8 * (-5) + (-2)  = -42

    ; |num| > |div|, num = k * div
    (mod 9 3) 0       ; (9 / 3) * 3 + (9 mod 3) = 3 * 3 + 0 = 9
    (mod 9 -3) 0
    (mod -9 3) 0
    (mod -9 -3) 0

    ; |num| < |div|
    (mod 2 5) 2       ; (2 / 5) * 5 + (2 mod 5)        = 0 * 5 + 2          = 2
    (mod 2 -5) -3     ; (2 / -5) * (-5) + (2 mod -5)   = (-1) * (-5) + (-3) = 2
    (mod -2 5) 3      ; (-2 / 5) * 5 + (-2 mod 5)      = (-1) * 5 + 3       = -2
    (mod -2 -5) -2    ; (-2 / -5) * (-5) + (-2 mod -5) = 0 * (-5) + (-2)    = -2

    ; num = 0, div != 0
    (mod 0 3) 0       ; (0 / 3) * 3 + (0 mod 3) = 0 * 3 + 0 = 0
    (mod 0 -3) 0
  )
)

;; rem & quot
;; http://en.wikipedia.org/wiki/Remainder

(deftest test-rem
  ; wrong number of args
  (is (thrown? IllegalArgumentException (rem)))
  (is (thrown? IllegalArgumentException (rem 1)))
  (is (thrown? IllegalArgumentException (rem 3 2 1)))

  ; divide by zero
  (is (thrown? ArithmeticException (rem 9 0)))
  (is (thrown? ArithmeticException (rem 0 0)))
  
  (are (= _1 _2)
    (rem 4 2) 0
    (rem 3 2) 1
    (rem 6 4) 2
    (rem 0 5) 0

    (rem 2 1/2) 0
    (rem 2/3 1/2) 1/6
    (rem 1 2/3) 1/3

    (rem 4.0 2.0) 0.0
    (rem 4.5 2.0) 0.5

    ; |num| > |div|, num != k * div
    (rem 42 5) 2      ; (8 * 5) + 2 == 42
    (rem 42 -5) 2     ; (-8 * -5) + 2 == 42
    (rem -42 5) -2    ; (-8 * 5) + -2 == -42
    (rem -42 -5) -2   ; (8 * -5) + -2 == -42

    ; |num| > |div|, num = k * div
    (rem 9 3) 0
    (rem 9 -3) 0
    (rem -9 3) 0
    (rem -9 -3) 0

    ; |num| < |div|
    (rem 2 5) 2
    (rem 2 -5) 2
    (rem -2 5) -2
    (rem -2 -5) -2
    
    ; num = 0, div != 0
    (rem 0 3) 0
    (rem 0 -3) 0
  )
)

(deftest test-quot
  ; wrong number of args
  (is (thrown? IllegalArgumentException (quot)))
  (is (thrown? IllegalArgumentException (quot 1)))
  (is (thrown? IllegalArgumentException (quot 3 2 1)))

  ; divide by zero
  (is (thrown? ArithmeticException (quot 9 0)))
  (is (thrown? ArithmeticException (quot 0 0)))
  
  (are (= _1 _2)
    (quot 4 2) 2
    (quot 3 2) 1
    (quot 6 4) 1
    (quot 0 5) 0

    (quot 2 1/2) 4
    (quot 2/3 1/2) 1
    (quot 1 2/3) 1

    (quot 4.0 2.0) 2.0
    (quot 4.5 2.0) 2.0

    ; |num| > |div|, num != k * div
    (quot 42 5) 8     ; (8 * 5) + 2 == 42
    (quot 42 -5) -8   ; (-8 * -5) + 2 == 42
    (quot -42 5) -8   ; (-8 * 5) + -2 == -42
    (quot -42 -5) 8   ; (8 * -5) + -2 == -42

    ; |num| > |div|, num = k * div
    (quot 9 3) 3
    (quot 9 -3) -3
    (quot -9 3) -3
    (quot -9 -3) 3

    ; |num| < |div|
    (quot 2 5) 0
    (quot 2 -5) 0
    (quot -2 5) 0
    (quot -2 -5) 0

    ; num = 0, div != 0
    (quot 0 3) 0
    (quot 0 -3) 0
  )
)


;; *** Predicates ***

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

