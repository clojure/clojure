;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Stephen C. Gilardi
;;  scgilardi (gmail)
;;  Created 30 October 2008
;;

(ns clojure.test-clojure.numbers
  (:use clojure.test
        clojure.template))


; TODO:
; ==
; and more...


;; *** Types ***


(deftest Coerced-BigDecimal
  (let [v (bigdec 3)]
    (are [x] (true? x)
     (instance? BigDecimal v)
     (number? v)
     (decimal? v)
     (not (float? v)))))

(deftest BigInteger-conversions
  (are [x] (biginteger x)
    Long/MAX_VALUE
    13178456923875639284562345789M
    13178456923875639284562345789N))

(deftest unchecked-cast-num-obj
  (do-template [prim-array cast]
    (are [n]
      (let [a (prim-array 1)]
        (aset a 0 (cast n)))
      (Byte. Byte/MAX_VALUE)
      (Short. Short/MAX_VALUE)
      (Integer. Integer/MAX_VALUE)
      (Long. Long/MAX_VALUE)
      (Float. Float/MAX_VALUE)
      (Double. Double/MAX_VALUE))
    byte-array
    unchecked-byte
    short-array
    unchecked-short
    char-array
    unchecked-char
    int-array
    unchecked-int
    long-array
    unchecked-long
    float-array
    unchecked-float
    double-array
    unchecked-double))

(deftest unchecked-cast-num-prim
  (do-template [prim-array cast]
    (are [n]
      (let [a (prim-array 1)]
        (aset a 0 (cast n)))
      Byte/MAX_VALUE
      Short/MAX_VALUE
      Integer/MAX_VALUE
      Long/MAX_VALUE
      Float/MAX_VALUE
      Double/MAX_VALUE)
    byte-array
    unchecked-byte
    short-array
    unchecked-short
    char-array
    unchecked-char
    int-array
    unchecked-int
    long-array
    unchecked-long
    float-array
    unchecked-float
    double-array
    unchecked-double))

(deftest unchecked-cast-char
  ; in keeping with the checked cast functions, char and Character can only be cast to int
  (is (unchecked-int (char 0xFFFF)))
  (is (let [c (char 0xFFFF)] (unchecked-int c)))) ; force primitive char

(def expected-casts
  [
   [:input           [-1            0           1           Byte/MAX_VALUE  Short/MAX_VALUE  Integer/MAX_VALUE  Long/MAX_VALUE         Float/MAX_VALUE    Double/MAX_VALUE]]
   [char             [:error        (char 0)    (char 1)    (char 127)      (char 32767)     :error             :error                 :error             :error]]
   [unchecked-char   [(char 65535)  (char 0)    (char 1)    (char 127)      (char 32767)     (char 65535)       (char 65535)           (char 65535)       (char 65535)]]
   [byte             [-1            0           1           Byte/MAX_VALUE  :error           :error             :error                 :error             :error]]
   [unchecked-byte   [-1            0           1           Byte/MAX_VALUE  -1               -1                 -1                     -1                 -1]]
   [short            [-1            0           1           Byte/MAX_VALUE  Short/MAX_VALUE  :error             :error                 :error             :error]]
   [unchecked-short  [-1            0           1           Byte/MAX_VALUE  Short/MAX_VALUE  -1                 -1                     -1                 -1]] 
   [int              [-1            0           1           Byte/MAX_VALUE  Short/MAX_VALUE  Integer/MAX_VALUE  :error                 :error             :error]]
   [unchecked-int    [-1            0           1           Byte/MAX_VALUE  Short/MAX_VALUE  Integer/MAX_VALUE  -1                     Integer/MAX_VALUE  Integer/MAX_VALUE]]
   [long             [-1            0           1           Byte/MAX_VALUE  Short/MAX_VALUE  Integer/MAX_VALUE  Long/MAX_VALUE         :error             :error]]
   [unchecked-long   [-1            0           1           Byte/MAX_VALUE  Short/MAX_VALUE  Integer/MAX_VALUE  Long/MAX_VALUE         Long/MAX_VALUE     Long/MAX_VALUE]]
                                                                                             ;; 2.14748365E9 if when float/double conversion is avoided...
   [float            [-1.0          0.0         1.0         127.0           32767.0          2.147483648E9      9.223372036854776E18   Float/MAX_VALUE    :error]]
   [unchecked-float  [-1.0          0.0         1.0         127.0           32767.0          2.147483648E9      9.223372036854776E18   Float/MAX_VALUE    Float/POSITIVE_INFINITY]]
   [double           [-1.0          0.0         1.0         127.0           32767.0          2.147483647E9      9.223372036854776E18   Float/MAX_VALUE    Double/MAX_VALUE]]
   [unchecked-double [-1.0          0.0         1.0         127.0           32767.0          2.147483647E9      9.223372036854776E18   Float/MAX_VALUE    Double/MAX_VALUE]]])

(deftest test-expected-casts
  (let [[[_ inputs] & expectations] expected-casts]
    (doseq [[f vals] expectations]
      (let [wrapped (fn [x]
                      (try
                       (f x)
                       (catch IllegalArgumentException e :error)))]
        (is (= vals (map wrapped inputs)))))))

;; *** Functions ***

(defonce DELTA 1e-12)

(deftest test-add
  (are [x y] (= x y)
      (+) 0
      (+ 1) 1
      (+ 1 2) 3
      (+ 1 2 3) 6

      (+ -1) -1
      (+ -1 -2) -3
      (+ -1 +2 -3) -2

      (+ 1 -1) 0
      (+ -1 1) 0

      (+ 2/3) 2/3
      (+ 2/3 1) 5/3
      (+ 2/3 1/3) 1 )

  (are [x y] (< (- x y) DELTA)
      (+ 1.2) 1.2
      (+ 1.1 2.4) 3.5
      (+ 1.1 2.2 3.3) 6.6 )

  (is (> (+ Integer/MAX_VALUE 10) Integer/MAX_VALUE))  ; no overflow
  (is (thrown? ClassCastException (+ "ab" "cd"))) )    ; no string concatenation


(deftest test-subtract
  (is (thrown? IllegalArgumentException (-)))
  (are [x y] (= x y)
      (- 1) -1
      (- 1 2) -1
      (- 1 2 3) -4

      (- -2) 2
      (- 1 -2) 3
      (- 1 -2 -3) 6

      (- 1 1) 0
      (- -1 -1) 0

      (- 2/3) -2/3
      (- 2/3 1) -1/3
      (- 2/3 1/3) 1/3 )

  (are [x y] (< (- x y) DELTA)
      (- 1.2) -1.2
      (- 2.2 1.1) 1.1
      (- 6.6 2.2 1.1) 3.3 )

  (is (< (- Integer/MIN_VALUE 10) Integer/MIN_VALUE)) )  ; no underflow


(deftest test-multiply
  (are [x y] (= x y)
      (*) 1
      (* 2) 2
      (* 2 3) 6
      (* 2 3 4) 24

      (* -2) -2
      (* 2 -3) -6
      (* 2 -3 -1) 6

      (* 1/2) 1/2
      (* 1/2 1/3) 1/6
      (* 1/2 1/3 -1/4) -1/24 )

  (are [x y] (< (- x y) DELTA)
      (* 1.2) 1.2
      (* 2.0 1.2) 2.4
      (* 3.5 2.0 1.2) 8.4 )

  (is (> (* 3 (int (/ Integer/MAX_VALUE 2.0))) Integer/MAX_VALUE)) )  ; no overflow

(deftest test-ratios-simplify-to-ints-where-appropriate
  (testing "negative denominator (assembla #275)"
    (is (integer? (/ 1 -1/2)))
    (is (integer? (/ 0 -1/2)))))

(deftest test-divide
  (are [x y] (= x y)
      (/ 1) 1
      (/ 2) 1/2
      (/ 3 2) 3/2
      (/ 4 2) 2
      (/ 24 3 2) 4
      (/ 24 3 2 -1) -4

      (/ -1) -1
      (/ -2) -1/2
      (/ -3 -2) 3/2
      (/ -4 -2) 2
      (/ -4 2) -2 )

  (are [x y] (< (- x y) DELTA)
      (/ 4.5 3) 1.5
      (/ 4.5 3.0 3.0) 0.5 )

  (is (thrown? ArithmeticException (/ 0)))
  (is (thrown? ArithmeticException (/ 2 0)))
  (is (thrown? IllegalArgumentException (/))) )


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
;  (is (thrown? IllegalArgumentException (mod)))
;  (is (thrown? IllegalArgumentException (mod 1)))
;  (is (thrown? IllegalArgumentException (mod 3 2 1)))

  ; divide by zero
  (is (thrown? ArithmeticException (mod 9 0)))
  (is (thrown? ArithmeticException (mod 0 0)))

  (are [x y] (= x y)
    (mod 4 2) 0
    (mod 3 2) 1
    (mod 6 4) 2
    (mod 0 5) 0

    (mod 2 1/2) 0
    (mod 2/3 1/2) 1/6
    (mod 1 2/3) 1/3

    (mod 4.0 2.0) 0.0
    (mod 4.5 2.0) 0.5

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

    ; large args
    (mod 3216478362187432 432143214) 120355456
  )
)

;; rem & quot
;; http://en.wikipedia.org/wiki/Remainder

(deftest test-rem
  ; wrong number of args
;  (is (thrown? IllegalArgumentException (rem)))
;  (is (thrown? IllegalArgumentException (rem 1)))
;  (is (thrown? IllegalArgumentException (rem 3 2 1)))

  ; divide by zero
  (is (thrown? ArithmeticException (rem 9 0)))
  (is (thrown? ArithmeticException (rem 0 0)))
  
  (are [x y] (= x y)
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
;  (is (thrown? IllegalArgumentException (quot)))
;  (is (thrown? IllegalArgumentException (quot 1)))
;  (is (thrown? IllegalArgumentException (quot 3 2 1)))

  ; divide by zero
  (is (thrown? ArithmeticException (quot 9 0)))
  (is (thrown? ArithmeticException (quot 0 0)))
  
  (are [x y] (= x y)
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
  (are [x] (true? x)
    (even? -4)
    (not (even? -3))
    (even? 0)
    (not (even? 5))
    (even? 8))
  (is (thrown? IllegalArgumentException (even? 1/2)))
  (is (thrown? IllegalArgumentException (even? (double 10)))))

(deftest test-odd?
  (are [x] (true? x)
    (not (odd? -4))
    (odd? -3)
    (not (odd? 0))
    (odd? 5)
    (not (odd? 8)))
  (is (thrown? IllegalArgumentException (odd? 1/2)))
  (is (thrown? IllegalArgumentException (odd? (double 10)))))

(defn- expt
  "clojure.contrib.math/expt is a better and much faster impl, but this works.
Math/pow overflows to Infinity."
  [x n] (apply *' (replicate n x)))

(deftest test-bit-shift-left
  (are [x y] (= x y)
       2r10 (bit-shift-left 2r1 1)
       2r100 (bit-shift-left 2r1 2)
       2r1000 (bit-shift-left 2r1 3)
       2r00101110 (bit-shift-left 2r00010111 1)
       2r00101110 (apply bit-shift-left [2r00010111 1])
       0 (bit-shift-left 2r10 -1) ; truncated to least 6-bits, 63
       (expt 2 32) (bit-shift-left 1 32)
       (expt 2 16) (bit-shift-left 1 10000) ; truncated to least 6-bits, 16
       )
  (is (thrown? IllegalArgumentException (bit-shift-left 1N 1))))

(deftest test-bit-shift-right
  (are [x y] (= x y)
       2r0 (bit-shift-right 2r1 1)
       2r010 (bit-shift-right 2r100 1)
       2r001 (bit-shift-right 2r100 2)
       2r000 (bit-shift-right 2r100 3)
       2r0001011 (bit-shift-right 2r00010111 1)
       2r0001011 (apply bit-shift-right [2r00010111 1])
       0 (bit-shift-right 2r10 -1) ; truncated to least 6-bits, 63
       1 (bit-shift-right (expt 2 32) 32)
       1 (bit-shift-right (expt 2 16) 10000) ; truncated to least 6-bits, 16
       )
  (is (thrown? IllegalArgumentException (bit-shift-right 1N 1))))

(deftest test-bit-clear
  (is (= 2r1101 (bit-clear 2r1111 1)))
  (is (= 2r1101 (bit-clear 2r1101 1))))

(deftest test-bit-set
  (is (= 2r1111 (bit-set 2r1111 1)))
  (is (= 2r1111 (bit-set 2r1101 1))))

(deftest test-bit-flip
  (is (= 2r1101 (bit-flip 2r1111 1)))
  (is (= 2r1111 (bit-flip 2r1101 1))))

(deftest test-bit-test
  (is (true? (bit-test 2r1111 1)))
  (is (false? (bit-test 2r1101 1))))

;; arrays
(deftest test-array-types
  (are [x y z] (= (Class/forName x) (class y) (class z))
       "[Z" (boolean-array 1) (booleans (boolean-array 1 true))
       "[B" (byte-array 1) (bytes (byte-array 1 (byte 1)))
       "[C" (char-array 1) (chars (char-array 1 \a))
       "[S" (short-array 1) (shorts (short-array 1 (short 1)))
       "[F" (float-array 1) (floats (float-array 1 1))
       "[D" (double-array 1) (doubles (double-array 1 1))
       "[I" (int-array 1) (ints (int-array 1 1))
       "[J" (long-array 1) (longs (long-array 1 1))))


(deftest test-ratios
  (is (== (denominator 1/2) 2))
  (is (== (numerator 1/2) 1))
  (is (= (bigint (/ 100000000000000000000 3)) 33333333333333333333))
  (is (= (long 10000000000000000000/3) 3333333333333333333)))

(deftest test-arbitrary-precision-subtract
  (are [x y] (= x y)
       9223372036854775808N (-' 0 -9223372036854775808)
       clojure.lang.BigInt  (class (-' 0 -9223372036854775808))
       java.lang.Long       (class (-' 0 -9223372036854775807))))

(deftest test-min-max
  (testing "min/max on different numbers of floats and doubles"
    (are [xmin xmax a]
         (and (= (Float. xmin) (min (Float. a)))
              (= (Float. xmax) (max (Float. a)))
              (= xmin (min a))
              (= xmax (max a)))
         0.0 0.0 0.0)
    (are [xmin xmax a b]
         (and (= (Float. xmin) (min (Float. a) (Float. b)))
              (= (Float. xmax) (max (Float. a) (Float. b)))
              (= xmin (min a b))
              (= xmax (max a b)))
         -1.0  0.0  0.0 -1.0
         -1.0  0.0 -1.0  0.0
         0.0  1.0  0.0  1.0
         0.0  1.0  1.0  0.0)
    (are [xmin xmax a b c]
         (and (= (Float. xmin) (min (Float. a) (Float. b) (Float. c)))
              (= (Float. xmax) (max (Float. a) (Float. b) (Float. c)))
              (= xmin (min a b c))
              (= xmax (max a b c)))
         -1.0  1.0  0.0  1.0 -1.0
         -1.0  1.0  0.0 -1.0  1.0
         -1.0  1.0 -1.0  1.0  0.0))
  (testing "min/max preserves type of winner"
    (is (= java.lang.Long (class (max 10))))
    (is (= java.lang.Long (class (max 1.0 10))))
    (is (= java.lang.Long (class (max 10 1.0))))
    (is (= java.lang.Long (class (max 10 1.0 2.0))))
    (is (= java.lang.Long (class (max 1.0 10 2.0))))
    (is (= java.lang.Long (class (max 1.0 2.0 10))))
    (is (= java.lang.Double (class (max 1 2 10.0 3 4 5))))
    (is (= java.lang.Long (class (min 10))))
    (is (= java.lang.Long (class (min 1.0 -10))))
    (is (= java.lang.Long (class (min -10 1.0))))
    (is (= java.lang.Long (class (min -10 1.0 2.0))))
    (is (= java.lang.Long (class (min 1.0 -10 2.0))))
    (is (= java.lang.Long (class (min 1.0 2.0 -10))))
    (is (= java.lang.Double (class (min 1 2 -10.0 3 4 5))))))

(deftest clj-868
  (testing "min/max: NaN is contagious"
    (letfn [(fnan? [^Float x] (Float/isNaN x))
            (dnan? [^double x] (Double/isNaN x))]
      (are [minmax]
           (are [nan? nan zero]
                (every? nan? (map minmax
                                  [ nan zero zero]
                                  [zero  nan zero]
                                  [zero zero  nan]))
                fnan?  Float/NaN  (Float. 0.0)
                dnan? Double/NaN          0.0)
           min
           max))))

