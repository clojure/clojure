;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.math
  (:require
    [clojure.test :refer :all]
    [clojure.math :as m]))

(set! *warn-on-reflection* true)

(defn neg-zero?
  [^double d]
  (and (zero? d) (< (Double/compare d 0.0) 0)))

(defn pos-zero?
  [^double d]
  (and (zero? d) (not (< (Double/compare d 0.0) 0))))

(defn ulp=
  "Tests that y = x +/- m*ulp(x)"
  [x y ^double m]
  (let [mu (* (m/ulp x) m)]
    (<= (- x mu) y (+ x mu))))

(deftest test-sin
  (is (NaN? (m/sin ##NaN)))
  (is (NaN? (m/sin ##-Inf)))
  (is (NaN? (m/sin ##Inf)))
  (is (pos-zero? (m/sin 0.0)))
  (is (neg-zero? (m/sin -0.0)))
  (is (ulp= (m/sin m/PI) (- (m/sin (- m/PI))) 1)))

(deftest test-cos
  (is (NaN? (m/cos ##NaN)))
  (is (NaN? (m/cos ##-Inf)))
  (is (NaN? (m/cos ##Inf)))
  (is (= 1.0 (m/cos 0.0) (m/cos -0.0)))
  (is (ulp= (m/cos m/PI) (m/cos (- m/PI)) 1)))

(deftest test-tan
  (is (NaN? (m/tan ##NaN)))
  (is (NaN? (m/tan ##-Inf)))
  (is (NaN? (m/tan ##Inf)))
  (is (pos-zero? (m/tan 0.0)))
  (is (neg-zero? (m/tan -0.0)))
  (is (ulp= (- (m/tan m/PI)) (m/tan (- m/PI)) 1)))

(deftest test-asin
  (is (NaN? (m/asin ##NaN)))
  (is (NaN? (m/asin 2.0)))
  (is (NaN? (m/asin -2.0)))
  (is (zero? (m/asin -0.0))))

(deftest test-acos
  (is (NaN? (m/acos ##NaN)))
  (is (NaN? (m/acos -2.0)))
  (is (NaN? (m/acos 2.0)))
  (is (ulp= (* 2 (m/acos 0.0)) m/PI 1)))

(deftest test-atan
  (is (NaN? (m/atan ##NaN)))
  (is (pos-zero? (m/atan 0.0)))
  (is (neg-zero? (m/atan -0.0)))
  (is (ulp= (m/atan 1) 0.7853981633974483 1)))

(deftest test-radians-degrees-roundtrip
  (doseq [d (range 0.0 360.0 5.0)]
    (is (ulp= (m/round d) (m/round (-> d m/to-radians m/to-degrees)) 1))))

(deftest test-exp
  (is (NaN? (m/exp ##NaN)))
  (is (= ##Inf (m/exp ##Inf)))
  (is (pos-zero? (m/exp ##-Inf)))
  (is (ulp= (m/exp 0.0) 1.0 1))
  (is (ulp= (m/exp 1) m/E 1)))

(deftest test-log
  (is (NaN? (m/log ##NaN)))
  (is (NaN? (m/log -1.0)))
  (is (= ##Inf (m/log ##Inf)))
  (is (= ##-Inf (m/log 0.0)))
  (is (ulp= (m/log m/E) 1.0 1)))

(deftest test-log10
  (is (NaN? (m/log10 ##NaN)))
  (is (NaN? (m/log10 -1.0)))
  (is (= ##Inf (m/log10 ##Inf)))
  (is (= ##-Inf (m/log10 0.0)))
  (is (ulp= (m/log10 10) 1.0 1)))

(deftest test-sqrt
  (is (NaN? (m/sqrt ##NaN)))
  (is (NaN? (m/sqrt -1.0)))
  (is (= ##Inf (m/sqrt ##Inf)))
  (is (pos-zero? (m/sqrt 0)))
  (is (= (m/sqrt 4.0) 2.0)))

(deftest test-cbrt
  (is (NaN? (m/cbrt ##NaN)))
  (is (= ##-Inf (m/cbrt ##-Inf)))
  (is (= ##Inf (m/cbrt ##Inf)))
  (is (pos-zero? (m/cbrt 0)))
  (is (= 2.0 (m/cbrt 8.0))))

(deftest test-IEEE-remainder
  (is (NaN? (m/IEEE-remainder ##NaN 1.0)))
  (is (NaN? (m/IEEE-remainder 1.0 ##NaN)))
  (is (NaN? (m/IEEE-remainder ##Inf 2.0)))
  (is (NaN? (m/IEEE-remainder ##-Inf 2.0)))
  (is (NaN? (m/IEEE-remainder 2 0.0)))
  (is (= 1.0 (m/IEEE-remainder 5.0 4.0))))

(deftest test-ceil
  (is (NaN? (m/ceil ##NaN)))
  (is (= ##Inf (m/ceil ##Inf)))
  (is (= ##-Inf (m/ceil ##-Inf)))
  (is (= 4.0 (m/ceil m/PI))))

(deftest test-floor
  (is (NaN? (m/floor ##NaN)))
  (is (= ##Inf (m/floor ##Inf)))
  (is (= ##-Inf (m/floor ##-Inf)))
  (is (= 3.0 (m/floor m/PI))))

(deftest test-rint
  (is (NaN? (m/rint ##NaN)))
  (is (= ##Inf (m/rint ##Inf)))
  (is (= ##-Inf (m/rint ##-Inf)))
  (is (= 1.0 (m/rint 1.2)))
  (is (neg-zero? (m/rint -0.01))))

(deftest test-atan2
  (is (NaN? (m/atan2 ##NaN 1.0)))
  (is (NaN? (m/atan2 1.0 ##NaN)))
  (is (pos-zero? (m/atan2 0.0 1.0)))
  (is (neg-zero? (m/atan2 -0.0 1.0)))
  (is (ulp= (m/atan2 0.0 -1.0) m/PI 2))
  (is (ulp= (m/atan2 -0.0 -1.0) (- m/PI) 2))
  (is (ulp= (* 2.0 (m/atan2 1.0 0.0)) m/PI 2))
  (is (ulp= (* -2.0 (m/atan2 -1.0 0.0)) m/PI 2))
  (is (ulp= (* 4.0 (m/atan2 ##Inf ##Inf)) m/PI 2))
  (is (ulp= (/ (* 4.0 (m/atan2 ##Inf ##-Inf)) 3.0) m/PI 2))
  (is (ulp= (* -4.0 (m/atan2 ##-Inf ##Inf)) m/PI 2))
  (is (ulp= (/ (* -4.0 (m/atan2 ##-Inf ##-Inf)) 3.0) m/PI 2)))

(deftest test-pow
  (is (= 1.0 (m/pow 4.0 0.0)))
  (is (= 1.0 (m/pow 4.0 -0.0)))
  (is (= 4.2 (m/pow 4.2 1.0)))
  (is (NaN? (m/pow 4.2 ##NaN)))
  (is (NaN? (m/pow ##NaN 2.0)))
  (is (= ##Inf (m/pow 2.0 ##Inf)))
  (is (= ##Inf (m/pow 0.5 ##-Inf)))
  (is (= 0.0 (m/pow 2.0 ##-Inf)))
  (is (= 0.0 (m/pow 0.5 ##Inf)))
  (is (NaN? (m/pow 1.0 ##Inf)))
  (is (pos-zero? (m/pow 0.0 1.5)))
  (is (pos-zero? (m/pow ##Inf -2.0)))
  (is (= ##Inf (m/pow 0.0 -2.0)))
  (is (= ##Inf (m/pow ##Inf 2.0)))
  (is (pos-zero? (m/pow -0.0 1.5)))
  (is (pos-zero? (m/pow ##-Inf -1.5)))
  (is (neg-zero? (m/pow -0.0 3.0)))
  (is (neg-zero? (m/pow ##-Inf -3.0)))
  (is (= ##Inf (m/pow -0.0 -1.5)))
  (is (= ##Inf (m/pow ##-Inf 2.5)))
  (is (= ##-Inf (m/pow -0.0 -3.0)))
  (is (= ##-Inf (m/pow ##-Inf 3.0)))
  (is (= 4.0 (m/pow -2.0 2.0)))
  (is (= -8.0 (m/pow -2.0 3.0)))
  (is (= 8.0 (m/pow 2.0 3.0))))

(deftest test-round
  (is (= 0 (m/round ##NaN)))
  (is (= Long/MIN_VALUE (m/round ##-Inf)))
  (is (= Long/MIN_VALUE (m/round (- Long/MIN_VALUE 2.0))))
  (is (= Long/MAX_VALUE (m/round ##Inf)))
  (is (= Long/MAX_VALUE (m/round (+ Long/MAX_VALUE 2.0))))
  (is (= 4 (m/round 3.5))))

(deftest test-add-exact
  (try
    (m/add-exact Long/MAX_VALUE 1)
    (is false)
    (catch ArithmeticException _
      (is true))))

(deftest test-subtract-exact
  (try
    (m/subtract-exact Long/MIN_VALUE 1)
    (is false)
    (catch ArithmeticException _
      (is true))))

(deftest test-multiply-exact
  (try
    (m/multiply-exact Long/MAX_VALUE 2)
    (is false)
    (catch ArithmeticException _
      (is true))))

(deftest test-increment-exact
  (try
    (m/increment-exact Long/MAX_VALUE)
    (is false)
    (catch ArithmeticException _
      (is true))))

(deftest test-decrement-exact
  (try
    (m/decrement-exact Long/MIN_VALUE)
    (is false)
    (catch ArithmeticException _
      (is true))))

(deftest test-negate-exact
  (is (= (inc Long/MIN_VALUE) (m/negate-exact Long/MAX_VALUE)))
  (try
    (m/negate-exact Long/MIN_VALUE)
    (is false)
    (catch ArithmeticException _
      (is true))))

(deftest test-floor-div
  (is (= Long/MIN_VALUE (m/floor-div Long/MIN_VALUE -1)))
  (is (= -1 (m/floor-div -2 5))))

(deftest test-floor-mod
  (is (= 3 (m/floor-mod -2 5))))

(deftest test-ulp
  (is (NaN? (m/ulp ##NaN)))
  (is (= ##Inf (m/ulp ##Inf)))
  (is (= ##Inf (m/ulp ##-Inf)))
  (is (= Double/MIN_VALUE (m/ulp 0.0)))
  (is (= (m/pow 2 971) (m/ulp Double/MAX_VALUE)))
  (is (= (m/pow 2 971) (m/ulp (- Double/MAX_VALUE)))))

(deftest test-signum
  (is (NaN? (m/signum ##NaN)))
  (is (zero? (m/signum 0.0)))
  (is (zero? (m/signum -0.0)))
  (is (= 1.0 (m/signum 42.0)))
  (is (= -1.0 (m/signum -42.0))))

(deftest test-sinh
  (is (NaN? (m/sinh ##NaN)))
  (is (= ##Inf (m/sinh ##Inf)))
  (is (= ##-Inf (m/sinh ##-Inf)))
  (is (= 0.0 (m/sinh 0.0))))

(deftest test-cosh
  (is (NaN? (m/cosh ##NaN)))
  (is (= ##Inf (m/cosh ##Inf)))
  (is (= ##Inf (m/cosh ##-Inf)))
  (is (= 1.0 (m/cosh 0.0))))

(deftest test-tanh
  (is (NaN? (m/tanh ##NaN)))
  (is (= 1.0 (m/tanh ##Inf)))
  (is (= -1.0 (m/tanh ##-Inf)))
  (is (= 0.0 (m/tanh 0.0))))

(deftest test-hypot
  (is (= ##Inf (m/hypot 1.0 ##Inf)))
  (is (= ##Inf (m/hypot ##Inf 1.0)))
  (is (NaN? (m/hypot ##NaN 1.0)))
  (is (NaN? (m/hypot 1.0 ##NaN)))
  (is (= 13.0 (m/hypot 5.0 12.0))))

(deftest test-expm1
  (is (NaN? (m/expm1 ##NaN)))
  (is (= ##Inf (m/expm1 ##Inf)))
  (is (= -1.0 (m/expm1 ##-Inf)))
  (is (= 0.0 (m/expm1 0.0))))

(deftest test-log1p
  (is (NaN? (m/log1p ##NaN)))
  (is (= ##Inf (m/log1p ##Inf)))
  (is (= ##-Inf (m/log1p -1.0)))
  (is (pos-zero? (m/log1p 0.0)))
  (is (neg-zero? (m/log1p -0.0))))

(deftest test-copy-sign
  (is (= 1.0 (m/copy-sign 1.0 42.0)))
  (is (= -1.0 (m/copy-sign 1.0 -42.0)))
  (is (= -1.0 (m/copy-sign 1.0 ##-Inf))))

(deftest test-get-exponent
  (is (= (inc Double/MAX_EXPONENT) (m/get-exponent ##NaN)))
  (is (= (inc Double/MAX_EXPONENT) (m/get-exponent ##Inf)))
  (is (= (inc Double/MAX_EXPONENT) (m/get-exponent ##-Inf)))
  (is (= (dec Double/MIN_EXPONENT) (m/get-exponent 0.0)))
  (is (= 0 (m/get-exponent 1.0)))
  (is (= 13 (m/get-exponent 12345.678))))

(deftest test-next-after
  (is (NaN? (m/next-after ##NaN 1)))
  (is (NaN? (m/next-after 1 ##NaN)))
  (is (pos-zero? (m/next-after 0.0 0.0)))
  (is (neg-zero? (m/next-after -0.0 -0.0)))
  (is (= Double/MAX_VALUE (m/next-after ##Inf 1.0)))
  (is (pos-zero? (m/next-after Double/MIN_VALUE -1.0))))

(deftest test-next-up
  (is (NaN? (m/next-up ##NaN)))
  (is (= ##Inf (m/next-up ##Inf)))
  (is (= Double/MIN_VALUE (m/next-up 0.0))))

(deftest test-next-down
  (is (NaN? (m/next-down ##NaN)))
  (is (= ##-Inf (m/next-down ##-Inf)))
  (is (= (- Double/MIN_VALUE) (m/next-down 0.0))))

(deftest test-scalb
  (is (NaN? (m/scalb ##NaN 1)))
  (is (= ##Inf (m/scalb ##Inf 1)))
  (is (= ##-Inf (m/scalb ##-Inf 1)))
  (is (pos-zero? (m/scalb 0.0 2)))
  (is (neg-zero? (m/scalb -0.0 2)))
  (is (= 32.0 (m/scalb 2.0 4))))
