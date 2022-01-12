;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns
  ^{:author "Alex Miller",
    :doc "Clojure wrapper functions for java.lang.Math static methods.

  Function calls are inlined for performance, and type hinted for primitive
  long or double parameters where appropriate. In general, Math methods are
  optimized for performance and have bounds for error tolerance. If
  greater precision is needed, use java.lang.StrictMath directly instead.

  For more complete information, see:
  https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html"}
  clojure.math)

(set! *warn-on-reflection* true)

(def
  ^{:doc "Constant for e, the base for natural logarithms.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#E"
    :added "1.11"
    :const true
    :tag 'double}
  E
  Math/E)

(def
  ^{:doc "Constant for pi, the ratio of the circumference of a circle to its diameter.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#PI"
    :added "1.11"
    :const true
    :tag 'double}
  PI
  Math/PI)

(defn sin
  {:doc "Returns the sine of an angle.
  If a is ##NaN, ##-Inf, ##Inf => ##NaN
  If a is zero => zero with the same sign as a
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#sin-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/sin (double ~a)))
   :added "1.11"}
  ^double [^double a]
  (Math/sin a))

(defn cos
  {:doc "Returns the cosine of an angle.
  If a is ##NaN, ##-Inf, ##Inf => ##NaN
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#cos-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/cos (double ~a)))
   :added "1.11"}
  ^double [^double a]
  (Math/cos a))

(defn tan
  {:doc "Returns the tangent of an angle.
  If a is ##NaN, ##-Inf, ##Inf => ##NaN
  If a is zero => zero with the same sign as a
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#tan-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/tan (double ~a)))
   :added "1.11"}
  ^double [^double a]
  (Math/tan a))

(defn asin
  {:doc "Returns the arc sine of an angle, in the range -pi/2 to pi/2.
  If a is ##NaN or |a|>1 => ##NaN
  If a is zero => zero with the same sign as a
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#asin-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/asin (double ~a)))
   :added "1.11"}
  ^double [^double a]
  (Math/asin a))

(defn acos
  {:doc "Returns the arc cosine of a, in the range 0.0 to pi.
  If a is ##NaN or |a|>1 => ##NaN
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#acos-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/acos (double ~a)))
   :added "1.11"}
  ^double [^double a]
  (Math/acos a))

(defn atan
  {:doc "Returns the arc tangent of a, in the range of -pi/2 to pi/2.
  If a is ##NaN => ##NaN
  If a is zero => zero with the same sign as a
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#atan-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/atan (double ~a)))
   :added "1.11"}
  ^double [^double a]
  (Math/atan a))

(defn to-radians
  {:doc "Converts an angle in degrees to an approximate equivalent angle in radians.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#toRadians-double-"
   :inline-arities #{1}
   :inline (fn [deg] `(Math/toRadians (double ~deg)))
   :added "1.11"}
  ^double [^double deg]
  (Math/toRadians deg))

(defn to-degrees
  {:doc "Converts an angle in radians to an approximate equivalent angle in degrees.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#toDegrees-double-"
   :inline-arities #{1}
   :inline (fn [r] `(Math/toDegrees (double ~r)))
   :added "1.11"}
  ^double [^double r]
  (Math/toDegrees r))

(defn exp
  {:doc "Returns Euler's number e raised to the power of a.
  If a is ##NaN => ##NaN
  If a is ##Inf => ##Inf
  If a is ##-Inf => +0.0
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#exp-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/exp (double ~a)))
   :added "1.11"}
  ^double [^double a]
  (Math/exp a))

(defn log
  {:doc "Returns the natural logarithm (base e) of a.
  If a is ##NaN or negative => ##NaN
  If a is ##Inf => ##Inf
  If a is zero => ##-Inf
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#log-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/log (double ~a)))
   :added "1.11"}
  ^double [^double a]
  (Math/log a))

(defn log10
  {:doc "Returns the logarithm (base 10) of a.
  If a is ##NaN or negative => ##NaN
  If a is ##Inf => ##Inf
  If a is zero => ##-Inf
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#log10-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/log10 (double ~a)))
   :added "1.11"}
  ^double [^double a]
  (Math/log10 a))

(defn sqrt
  {:doc "Returns the positive square root of a.
  If a is ##NaN or negative => ##NaN
  If a is ##Inf => ##Inf
  If a is zero => a
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#sqrt-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/sqrt (double ~a)))
   :added "1.11"}
  ^double [^double a]
  (Math/sqrt a))

(defn cbrt
  {:doc "Returns the cube root of a.
  If a is ##NaN => ##NaN
  If a is ##Inf or ##-Inf => a
  If a is zero => zero with sign matching a
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#cbrt-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/cbrt (double ~a)))
   :added "1.11"}
  ^double [^double a]
  (Math/cbrt a))

(defn IEEE-remainder
  {:doc "Returns the remainder per IEEE 754 such that
    remainder = dividend - divisor * n
  where n is the integer closest to the exact value of dividend / divisor.
  If two integers are equally close, then n is the even one.
  If the remainder is zero, sign will match dividend.
  If dividend or divisor is ##NaN, or dividend is ##Inf or ##-Inf, or divisor is zero => ##NaN
  If dividend is finite and divisor is infinite => dividend
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#IEEEremainder-double-double-"
   :inline-arities #{2}
   :inline (fn [dividend divisor] `(Math/IEEEremainder (double ~dividend) (double ~divisor)))
   :added "1.11"}
  ^double [^double dividend ^double divisor]
  (Math/IEEEremainder dividend divisor))

(defn ceil
  {:doc "Returns the smallest double greater than or equal to a, and equal to a
  mathematical integer.
  If a is ##NaN or ##Inf or ##-Inf or already equal to an integer => a
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#ceil-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/ceil (double ~a)))
   :added "1.11"}
  ^double [^double a]
  (Math/ceil a))

(defn floor
  {:doc "Returns the largest double less than or equal to a, and equal to a
  mathematical integer.
  If a is ##NaN or ##Inf or ##-Inf or already equal to an integer => a
  If a is less than zero but greater than -1.0 => -0.0
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floor-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/floor (double ~a)))
   :added "1.11"}
  ^double [^double a]
  (Math/floor a))

(defn rint
  {:doc "Returns the double closest to a and equal to a mathematical integer.
  If two values are equally close, return the even one.
  If a is ##NaN or ##Inf or ##-Inf or zero => a
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#rint-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/rint (double ~a)))
   :added "1.11"}
  ^double [^double a]
  (Math/rint a))

(defn atan2
  {:doc "Returns the angle theta from the conversion of rectangular coordinates (x, y) to polar coordinates (r, theta).
  Computes the phase theta by computing an arc tangent of y/x in the range of -pi to pi.
  For more details on special cases, see:
  https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#atan2-double-double-"
   :inline-arities #{2}
   :inline (fn [y x] `(Math/atan2 (double ~y) (double ~x)))
   :added "1.11"}
  ^double [^double y ^double x]
  (Math/atan2 y x))

(defn pow
  {:doc "Returns the value of a raised to the power of b.
  For more details on special cases, see:
  https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#pow-double-double-"
   :inline-arities #{2}
   :inline (fn [a b] `(Math/pow (double ~a) (double ~b)))
   :added "1.11"}
  ^double [^double a ^double b]
  (Math/pow a b))

(defn round
  {:doc "Returns the closest long to a. If equally close to two values, return the one
  closer to ##Inf.
  If a is ##NaN => 0
  If a is ##-Inf or < Long/MIN_VALUE => Long/MIN_VALUE
  If a is ##Inf or > Long/MAX_VALUE => Long/MAX_VALUE
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#round-double-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/round (double ~a)))
   :added "1.11"}
  ^long [^double a]
  (Math/round a))

(defn random
  {:doc "Returns a positive double between 0.0 and 1.0, chosen pseudorandomly with
  approximately random distribution.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#random--"
   :inline-arities #{0}
   :inline (fn [] `(Math/random))
   :added "1.11"}
  ^double []
  (Math/random))

(defn add-exact
  {:doc "Returns the sum of x and y, throws ArithmeticException on overflow.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#addExact-long-long-"
   :inline-arities #{2}
   :inline (fn [x y] `(Math/addExact (long ~x) (long ~y)))
   :added "1.11"}
  ^long [^long x ^long y]
  (Math/addExact x y))

(defn subtract-exact
  {:doc "Returns the difference of x and y, throws ArithmeticException on overflow.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#subtractExact-long-long-"
   :inline-arities #{2}
   :inline (fn [x y] `(Math/subtractExact (long ~x) (long ~y)))
   :added "1.11"}
  ^long [^long x ^long y]
  (Math/subtractExact x y))

(defn multiply-exact
  {:doc "Returns the product of x and y, throws ArithmeticException on overflow.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#multiplyExact-long-long-"
   :inline-arities #{2}
   :inline (fn [x y] `(Math/multiplyExact (long ~x) (long ~y)))
   :added "1.11"}
  ^long [^long x ^long y]
  (Math/multiplyExact x y))

(defn increment-exact
  {:doc "Returns a incremented by 1, throws ArithmeticException on overflow.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#incrementExact-long-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/incrementExact (long ~a)))
   :added "1.11"}
  ^long [^long a]
  (Math/incrementExact a))

(defn decrement-exact
  {:doc "Returns a decremented by 1, throws ArithmeticException on overflow.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#decrementExact-long-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/decrementExact (long ~a)))
   :added "1.11"}
  ^long [^long a]
  (Math/decrementExact a))

(defn negate-exact
  {:doc "Returns the negation of a, throws ArithmeticException on overflow.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#negateExact-long-"
   :inline-arities #{1}
   :inline (fn [a] `(Math/negateExact (long ~a)))
   :added "1.11"}
  ^long [^long a]
  (Math/negateExact a))

(defn floor-div
  {:doc "Integer division that rounds to negative infinity (as opposed to zero).
  The special case (floorDiv Long/MIN_VALUE -1) overflows and returns Long/MIN_VALUE.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floorDiv-long-long-"
   :inline-arities #{2}
   :inline (fn [x y] `(Math/floorDiv (long ~x) (long ~y)))
   :added "1.11"}
  ^long [^long x ^long y]
  (Math/floorDiv x y))

(defn floor-mod
  {:doc "Integer modulus x - (floorDiv(x, y) * y). Sign matches y and is in the
  range -|y| < r < |y|.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floorMod-long-long-"
   :inline-arities #{2}
   :inline (fn [x y] `(Math/floorMod (long ~x) (long ~y)))
   :added "1.11"}
  ^long [^long x ^long y]
  (Math/floorMod x y))

(defn ulp
  {:doc "Returns the size of an ulp (unit in last place) for d.
  If d is ##NaN => ##NaN
  If d is ##Inf or ##-Inf => ##Inf
  If d is zero => Double/MIN_VALUE
  If d is +/- Double/MAX_VALUE => 2^971
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#ulp-double-"
   :inline-arities #{1}
   :inline (fn [d] `(Math/ulp (double ~d)))
   :added "1.11"}
  ^double [^double d]
  (Math/ulp d))

(defn signum
  {:doc "Returns the signum function of d - zero for zero, 1.0 if >0, -1.0 if <0.
  If d is ##NaN => ##NaN
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#signum-double-"
   :inline-arities #{1}
   :inline (fn [d] `(Math/signum (double ~d)))
   :added "1.11"}
  ^double [^double d]
  (Math/signum d))

(defn sinh
  {:doc "Returns the hyperbolic sine of x, (e^x - e^-x)/2.
  If x is ##NaN => ##NaN
  If x is ##Inf or ##-Inf or zero => x
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#sinh-double-"
   :inline-arities #{1}
   :inline (fn [x] `(Math/sinh (double ~x)))
   :added "1.11"}
  ^double [^double x]
  (Math/sinh x))

(defn cosh
  {:doc "Returns the hyperbolic cosine of x, (e^x + e^-x)/2.
  If x is ##NaN => ##NaN
  If x is ##Inf or ##-Inf => ##Inf
  If x is zero => 1.0
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#cosh-double-"
   :inline-arities #{1}
   :inline (fn [x] `(Math/cosh (double ~x)))
   :added "1.11"}
  ^double [^double x]
  (Math/cosh x))

(defn tanh
  {:doc "Returns the hyperbolic tangent of x, sinh(x)/cosh(x).
  If x is ##NaN => ##NaN
  If x is zero => zero, with same sign
  If x is ##Inf => +1.0
  If x is ##-Inf => -1.0
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#tanh-double-"
   :inline-arities #{1}
   :inline (fn [x] `(Math/tanh (double ~x)))
   :added "1.11"}
  ^double [^double x]
  (Math/tanh x))

(defn hypot
  {:doc "Returns sqrt(x^2 + y^2) without intermediate underflow or overflow.
  If x or y is ##Inf or ##-Inf => ##Inf
  If x or y is ##NaN and neither is ##Inf or ##-Inf => ##NaN
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#hypot-double-double-"
   :inline-arities #{2}
   :inline (fn [x y] `(Math/hypot (double ~x) (double ~y)))
   :added "1.11"}
  ^double [^double x ^double y]
  (Math/hypot x y))

(defn expm1
  {:doc "Returns e^x - 1. Near 0, expm1(x)+1 is more accurate to e^x than exp(x).
  If x is ##NaN => ##NaN
  If x is ##Inf => #Inf
  If x is ##-Inf => -1.0
  If x is zero => x
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#expm1-double-"
   :inline-arities #{1}
   :inline (fn [x] `(Math/expm1 (double ~x)))
   :added "1.11"}
  ^double [^double x]
  (Math/expm1 x))

(defn log1p
  {:doc "Returns ln(1+x). For small values of x, log1p(x) is more accurate than
  log(1.0+x).
  If x is ##NaN or < -1 => ##NaN
  If x is ##Inf => ##Inf
  If x is -1 => ##-Inf
  If x is 0 => 0 with sign matching x
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#log1p-double-"
   :inline-arities #{1}
   :inline (fn [x] `(Math/log1p (double ~x)))
   :added "1.11"}
  ^double [^double x]
  (Math/log1p x))

(defn copy-sign
  {:doc "Returns a double with the magnitude of the first argument and the sign of
  the second.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#copySign-double-double-"
   :inline-arities #{2}
   :inline (fn [magnitude sign] `(Math/copySign (double ~magnitude) (double ~sign)))
   :added "1.11"}
  ^double [^double magnitude ^double sign]
  (Math/copySign magnitude sign))

(defn get-exponent
  {:doc "Returns the exponent of d.
  If d is ##NaN, ##Inf, ##-Inf => Double/MAX_EXPONENT + 1
  If d is zero or subnormal => Double/MIN_EXPONENT - 1
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#getExponent-double-"
   :inline-arities #{1}
   :inline (fn [d] `(Math/getExponent (double ~d)))
   :added "1.11"}
  [^double d]
  (Math/getExponent d))

(defn next-after
  {:doc "Returns the adjacent floating point number to start in the direction of
  the second argument. If the arguments are equal, the second is returned.
  If either arg is #NaN => #NaN
  If both arguments are signed zeros => direction
  If start is +-Double/MIN_VALUE and direction would cause a smaller magnitude
    => zero with sign matching start
  If start is ##Inf or ##-Inf and direction would cause a smaller magnitude
    => Double/MAX_VALUE with same sign as start
  If start is equal to +=Double/MAX_VALUE and direction would cause a larger magnitude
    => ##Inf or ##-Inf with sign matching start
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextAfter-double-double-"
   :inline-arities #{2}
   :inline (fn [start direction] `(Math/nextAfter (double ~start) (double ~direction)))
   :added "1.11"}
  ^double [^double start ^double direction]
  (Math/nextAfter start direction))

(defn next-up
  {:doc "Returns the adjacent double of d in the direction of ##Inf.
  If d is ##NaN => ##NaN
  If d is ##Inf => ##Inf
  If d is zero => Double/MIN_VALUE
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextUp-double-"
   :inline-arities #{1}
   :inline (fn [d] `(Math/nextUp (double ~d)))
   :added "1.11"}
  ^double [^double d]
  (Math/nextUp d))

(defn next-down
  {:doc "Returns the adjacent double of d in the direction of ##-Inf.
  If d is ##NaN => ##NaN
  If d is ##-Inf => ##-Inf
  If d is zero => -Double/MIN_VALUE
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextDown-double-"
   :inline-arities #{1}
   :inline (fn [d] `(Math/nextDown (double ~d)))
   :added "1.11"}
  ^double [^double d]
  (Math/nextDown d))

(defn scalb
  {:doc "Returns d * 2^scaleFactor, scaling by a factor of 2. If the exponent
  is between Double/MIN_EXPONENT and Double/MAX_EXPONENT, the answer is exact.
  If d is ##NaN => ##NaN
  If d is ##Inf or ##-Inf => ##Inf or ##-Inf respectively
  If d is zero => zero of same sign as d
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextDown-double-"
   :inline-arities #{2}
   :inline (fn [d scaleFactor] `(Math/scalb (double ~d) (int ~scaleFactor)))
   :added "1.11"}
  ^double [^double d scaleFactor]
  (Math/scalb d (int scaleFactor)))

