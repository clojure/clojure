;; This code was used to generate the clojure.math namespace in
;; Clojure 1.11 to wrap Java 1.8 java.lang.Math methods. There are
;; many small tweaks in this to get exactly the output that was
;; desired and it was not intended to be reused in any way, it is
;; included here for future reference.

(ns gen-math
  (:require
    [clojure.reflect :as reflect]
    [clojure.set :as set]
    [clojure.string :as str])
  (:import
    [java.io StringWriter Writer]))

;; manually created
(declare HEADER)
(declare FNS)
(declare DOCS)
(declare ARGS)
(declare ARGTYPES)

(def const-template
  "(def
  ^{:doc %s
    :added %s
    :const true
    :tag %s}
  %s
  %s)\n\n")

(defn- emit-constant
  [^Writer writer {:keys [cname name added type]}]
  (let [sym (symbol (str cname) (str name))
        doc (str "\"" (get DOCS (symbol name)) "\"")
        tag (str "'" type)]
    (.write writer
      (format const-template doc (pr-str added) tag name sym))))

(def fn-template
  "(defn %s
  {:doc %s
   :inline-arities %s
   :inline %s
   :added %s}
  %s%s
  %s)\n\n")

(defn- clojurize
  [sym]
  (or
    (get '{IEEEremainder IEEE-remainder} sym)
    (let [s (name sym)]
      (symbol
        (str
          (reduce
            (fn [^StringBuilder b ^Character c]
              (if (Character/isUpperCase c)
                (.. b (append "-") (append (Character/toLowerCase c)))
                (.append b c)))
            (StringBuilder.)
            s))))))

(defn- inline-body
  [params param-types]
  (str/join " "
    (map (fn [p pt] (format "(%s ~%s)" pt p))
      params param-types)))

(defn- body
  [params param-types on-types]
  (map (fn [p pt] (if (contains? on-types pt) `(~pt ~p) p))
    params param-types))

(defn- emit-fn
  [^Writer writer {:keys [cname fname sigs]}]
  (let [sym (symbol (str cname) (str fname))
        arities (group-by #(-> % :parameter-types count) sigs)
        arity (-> arities keys first) ;; NOTE: ignore multiple arities, none in Math
        arity-sigs (get arities arity)
        cname (clojurize fname)
        doc (str "\"" (get DOCS cname) "\"")
        sig (if (= 1 (count arity-sigs)) (first arity-sigs) (get ARGTYPES cname))
        {pts :parameter-types, rt :return-type} sig
        ps (get ARGS cname)
        ;; coerce all args in inline body
        inline-body (format "(fn %s `(%s%s))" (pr-str ps) (if (< 0 (count ps)) (str sym " ") sym) (inline-body ps pts))
        ;; ps are hinted, so coerce only ps that can't be hinted - int type
        body `(~sym ~@(body ps pts #{'int}))
        rts (if (#{'long 'double} rt) (str "^" rt " ") "")
        hints (map #(if (#{'long 'double} %) (symbol (str "^" %)) nil) pts)
        pst (vec (remove nil? (interleave hints ps)))]
    (.write writer
      (format fn-template cname doc #{arity} inline-body (pr-str "1.11") rts pst body))))

(defn gen-static-wrappers
  [csym]
  (let [added "1.11"
        members (:members (reflect/type-reflect (resolve csym)))
        statics (filter #(set/subset? #{:public :static} (:flags %)) members)
        {fs false, ms true} (group-by #(contains? % :return-type) statics)
        methods (->> ms (filter (fn [m]
                                  (or (= 'scalb (:name m))
                                    (empty? (set/intersection #{'int 'float} (set (:parameter-types m))))))))
        by-name (group-by :name methods)
        writer (StringWriter.)]
    (.write writer HEADER)
    (doseq [f fs]
      (emit-constant writer (merge f {:cname csym, :added added})))
    (doseq [n FNS]
      (emit-fn writer {:cname csym, :fname n, :added added, :sigs (get by-name n)}))
    (spit "src/clj/clojure/math.clj" (str writer))))

(comment
  (gen-static-wrappers 'Math)
  )

;;;; Manually provided info used during the generator

(def ^String HEADER
  ";   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns
  ^{:author \"Alex Miller\",
    :doc \"Clojure wrapper functions for java.lang.Math static methods.

  Function calls are inlined for performance, and type hinted for primitive
  long or double parameters where appropriate. In general, Math methods are
  optimized for performance and have bounds for error tolerance. If
  greater precision is needed, use java.lang.StrictMath directly instead.

  For more complete information, see:
  https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html\"}
  clojure.math)

(set! *warn-on-reflection* true)

")

;; fns

;; omitted: toIntExact
;; omitted but include in core w/polymorphic impl: abs, min, max
(def FNS
  '[sin cos tan asin acos atan toRadians toDegrees exp log log10
    sqrt cbrt IEEEremainder ceil floor rint atan2 pow round random
    addExact subtractExact multiplyExact incrementExact decrementExact negateExact
    floorDiv floorMod ulp signum sinh cosh tanh hypot expm1 log1p copySign getExponent
    nextAfter nextUp nextDown scalb])

;; docstrings to use
(def DOCS
  '{
    E "Constant for e, the base for natural logarithms.\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#E"
    PI "Constant for pi, the ratio of the circumference of a circle to its diameter.\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#PI"
    sin "Returns the sine of an angle.\n  If a is ##NaN, ##-Inf, ##Inf => ##NaN\n  If a is zero => zero with the same sign as a\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#sin-double-"
    cos "Returns the cosine of an angle.\n  If a is ##NaN, ##-Inf, ##Inf => ##NaN\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#cos-double-"
    tan "Returns the tangent of an angle.\n  If a is ##NaN, ##-Inf, ##Inf => ##NaN\n  If a is zero => zero with the same sign as a\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#tan-double-"
    asin "Returns the arc sine of an angle, in the range -pi/2 to pi/2.\n  If a is ##NaN or |a|>1 => ##NaN\n  If a is zero => zero with the same sign as a\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#asin-double-"
    acos "Returns the arc cosine of a, in the range 0.0 to pi.\n  If a is ##NaN or |a|>1 => ##NaN\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#acos-double-"
    atan "Returns the arc tangent of a, in the range of -pi/2 to pi/2.\n  If a is ##NaN => ##NaN\n  If a is zero => zero with the same sign as a\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#atan-double-"
    to-radians "Converts an angle in degrees to an approximate equivalent angle in radians.\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#toRadians-double-"
    to-degrees "Converts an angle in radians to an approximate equivalent angle in degrees.\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#toDegrees-double-"
    exp "Returns Euler's number e raised to the power of a.\n  If a is ##NaN => ##NaN\n  If a is ##Inf => ##Inf\n  If a is ##-Inf => +0.0\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#exp-double-"
    log "Returns the natural logarithm (base e) of a.\n  If a is ##NaN or negative => ##NaN\n  If a is ##Inf => ##Inf\n  If a is zero => ##-Inf\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#log-double-"
    log10 "Returns the logarithm (base 10) of a.\n  If a is ##NaN or negative => ##NaN\n  If a is ##Inf => ##Inf\n  If a is zero => ##-Inf\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#log10-double-"
    sqrt "Returns the positive square root of a.\n  If a is ##NaN or negative => ##NaN\n  If a is ##Inf => ##Inf\n  If a is zero => a\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#sqrt-double-"
    cbrt "Returns the cube root of a.\n  If a is ##NaN => ##NaN\n  If a is ##Inf or ##-Inf => a\n  If a is zero => zero with sign matching a\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#cbrt-double-"
    IEEE-remainder "Returns the remainder per IEEE 754 such that\n    remainder = dividend - divisor * n\n  where n is the integer closest to the exact value of dividend / divisor.\n  If two integers are equally close, then n is the even one.\n  If the remainder is zero, sign will match dividend.\n  If dividend or divisor is ##NaN, or dividend is ##Inf or ##-Inf, or divisor is zero => ##NaN\n  If dividend is finite and divisor is infinite => dividend\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#IEEEremainder-double-double-"
    ceil "Returns the smallest double greater than or equal to a, and equal to a\n  mathematical integer.\n  If a is ##NaN or ##Inf or ##-Inf or already equal to an integer => a\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#ceil-double-"
    floor "Returns the largest double less than or equal to a, and equal to a\n  mathematical integer.\n  If a is ##NaN or ##Inf or ##-Inf or already equal to an integer => a\n  If a is less than zero but greater than -1.0 => -0.0\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floor-double-"
    rint "Returns the double closest to a and equal to a mathematical integer.\n  If two values are equally close, return the even one.\n  If a is ##NaN or ##Inf or ##-Inf or zero => a\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#rint-double-"
    atan2 "Returns the angle theta from the conversion of rectangular coordinates (x, y) to polar coordinates (r, theta).\n  Computes the phase theta by computing an arc tangent of y/x in the range of -pi to pi.\n  For more details on special cases, see:\n  https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#atan2-double-double-"
    pow "Returns the value of a raised to the power of b.\n  For more details on special cases, see:\n  https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#pow-double-double-"
    round "Returns the closest long to a. If equally close to two values, return the one\n  closer to ##Inf.\n  If a is ##NaN => 0\n  If a is ##-Inf or < Long/MIN_VALUE => Long/MIN_VALUE\n  If a is ##Inf or > Long/MAX_VALUE => Long/MAX_VALUE\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#round-double-"
    random "Returns a positive double between 0.0 and 1.0, chosen pseudorandomly with\n  approximately random distribution.\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#random--"
    add-exact "Returns the sum of x and y, throws ArithmeticException on overflow.\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#addExact-long-long-"
    subtract-exact "Returns the difference of x and y, throws ArithmeticException on overflow.\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#subtractExact-long-long-"
    multiply-exact "Returns the product of x and y, throws ArithmeticException on overflow.\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#multiplyExact-long-long-"
    increment-exact "Returns a incremented by 1, throws ArithmeticException on overflow.\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#incrementExact-long-"
    decrement-exact "Returns a decremented by 1, throws ArithmeticException on overflow.\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#decrementExact-long-"
    negate-exact "Returns the negation of a, throws ArithmeticException on overflow.\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#negateExact-long-"
    floor-div "Integer division that rounds to negative infinity (as opposed to zero).\n  The special case (floorDiv Long/MIN_VALUE -1) overflows and returns Long/MIN_VALUE.\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floorDiv-long-long-"
    floor-mod "Integer modulus x - (floorDiv(x, y) * y). Sign matches y and is in the\n  range -|y| < r < |y|.\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floorMod-long-long-"
    ulp "Returns the size of an ulp (unit in last place) for d.\n  If d is ##NaN => ##NaN\n  If d is ##Inf or ##-Inf => ##Inf\n  If d is zero => Double/MIN_VALUE\n  If d is +/- Double/MAX_VALUE => 2^971\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#ulp-double-"
    signum "Returns the signum function of d - zero for zero, 1.0 if >0, -1.0 if <0.\n  If d is ##NaN => ##NaN\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#signum-double-"
    sinh "Returns the hyperbolic sine of x, (e^x - e^-x)/2.\n  If x is ##NaN => ##NaN\n  If x is ##Inf or ##-Inf or zero => x\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#sinh-double-"
    cosh "Returns the hyperbolic cosine of x, (e^x + e^-x)/2.\n  If x is ##NaN => ##NaN\n  If x is ##Inf or ##-Inf => ##Inf\n  If x is zero => 1.0\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#cosh-double-"
    tanh "Returns the hyperbolic tangent of x, sinh(x)/cosh(x).\n  If x is ##NaN => ##NaN\n  If x is zero => zero, with same sign\n  If x is ##Inf => +1.0\n  If x is ##-Inf => -1.0\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#tanh-double-"
    hypot "Returns sqrt(x^2 + y^2) without intermediate underflow or overflow.\n  If x or y is ##Inf or ##-Inf => ##Inf\n  If x or y is ##NaN and neither is ##Inf or ##-Inf => ##NaN\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#hypot-double-double-"
    expm1 "Returns e^x - 1. Near 0, expm1(x)+1 is more accurate to e^x than exp(x).\n  If x is ##NaN => ##NaN\n  If x is ##Inf => #Inf\n  If x is ##-Inf => -1.0\n  If x is zero => x\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#expm1-double-"
    log1p "Returns ln(1+x). For small values of x, log1p(x) is more accurate than\n  log(1.0+x).\n  If x is ##NaN or < -1 => ##NaN\n  If x is ##Inf => ##Inf\n  If x is -1 => ##-Inf\n  If x is 0 => 0 with sign matching x\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#log1p-double-"
    copy-sign "Returns a double with the magnitude of the first argument and the sign of\n  the second.\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#copySign-double-double-"
    get-exponent "Returns the exponent of d.\n  If d is ##NaN, ##Inf, ##-Inf => Double/MAX_EXPONENT + 1\n  If d is zero or subnormal => Double/MIN_EXPONENT - 1\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#getExponent-double-"
    next-after "Returns the adjacent floating point number to start in the direction of\n  the second argument. If the arguments are equal, the second is returned.\n  If either arg is #NaN => #NaN\n  If both arguments are signed zeros => direction\n  If start is +-Double/MIN_VALUE and direction would cause a smaller magnitude\n    => zero with sign matching start\n  If start is ##Inf or ##-Inf and direction would cause a smaller magnitude\n    => Double/MAX_VALUE with same sign as start\n  If start is equal to +=Double/MAX_VALUE and direction would cause a larger magnitude\n    => ##Inf or ##-Inf with sign matching start\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextAfter-double-double-"
    next-up "Returns the adjacent double of d in the direction of ##Inf.\n  If d is ##NaN => ##NaN\n  If d is ##Inf => ##Inf\n  If d is zero => Double/MIN_VALUE\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextUp-double-"
    next-down "Returns the adjacent double of d in the direction of ##-Inf.\n  If d is ##NaN => ##NaN\n  If d is ##-Inf => ##-Inf\n  If d is zero => -Double/MIN_VALUE\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextDown-double-"
    scalb "Returns d * 2^scaleFactor, scaling by a factor of 2. If the exponent\n  is between Double/MIN_EXPONENT and Double/MAX_EXPONENT, the answer is exact.\n  If d is ##NaN => ##NaN\n  If d is ##Inf or ##-Inf => ##Inf or ##-Inf respectively\n  If d is zero => zero of same sign as d\n  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextDown-double-"
    })

(def FNS
  '[sin cos tan asin acos atan toRadians toDegrees exp log log10
    sqrt cbrt IEEEremainder ceil floor rint atan2 pow round random
    addExact subtractExact multiplyExact incrementExact decrementExact negateExact
    floorDiv floorMod ulp signum sinh cosh tanh hypot expm1 log1p copySign getExponent
    nextAfter nextUp nextDown scalb])

;; arg names to use (match java.lang.Math signatures)
(def ARGS
  '{
    sin [a]
    cos [a]
    tan [a]
    asin [a]
    acos [a]
    atan [a]
    to-radians [deg]
    to-degrees [r]
    exp [a]
    log [a]
    log10 [a]
    sqrt [a]
    cbrt [a]
    IEEE-remainder [dividend divisor]
    ceil [a]
    floor [a]
    rint [a]
    atan2 [y x]
    pow [a b]
    round [a]
    random []
    add-exact [x y]
    subtract-exact [x y]
    multiply-exact [x y]
    increment-exact [a]
    decrement-exact [a]
    negate-exact [a]
    floor-div [x y]
    floor-mod [x y]
    ulp [d]
    signum [d]
    sinh [x]
    cosh [x]
    tanh [x]
    hypot [x y]
    expm1 [x]
    log1p [x]
    copy-sign [magnitude sign]
    get-exponent [d]
    next-after [start direction]
    next-up [d]
    next-down [d]
    scalb [d scaleFactor]
    })

;; type signature to use (otherwise automatically determined)
(def ARGTYPES
  '{scalb {:parameter-types [double int] :return-type double}})
