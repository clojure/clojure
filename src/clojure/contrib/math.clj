;;; math.clj: math functions that deal intelligently with the various
;;; types in Clojure's numeric tower, as well as math functions
;;; commonly found in Scheme implementations.

;; by Mark Engelberg (mark.engelberg@gmail.com)
;; January 17, 2009

;; expt - (expt x y) is x to the yth power, returns an exact number
;;   if the base is an exact number, and the power is an integer,
;;   otherwise returns a double.
;; abs - (abs n) is the absolute value of n
;; gcd - (gcd m n) returns the greatest common divisor of m and n
;; lcm - (lcm m n) returns the least common multiple of m and n

;; The behavior of the next three functions on doubles is consistent
;; with the behavior of the corresponding functions
;; in Java's Math library, but on exact numbers, returns an integer.

;; floor - (floor n) returns the greatest integer less than or equal to n.
;;   If n is an exact number, floor returns an integer,
;;   otherwise a double.
;; ceil - (ceil n) returns the least integer greater than or equal to n.
;;   If n is an exact number, ceil returns an integer,
;;   otherwise a double.
;; round - (round n) rounds to the nearest integer.
;;   round always returns an integer.  round rounds up for values
;;   exactly in between two integers.


;; sqrt - Implements the sqrt behavior I'm accustomed to from PLT Scheme,
;;   specifically, if the input is an exact number, and is a square
;;   of an exact number, the output will be exact.  The downside
;;   is that for the common case (inexact square root), some extra
;;   computation is done to look for an exact square root first.
;;   So if you need blazingly fast square root performance, and you
;;   know you're just going to need a double result, you're better
;;   off calling java's Math/sqrt, or alternatively, you could just
;;   convert your input to a double before calling this sqrt function.
;;   If Clojure ever gets complex numbers, then this function will
;;   need to be updated (so negative inputs yield complex outputs).
;; exact-integer-sqrt - Implements a math function from the R6RS Scheme
;;   standard.  (exact-integer-sqrt k) where k is a non-negative integer,
;;   returns [s r] where k = s^2+r and k < (s+1)^2.  In other words, it
;;   returns the floor of the square root and the "remainder".

(ns 
  #^{:author "Mark Engelberg",
     :doc "Math functions that deal intelligently with the various
types in Clojure's numeric tower, as well as math functions
commonly found in Scheme implementations.

expt - (expt x y) is x to the yth power, returns an exact number
  if the base is an exact number, and the power is an integer,
  otherwise returns a double.
abs - (abs n) is the absolute value of n
gcd - (gcd m n) returns the greatest common divisor of m and n
lcm - (lcm m n) returns the least common multiple of m and n

The behavior of the next three functions on doubles is consistent
with the behavior of the corresponding functions
in Java's Math library, but on exact numbers, returns an integer.

floor - (floor n) returns the greatest integer less than or equal to n.
  If n is an exact number, floor returns an integer,
  otherwise a double.
ceil - (ceil n) returns the least integer greater than or equal to n.
  If n is an exact number, ceil returns an integer,
  otherwise a double.
round - (round n) rounds to the nearest integer.
  round always returns an integer.  round rounds up for values
  exactly in between two integers.


sqrt - Implements the sqrt behavior I'm accustomed to from PLT Scheme,
  specifically, if the input is an exact number, and is a square
  of an exact number, the output will be exact.  The downside
  is that for the common case (inexact square root), some extra
  computation is done to look for an exact square root first.
  So if you need blazingly fast square root performance, and you
  know you're just going to need a double result, you're better
  off calling java's Math/sqrt, or alternatively, you could just
  convert your input to a double before calling this sqrt function.
  If Clojure ever gets complex numbers, then this function will
  need to be updated (so negative inputs yield complex outputs).
exact-integer-sqrt - Implements a math function from the R6RS Scheme
  standard.  (exact-integer-sqrt k) where k is a non-negative integer,
  returns [s r] where k = s^2+r and k < (s+1)^2.  In other words, it
  returns the floor of the square root and the "remainder".
"}
  clojure.contrib.math)

(derive ::integer ::exact)
(derive java.lang.Integer ::integer)
(derive java.math.BigInteger ::integer)
(derive java.lang.Long ::integer)
(derive java.math.BigDecimal ::exact)
(derive clojure.lang.Ratio ::exact)
(derive java.lang.Double ::inexact)
(derive java.lang.Float ::inexact)

(defmulti #^{:arglists '([base pow])
	     :doc "(expt base pow) is base to the pow power.
Returns an exact number if the base is an exact number and the power is an integer, otherwise returns a double."}
  expt (fn [x y] [(class x) (class y)]))

(defn- expt-int [base pow]
  (loop [n pow, y 1, z base]
    (let [t (bit-and n 1), n (bit-shift-right n 1)]
      (cond
       (zero? t) (recur n y (* z z))
       (zero? n) (* z y)
       :else (recur n (* z y) (* z z))))))

(defmethod expt [::exact ::integer] [base pow]
  (cond
   (pos? pow) (expt-int base pow)
   (zero? pow) 1
   :else (/ 1 (expt-int base (- pow)))))

(defmethod expt :default [base pow] (Math/pow base pow))

(defn abs "(abs n) is the absolute value of n" [n]
  (cond
   (not (number? n)) (throw (IllegalArgumentException.
			     "abs requires a number"))
   (neg? n) (- n)
   :else n))

(defmulti #^{:arglists '([n])
	     :doc "(floor n) returns the greatest integer less than or equal to n.
If n is an exact number, floor returns an integer, otherwise a double."}
  floor class)
(defmethod floor ::integer [n] n)
(defmethod floor java.math.BigDecimal [n] (.. n (setScale 0 BigDecimal/ROUND_FLOOR) (toBigInteger)))
(defmethod floor clojure.lang.Ratio [n]
  (if (pos? n) (quot (. n numerator) (. n denominator))
      (dec (quot (. n numerator) (. n denominator)))))
(defmethod floor :default [n]
  (Math/floor n))

(defmulti #^{:arglists '([n])
	     :doc "(ceil n) returns the least integer greater than or equal to n.
If n is an exact number, ceil returns an integer, otherwise a double."}
  ceil class)
(defmethod ceil ::integer [n] n)
(defmethod ceil java.math.BigDecimal [n] (.. n (setScale 0 BigDecimal/ROUND_CEILING) (toBigInteger)))
(defmethod ceil clojure.lang.Ratio [n]
  (if (pos? n) (inc (quot (. n numerator) (. n denominator)))
      (quot (. n numerator) (. n denominator))))
(defmethod ceil :default [n]
  (Math/ceil n))

(defmulti #^{:arglists '([n])
	     :doc "(round n) rounds to the nearest integer.
round always returns an integer.  Rounds up for values exactly in between two integers."}
  round class)
(defmethod round ::integer [n] n)
(defmethod round java.math.BigDecimal [n] (floor (+ n 0.5M)))
(defmethod round clojure.lang.Ratio [n] (floor (+ n 1/2)))
(defmethod round :default [n] (Math/round n))

(defn gcd "(gcd a b) returns the greatest common divisor of a and b" [a b]
  (if (or (not (integer? a)) (not (integer? b)))
    (throw (IllegalArgumentException. "gcd requires two integers"))  
    (loop [a (abs a) b (abs b)]
      (if (zero? b) a,
	  (recur b (mod a b))))))

(defn lcm
  "(lcm a b) returns the least common multiple of a and b"
  [a b]
  (when (or (not (integer? a)) (not (integer? b)))
    (throw (IllegalArgumentException. "lcm requires two integers")))
  (cond (zero? a) 0
        (zero? b) 0
        :else (abs (* b (quot a (gcd a b))))))

; Length of integer in binary, used as helper function for sqrt.
(defmulti #^{:private true} integer-length class)
(defmethod integer-length java.lang.Integer [n]
  (count (Integer/toBinaryString n)))
(defmethod integer-length java.lang.Long [n]
  (count (Long/toBinaryString n)))
(defmethod integer-length java.math.BigInteger [n]
  (count (. n toString 2)))

;; Produces the largest integer less than or equal to the square root of n
;; Input n must be a non-negative integer
(defn- integer-sqrt [n]
  (cond
   (> n 24)
   (let [n-len (integer-length n)]
     (loop [init-value (if (even? n-len)
			 (bit-shift-left 1 (bit-shift-right n-len 1))
			 (bit-shift-left 2 (bit-shift-right n-len 1)))]
       (let [iterated-value (bit-shift-right (+ init-value (quot n init-value)) 1)]
	 (if (>= iterated-value init-value)
	   init-value
	   (recur iterated-value)))))
   (> n 15) 4
   (> n  8) 3
   (> n  3) 2
   (> n  0) 1
   (> n -1) 0))

(defn exact-integer-sqrt "(exact-integer-sqrt n) expects a non-negative integer n, and returns [s r] where n = s^2+r and n < (s+1)^2.  In other words, it returns the floor of the square root and the 'remainder'.
For example, (exact-integer-sqrt 15) is [3 6] because 15 = 3^2+6."
  [n]
  (if (or (not (integer? n)) (neg? n))
    (throw (IllegalArgumentException. "exact-integer-sqrt requires a non-negative integer"))
    (let [isqrt (integer-sqrt n),
	  error (- n (* isqrt isqrt))]
      [isqrt error])))

(defmulti #^{:arglists '([n])
	     :doc "Square root, but returns exact number if possible."}
  sqrt class)
(defmethod sqrt ::integer [n]
  (if (neg? n) Double/NaN
      (let [isqrt (integer-sqrt n),
	    error (- n (* isqrt isqrt))]
	(if (zero? error) isqrt
	    (Math/sqrt n)))))

(defmethod sqrt clojure.lang.Ratio [n]
  (if (neg? n) Double/NaN
      (let [numerator (.numerator n),
	    denominator (.denominator n),
	    sqrtnum (sqrt numerator)]
	(if (float? sqrtnum)
	  (Math/sqrt n)
	  (let [sqrtden (sqrt denominator)]
	    (if (float? sqrtnum)
	      (Math/sqrt n)
	      (/ sqrtnum sqrtden)))))))

(defmethod sqrt java.math.BigDecimal [n]
  (if (neg? n) Double/NaN
      (let [frac (rationalize n),
	    sqrtfrac (sqrt frac)]
	(if (ratio? sqrtfrac)
	  (/ (BigDecimal. (.numerator sqrtfrac))
	     (BigDecimal. (.denominator sqrtfrac)))
	  sqrtfrac))))

(defmethod sqrt :default [n]
  (Math/sqrt n))
