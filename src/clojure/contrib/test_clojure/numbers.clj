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
    (all-true
     (instance? Byte v)
     (number? v)
     (integer? v)
     (rational? v))))

(deftest Coerced-Short
  (let [v (short 3)]
    (all-true
     (instance? Short v)
     (number? v)
     (integer? v)
     (rational? v))))

(deftest Coerced-Integer
  (let [v (int 3)]
    (all-true
     (instance? Integer v)
     (number? v)
     (integer? v)
     (rational? v))))

(deftest Coerced-Long
  (let [v (long 3)]
    (all-true
     (instance? Long v)
     (number? v)
     (integer? v)
     (rational? v))))

(deftest Coerced-BigInteger
  (let [v (bigint 3)]
    (all-true
     (instance? BigInteger v)
     (number? v)
     (integer? v)
     (rational? v))))

(deftest Coerced-Float
  (let [v (float 3)]
    (all-true
     (instance? Float v)
     (number? v)
     (float? v))))

(deftest Coerced-Double
  (let [v (double 3)]
    (all-true
     (instance? Double v)
     (number? v)
     (float? v))))

(deftest Coerced-BigDecimal
  (let [v (bigdec 3)]
    (all-true
     (instance? BigDecimal v)
     (number? v)
     (decimal? v)
     (not (float? v)))))
