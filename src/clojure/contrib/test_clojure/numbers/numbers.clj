;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  Tests for the Clojure functions documented at the URL:
;;
;;    http://clojure.org/Reader
;;
;;  scgilardi (gmail)
;;  Created 22 October 2008

(ns clojure.contrib.test-clojure.numbers
  (:use clojure.contrib.test-is))

(deftest Coerced-Byte
  (let [v (byte 3)]
    (is (instance? Byte v))
    (is (number? v))
    (is (integer? v))
    (is (rational? v))))

(deftest Coerced-Short
  (let [v (short 3)]
    (is (instance? Short v))
    (is (number? v))
    (is (integer? v))
    (is (rational? v))))

(deftest Coerced-Integer
  (let [v (int 3)]
    (is (instance? Integer v))
    (is (number? v))
    (is (integer? v))
    (is (rational? v))))

(deftest Coerced-Long
  (let [v (long 3)]
    (is (instance? Long v))
    (is (number? v))
    (is (integer? v))
    (is (rational? v))))

(deftest Coerced-BigInteger
  (let [v (bigint 3)]
    (is (instance? BigInteger v))
    (is (number? v))
    (is (integer? v))
    (is (rational? v))))

(deftest Coerced-Float
  (let [v (float 3)]
    (is (instance? Float v))
    (is (number? v))
    (is (float? v))))

(deftest Coerced-Double
  (let [v (double 3)]
    (is (instance? Double v))
    (is (number? v))
    (is (float? v))))

(deftest Coerced-BigDecimal
  (let [v (bigdec 3)]
    (is (instance? BigDecimal v))
    (is (number? v))
    (is (decimal? v))
    (is (not (float? v)))))
