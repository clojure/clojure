;;  Copyright (c) Stephen C. Gilardi. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.
;;
;;  pred.clj
;;
;;  Some clojure predicates
;;
;;  scgilardi (gmail)
;;  28 June 2008

(clojure/in-ns 'clojure.contrib.pred)
(clojure/refer 'clojure)

(defn coll?
  "Returns true if x implements IPersistentCollection"
  [x]
  (instance? clojure.lang.IPersistentCollection x))

(defn list?
  "Returns true if x implements IPersistentList"
  [x]
  (instance? clojure.lang.IPersistentList x))

;; map

(defn set?
  "Returns true if x implements IPersistentSet"
  [x]
  (instance? clojure.lang.IPersistentSet x))

(defn stack?
  "Returns true if x implements IPersistentStack"
  [x]
  (instance? clojure.lang.IPersistentStack x))

;; vector

(defn ref?
  "Returns true if x implements IRef"
  [x]
  (instance? clojure.lang.IRef x))

;; seq
;; var

(defn map-entry?
  "Returns true if x is a MapEntry"
  [x]
  (instance? clojure.lang.MapEntry x))

(defn atom?
  "Returns true if x is not a collection"
  [x]
  (not (coll? x)))

(defn number?
  "Returns true if x is a Number"
  [x]
  (instance? Number x))

(defn ratio?
  "Returns true if x is a Ratio"
  [x]
  (instance? clojure.lang.Ratio x))

(defn range?
  "Returns true if x is a Range"
  [x]
  (instance? clojure.lang.Range x))

(defn function?
  "Returns true if x implements IFn"
  [x]
  (instance? clojure.lang.IFn x))

(defmacro macro?
  "Returns true if x is a function and the symbol of the
  same name can be resolved and has its :macro metadata
  set"
  [x]
  `(and (function? ~x) (boolean (:macro ^#'~x))))

(defn integer?
  "Returns true if x is an integer"
  [x]
  (or (instance? Integer x)
      (instance? Long x)
      (instance? BigInteger x)))

(defn even?
  "Returns true if x is even, throws an exception if x is not an integer"
  [x]
  (zero? (bit-and x 1)))

(defn odd?
  "Returns true if x is odd, throws an exception if x is not an integer"
  [x]
  (not (even? x)))

(defn empty?
  "Returns true if coll is empty"
  [coll]
  (not (seq coll)))
