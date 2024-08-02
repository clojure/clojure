;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.clearing
  (:import
    [java.lang.reflect Field])
  (:require
    [clojure.string :as str]
    [clojure.test :refer :all]))

(set! *warn-on-reflection* true)

(defn fields
  [o]
  (.getDeclaredFields (class o)))

(defn primitive?
  [^Field field]
  (.isPrimitive (.getType field)))

(defn special-fn-field?
  [^String field-name]
  (or (= field-name "__meta")
    (str/starts-with? field-name "__cached_class__")
    (str/starts-with? field-name "const__")
    (str/ends-with? field-name "__")))

(defn clearable-closed-overs
  [fobj]
  (->> (fields fobj)
    (remove primitive?) ;; can't clear primitives
    (remove #(special-fn-field? (.getName ^Field %)))))

(defn private-field-value [^Object obj ^Field field]
  (. field (setAccessible true))
  (. field (get obj)))

;; Check whether all non-primitive closed-overs in a function are nil
(defn cleared?
  [fobj]
  (every? #(nil? (private-field-value fobj %)) (clearable-closed-overs fobj)))

;; ---

;; After invocation, check all closed-over non-primitive fields in a :once fn

(defn check-clear
  [f]
  (is (not (cleared? f)))
  (f)
  (cleared? f))

(deftest test-clearing
  (let [x :a]
    ;; base case
    (is (check-clear (^{:once true} fn* [] x)))

    ;; conditional above fn
    (when true
      (is (check-clear (^{:once true} fn* [] x))))
    (case x
      :a (is (check-clear (^{:once true} fn* [] x))))

    ;; loop above fn
    (loop []
      (is (check-clear (^{:once true} fn* [] x))))

    ;; conditional below fn
    (is (check-clear (^{:once true} fn* [] (when true x))))

    ;; loop below fn
    (is (not (check-clear (^{:once true} fn* [] (loop [] x)))))
    (is (not (check-clear (^{:once true} fn* [] (loop [] x) nil))))

    ;; recur in :once below fn
    (is (not (check-clear (^{:once true} fn* [] (if false (recur) x)))))
    ))

(deftest test-nested
  (let [x :a]
    ;; nested fns
    (let [inner (^{:once true} fn* [] x)
          outer (fn* [] inner)]
      (is (not (check-clear outer))) ;; outer not :once
      (is (check-clear inner)))

    (let [inner (^{:once true} fn* [] x)
          outer (^{:once true} fn* [] inner)]
      (is (check-clear outer))
      (is (check-clear inner)))

    (let [inner (^{:once true} fn* [] x)
          middle (^{:once true} fn* [] inner)
          outer (^{:once true} fn* [] middle)]
      (is (check-clear outer))
      (is (check-clear middle))
      (is (check-clear inner)))))

;; Repro from CLJ-2145
(defn consume [x] (doseq [_ x] _))
(defn call-and-keep [f] (f) f)
(defn repro [x]
  (if true (call-and-keep (^:once fn* [] (consume x)))))
(deftest CLJ-2145-repro
  (let [f (repro (range 100))] ;; 1e9 to exhaust
    (is (cleared? f))))