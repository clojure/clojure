(ns clojure.contrib.test-contrib.fnmap
  (:use clojure.contrib.fnmap
        clojure.contrib.test-is))

(deftest acts-like-map
  (let [m1 (fnmap get assoc :key1 1 :key2 2)]
    (are (= _2 (get m1 _1))
         :key1 1
         :key2 2
         :nonexistent-key nil)
    (are (= _2 (_1 m1))
         :key1 1
         :key2 2
         :nonexistent-key nil)
    (let [m2 (assoc m1 :key3 3 :key4 4)]
      (are (= _2 (get m2 _1))
           :key1 1
           :key2 2
           :key3 3
           :key4 4
           :nonexistent-key nil))))

(defn assoc-validate [m key value]
  (if (integer? value)
    (assoc m key value)
    (throw (Exception. "Only integers allowed in this map!"))))

(deftest validators
  (let [m (fnmap get assoc-validate)]
    (is (= 2 (:key2 (assoc m :key2 2))))
    (is (thrown? Exception (assoc m :key3 3.14)))))

(defn get-transform [m key]
  (when-let [value (m key)]
    (- value)))

(deftest transforms
  (let [m (fnmap get-transform assoc)]
    (is (= -2 (:key2 (assoc m :key2 2))))))
