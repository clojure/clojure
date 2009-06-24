(ns clojure.contrib.test-contrib.walk
  (:require [clojure.contrib.walk :as w])
  (:use clojure.test))

(deftest t-prewalk-replace
  (is (= (w/prewalk-replace {:a :b} [:a {:a :a} (list 3 :c :a)])
         [:b {:b :b} (list 3 :c :b)])))

(deftest t-postwalk-replace 
  (is (= (w/postwalk-replace {:a :b} [:a {:a :a} (list 3 :c :a)])
         [:b {:b :b} (list 3 :c :b)])))

(deftest t-stringify-keys
  (is (= (w/stringify-keys {:a 1, nil {:b 2 :c 3}, :d 4})
         {"a" 1, nil {"b" 2 "c" 3}, "d" 4})))

(deftest t-prewalk-order
  (is (= (let [a (atom [])]
           (w/prewalk (fn [form] (swap! a conj form) form)
                      [1 2 {:a 3} (list 4 [5])])
           @a)
         [[1 2 {:a 3} (list 4 [5])]
          1 2 {:a 3} [:a 3] :a 3 (list 4 [5])
          4 [5] 5])))

(deftest t-postwalk-order
  (is (= (let [a (atom [])]
           (w/postwalk (fn [form] (swap! a conj form) form)
                      [1 2 {:a 3} (list 4 [5])])
           @a)
         [1 2
          :a 3 [:a 3] {:a 3}
          4 5 [5] (list 4 [5])
          [1 2 {:a 3} (list 4 [5])]])))