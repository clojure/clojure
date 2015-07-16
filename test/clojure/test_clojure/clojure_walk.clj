(ns clojure.test-clojure.clojure-walk
  (:require [clojure.walk :as w])
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

(defrecord Foo [a b c])

(deftest walk
         "Checks that walk returns the correct result and type of collection"
         (let [colls ['(1 2 3)
                      [1 2 3]
                      #{1 2 3}
                      (sorted-set-by > 1 2 3)
                      {:a 1, :b 2, :c 3}
                      (sorted-map-by > 1 10, 2 20, 3 30)
                      (->Foo 1 2 3)
                      (map->Foo {:a 1 :b 2 :c 3 :extra 4})]]
           (doseq [c colls]
             (let [walked (w/walk identity identity c)]
               (is (= c walked))
									;;(is (= (type c) (type walked)))
               (if (map? c)
                 (is (= (w/walk #(update-in % [1] inc) #(reduce + (vals %)) c)
                        (reduce + (map (comp inc val) c))))
                 (is (= (w/walk inc #(reduce + %) c)
                        (reduce + (map inc c)))))
               (when (or (instance? clojure.lang.PersistentTreeMap c)
                         (instance? clojure.lang.PersistentTreeSet c))
                 (is (= (.comparator c) (.comparator walked))))))))

