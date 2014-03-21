;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Author: Tassilo Horn

(ns clojure.test-clojure.reducers
  (:require [clojure.core.reducers :as r]
            [clojure.test.generative :refer (defspec)]
            [clojure.data.generators :as gen])
  (:use clojure.test))

(defmacro defequivtest
  ;; f is the core fn, r is the reducers equivalent, rt is the reducible ->
  ;; coll transformer
  [name [f r rt] fns]
  `(deftest ~name
     (let [c# (range -100 1000)]
       (doseq [fn# ~fns]
         (is (= (~f fn# c#)
                (~rt (~r fn# c#))))))))

(defequivtest test-map
  [map r/map #(into [] %)]
  [inc dec #(Math/sqrt (Math/abs %))])

(defequivtest test-mapcat
  [mapcat r/mapcat #(into [] %)]
  [(fn [x] [x])
   (fn [x] [x (inc x)])
   (fn [x] [x (inc x) x])])

(deftest test-mapcat-obeys-reduced
  (is (= [1 "0" 2 "1" 3]
        (->> (concat (range 100) (lazy-seq (throw (Exception. "Too eager"))))
          (r/mapcat (juxt inc str))
          (r/take 5)
          (into [])))))

(defequivtest test-reduce
  [reduce r/reduce identity]
  [+' *'])

(defequivtest test-filter
  [filter r/filter #(into [] %)]
  [even? odd? #(< 200 %) identity])


(deftest test-sorted-maps
  (let [m (into (sorted-map)
                '{1 a, 2 b, 3 c, 4 d})]
    (is (= "1a2b3c4d" (reduce-kv str "" m))
        "Sorted maps should reduce-kv in sorted order")
    (is (= 1 (reduce-kv (fn [acc k v]
                          (reduced (+ acc k)))
                        0 m))
        "Sorted maps should stop reduction when asked")))

(deftest test-nil
  (is (= {:k :v} (reduce-kv assoc {:k :v} nil)))
  (is (= 0 (r/fold + nil))))

(defn gen-num []
  (gen/uniform 0 2000))

(defn reduced-at-probe
  [m p]
  (reduce-kv (fn [_ k v] (when (== p k) (reduced :foo))) nil m))

(defspec reduced-always-returns
  (fn [probe to-end]
    (let [len (+ probe to-end 1)
          nums (range len)
          m (zipmap nums nums)]
      (reduced-at-probe m probe)))
  [^{:tag `gen-num} probe ^{:tag `gen-num} to-end]
  (assert (= :foo %)))

(deftest test-fold-runtime-exception
  (is (thrown? IndexOutOfBoundsException
               (let [test-map-count 1234
                     k-fail (rand-int test-map-count)]
                 (r/fold (fn ([])
                           ([ret [k v]])
                           ([ret k v] (when (= k k-fail)
                                        (throw (IndexOutOfBoundsException.)))))
                         (zipmap (range test-map-count) (repeat :dummy)))))))
