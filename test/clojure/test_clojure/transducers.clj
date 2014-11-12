;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Alex Miller

(ns clojure.test-clojure.transducers
  (:require [clojure.string :as s]
            [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ctest]))

(defmacro fbind [source-gen f]
  `(gen/bind ~source-gen
             (fn [s#]
               (gen/return {:desc (str ~(str f) " " s#)
                            :seq  (partial ~f s#)
                            :xf   (~f s#)}))))

(def gen-mapfn
  (gen/elements [inc dec]))

(def gen-predfn
  (gen/elements [odd? even? pos? zero? empty? sequential?]))

(def gen-indexedfn
  (gen/elements [(fn [index item] index)
                 (fn [index item] item)
                 (fn [index item] (+ index item))]))

(def gen-take (fbind gen/s-pos-int take))
(def gen-drop (fbind gen/pos-int drop))
(def gen-map (fbind gen-mapfn map))
(def gen-mapcat (fbind gen-mapfn mapcat))
(def gen-filter (fbind gen-predfn filter))
(def gen-remove (fbind gen-predfn remove))
(def gen-keep (fbind gen-predfn keep))
(def gen-partition-all (fbind gen/s-pos-int partition-all))
(def gen-partition-by (fbind gen-predfn partition-by))
(def gen-take-while (fbind gen-predfn take-while))
(def gen-take-nth (fbind gen/s-pos-int take-nth))
(def gen-drop-while (fbind gen-predfn drop-while))
(def gen-keep-indexed (fbind gen-indexedfn keep-indexed))
(def gen-replace (fbind (gen/return (hash-map (range 100) (range 1 100))) replace))

(def gen-action
  (gen/one-of [gen-take gen-drop gen-map gen-mapcat
               gen-filter gen-remove gen-keep
               gen-partition-all gen-partition-by gen-take-while
               gen-take-nth gen-drop-while gen-keep-indexed]))

(def gen-actions
  (gen/vector gen-action))

(def gen-coll
  (gen/vector gen/int))

(defn apply-as-seq [coll actions]
  (doall
    (loop [s coll
           [action & actions'] actions]
          (if action
            (recur ((:seq action) s) actions')
            s))))

(defn apply-as-xf-seq
  [coll actions]
  (doall (sequence (apply comp (map :xf actions)) coll)))

(defn apply-as-xf-into
  [coll actions]
  (into [] (apply comp (map :xf actions)) coll))

(defn apply-as-xf-transduce
  [coll actions]
  (transduce (apply comp (map :xf actions)) conj coll))

(defn- possible-exception? [ex]
       (or (instance? IllegalArgumentException ex)
           (instance? ClassCastException ex)))

(defn seq-and-transducer-same-result
  [coll actions]
  #_(println "test" (map :desc actions) "on" coll)
  (let [s (try (apply-as-seq coll actions)
               (catch Throwable e e))
        xs (try (apply-as-xf-seq coll actions)
                (catch Throwable e e))
        xi (try (apply-as-xf-into coll actions)
                (catch Throwable e e))
        xt (try (apply-as-xf-transduce coll actions)
                (catch Throwable e e))]
    (if (or ((every-pred possible-exception?) s xs xi xt)
            (= s xs xi xt))
      true
      (throw (ex-info "Applied actions to coll as seq, sequence transducer, and into transducer and got different results."
                      {:coll    coll
                       :actions (s/join "," (map :desc actions))
                       :s       s
                       :xs      xs
                       :xi      xi
                       :xt      xt})))))

(ctest/defspec seq-and-transducer
               100000
               (prop/for-all* [gen-coll gen-actions]
                              seq-and-transducer-same-result))

(deftest test-transduce
  (let [long+ (fn ([a b] (+ (long a) (long b)))
                ([a] a)
                ([] 0))
        mapinc (map inc)
        mapinclong (map (comp inc long))
        arange (range 100)
        avec (into [] arange)
        alist (into () arange)
        obj-array (into-array arange)
        int-array (into-array Integer/TYPE (map #(Integer. (int %)) arange))
        long-array (into-array Long/TYPE arange)
        float-array (into-array Float/TYPE arange)
        char-array (into-array Character/TYPE (map char arange))
        double-array (into-array Double/TYPE arange)
        byte-array (into-array Byte/TYPE (map byte arange))
        int-vec (into (vector-of :int) arange)
        long-vec (into (vector-of :long) arange)
        float-vec (into (vector-of :float) arange)
        char-vec (into (vector-of :char) (map char arange))
        double-vec (into (vector-of :double) arange)
        byte-vec (into (vector-of :byte) (map byte arange))]
    (is (== 5050
            (transduce mapinc + arange)
            (transduce mapinc + avec)
            (transduce mapinc + alist)
            (transduce mapinc + obj-array)
            (transduce mapinc + int-array)
            (transduce mapinc + long-array)
            (transduce mapinc + float-array)
            (transduce mapinclong + char-array)
            (transduce mapinc + double-array)
            (transduce mapinclong + byte-array)
            (transduce mapinc + int-vec)
            (transduce mapinc + long-vec)
            (transduce mapinc + float-vec)
            (transduce mapinclong + char-vec)
            (transduce mapinc + double-vec)
            (transduce mapinclong + byte-vec)
            ))
    (is (== 5051
            (transduce mapinc + 1 arange)
            (transduce mapinc + 1 avec)
            (transduce mapinc + 1 alist)
            (transduce mapinc + 1 obj-array)
            (transduce mapinc + 1 int-array)
            (transduce mapinc + 1 long-array)
            (transduce mapinc + 1 float-array)
            (transduce mapinclong + 1 char-array)
            (transduce mapinc + 1 double-array)
            (transduce mapinclong + 1 byte-array)
            (transduce mapinc + 1 int-vec)
            (transduce mapinc + 1 long-vec)
            (transduce mapinc + 1 float-vec)
            (transduce mapinclong + 1 char-vec)
            (transduce mapinc + 1 double-vec)
            (transduce mapinclong + 1 byte-vec)))))

(deftest test-dedupe
  (are [x y] (= (transduce (dedupe) conj x) y)
             [] []
             [1] [1]
             [1 2 3] [1 2 3]
             [1 2 3 1 2 2 1 1] [1 2 3 1 2 1]
             [1 1 1 2] [1 2]
             [1 1 1 1] [1]

             "" []
             "a" [\a]
             "aaaa" [\a]
             "aabaa" [\a \b \a]
             "abba" [\a \b \a]

             [nil nil nil] [nil]
             [1 1.0 1.0M 1N] [1 1.0 1.0M 1N]
             [0.5 0.5] [0.5]))

(deftest test-cat
  (are [x y] (= (transduce cat conj x) y)
             [] []
             [[1 2]] [1 2]
             [[1 2] [3 4]] [1 2 3 4]
             [[] [3 4]] [3 4]
             [[1 2] []] [1 2]
             [[] []] []
             [[1 2] [3 4] [5 6]] [1 2 3 4 5 6]))

(deftest test-partition-all
  (are [n coll y] (= (transduce (partition-all n) conj coll) y)
                  2 [1 2 3] '((1 2) (3))
                  2 [1 2 3 4] '((1 2) (3 4))
                  2 [] ()
                  1 [] ()
                  1 [1 2 3] '((1) (2) (3))
                  5 [1 2 3] '((1 2 3))))

(deftest test-take
  (are [n y] (= (transduce (take n) conj [1 2 3 4 5]) y)
             1 '(1)
             3 '(1 2 3)
             5 '(1 2 3 4 5)
             9 '(1 2 3 4 5)
             0 ()
             -1 ()
             -2 ()))

(deftest test-drop
  (are [n y] (= (transduce (drop n) conj [1 2 3 4 5]) y)
             1 '(2 3 4 5)
             3 '(4 5)
             5 ()
             9 ()
             0 '(1 2 3 4 5)
             -1 '(1 2 3 4 5)
             -2 '(1 2 3 4 5)))

(deftest test-take-nth
  (are [n y] (= (transduce (take-nth n) conj [1 2 3 4 5]) y)
             1 '(1 2 3 4 5)
             2 '(1 3 5)
             3 '(1 4)
             4 '(1 5)
             5 '(1)
             9 '(1)))

(deftest test-take-while
  (are [coll y] (= (transduce (take-while pos?) conj coll) y)
                [] ()
                [1 2 3 4] '(1 2 3 4)
                [1 2 3 -1] '(1 2 3)
                [1 -1 2 3] '(1)
                [-1 1 2 3] ()
                [-1 -2 -3] ()))

(deftest test-drop-while
  (are [coll y] (= (transduce (drop-while pos?) conj coll) y)
                [] ()
                [1 2 3 4] ()
                [1 2 3 -1] '(-1)
                [1 -1 2 3] '(-1 2 3)
                [-1 1 2 3] '(-1 1 2 3)
                [-1 -2 -3] '(-1 -2 -3)))

(deftest test-re-reduced
  (is (= [:a] (transduce (take 1) conj [:a])))
  (is (= [:a] (transduce (comp (take 1) (take 1)) conj [:a])))
  (is (= [:a] (transduce (comp (take 1) (take 1) (take 1)) conj [:a])))
  (is (= [:a] (transduce (comp (take 1) (take 1) (take 1) (take 1)) conj [:a])))
  (is (= [[:a]] (transduce (comp (partition-by keyword?) (take 1)) conj [] [:a])))
  (is (= [[:a]] (sequence (comp (partition-by keyword?) (take 1)) [:a])))
  (is (= [[[:a]]] (sequence (comp (partition-by keyword?) (take 1)  (partition-by keyword?) (take 1)) [:a])))
  (is (= [[0]] (transduce (comp (take 1) (partition-all 3) (take 1)) conj [] (range 15)))))
