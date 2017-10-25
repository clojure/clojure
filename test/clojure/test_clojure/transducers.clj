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
            [clojure.test.check :as chk]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ctest]))

(defmacro fbind [source-gen f]
  `(gen/fmap
     (fn [s#]
       {:desc (list '~f (:name s#))
        :seq  (partial ~f (:val s#))
        :xf   (~f (:val s#))})
     ~source-gen))

(defmacro pickfn [& fns]
  `(gen/elements
     [~@(for [f fns] `{:val ~f :name '~f})]))

(defn literal
  [g]
  (gen/fmap
    (fn [s] {:val s :name s})
    g))

;; These $ versions are "safe" when used with possibly mixed numbers, sequences, etc

(defn- inc$ [n]
  (if (number? n) (inc n) 1))

(defn- dec$ [n]
  (if (number? n) (dec n) 1))

(defn- odd?$ [n]
  (if (number? n) (odd? n) false))

(defn- pos?$ [n]
  (if (number? n) (pos? n) false))

(defn- empty?$ [s]
  (if (instance? clojure.lang.Seqable s) (empty? s) false))

(def gen-mapfn
  (pickfn inc$ dec$))

(def gen-mapcatfn
  (pickfn vector
          #(if (instance? clojure.lang.Seqable %) (partition-all 3 %) (vector %))))

(def gen-predfn
  (pickfn odd?$ pos?$ empty?$ sequential?))

(def gen-indexedfn
  (pickfn (fn [index item] index)
          (fn [index item] item)
          (fn [index item] (if (number? item) (+ index item) index))))

(def gen-take (fbind (literal gen/s-pos-int) take))
(def gen-drop (fbind (literal gen/pos-int) drop))
(def gen-drop-while (fbind gen-predfn drop-while))
(def gen-map (fbind gen-mapfn map))
(def gen-mapcat (fbind gen-mapcatfn mapcat))
(def gen-filter (fbind gen-predfn filter))
(def gen-remove (fbind gen-predfn remove))
(def gen-keep (fbind gen-predfn keep))
(def gen-partition-all (fbind (literal gen/s-pos-int) partition-all))
(def gen-partition-by (fbind gen-predfn partition-by))
(def gen-take-while (fbind gen-predfn take-while))
(def gen-take-nth (fbind (literal gen/s-pos-int) take-nth))
(def gen-keep-indexed (fbind gen-indexedfn keep-indexed))
(def gen-map-indexed (fbind gen-indexedfn map-indexed))
(def gen-replace (fbind (literal (gen/return (hash-map (range 100) (range 1 100)))) replace))
(def gen-distinct (gen/return {:desc 'distinct :seq (partial distinct) :xf (distinct)}))
(def gen-dedupe (gen/return {:desc 'dedupe :seq (partial dedupe) :xf (dedupe)}))
(def gen-interpose (fbind (literal gen/s-pos-int) interpose))

(def gen-action
  (gen/one-of [gen-take gen-drop gen-map gen-mapcat
               gen-filter gen-remove gen-keep
               gen-partition-all gen-partition-by gen-take-while
               gen-take-nth gen-drop-while
               gen-keep-indexed gen-map-indexed
               gen-distinct gen-dedupe gen-interpose]))

(def gen-actions
  (gen/vector gen-action 1 5))

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

(defn apply-as-xf-eduction
  [coll actions]
  (into [] (eduction (apply comp (map :xf actions)) coll)))

(defn apply-as-xf-transduce
  [coll actions]
  (transduce (apply comp (map :xf actions)) conj coll))

(defmacro return-exc [& forms]
  `(try ~@forms (catch Throwable e# e#)))

(defn build-results
  [coll actions]
  (let [s (return-exc (apply-as-seq coll actions))
        xs (return-exc (apply-as-xf-seq coll actions))
        xi (return-exc (apply-as-xf-into coll actions))
        xe (return-exc (apply-as-xf-eduction coll actions))
        xt (return-exc (apply-as-xf-transduce coll actions))]
    {:coll    coll
     :actions (concat '(->> coll) (map :desc actions))
     :s       s
     :xs      xs
     :xi      xi
     :xe      xe
     :xt      xt}))

(def result-gen
  (gen/fmap
    (fn [[c a]] (build-results c a))
    (gen/tuple gen-coll gen-actions)))

(defn result-good?
  [{:keys [s xs xi xe xt]}]
  (= s xs xi xe xt))

(deftest seq-and-transducer
  (let [res (chk/quick-check
              200000
              (prop/for-all* [result-gen] result-good?))]
    (when-not (:result res)
      (is
        (:result res)
        (->
          res
          :shrunk
          :smallest
          first
          clojure.pprint/pprint
          with-out-str)))))

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
  (is (= [[0]] (transduce (comp (take 1) (partition-all 3) (take 1)) conj [] (range 15))))
  (is (= [1] (transduce (take 1) conj (seq (long-array [1 2 3 4]))))))

(deftest test-sequence-multi-xform
  (is (= [11 12 13 14] (sequence (map +) [1 2 3 4] (repeat 10))))
  (is (= [11 12 13 14] (sequence (map +) (repeat 10) [1 2 3 4])))
  (is (= [31 32 33 34] (sequence (map +) (repeat 10) (repeat 20) [1 2 3 4]))))

(deftest test-eduction
  (testing "one xform"
    (is (= [1 2 3 4 5]
           (eduction (map inc) (range 5)))))
  (testing "multiple xforms"
    (is (= ["2" "4"]
           (eduction (map inc) (filter even?) (map str) (range 5)))))
  (testing "materialize at the end"
    (is (= [1 1 1 1 2 2 2 3 3 4]
          (->> (range 5)
            (eduction (mapcat range) (map inc))
            sort)))
    (is (= [1 1 2 1 2 3 1 2 3 4]
          (vec (->> (range 5)
                 (eduction (mapcat range) (map inc))
                 to-array))))
    (is (= {1 4, 2 3, 3 2, 4 1}
          (->> (range 5)
            (eduction (mapcat range) (map inc))
            frequencies)))
    (is (= ["drib" "god" "hsif" "kravdraa" "tac"]
          (->> ["cat" "dog" "fish" "bird" "aardvark"]
            (eduction (map clojure.string/reverse))
            (sort-by first)))))
  (testing "expanding transducer with nils"
           (is (= '(1 2 3 nil 4 5 6 nil)
                  (eduction cat [[1 2 3 nil] [4 5 6 nil]])))))

(deftest test-eduction-completion
  (testing "eduction completes inner xformed reducing fn"
    (is (= [[0 1 2] [3 4 5] [6 7]]
           (into []
                 (comp cat (partition-all 3))
                 (eduction (partition-all 5) (range 8))))))
  (testing "outer reducing fn completed only once"
    (let [counter (atom 0)
          ;; outer rfn
          rf      (completing conj #(do (swap! counter inc)
                                        (vec %)))
          coll    (eduction  (map inc) (range 5))
          res     (transduce (map str) rf [] coll)]
      (is (= 1 @counter))
      (is (= ["1" "2" "3" "4" "5"] res)))))

(deftest test-run!
  (is (nil? (run! identity [1])))
  (is (nil? (run! reduced (range)))))

(deftest test-distinct
  (are [out in] (= out (sequence (distinct in)) (sequence (distinct) in))
       [] []
       (range 10) (range 10)
       [0] (repeat 10 0)
       [0 1 2] [0 0 1 1 2 2 1 1 0 0]
       [1] [1 1N]))

(deftest test-interpose
  (are [out in] (= out (sequence (interpose :s) in))
       [] (range 0)
       [0] (range 1)
       [0 :s 1] (range 2)
       [0 :s 1 :s 2] (range 3))
  (testing "Can end reduction on separator or input"
    (let [expected (interpose :s (range))]
      (dotimes [i 10]
        (is (= (take i expected)
          (sequence (comp (interpose :s) (take i))
                    (range))))))))

(deftest test-map-indexed
  (is (= []
         (sequence (map-indexed vector) [])))
  (is (= [[0 1] [1 2] [2 3] [3 4]]
         (sequence (map-indexed vector) (range 1 5)))))
