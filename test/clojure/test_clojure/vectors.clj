;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Stuart Halloway

(ns clojure.test-clojure.vectors
  (:use clojure.test))

(deftest test-reversed-vec
  (let [r (range 6)
        v (into (vector-of :int) r)
        reversed (.rseq v)]
    (testing "returns the right impl"
      (is (= clojure.lang.APersistentVector$RSeq (class reversed))))
    (testing "RSeq methods"
      (is (= [5 4 3 2 1 0] reversed))
      (is (= 5 (.index reversed)))
      (is (= 5 (.first reversed)))
      (is (= [4 3 2 1 0] (.next reversed)))
      (is (= [3 2 1 0] (.. reversed next next)))
      (is (= 6 (.count reversed))))
    (testing "clojure calling through"
      (is (= 5 (first reversed)))
      (is (= 5 (nth reversed 0))))
    (testing "empty reverses to nil"
      (is (nil? (.. v empty rseq))))))

(deftest test-vecseq
  (let [r (range 100)
        vs (into (vector-of :int) r)
        vs-1 (next vs)
        vs-32 (.chunkedNext (seq vs))]
    (testing "="
      (are [a b] (= a b)
           vs vs
           vs-1 vs-1
           vs-32 vs-32)
      (are [a b] (not= a b)
           vs vs-1
           vs-1 vs
           vs vs-32
           vs-32 vs))
    (testing "IPersistentCollection.empty"
      (are [a] (identical? clojure.lang.PersistentList/EMPTY (.empty (seq a)))
           vs vs-1 vs-32))
    (testing "IPersistentCollection.cons"
      (are [result input] (= result (.cons input :foo))
           [:foo 1] (seq (into (vector-of :int) [1]))))
    (testing "IPersistentCollection.count"
      (are [ct s] (= ct (.count (seq s)))
           100 vs
           99 vs-1
           68 vs-32)
      ;; can't manufacture this scenario: ASeq defers to Counted, but
      ;; LazySeq doesn't, so Counted never gets checked on reified seq below
      #_(testing "hops to counted when available"
        (is (= 200
               (.count (concat
                        (seq vs)
                        (reify clojure.lang.ISeq
                               (seq [this] this)
                               clojure.lang.Counted
                               (count [_] 100))))))))
    (testing "IPersistentCollection.equiv"
      (are [a b] (true? (.equiv a b))
           vs vs
           vs-1 vs-1
           vs-32 vs-32
           vs r)
      (are [a b] (false? (.equiv a b))
           vs vs-1
           vs-1 vs
           vs vs-32
           vs-32 vs
           vs nil))))
