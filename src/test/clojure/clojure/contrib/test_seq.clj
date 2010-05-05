(ns clojure.contrib.test-seq
  (:use clojure.test)
  (:require [clojure.contrib.seq :as seq]))


(deftest test-positions
  (are [expected pred coll] (= expected (seq/positions pred coll))
       [2] string? [:a :b "c"]
       () :d [:a :b :c]
       [0 2] #{:d} [:d :a :d :a]))

;Upon further inspection, flatten behaves... wierd.
;These tests are what passes on August 7, 2009
(deftest test-flatten-present
  (are [expected nested-val] (= (seq/flatten nested-val) expected)
       ;simple literals
       [] nil
       [] 1
       [] 'test
       [] :keyword
       [] 1/2
       [] #"[\r\n]"
       [] true
       [] false
       ;vectors
       [1 2 3 4 5] [[1 2] [3 4 [5]]]
       [1 2 3 4 5] [1 2 3 4 5]
       [#{1 2} 3 4 5] [#{1 2} 3 4 5]
       ;sets
       [] #{}
       [] #{#{1 2} 3 4 5}
       [] #{1 2 3 4 5}
       [] #{#{1 2} 3 4 5}
       ;lists
       [] '()
       [1 2 3 4 5] `(1 2 3 4 5)
       ;maps
       [] {:a 1 :b 2}
       [:a 1 :b 2] (seq {:a 1 :b 2})
       [] {[:a :b] 1 :c 2}
       [:a :b 1 :c 2] (seq {[:a :b] 1 :c 2})
       [:a 1 2 :b 3] (seq {:a [1 2] :b 3})
       ;Strings
       [] "12345"
       [\1 \2 \3 \4 \5] (seq "12345")
       ;fns
       [] count
       [count even? odd?] [count even? odd?]))

(deftest test-separate
  (are [test-seq] (= (seq/separate even? test-seq) [[2 4] [1 3 5]])
       [1 2 3 4 5]
       #{1 2 3 4 5}
       '(1 2 3 4 5)))

;Note - this does not make sense for maps and sets, because order is expected
(deftest test-indexed
  (are [expected test-seq] (= (seq/indexed test-seq) expected)
       [[0 :a] [1 :b] [2 :c] [3 :d]] [:a :b :c :d]
       [[0 :a] [1 :b] [2 :c] [3 :d]] '(:a :b :c :d)
       [[0 \1] [1 \2] [2 \3] [3 \4]] "1234"))

(deftest test-group-by
  (is (= (seq/group-by even? [1 2 3 4 5]) 
	 {false [1 3 5], true [2 4]})))

;Note - this does not make sense for maps and sets, because order is expected
(deftest test-partition-by
  (are [test-seq] (= (seq/partition-by (comp even? count) test-seq)
		     [["a"] ["bb" "cccc" "dd"] ["eee" "f"] ["" "hh"]])
       ["a" "bb" "cccc" "dd" "eee" "f" "" "hh"]
       '("a" "bb" "cccc" "dd" "eee" "f" "" "hh"))
  (is (=(partition-by #{\a \e \i \o \u} "abcdefghijklm")
       [[\a] [\b \c \d] [\e] [\f \g \h] [\i] [\j \k \l \m]])))

(deftest test-frequencies
  (are [expected test-seq] (= (seq/frequencies test-seq) expected)
       {\p 2, \s 4, \i 4, \m 1} "mississippi"
       {1 4 2 2 3 1} [1 1 1 1 2 2 3]
       {1 4 2 2 3 1} '(1 1 1 1 2 2 3)))

;Note - this does not make sense for maps and sets, because order is expected
;This is a key differnce between reductions and reduce.
(deftest test-reductions
  (is (= (seq/reductions + [1 2 3 4 5])
	 [1 3 6 10 15]))
  (is (= (reductions + 10 [1 2 3 4 5])
	 [10 11 13 16 20 25])))

;Note - this does not make sense for maps and sets, because order is expected
(deftest test-rotations
  (is (= (seq/rotations [1 2 3 4])
	 [[1 2 3 4] 
	  [2 3 4 1]
	  [3 4 1 2]
	  [4 1 2 3]])))

;Note - this does not make sense for maps and sets, because order is expected
(deftest test-partition-all
  (is (= (seq/partition-all 4 [1 2 3 4 5 6 7 8 9])
	 [[1 2 3 4] [5 6 7 8] [9]]))
  (is (= (seq/partition-all 4 2 [1 2 3 4 5 6 7 8 9])
	 [[1 2 3 4] [3 4 5 6] [5 6 7 8] [7 8 9] [9]])))

;Thanks to Andy Fingerhut for the idea of testing invariants
(deftest test-shuffle-invariants
  (is (= (count (seq/shuffle [1 2 3 4])) 4))
  (let [shuffled-seq (seq/shuffle [1 2 3 4])]
    (is (every? #{1 2 3 4} shuffled-seq))))

;Thanks to Andy Fingerhut for the idea of testing invariants
(deftest test-rand-elt-invariants
  (let [elt (seq/rand-elt [:a :b :c :d])]
    (is (#{:a :b :c :d} elt))))

;Note - this does not make sense for maps and sets, because order is expected
(deftest test-find-first
  (is (= (seq/find-first even? [1 2 3 4 5]) 2))
  (is (= (seq/find-first even? '(1 2 3 4 5)) 2)))

(deftest test-includes
  (are [coll k] (false? (seq/includes? coll k))
       [1 2 3] 0
       [] nil
       [:a :b] :c)
  (are [coll k] (true? (seq/includes? coll k))
       [1 2 3] 1
       [:a :b] :b))
