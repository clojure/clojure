(ns clojure.contrib.test-seq
  (:use clojure.test)
  (:require [clojure.contrib.seq :as seq]))


(deftest test-positions
  (are [expected pred coll] (= expected (seq/positions pred coll))
       [2] string? [:a :b "c"]
       () :d [:a :b :c]
       [0 2] #{:d} [:d :a :d :a]))

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

;Note - this does not make sense for maps and sets, because order is expected
(deftest test-rotations
  (is (= (seq/rotations [1 2 3 4])
	 [[1 2 3 4] 
	  [2 3 4 1]
	  [3 4 1 2]
	  [4 1 2 3]])))

;Note - this does not make sense for maps and sets, because order is expected
(deftest test-find-first
  (is (= (seq/find-first even? [1 2 3 4 5]) 2))
  (is (= (seq/find-first even? '(1 2 3 4 5)) 2)))
