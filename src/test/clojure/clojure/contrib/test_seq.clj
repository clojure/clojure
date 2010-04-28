(ns clojure.contrib.test-seq
  (:use clojure.test
	clojure.contrib.seq))


(deftest test-positions
  (are [expected pred coll] (= expected (positions pred coll))
       [2] string? [:a :b "c"]
       () :d [:a :b :c]
       [0 2] #{:d} [:d :a :d :a]))

;Upon further inspection, flatten behaves... wierd.
;These tests are what passes on August 7, 2009
(deftest test-flatten-present
  (are [expected nested-val] (= (flatten nested-val) expected)
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
  (are [test-seq] (= (separate even? test-seq) [[2 4] [1 3 5]])
       [1 2 3 4 5]
       #{1 2 3 4 5}
       '(1 2 3 4 5)))

;Note - this does not make sense for maps and sets, because order is expected
(deftest test-indexed
  (are [expected test-seq] (= (indexed test-seq) expected)
       [[0 :a] [1 :b] [2 :c] [3 :d]] [:a :b :c :d]
       [[0 :a] [1 :b] [2 :c] [3 :d]] '(:a :b :c :d)
       [[0 \1] [1 \2] [2 \3] [3 \4]] "1234"))

;Note - this does not make sense for maps and sets, because order is expected
(deftest test-rotations
  (is (= (rotations [1 2 3 4])
	 [[1 2 3 4] 
	  [2 3 4 1]
	  [3 4 1 2]
	  [4 1 2 3]])))

;Note - this does not make sense for maps and sets, because order is expected
(deftest test-find-first
  (is (= (find-first even? [1 2 3 4 5]) 2))
  (is (= (find-first even? '(1 2 3 4 5)) 2)))
