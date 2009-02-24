;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  test-graph
;;
;;  Basic Graph Theory Algorithms Tests
;;
;;  straszheimjeffrey (gmail)
;;  Created 23 June 2009

(ns clojure.contrib.test-contrib.test-graph
  (use clojure.contrib.test-is
       clojure.contrib.graph))


(def empty-graph (struct directed-graph 0 {}))

(def test-graph-1
     (struct directed-graph 5
             {0 [1 2]
              1 [0 2]
              2 [3 4]
              3 [0 1]
              4 [3]}))

(deftest test-reverse-graph
  (is (= (reverse-graph test-graph-1)
         {:count 5, :neighbors {
                                0 [1 3]
                                1 [0 3]
                                2 [0 1]
                                3 [2 4]
                                4 [2]}}))
  (is (= (reverse-graph (reverse-graph test-graph-1))
         test-graph-1))
  (is (= (reverse-graph empty-graph) empty-graph)))

(def test-graph-2
     (struct directed-graph 10
             {0 [1 2]
              1 [0 2]
              2 [3 4]
              3 [0 1]
              4 [3]
              5 [5]
              6 [0 5]
              7 []
              8 [9]
              9 [8]}))

(deftest test-post-ordered-nodes
  (is (= (post-ordered-nodes test-graph-2)
         [3 4 2 1 0 5 6 7 9 8]))
  (is (empty? (post-ordered-nodes empty-graph))))


(deftest test-scc
  (is (= (scc test-graph-2)
         [#{8 9} #{7} #{6} #{5} #{0 1 2 3 4}]))
  (is (empty? (scc empty-graph))))


(deftest test-self-recursive-sets
  (is (= (self-recursive-sets test-graph-2)
         [#{8 9} #{5} #{0 1 2 3 4}]))
  (is (empty? (self-recursive-sets empty-graph))))


(def test-graph-3
     (struct directed-graph 6
             {0 [1]
              1 [2]
              2 [3]
              3 [4]
              4 [5]
              5 []}))

(def test-graph-4
     (struct directed-graph 8
             {0 []
              1 [0]
              2 [0]
              3 [0 1]
              4 [3 2]
              5 [4]
              6 [3]
              7 [5]}))

  (def test-graph-5
       (struct directed-graph 8
               {0 []
                1 []
                2 [1]
                3 []
                4 []
                5 []
                6 [5]
                7 []}))

(deftest test-dependency-list
  (is (thrown-with-msg? Exception #".*Fixed point overflow.*"
                        (dependency-list test-graph-2)))
  (is (= (dependency-list test-graph-3)
         [#{5} #{4} #{3} #{2} #{1} #{0}]))
  (is (= (dependency-list test-graph-4)
         [#{0} #{1 2} #{3} #{4 6} #{5} #{7}]))
  (is (= (dependency-list test-graph-5)
         [#{0 1 3 4 5 7} #{2 6}]))
  (is (= (dependency-list empty-graph)
         [#{}])))

(deftest test-stratification-list
  (is (thrown-with-msg? Exception #".*Fixed point overflow.*"
                        (stratification-list test-graph-2 test-graph-2)))
  (is (= (stratification-list test-graph-4 test-graph-5)
         [#{0} #{1 2} #{3} #{4} #{5 6} #{7}]))
  (is (= (stratification-list empty-graph empty-graph)
         [#{}])))

(comment
  (run-tests)
)



