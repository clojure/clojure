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
  (is (= (map set (scc test-graph-2))
         [#{8 9} #{7} #{6} #{5} #{0 1 2 3 4}]))
  (is (empty? (scc empty-graph))))

(comment
  (run-tests)
)



