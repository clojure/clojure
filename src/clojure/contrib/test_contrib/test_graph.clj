;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
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


(def empty-graph (struct directed-graph #{} {}))

(def test-graph-1
     (struct directed-graph
             #{:a :b :c :d :e}
             {:a #{:b :c}
              :b #{:a :c}
              :c #{:d :e}
              :d #{:a :b}
              :e #{:d}}))

(deftest test-reverse-graph
  (is (= (reverse-graph test-graph-1)
         (struct directed-graph
                 #{:a :b :c :d :e}
                 {:c #{:b :a}
                  :e #{:c}
                  :d #{:c :e}
                  :b #{:d :a}
                  :a #{:d :b}})))
  (is (= (reverse-graph (reverse-graph test-graph-1))
         test-graph-1))
  (is (= (reverse-graph empty-graph) empty-graph)))

(def test-graph-2
     (struct directed-graph
             #{:a :b :c :d :e :f :g :h :i :j}
             {:a #{:b :c}
              :b #{:a :c} 
              :c #{:d :e}
              :d #{:a :b}
              :e #{:d}
              :f #{:f}
              :g #{:a :f}
              :h #{}
              :i #{:j}
              :j #{:i}}))

(deftest test-post-ordered-nodes
  (is (= (set (post-ordered-nodes test-graph-2))
         #{:a :b :c :d :e :f :g :h :i :j}))
  (is (empty? (post-ordered-nodes empty-graph))))


(deftest test-scc
  (is (= (set (scc test-graph-2))
         #{#{:h} #{:g} #{:i :j} #{:b :c :a :d :e} #{:f}}))
  (is (empty? (scc empty-graph))))

(deftest test-component-graph
  (let [cg (component-graph test-graph-2)
        ecg (component-graph empty-graph)]
    (is (= (:nodes cg) (set (scc test-graph-2))))
    (is (= (get-neighbors cg #{:a :b :c :d :e})
           #{#{:a :b :c :d :e}}))
    (is (= (get-neighbors cg #{:g})
           #{#{:a :b :c :d :e} #{:f}}))
    (is (= (get-neighbors cg #{:i :j})
           #{#{:i :j}}))
    (is (= (get-neighbors cg #{:h})
           #{}))
    (is (= ecg empty-graph))))

(comment
  (run-tests)
)


(deftest test-self-recursive-sets
  (is (= (set (self-recursive-sets test-graph-2))
         #{#{:i :j} #{:b :c :a :d :e} #{:f}}))
  (is (empty? (self-recursive-sets empty-graph))))


(def test-graph-3
     (struct directed-graph
             #{:a :b :c :d :e :f}
             {:a #{:b}
              :b #{:c}
              :c #{:d}
              :d #{:e}
              :e #{:f}
              :f #{}}))

(def test-graph-4
     (struct directed-graph
             #{:a :b :c :d :e :f :g :h}
             {:a #{}
              :b #{:a}
              :c #{:a}
              :d #{:a :b}
              :e #{:d :c}
              :f #{:e}
              :g #{:d}
              :h #{:f}}))

(def test-graph-5
     (struct directed-graph
             #{:a :b :c :d :e :f :g :h}
             {:a #{}
              :b #{}
              :c #{:b}
              :d #{}
              :e #{}
              :f #{}
              :g #{:f}
              :h #{}}))

(deftest test-dependency-list
  (is (thrown-with-msg? Exception #".*Fixed point overflow.*"
                        (dependency-list test-graph-2)))
  (is (= (dependency-list test-graph-3)
         [#{:f} #{:e} #{:d} #{:c} #{:b} #{:a}]))
  (is (= (dependency-list test-graph-4)
         [#{:a} #{:b :c} #{:d} #{:g :e} #{:f} #{:h}]))
  (is (= (dependency-list test-graph-5)
         [#{:f :b :a :d :h :e} #{:g :c}]))
  (is (= (dependency-list empty-graph)
         [#{}])))

(deftest test-stratification-list
  (is (thrown-with-msg? Exception #".*Fixed point overflow.*"
                        (stratification-list test-graph-2 test-graph-2)))
  (is (= (stratification-list test-graph-4 test-graph-5)
         [#{:a} #{:b :c} #{:d} #{:e} #{:f :g} #{:h}]))
  (is (= (stratification-list empty-graph empty-graph)
         [#{}])))

(comment
  (run-tests)
)


;; End of file
