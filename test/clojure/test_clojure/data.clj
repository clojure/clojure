;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.data
  (:use clojure.data clojure.test)
  (import java.util.HashSet))

(deftest diff-test
  (are [d x y] (= d (diff x y))
       [nil nil nil] nil nil
       [1 2 nil] 1 2
       [nil nil [1 2 3]] [1 2 3] '(1 2 3)
       [1 [:a :b] nil] 1 [:a :b]
       [{:a 1} :b nil] {:a 1} :b
       [:team #{:p1 :p2} nil] :team #{:p1 :p2}
       [{0 :a} [:a] nil] {0 :a} [:a]
       [nil [nil 2] [1]] [1] [1 2]
       [nil nil [1 2]] [1 2] (into-array [1 2])
       [#{:a} #{:b} #{:c :d}] #{:a :c :d} #{:b :c :d}
       [nil nil {:a 1}] {:a 1} {:a 1}
       [{:a #{2}} {:a #{4}} {:a #{3}}] {:a #{2 3}} {:a #{3 4}}
       [#{1} #{3} #{2}] (HashSet. [1 2]) (HashSet. [2 3])
       [nil nil [1 2]] [1 2] (into-array [1 2])
       [nil nil [1 2]] (into-array [1 2]) [1 2]
       [{:a {:c [1]}} {:a {:c [0]}} {:a {:c [nil 2] :b 1}}] {:a {:b 1 :c [1 2]}} {:a {:b 1 :c [0 2]}}))

