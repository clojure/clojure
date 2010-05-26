(ns clojure.test-clojure.transients
  (:use clojure.test))

(deftest popping-off
  (testing "across a node boundary"
    (let [v (-> (range 33) vec)]
      (is (= (subvec v 0 31) (-> v transient pop! pop! persistent!)))))
  (testing "off the end"
    (is (thrown-with-msg? IllegalStateException #"Can't pop empty vector"
          (-> [] transient pop!)))))
