(ns clojure.contrib.test-contrib.java
  (:use clojure.test clojure.contrib.java))

(deftest t-as-str
  (is (= "foo" (as-str "foo")))
  (is (= "foo" (as-str 'foo)))
  (is (= "foo" (as-str :foo)))
  (is (= "[1 2 3]" (as-str [1 2 3])))
  (is (= "Hello, World!" (as-str "Hello, " :World \!)))
  (is (= (str {:foo :bar}) (as-str {:foo :bar}))))
