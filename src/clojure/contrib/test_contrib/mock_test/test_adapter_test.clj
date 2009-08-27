(ns clojure.contrib.test-contrib.mock-test.test-adapter-test
 (:use clojure.contrib.mock.test-adapter
   [clojure.contrib.test-contrib.mock-test :only (assert-called)]
   clojure.test))

(deftest test-report-problem-called
  (def #^{:private true} fn1 (fn [x] "dummy code"))
  (def #^{:private true} fn2 (fn [x y] "dummy code2"))
  (let [under-test (fn [x] (fn1 x))]
    (assert-called clojure.contrib.mock.test-adapter/report-problem
      true (expect [fn1 (times 5)] (under-test "hi")))))

(deftest test-is-report-called
  (assert-called clojure.test/report true
    (clojure.contrib.mock.test-adapter/report-problem
      'fn-name 5 6 "fake problem")))


