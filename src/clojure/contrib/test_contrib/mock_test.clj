(ns clojure.contrib.test-contrib.mock-test
 (:use clojure.test)
 (:require [clojure.contrib.mock :as mock]))

; Used as dummy dependency functions
(defn fn1 [x] :ignore)
(defn fn2 [x y] :ignore)
(defn fn3 ([x] :ignore)
  ([x y z] :ignore))
(defn fn4 [x y & r] :ignore)

;functions created using fn directly lack the argslist meta data
(def deffed-differently (fn [x] :ignore))

(defmacro assert-called [fn-name called? & body]
  `(let [called-status?# (atom false)]
     (binding [~fn-name (fn [& args#] (reset! called-status?# true))] ~@body)
     (is (= ~called? @called-status?#))))

(deftest test-convenience
  (testing "once"
    (is (false? (mock/once 0)))
    (is (false? (mock/once 123)))
    (is (true? (mock/once 1))))

  (testing "never"
    (is (false? (mock/never 4)))
    (is (true? (mock/never 0))))

  (testing "more-than"
    (is (false? ((mock/more-than 5) 3)))
    (is (true? ((mock/more-than 5) 9))))

  (testing "less-than"
    (is (true? ((mock/less-than 5) 3)))
    (is (false? ((mock/less-than 5) 9))))

  (testing "between"
    (is (true? ((mock/between 5 8) 6)))
    (is (false? ((mock/between 5 8) 5)))))


(deftest test-returns
  (is (= {:returns 5} (mock/returns 5)))
  (is (= {:other-key "test" :returns nil} (mock/returns nil {:other-key "test"}))))


(deftest test-has-args
  (let [ex (:has-args (mock/has-args [1]))]
    (is (fn? ex))
    (is (ex 'fn1 1))
    (is (ex 'fn1 1 5 6))
    (assert-called mock/unexpected-args true (ex 'fn1 5)))
  (is (contains? (mock/has-args [] {:pre-existing-key "test"}) :pre-existing-key))
  (is (true? (((mock/has-args [5]) :has-args)'fn1 5))))


(deftest test-has-matching-signature
  (assert-called mock/no-matching-function-signature true
    (mock/has-matching-signature? 'clojure.contrib.test-contrib.mock-test/fn2 [1]))
  (assert-called mock/no-matching-function-signature true
    (mock/has-matching-signature? 'clojure.contrib.test-contrib.mock-test/fn3 [1 3]))
  (assert-called mock/no-matching-function-signature false
    (mock/has-matching-signature? 'clojure.contrib.test-contrib.mock-test/fn3 [1 3 5]))
  (assert-called mock/no-matching-function-signature false
    (mock/has-matching-signature? 'clojure.contrib.test-contrib.mock-test/fn4 [1 3 5 7 9]))
  (assert-called mock/no-matching-function-signature false
    (mock/has-matching-signature? 'clojure.contrib.test-contrib.mock-test/fn4 [1 3]))
  (assert-called mock/no-matching-function-signature true
    (mock/has-matching-signature? 'clojure.contrib.test-contrib.mock-test/fn4 [1]))
  (assert-called mock/no-matching-function-signature false
    (mock/has-matching-signature? 'clojure.contrib.test-contrib.mock-test/deffed-differently [1])))


(deftest test-times
  (is (fn? ((mock/times #(= 1 %)) :times)))
  (is (contains? (mock/times #(= 1 %) {:existing-key "test"}) :existing-key)))

(deftest test-make-mock
  (testing "invalid arguments"
    (is (thrown? IllegalArgumentException (mock/make-mock [5]))))

  (testing "valid counter and unevaluated returns"
    (let [[mock counter count-checker] (mock/make-mock 'fn1 (mock/returns 5 (mock/times 1)))]
      (is (fn? mock))
      (is (= 0 @counter))
      (is (= 5 (mock :ignore-me)))
      (is (= 1 @counter))))

  (testing "returns as expected"
    (let [[mock] (mock/make-mock 'fn1 (mock/returns 5))]
      (is (= 5 (mock :ignore))))
    (let [[mock] (mock/make-mock 'fn1 (mock/returns #(* 2 %)))]
      (is (= 10 ((mock :ignore) 5)) ":returns a function should not automatically
                                     evaluate it.")))

  (testing "calls replacement-fn and returns the result"
    (let [[mock] (mock/make-mock 'fn1 (mock/calls #(* 3 %)))]
      (is (= 15 (mock 5))))
    (let [[mock] (mock/make-mock 'fn1 (mock/calls #(* 2 %) (mock/returns 3)))]
      (is (= 10 (mock 5)))))

  (testing "argument validation"
    (let [[mock] (mock/make-mock 'fn1 (mock/has-args [#(= 5 %)]))]
      (assert-called mock/unexpected-args true (mock "test"))
      (is (nil? (mock 5))))))


(deftest test-make-count-checker
  (let [checker (mock/make-count-checker 5 5)]
    (assert-called mock/incorrect-invocation-count false (checker 'fn1 5))
    (assert-called mock/incorrect-invocation-count true (checker 'fn1 3))))


(deftest test-validate-counts
  (assert-called mock/incorrect-invocation-count false
    (mock/validate-counts (list [(fn []) (atom 0) (mock/make-count-checker #(< % 6) '#(< % 6)) 'fn1])))
  (assert-called mock/incorrect-invocation-count true
    (mock/validate-counts (list [(fn []) (atom 0) (mock/make-count-checker 4 4) 'fn1]))))


(deftest test-expect-macro
  (let [under-test (fn [x] (fn1 x))]
    (is (true? (mock/expect [fn1 (mock/times 1 (mock/has-args [#(= 3 %)]))]
                 (under-test 3))))
    (assert-called mock/unexpected-args true (mock/expect [fn1 (mock/times 1 (mock/has-args [#(= 4 %)]))]
                             (under-test 3))))
  (let [under-test (fn [] (fn2 (fn1 1) 3))]
    (is (true? (mock/expect [fn1 (mock/times 1 (mock/has-args [#(= 1 %)] (mock/returns 2)))
                        fn2 (mock/times 1 (mock/has-args [#(= 2 %) #(= 3 %)] (mock/returns 5)))]
                 (under-test))))))