(ns clojure.contrib.test-contrib.expect-test
 (:use clojure.test)
 (:require [clojure.contrib.expect :as expect]))

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
     (binding [~fn-name (fn [& args#] (swap! called-status?# (fn [& args#] true)))] ~@body)
     (is (= ~called? @called-status?#))))

(deftest test-convenience
  (testing "once"
    (is (false? (expect/once 0)))
    (is (false? (expect/once 123)))
    (is (true? (expect/once 1))))

  (testing "never"
    (is (false? (expect/never 4)))
    (is (true? (expect/never 0))))

  (testing "more-than"
    (is (false? ((expect/more-than 5) 3)))
    (is (true? ((expect/more-than 5) 9))))

  (testing "less-than"
    (is (true? ((expect/less-than 5) 3)))
    (is (false? ((expect/less-than 5) 9))))

  (testing "between"
    (is (true? ((expect/between 5 8) 6)))
    (is (false? ((expect/between 5 8) 5)))))


(deftest test-returns
  (is (= {:returns 5} (expect/returns 5)))
  (is (= {:other-key "test" :returns nil} (expect/returns nil {:other-key "test"}))))


(deftest test-has-args
  (let [ex (:has-args (expect/has-args [1]))]
    (is (fn? ex))
    (is (ex 'fn1 1))
    (is (ex 'fn1 1 5 6))
    (assert-called expect/unexpected-args true (ex 'fn1 5)))
  (is (contains? (expect/has-args [] {:pre-existing-key "test"}) :pre-existing-key))
  (is (true? (((expect/has-args [5]) :has-args)'fn1 5))))


(deftest test-has-matching-signature
  (assert-called expect/no-matching-function-signature true
    (expect/has-matching-signature? 'clojure.contrib.test-contrib.expect-test/fn2 [1]))
  (assert-called expect/no-matching-function-signature true
    (expect/has-matching-signature? 'clojure.contrib.test-contrib.expect-test/fn3 [1 3]))
  (assert-called expect/no-matching-function-signature false
    (expect/has-matching-signature? 'clojure.contrib.test-contrib.expect-test/fn3 [1 3 5]))
  (assert-called expect/no-matching-function-signature false
    (expect/has-matching-signature? 'clojure.contrib.test-contrib.expect-test/fn4 [1 3 5 7 9]))
  (assert-called expect/no-matching-function-signature false
    (expect/has-matching-signature? 'clojure.contrib.test-contrib.expect-test/fn4 [1 3]))
  (assert-called expect/no-matching-function-signature true
    (expect/has-matching-signature? 'clojure.contrib.test-contrib.expect-test/fn4 [1]))
  (assert-called expect/no-matching-function-signature false
    (expect/has-matching-signature? 'clojure.contrib.test-contrib.expect-test/deffed-differently [1])))


(deftest test-times
  (is (fn? ((expect/times #(= 1 %)) :times)))
  (is (contains? (expect/times #(= 1 %) {:existing-key "test"}) :existing-key)))

(deftest test-make-mock
  (testing "invalid arguments"
    (is (thrown? IllegalArgumentException (expect/make-mock [5]))))

  (testing "valid counter and unevaluated returns"
    (let [[mock counter count-checker] (expect/make-mock 'fn1 (expect/returns 5 (expect/times 1)))]
      (is (fn? mock))
      (is (= 0 @counter))
      (is (= 5 (mock :ignore-me)))
      (is (= 1 @counter))))

  (testing "returns as expected"
    (let [[mock] (expect/make-mock 'fn1 (expect/returns 5))]
      (is (= 5 (mock :ignore))))
    (let [[mock] (expect/make-mock 'fn1 (expect/returns #(* 2 %)))]
      (is (= 10 ((mock :ignore) 5)) ":returns a function should not automatically
                                     evaluate it.")))

  (testing "calls replacement-fn and returns the result"
    (let [[mock] (expect/make-mock 'fn1 (expect/calls #(* 3 %)))]
      (is (= 15 (mock 5))))
    (let [[mock] (expect/make-mock 'fn1 (expect/calls #(* 2 %) (expect/returns 3)))]
      (is (= 10 (mock 5)))))

  (testing "argument validation"
    (let [[mock] (expect/make-mock 'fn1 (expect/has-args [#(= 5 %)]))]
      (assert-called expect/unexpected-args true (mock "test"))
      (is (nil? (mock 5))))))


(deftest test-make-count-checker
  (let [checker (expect/make-count-checker 5 5)]
    (assert-called expect/incorrect-invocation-count false (checker 'fn1 5))
    (assert-called expect/incorrect-invocation-count true (checker 'fn1 3))))


(deftest test-validate-counts
  (assert-called expect/incorrect-invocation-count false
    (expect/validate-counts (list [(fn []) (atom 0) (expect/make-count-checker #(< % 6) '#(< % 6)) 'fn1])))
  (assert-called expect/incorrect-invocation-count true
    (expect/validate-counts (list [(fn []) (atom 0) (expect/make-count-checker 4 4) 'fn1]))))


(deftest test-expect-macro
  (let [under-test (fn [x] (fn1 x))]
    (is (true? (expect/expect [fn1 (expect/times 1 (expect/has-args [#(= 3 %)]))]
                 (under-test 3))))
    (assert-called expect/unexpected-args true (expect/expect [fn1 (expect/times 1 (expect/has-args [#(= 4 %)]))]
                             (under-test 3))))
  (let [under-test (fn [] (fn2 (fn1 1) 3))]
    (is (true? (expect/expect [fn1 (expect/times 1 (expect/has-args [#(= 1 %)] (expect/returns 2)))
                        fn2 (expect/times 1 (expect/has-args [#(= 2 %) #(= 3 %)] (expect/returns 5)))]
                 (under-test))))))