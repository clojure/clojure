;;; test_is/tests.clj: unit tests for test_is.clj

;; by Stuart Sierra, http://stuartsierra.com/
;; January 15, 2009

;; Thanks to Chas Emerick, Allen Rohner, and Stuart Halloway for
;; contributions and suggestions.

;; Copyright (c) Stuart Sierra, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns clojure.contrib.test-is.tests
  (:use clojure.contrib.test-is))

(deftest can-test-symbol
  (let [x true]
    (is x "Should pass"))
  (let [x false]
    (is x "Should fail")))

(deftest can-test-boolean
  (is true "Should pass")
  (is false "Should fail"))

(deftest can-test-nil
  (is nil "Should fail"))

(deftest can-test-=
  (is (= 2 (+ 1 1)) "Should pass")
  (is (= 3 (+ 2 2)) "Should fail"))

(deftest can-test-instance
  (is (instance? Integer (+ 2 2)) "Should pass")
  (is (instance? Float (+ 1 1)) "Should fail"))

(deftest can-test-thrown
  (is (thrown? ArithmeticException (/ 1 0)) "Should pass")
  (is (thrown? Exception (+ 1 1)) "Should fail")
  (is (thrown? ArithmeticException (throw (RuntimeException.))) "Should error"))

(deftest can-catch-unexpected-exceptions
  (is (= 1 (throw (Exception.))) "Should error"))


;; Here, we create an alternate version of test-is/report, that
;; compares the event with the message.  The alternate calls the
;; original with modified arguments.

(declare original-report)

(defn custom-report [event msg expected actual]
  (if (or (and (= event :fail) (= msg "Should fail"))
          (and (= event :pass) (= msg "Should pass"))
          (and (= event :error) (= msg "Should error")))
    (original-report :pass msg expected actual)
    (original-report :fail (str msg " but got " event) expected actual)))

;; test-ns-hook will be used by test-is/test-ns to run tests in this
;; namespace.
(defn test-ns-hook []
  (binding [original-report report
            report custom-report]
    (test-all-vars (find-ns 'clojure.contrib.test-is.tests))))
