;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.run-single-test
  (:require [clojure.test :refer [is deftest run-test run-tests]]
            [clojure.test-helper :refer [with-err-string-writer]]
            [clojure.test-clojure.test-fixtures :as tf]))

(defn not-a-test
  [])

(defmacro should-print-to-err
  [re & body]
  `(is (re-find ~re (with-err-string-writer ~@body))))

(deftest reports-missing-var
  (should-print-to-err #"^Unable to resolve .*/function-missing to a test function.*"
    (let [result (eval `(run-test function-missing))]
      (is (nil? result)))))

(deftest reports-non-test-var
  (should-print-to-err #"^.*/not-a-test is not a test.*"
    (let [result (eval `(run-test not-a-test))]
      (is (nil? result)))))

(deftest can-run-test-with-fixtures
  (is (= {:test 1, :pass 2, :fail 0, :error 0, :type :summary}
        (run-test tf/can-use-once-fixtures))))
