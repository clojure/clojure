;;; test_adapter.clj: clojure.test adapter for mocking/expectation framework for Clojure

;; by Matt Clark

;; Copyright (c) Matt Clark, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
;; By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.mock.test-adapter
 (:require [clojure.contrib.mock :as mock])
 (:use clojure.test
       clojure.contrib.ns-utils))

(immigrate 'clojure.contrib.mock)

(defn report-problem
 "This function is designed to be used in a binding macro to override
the report-problem function in clojure.contrib.mock. Instead of printing
the error to the console, the error is logged via clojure.test."
 [fn-name expected actual msg]
 (report {:type :fail,
          :message (str msg " Function name: " fn-name),
          :expected expected,
          :actual actual}))


(defmacro expect [& body]
  "Use this macro instead of the standard c.c.mock expect macro to have
failures reported through clojure.test."
  `(binding [mock/report-problem report-problem]
     (mock/expect ~@body)))



