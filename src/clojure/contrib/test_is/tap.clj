;;; test_is/tap.clj: Extension to test-is for TAP output

;; by Stuart Sierra, http://stuartsierra.com/
;; March 31, 2009

;; Inspired by ClojureCheck by Meikel Brandmeyer:
;; http://kotka.de/projects/clojure/clojurecheck.html

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.



;; DOCUMENTATION
;;
;; This is an extension to clojure.contrib.test-is that adds support
;; for the Test Anything Protocol (TAP).  
;;
;; TAP is a simple text-based syntax for reporting test results.  TAP
;; was originally develped for Perl, and now has implementations in
;; several languages.  For more information on TAP, see
;; http://testanything.org/ and
;; http://search.cpan.org/~petdance/TAP-1.0.0/TAP.pm
;;
;; To use this library, wrap any calls to
;; clojure.contrib.test-is/run-tests in the with-tap-output macro,
;; like this:
;;
;;   (use 'clojure.contrib.test-is)
;;   (use 'clojure.contrib.test-is.tap)
;;
;;   (with-tap-output
;;    (run-tests 'my.cool.library))



(ns clojure.contrib.test-is.tap
  (:require [clojure.contrib.test-is :as t]
            [clojure.contrib.stacktrace :as stack]))

(defn print-tap-plan
  "Prints a TAP plan line like '1..n'.  n is the number of tests"
  [n]
  (println (str "1.." n)))

(defn print-tap-diagnostic
  "Prints a TAP diagnostic line.  data is a (possibly multi-line)
  string."
  [data]
  (doseq [line (.split data "\n")]
    (println "#" line)))

(defn print-tap-pass
  "Prints a TAP 'ok' line.  msg is a string, with no line breaks"
  [msg]
  (println "ok" msg))

(defn print-tap-fail 
  "Prints a TAP 'not ok' line.  msg is a string, with no line breaks"
  [msg]
  (println "not ok" msg))

;; This multimethod will override test-is/report
(defmulti tap-report (fn [event msg expected actual] event))

(defmethod tap-report :info [event msg expected actual]
  (print-tap-diagnostic msg))

(defmethod tap-report :pass [event msg expected actual]
  (t/inc-report-counter :pass)
  (print-tap-pass (t/testing-vars-str))
  (when (seq t/*testing-contexts*)
    (print-tap-diagnostic (t/testing-contexts-str)))
  (when msg
    (print-tap-diagnostic msg))
  (print-tap-diagnostic (str "expected:" (pr-str expected)))
  (print-tap-diagnostic (str "  actual:" (pr-str actual))))

(defmethod tap-report :error [event msg expected actual]
  (t/inc-report-counter :error)
  (print-tap-fail (t/testing-vars-str))
  (when (seq t/*testing-contexts*)
    (print-tap-diagnostic (t/testing-contexts-str)))
  (when msg
    (print-tap-diagnostic msg))
  (print-tap-diagnostic "expected:" (pr-str expected))
  (print-tap-diagnostic "  actual: ")
  (print-tap-diagnostic
   (with-out-str
    (if (instance? Throwable actual)
      (stack/print-cause-trace actual t/*stack-trace-depth*)
      (prn actual)))))

;; This function will overriede test-is/print-results
(defn tap-print-results [r]
  (print-tap-plan (+ (:pass r) (:fail r) (:error r))))


(defmacro with-tap-output
  "Execute body with modified test-is reporting functions that produce
  TAP output"
  [& body]
  `(binding [t/report tap-report
             t/print-results tap-print-results]
     ~@body))
