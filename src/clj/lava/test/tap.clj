;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;;; test_is/tap.clj: Extension to test for TAP output

;; by Stuart Sierra
;; March 31, 2009

;; Inspired by LavaCheck by Meikel Brandmeyer:
;; http://kotka.de/projects/lava/lavacheck.html


;; DOCUMENTATION
;;



(ns ^{:doc "lava.test extensions for the Test Anything Protocol (TAP)

  TAP is a simple text-based syntax for reporting test results.  TAP
  was originally develped for Perl, and now has implementations in
  several languages.  For more information on TAP, see
  http://testanything.org/ and
  http://search.cpan.org/~petdance/TAP-1.0.0/TAP.pm

  To use this library, wrap any calls to
  lava.test/run-tests in the with-tap-output macro,
  like this:

    (use 'lava.test)
    (use 'lava.test.tap)

    (with-tap-output
     (run-tests 'my.cool.library))"
       :author "Stuart Sierra"}
  lava.test.tap
  (:require [lava.test :as t]
            [lava.stacktrace :as stack]))

(defn print-tap-plan
  "Prints a TAP plan line like '1..n'.  n is the number of tests"
  {:added "1.1"}
  [n]
  (println (str "1.." n)))

(defn print-tap-diagnostic
  "Prints a TAP diagnostic line.  data is a (possibly multi-line)
  string."
  {:added "1.1"}
  [data]
  (doseq [line (.split ^String data "\n")]
    (println "#" line)))

(defn print-tap-pass
  "Prints a TAP 'ok' line.  msg is a string, with no line breaks"
  {:added "1.1"}
  [msg]
  (println "ok" msg))

(defn print-tap-fail 
  "Prints a TAP 'not ok' line.  msg is a string, with no line breaks"
  {:added "1.1"}
  [msg]
  (println "not ok" msg))

;; This multimethod will override test/report
(defmulti tap-report (fn [data] (:type data)))

(defmethod tap-report :default [data]
  (t/with-test-out
   (print-tap-diagnostic (pr-str data))))

(defmethod tap-report :pass [data]
  (t/with-test-out
   (t/inc-report-counter :pass)
   (print-tap-pass (t/testing-vars-str))
   (when (seq t/*testing-contexts*)
     (print-tap-diagnostic (t/testing-contexts-str)))
   (when (:message data)
     (print-tap-diagnostic (:message data)))
   (print-tap-diagnostic (str "expected:" (pr-str (:expected data))))
   (print-tap-diagnostic (str "  actual:" (pr-str (:actual data))))))

(defmethod tap-report :error [data]
  (t/with-test-out
   (t/inc-report-counter :error)
   (print-tap-fail (t/testing-vars-str))
   (when (seq t/*testing-contexts*)
     (print-tap-diagnostic (t/testing-contexts-str)))
   (when (:message data)
     (print-tap-diagnostic (:message data)))
   (print-tap-diagnostic "expected:" (pr-str (:expected data)))
   (print-tap-diagnostic "  actual: ")
   (print-tap-diagnostic
    (with-out-str
      (if (instance? Throwable (:actual data))
        (stack/print-cause-trace (:actual data) t/*stack-trace-depth*)
        (prn (:actual data)))))))

(defmethod tap-report :summary [data]
  (t/with-test-out
   (print-tap-plan (+ (:pass data) (:fail data) (:error data)))))


(defmacro with-tap-output
  "Execute body with modified test reporting functions that produce
  TAP output"
  {:added "1.1"}
  [& body]
  `(binding [t/report tap-report]
     ~@body))
