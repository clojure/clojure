;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Stuart Halloway

(ns clojure.test-clojure.rt
  (:use clojure.test))

(defmacro with-err-print-writer
  "Evaluate with err pointing to a temporary PrintWriter, and
   return err contents as a string."
  [& body]
  `(let [s# (java.io.StringWriter.)
         p# (java.io.PrintWriter. s#)]
     (binding [*err* p#]
       ~@body
       (str s#))))

(defmacro with-err-string-writer
  "Evaluate with err pointing to a temporary StringWriter, and
   return err contents as a string."
  [& body]
  `(let [s# (java.io.StringWriter.)]
     (binding [*err* s#]
       ~@body
       (str s#))))

(defmacro eval-in-temp-ns [form]
  `(binding [*ns* *ns*]
     (in-ns (gensym))
     (clojure.core/use 'clojure.core)
     (eval ~form)))

(defmacro should-print-err-message
  "Turn on all warning flags, and test that error message prints
   correctly for all semi-reasonable bindings of *err*."
  [msg-re form]
  (binding [*warn-on-reflection* true]
    (is (re-matches msg-re (with-err-string-writer (eval-in-temp-ns form))))
    (is (re-matches msg-re (with-err-print-writer (eval-in-temp-ns form))))))

(deftest error-messages
  (testing "binding a var that already refers to something"
    (should-print-err-message
     #"WARNING: prefers already refers to: #'clojure.core/prefers in namespace: .*\n"
     (defn prefers [] (throw (RuntimeException. "rebound!")))))
  (testing "reflection cannot resolve field"
    (should-print-err-message
     #"Reflection warning, clojure/test_clojure/rt.clj:\d+ - reference to field blah can't be resolved.\n"
     (defn foo [x] (.blah x))))
  (testing "reflection cannot resolve instance method"
    (should-print-err-message
     #"Reflection warning, clojure/test_clojure/rt.clj:\d+ - call to zap can't be resolved.\n"
     (defn foo [x] (.zap x 1))))
  (testing "reflection cannot resolve static method"
    (should-print-err-message
     #"Reflection warning, clojure/test_clojure/rt.clj:\d+ - call to valueOf can't be resolved.\n"
     (defn foo [] (Integer/valueOf #"boom"))))
  (testing "reflection cannot resolved constructor"
    (should-print-err-message
     #"Reflection warning, clojure/test_clojure/rt.clj:\d+ - call to java.lang.String ctor can't be resolved.\n"
     (defn foo [] (String. 1 2 3)))))
