;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Stuart Halloway

(ns clojure.test-clojure.rt
  (:use clojure.test clojure.test-helper))

(defn bare-rt-print
  "Return string RT would print prior to print-initialize"
  [x]
  (with-out-str
    (try
     (push-thread-bindings {#'clojure.core/print-initialized false})
     (clojure.lang.RT/print x *out*)
     (finally
      (pop-thread-bindings)))))

(deftest rt-print-prior-to-print-initialize
  (testing "pattern literals"
    (is (= "#\"foo\"" (bare-rt-print #"foo")))))

(deftest error-messages
  (testing "binding a core var that already refers to something"
    (should-print-err-message
     #"WARNING: prefers already refers to: #'clojure.core/prefers in namespace: .*\r?\n"
     (defn prefers [] (throw (RuntimeException. "rebound!")))))
  (testing "reflection cannot resolve field"
    (should-print-err-message
     #"Reflection warning, .*:\d+ - reference to field blah can't be resolved\.\r?\n"
     (defn foo [x] (.blah x))))
  (testing "reflection cannot resolve instance method"
    (should-print-err-message
     #"Reflection warning, .*:\d+ - call to zap can't be resolved with arguments of type \(long\)\.\r?\n"
     (defn foo [x] (.zap x 1))))
  (testing "reflection cannot resolve instance method, with nil literal"
    (should-print-err-message
     #"Reflection warning, .*:\d+ - call to zap can't be resolved with arguments of type \(nil\)\.\r?\n"
     (defn foo [x] (.zap x nil))))
  (testing "reflection cannot resolve static method"
    (should-print-err-message
     #"Reflection warning, .*:\d+ - call to valueOf can't be resolved with arguments of type \(java.util.regex.Pattern\)\.\r?\n"
     (defn foo [] (Integer/valueOf #"boom"))))
  (testing "reflection cannot resolve constructor"
    (should-print-err-message
     #"Reflection warning, .*:\d+ - call to java.lang.String ctor can't be resolved with arguments of type \(long, long, long\)\.\r?\n"
     (defn foo [] (String. 1 2 3)))))

(def example-var)
(deftest binding-root-clears-macro-metadata
  (alter-meta! #'example-var assoc :macro true)
  (is (contains? (meta #'example-var) :macro))
  (.bindRoot #'example-var 0)
  (is (not (contains? (meta #'example-var) :macro))))

(deftest last-var-wins-for-core
  (testing "you can replace a core name, with warning"
    (let [ns (temp-ns)
        replacement (gensym)]
      (with-err-string-writer (intern ns 'prefers replacement))
      (is (= replacement @('prefers (ns-publics ns))))))
  (testing "you can replace a name you defined before"
    (let [ns (temp-ns)
          s (gensym)
          v1 (intern ns 'foo s)
          v2 (intern ns 'bar s)]
      (with-err-string-writer (.refer ns 'flatten v1))
      (.refer ns 'flatten v2)
      (is (= v2 (ns-resolve ns 'flatten)))))
  (testing "you cannot intern over an existing non-core name"
    (let [ns (temp-ns 'clojure.set)
          replacement (gensym)]
      (is (thrown? IllegalStateException
                   (intern ns 'subset? replacement)))
      (is (nil? ('subset? (ns-publics ns))))
      (is (= #'clojure.set/subset? ('subset? (ns-refers ns))))))
  (testing "you cannot refer over an existing non-core name"
    (let [ns (temp-ns 'clojure.set)
          replacement (gensym)]
      (is (thrown? IllegalStateException
                   (.refer ns 'subset? #'clojure.set/intersection)))
      (is (nil? ('subset? (ns-publics ns))))
      (is (= #'clojure.set/subset? ('subset? (ns-refers ns)))))))
