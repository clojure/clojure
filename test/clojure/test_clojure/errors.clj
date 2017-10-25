;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Tests for error handling and messages

(ns clojure.test-clojure.errors
  (:use clojure.test)
  (:import clojure.lang.ArityException))

(defn f0 [] 0)

(defn f1 [a] a)

;; Function name that includes many special characters to test demunge
(defn f2:+><->!#%&*|b [x] x)

(defmacro m0 [] `(identity 0))

(defmacro m1 [a] `(inc ~a))

(deftest arity-exception
  ;; IllegalArgumentException is pre-1.3
  (is (thrown-with-msg? IllegalArgumentException #"Wrong number of args \(1\) passed to"
        (f0 1)))
  (is (thrown-with-msg? ArityException #"Wrong number of args \(0\) passed to"
        (f1)))
  (is (thrown-with-msg? ArityException #"Wrong number of args \(1\) passed to"
        (macroexpand `(m0 1))))
  (is (thrown-with-msg? ArityException #"Wrong number of args \(2\) passed to"
        (macroexpand `(m1 1 2))))
  (is (thrown-with-msg? ArityException #"\Q/f2:+><->!#%&*|b\E"
        (f2:+><->!#%&*|b 1 2))
        "ArityException messages should demunge function names"))

(deftest assert-arg-messages
  ; used to ensure that error messages properly use local names for macros
  (refer 'clojure.core :rename '{with-open renamed-with-open})
  
  ; would have used `are` here, but :line meta on &form doesn't survive successive macroexpansions
  (doseq [[msg-regex-str form] [["renamed-with-open" "(renamed-with-open [a])"]]]
    (is (thrown-with-msg? IllegalArgumentException
                          (re-pattern (format msg-regex-str *ns*))
                          (macroexpand (read-string form))))))

(deftest extract-ex-data
  (try
   (throw (ex-info "example error" {:foo 1}))
   (catch Throwable t
     (is (= {:foo 1} (ex-data t)))))
  (is (nil? (ex-data (RuntimeException. "example non ex-data")))))

(deftest Throwable->map-test
  (testing "base functionality"
    (let [{:keys [cause via trace]} (Throwable->map
                                     (Exception. "I am a string literal"))]
      (is (= cause "I am a string literal"))
      (is (= 1 (count via)))
      (is (vector? via))
      (is (= ["I am a string literal"] (map :message via)))))
  (testing "causes"
    (let [{:keys [cause via trace]} (Throwable->map
                                     (Exception. "I am not a number"
                                                 (Exception. "double two")))]
      (is (= cause "double two"))
      (is (= ["I am not a number" "double two"]
             (map :message via)))))
  (testing "ex-data"
    (let [{[{:keys [data]}] :via
           data-top-level :data}
          (Throwable->map (ex-info "ex-info"
                                   {:some "data"}))]
      (is (= data data-top-level {:some "data"}))))
  (testing "nil stack handled"
    (let [t (Throwable. "abc")]
      ;; simulate what can happen when Java omits stack traces
      (.setStackTrace t (into-array StackTraceElement []))
      (let [{:keys [cause via trace]} (Throwable->map t)]
        (is (= cause "abc"))
        (is (= trace []))

        ;; fail if printing throws an exception
        (try
          (with-out-str (pr t))
          (catch Throwable t (is nil)))))))

(deftest ex-info-disallows-nil-data
  (is (thrown? IllegalArgumentException (ex-info "message" nil)))
  (is (thrown? IllegalArgumentException (ex-info "message" nil (Throwable. "cause")))))

(deftest ex-info-arities-construct-equivalent-exceptions
  (let [ex1 (ex-info "message" {:foo "bar"})
        ex2 (ex-info "message" {:foo "bar"} nil)]
    (is (= (.getMessage ex1) (.getMessage ex2)))
    (is (= (.getData ex1) (.getData ex2)))
    (is (= (.getCause ex1) (.getCause ex2)))))
