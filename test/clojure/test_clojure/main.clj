;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Stuart Halloway


(ns clojure.test-clojure.main
  (:use clojure.test
        [clojure.test-helper :only [platform-newlines]])
  (:require [clojure.main :as main]))

(deftest eval-opt
  (testing "evals and prints forms"
    (is (= (platform-newlines "2\n4\n") (with-out-str (#'clojure.main/eval-opt "(+ 1 1) (+ 2 2)")))))

  (testing "skips printing nils"
    (is (= (platform-newlines ":a\n:c\n") (with-out-str (#'clojure.main/eval-opt ":a nil :c")))))

  (testing "does not block access to *in* (#299)"
    (with-in-str "(+ 1 1)"
      (is (= (platform-newlines "(+ 1 1)\n") (with-out-str (#'clojure.main/eval-opt "(read)")))))))

(defmacro with-err-str
  "Evaluates exprs in a context in which *err* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
  [& body]
  `(let [s# (new java.io.StringWriter)
         p# (new java.io.PrintWriter s#)]
     (binding [*err* p#]
       ~@body
       (str s#))))

(defn run-repl-and-return-err
  "Run repl, swallowing stdout and returing stderr."
  [in-str]
  (with-err-str
    (with-out-str
      (with-in-str in-str
        (main/repl)))))

;argh - test fragility, please fix
#_(deftest repl-exception-safety
  (testing "catches and prints exception on bad equals"
    (is (re-matches #"java\.lang\.NullPointerException\r?\n"
           (run-repl-and-return-err
            "(proxy [Object] [] (equals [o] (.toString nil)))")))))

(deftest null-stack-error-reporting
  (let [e (doto (Error. "xyz")
            (.setStackTrace (into-array java.lang.StackTraceElement nil)))
        tr-data (-> e Throwable->map main/ex-triage)]
    (is (= tr-data #:clojure.error{:phase :execution, :class 'java.lang.Error, :cause "xyz"}))
    (is (= (main/ex-str tr-data) (platform-newlines "Execution error (Error) at (REPL:1).\nxyz\n")))))

(defn s->lpr
  [s]
  (-> s (java.io.StringReader.) (clojure.lang.LineNumberingPushbackReader.)))

(deftest renumbering-read
  (are [s line-in line-out]
    (= line-out (-> (main/renumbering-read nil (s->lpr s) line-in) meta :line))
    "(let [x 1] x)" 100 100
    "^{:line 20 :clojure.core/eval-file \"a/b.clj\"} (let [x 1] x)" 100 20
    "^{:line 20} (let [x 1] x)" 100 20))

(deftest java-loc->source
  (are [c m out]
    (= out (#'main/java-loc->source c m))
    'user$eval1                'invokeStatic 'user/eval1
    'div$go                    'invokeStatic 'div/go
    'user$eval186$fn__187      'invoke       'user/eval186$fn
    'user$ok_fn$broken_fn__164 'invoke       'user/ok-fn$broken-fn
    'clojure.lang.Numbers      'divide       'clojure.lang.Numbers/divide))