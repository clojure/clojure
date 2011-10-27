;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.def
  (:use clojure.test clojure.test-helper
        clojure.test-clojure.protocols))

(deftest defn-error-messages
  (testing "bad arglist forms"
    (is (fails-with-cause? IllegalArgumentException '#"Parameter declaration arg1 should be a vector"
          (eval-in-temp-ns (defn foo (arg1 arg2)))))))

(deftest dynamic-redefinition
  ;; too many contextual things for this kind of caching to work...
  (testing "classes are never cached, even if their bodies are the same"
    (is (= :b
          (eval
            '(do
               (defmacro my-macro [] :a)
               (defn do-macro [] (my-macro))
               (defmacro my-macro [] :b)
               (defn do-macro [] (my-macro))
               (do-macro)))))))

(deftest nested-dynamic-declaration
  (testing "vars :dynamic meta data is applied immediately to vars declared anywhere"
    (is (= 10
          (eval
            '(do
               (list
                 (declare ^:dynamic p)
                 (defn q [] @p))
               (binding [p (atom 10)]
                 (q))))))))