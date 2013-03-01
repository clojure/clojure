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
  (testing "multiarity syntax invalid parameter declaration"
    (is (fails-with-cause? 
          IllegalArgumentException 
          #"Parameter declaration \"arg1\" should be a vector"
          (eval-in-temp-ns (defn foo (arg1 arg2))))))

  (testing "multiarity syntax invalid signature"
    (is (fails-with-cause? 
          IllegalArgumentException 
          #"Invalid signature \"\[a b\]\" should be a list"
          (eval-in-temp-ns (defn foo 
                             ([a] 1)
                             [a b])))))

  (testing "assume single arity syntax"
    (is (fails-with-cause? 
          IllegalArgumentException 
          #"Parameter declaration \"a\" should be a vector"
          (eval-in-temp-ns (defn foo a)))))

  (testing "bad name"
    (is (fails-with-cause? 
          IllegalArgumentException 
          #"First argument to defn must be a symbol"
          (eval-in-temp-ns (defn "bad docstring" testname [arg1 arg2])))))
         
  (testing "missing parameter/signature"
    (is (fails-with-cause? 
          IllegalArgumentException 
          #"Parameter declaration missing"
          (eval-in-temp-ns (defn testname)))))

  (testing "allow trailing map"
    (is (eval-in-temp-ns (defn a "asdf" ([a] 1) {:a :b}))))

  (testing "don't allow interleaved map"
    (is (fails-with-cause? 
          IllegalArgumentException 
          #"Invalid signature \"\{:a :b\}\" should be a list"
          (eval-in-temp-ns (defn a "asdf" ([a] 1) {:a :b} ([] 1)))))))

(deftest non-dynamic-warnings
  (testing "no warning for **"
    (is (empty? (with-err-print-writer
                  (eval-in-temp-ns (defn ** ([a b] (Math/pow (double a) (double b)))))))))
  (testing "warning for *hello*"
    (is (not (empty? (with-err-print-writer
                       (eval-in-temp-ns (def *hello* "hi"))))))))

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
