;;; condp.clj - generic case-like macro using template expressions

;; By Stuart Sierra, http://stuartsierra.com/
;; December 15, 2008

;; Copyright (c) Stuart Sierra, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns clojure.contrib.condp
    (:require clojure.contrib.template))

(defmacro condp
  "expr is a template expression (see template), clauses are test/expr
  pairs like cond.  Evalautes the template on each test value, one at
  a time.  If a test returns logical true, condp evaluates the
  corresponding expr and returns its value.  If none of the tests are
  true, and there are an odd number of clauses, the last clause is
  evaluated, otherwise returns nil."
  [expr & clauses]
  (let [test-fn-sym (gensym "test_")
        f (fn this [c]
            (cond
             (empty? c) nil
             (= 1 (count c)) (first c)
             :else (list 'if (list test-fn-sym (first c))
                         (second c)
                         (this (rrest c)))))]
    `(let [~test-fn-sym (clojure.contrib.template/template ~expr)]
       ~(f clauses))))

(defmacro econdp
  "Like condp but throws Exception if no tests match."
  [expr & clauses]
  (let [test-fn-sym (gensym "test_")
        f (fn this [c]
              (cond
               (empty? c) '(throw (Exception. "Nothing matched in econdp."))
               (= 1 (count c)) (throw (IllegalStateException. "Odd number of clauses in econdp."))
               :else (list 'if (list test-fn-sym (first c))
                           (second c)
                           (this (rrest c)))))]
    `(let [~test-fn-sym (clojure.contrib.template/template ~expr)]
       ~(f clauses))))
