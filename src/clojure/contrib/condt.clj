;;; condt.clj - generic case-like macro using template expressions

;; By Stuart Sierra, http://stuartsierra.com/
;; February 21, 2009

;; Copyright (c) Stuart Sierra, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; CHANGE LOG
;;
;; February 21, 2009: fixed to work with new lazy Clojure
;;
;; December 23, 2008: renamed to condt, since clojure.core now
;; contains a (different) condp as of Clojure SVN rev. 1180
;;
;; December 15, 2008: original version, named "condp"


(ns clojure.contrib.condt
    (:require clojure.contrib.template))

(defmacro condt
  "expr is a template expression (see template), clauses are test/expr
  pairs like cond.  Evalautes the template on each test value, one at
  a time.  If a test returns logical true, condt evaluates the
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
                         (this (nthnext c 2)))))]
    `(let [~test-fn-sym (clojure.contrib.template/template ~expr)]
       ~(f clauses))))

(defmacro econdt
  "Like condt but throws Exception if no tests match."
  [expr & clauses]
  (let [test-fn-sym (gensym "test_")
        f (fn this [c]
              (cond
               (empty? c) '(throw (Exception. "Nothing matched in econdt."))
               (= 1 (count c)) (throw (IllegalStateException. "Odd number of clauses in econdt."))
               :else (list 'if (list test-fn-sym (first c))
                           (second c)
                           (this (nthnext c 2)))))]
    `(let [~test-fn-sym (clojure.contrib.template/template ~expr)]
       ~(f clauses))))
