;;; fcase.clj -- simple variants of "case" for Clojure

;; by Stuart Sierra <mail@stuartsierra.com>
;; April 7, 2008

;; Copyright (c) 2008 Stuart Sierra. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; This file defines a generic "case" macro called "fcase" which takes
;; the equality-testing function as an argument.  It also defines a
;; traditional "case" macro that tests using "=" and variants that
;; test for regular expressions and class membership.


(clojure/in-ns 'fcase)
(clojure/refer 'clojure)


(defmacro fcase
  "Generic switch/case macro.  'fcase' is short for 'function case'.

  The 'compare-fn' is a fn of two arguments.

  The 'test-expr-clauses' are value-expression pairs without
  surrounding parentheses, like in Clojure's 'cond'.

  The 'case-value' is evaluated once and cached.  Then, 'compare-fn'
  is called once for each clause, with the clause's test value as its
  first argument and 'case-value' as its second argument.  If
  'compare-fn' returns logical true, the clause's expression is
  evaluated and returned.  If 'compare-fn' returns false/nil, we go to
  the next test value.

  If 'test-expr-clauses' contains an odd number of items, the last
  item is the default expression evaluated if no case-value matches.
  If there is no default expression and no case-value matches, fcase
  returns nil.

  See specific forms of this macro in 'case' and 're-case'.

  The test expressions in 'fcase' are always evaluated linearly, in
  order.  For a large number of case expressions it may be more
  efficient to use a hash lookup."
  [compare-fn case-value &
   test-expr-clauses]
  (let [test-val-sym (gensym "test_val")
	test-fn-sym (gensym "test_fn")
	cond-loop (fn this [clauses]
		      (cond
		       (>= (count clauses) 2)
		       (list 'if (list test-fn-sym (first clauses) test-val-sym)
			     (second clauses)
			     (this (rest (rest clauses))))
		       (= (count clauses) 1) (first clauses)))]
    (list 'let [test-val-sym case-value, test-fn-sym compare-fn]
	  (cond-loop test-expr-clauses))))

(defmacro case
  "Like cond, but test-value is compared against the value of each
  test expression with =.  If they are equal, executes the \"body\"
  expression.  Optional last expression is executed if none of the
  test expressions match."
  [test-value & clauses]
  `(fcase = ~test-value ~@clauses))

(defmacro re-case
  "Like case, but the test expressions are regular expressions, tested
  with re-find."
  [test-value & clauses]
  `(fcase re-find ~test-value ~@clauses))

(defmacro instance-case
  "Like case, but the test expressions are Java class names, tested with
  'instance?'."
  [test-value & clauses]
  `(fcase instance? ~test-value ~@clauses))

(defn- in-case-test [test-seq case-value]
  (some (fn [item] (= item case-value))
        test-seq))

(defmacro in-case
  "Like case, but test expressions are sequences.  The test expression
  is true if any item in the sequence is equal (tested with '=') to
  the test value."
  [test-value & clauses]
  `(fcase in-case-test ~test-value ~@clauses))
