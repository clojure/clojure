;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution.  By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license.  You must not remove this notice, or any other, from this
;;  software.
;;
;;  File: cond.clj
;;
;;  scgilardi (gmail)
;;  2 October 2008

(ns clojure.contrib.cond)

(defmacro cond-let
  "Takes a binding-form and a set of test/expr pairs. Evaluates each test
  one at a time. If a test returns logical true, cond-let evaluates and
  returns expr with binding-form bound to the value of test and doesn't
  evaluate any of the other tests or exprs. To provide a default value
  either provide a literal that evaluates to logical true and is
  binding-compatible with binding-form, or use :else as the test and don't
  refer to any parts of binding-form in the expr. (cond-let binding-form)
  returns nil."
  [binding-form & clauses]
  (when-let [test expr & more] clauses
    (if (= test :else)
      expr
      `(if ~test
         (let [~binding-form ~test] ~expr)
         (cond-let ~binding-form ~@more)))))
