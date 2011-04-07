;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Tests for error handling and messages

(ns lava.test-lava.errors
  (:use lava.test)
  (:import lava.lang.ArityException))

(defn f0 [] 0)

(defn f1 [a] a)

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
        (macroexpand `(m1 1 2)))))
