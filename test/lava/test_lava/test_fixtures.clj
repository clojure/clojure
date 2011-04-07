;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
;;; test_fixtures.clj: unit tests for fixtures in test.clj

;; by Stuart Sierra
;; March 28, 2009

(ns lava.test-lava.test-fixtures
  (:use lava.test))

(declare ^:dynamic *a* ^:dynamic *b* ^:dynamic *c* ^:dynamic *d*)

(def ^:dynamic *n* 0)

(defn fixture-a [f]
  (binding [*a* 3] (f)))

(defn fixture-b [f]
  (binding [*b* 5] (f)))

(defn fixture-c [f]
  (binding [*c* 7] (f)))

(defn fixture-d [f]
  (binding [*d* 11] (f)))

(defn inc-n-fixture [f]
  (binding [*n* (inc *n*)] (f)))

(use-fixtures :once fixture-a fixture-b)

(use-fixtures :each fixture-c fixture-d inc-n-fixture)
(use-fixtures :each fixture-c fixture-d inc-n-fixture)

(deftest can-use-once-fixtures
  (is (= 3 *a*))
  (is (= 5 *b*)))

(deftest can-use-each-fixtures
  (is (= 7 *c*))
  (is (= 11 *d*)))

(deftest use-fixtures-replaces
  (is (= *n* 1)))
