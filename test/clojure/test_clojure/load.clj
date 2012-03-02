;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.load
  (:use clojure.test))

(deftest test-load
  (testing "Should ignore self-loads without comment"
    (is (nil? (require 'clojure.test-clojure.load.cyclic0))))
  (testing "Should reject cyclic dependencies"
    (testing "a->b->a"
      (is (thrown-with-msg? Exception #".*Cyclic load dependency.*"
            (require 'clojure.test-clojure.load.cyclic1))))
    (testing "a->b->c->d->b"
      (is (thrown-with-msg? Exception #".*Cyclic load dependency.*"
            (require 'clojure.test-clojure.load.cyclic3))))))

(deftest test-load-lib
  (testing "Shouldn't leak failed namespace"
    (try (require 'clojure.test-clojure.load.invalid)
         (catch Exception _))
    (is (nil? (find-ns 'clojure.test-clojure.load.invalid)))))

(deftest test-require-refer
  (try
    (binding [*ns* *ns*]
      (ns clojure.test-clojure.require-scratch
        (:require [clojure.set :refer [difference]]
                  [clojure.walk :refer :all]))
      (is (fn? (eval 'difference)))
      (is (every? fn? (map eval '[postwalk-replace prewalk-replace walk]))))
    (finally
     (remove-ns 'clojure.test-clojure.require-scratch))))