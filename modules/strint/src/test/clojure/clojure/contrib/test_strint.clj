;   Copyright (c) Stuart Halloway, 2010-. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.contrib.test-strint
  (:use clojure.test)
  (:use [clojure.contrib strint with-ns]))

(def silent-read (with-ns 'clojure.contrib.strint silent-read))
(def interpolate (with-ns 'clojure.contrib.strint interpolate))
      
(deftest test-silent-read
  (testing "reading a valid form returns [read form, rest of string]"
    (is (= [[1] "[2]"] (silent-read "[1][2]"))))
  (testing "reading an invalid form returns nil"
    (is (= nil (silent-read "[")))))

(deftest test-interpolate
  (testing "a plain old string"
    (is (= ["a plain old string"] (interpolate "a plain old string"))))
  (testing "some value replacement forms"
    (is (= '["" foo " and " bar ""] (interpolate "~{foo} and ~{bar}"))))
  (testing "some fn-calling forms"
    (is (= '["" (+ 1 2) " and " (vector 3) ""] (interpolate "~(+ 1 2) and ~(vector 3)")))))

(deftest test-<<
  (testing "docstring examples"
    (let [v 30.5
          m {:a [1 2 3]}]
      (is (= "This trial required 30.5ml of solution."
             (<< "This trial required ~{v}ml of solution.")))
      (is (= "There are 30 days in November."
             (<< "There are ~(int v) days in November.")))
      (is (= "The total for your order is $6."
             (<< "The total for your order is $~(->> m :a (apply +))."))))))
