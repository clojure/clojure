;   Copyright (c) Laurent Petit, March 2009. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; test namespace for clojure.contrib.core

;; note to other contrib members: feel free to add to this lib

(ns clojure.contrib.core.tests
  (:use clojure.contrib.test-is)
  (:use clojure.contrib.core))

(deftest test-classic-versions
  (testing "Classic -> throws NPE if passed nil"
    (is (thrown? NullPointerException (-> nil .toString)))
    (is (thrown? NullPointerException (-> "foo" seq next next next .toString))))
  (testing "Classic .. throws NPE if one of the intermediate threaded values is nil"
    (is (thrown? NullPointerException (.. nil toString)))
    (is (thrown? NullPointerException (.. [nil] (get 0) toString)))))

(deftest test-new-versions
  (testing "Version -?> returns nil if passed nil"
    (is (nil? (-?> nil .toString)))
    (is (nil? (-?> "foo" seq next next next .toString))))
  (testing "Version -?> works well for some basic use cases"
    (is (= (list \O \O) (-?> "foo" .toUpperCase rest))))
  (testing "Version .?. returns nil if one of the intermediate threaded values is nil"
    (is (nil? (.?. nil toString)))
    (is (nil? (.?. [nil] (get 0) toString)))))
    