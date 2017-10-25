;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;;Author: Alex Miller

(ns clojure.test-clojure.volatiles
  (:use clojure.test))

(deftest volatile-basics
  (let [vol (volatile! "abc")]
    (is (volatile? vol))
    (is (= "abc" @vol))
    (is (= "def" (vreset! vol "def")))
    (is (= "def" @vol))))

(deftest volatile-vswap!
  (let [vol (volatile! 10)]
    (is (= 11 (vswap! vol inc)))
    (is (= 11 @vol)))
  (let [vol (volatile! 10)]
    (is (= 20 (vswap! vol + 10)))
    (is (= 20 @vol)))
  (let [vol (volatile! 10)]
    (is (= 25 (vswap! vol + 10 5)))
    (is (= 25 @vol))))

