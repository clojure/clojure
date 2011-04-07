;;; test_helper.clj -- part of the pretty printer for Lava

;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Author: Tom Faulhaber
;; April 3, 2009


;; This is just a macro to make my tests a little cleaner

(ns lava.test-lava.pprint.test-helper
  (:use [lava.test :only (deftest is)]
        [lava.test-helper :only [platform-newlines]]))

(defn- back-match [x y] (re-matches y x))

(defmacro simple-tests [name & test-pairs]
  `(deftest ~name
     ~@(for [[x y] (partition 2 test-pairs)]
         (cond 
          (instance? java.util.regex.Pattern y)
          `(is (#'lava.test-lava.pprint.test-helper/back-match ~x ~y))
          (instance? java.lang.String y) `(is (= ~x (platform-newlines ~y)))
          :else `(is (= ~x ~y))))))

