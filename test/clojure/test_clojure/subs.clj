;   Copyright (c) Ian Eure. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.subs
  (:use clojure.test))

(deftest test-subs
  (let [s "One morning, as Gregor Samsa was waking up from anxious dreams"]
    (testing "Returns substring relative to the start with positive args"
      (testing "With start"
        (is (= "waking up from anxious dreams" (subs s 33))))
      (testing "With start and end"
        (is (= "Gregor Samsa" (subs s 16 28)))))

    (testing "Returns substring relative to the end with negative args"
      (testing "With start"
        (is (= "anxious dreams" (subs s -14))))
      (testing "With start and end"
        (is (= "from anxious" (subs s 43 -7)))))))
