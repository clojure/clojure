;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Stuart Halloway


(ns clojure.test-clojure.main
  (:use clojure.test))

(deftest eval-opt
  (testing "evals and prints forms"
    (is (= "2\n4\n" (with-out-str (#'clojure.main/eval-opt "(+ 1 1) (+ 2 2)")))))

  (testing "skips printing nils"
    (is (= ":a\n:c\n" (with-out-str (#'clojure.main/eval-opt ":a nil :c")))))

  (testing "does not block access to *in* (#299)"
    (with-in-str "(+ 1 1)"
      (is (= "(+ 1 1)\n" (with-out-str (#'clojure.main/eval-opt "(read)")))))))
