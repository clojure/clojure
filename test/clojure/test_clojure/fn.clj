;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Ambrose Bonnaire-Sergeant

(ns clojure.test-clojure.fn
  (:use clojure.test))

(deftest fn-error-checking
  (testing "bad arglist"
    (is (fails-with-cause? java.lang.IllegalArgumentException 
          #"Parameter declaration a should be a vector"
          (eval '(fn "a" a)))))

  (testing "treat first param as args"
    (is (fails-with-cause? java.lang.IllegalArgumentException 
          #"Parameter declaration a should be a vector"
          (eval '(fn "a" [])))))

  (testing "looks like listy signature, but malformed declaration"
    (is (fails-with-cause? java.lang.IllegalArgumentException
          #"Parameter declaration 1 should be a vector"
          (eval '(fn (1))))))

  (testing "checks each signature"
    (is (fails-with-cause? java.lang.IllegalArgumentException
          #"Parameter declaration a should be a vector"
          (eval '(fn
                   ([a] 1)
                   ("a" 2))))))

  (testing "correct name but invalid args"
    (is (fails-with-cause? java.lang.IllegalArgumentException
          #"Parameter declaration a should be a vector"
          (eval '(fn a "a")))))

  (testing "first sig looks multiarity, rest of sigs should be lists"
    (is (fails-with-cause? java.lang.IllegalArgumentException 
          #"Invalid signature \[a b\] should be a list"
          (eval '(fn a
                   ([a] 1)
                   [a b])))))
  
  (testing "missing parameter declaration"
    (is (fails-with-cause? java.lang.IllegalArgumentException 
          #"Parameter declaration missing"
          (eval '(fn a))))
    (is (fails-with-cause? java.lang.IllegalArgumentException 
          #"Parameter declaration missing"
          (eval '(fn))))))
