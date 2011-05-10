;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka


(ns clojure.test-clojure.compilation
  (:use clojure.test)
  (:import compilation.TestDispatch))

; http://clojure.org/compilation

; compile
; gen-class, gen-interface


(deftest test-compiler-metadata
  (let [m (meta #'when)]
    (are [x y]  (= x y)
        (list? (:arglists m)) true
        (> (count (:arglists m)) 0) true

        (string? (:doc m)) true
        (> (.length (:doc m)) 0) true
        
        (string? (:file m)) true
        (> (.length (:file m)) 0) true

        (integer? (:line m)) true
        (> (:line m) 0) true

        (:macro m) true
        (:name m) 'when )))

(deftest test-embedded-constants
  (testing "Embedded constants"
    (is (eval `(= Boolean/TYPE ~Boolean/TYPE)))
    (is (eval `(= Byte/TYPE ~Byte/TYPE)))
    (is (eval `(= Character/TYPE ~Character/TYPE)))
    (is (eval `(= Double/TYPE ~Double/TYPE)))
    (is (eval `(= Float/TYPE ~Float/TYPE)))
    (is (eval `(= Integer/TYPE ~Integer/TYPE)))
    (is (eval `(= Long/TYPE ~Long/TYPE)))
    (is (eval `(= Short/TYPE ~Short/TYPE)))))
 
(deftest test-compiler-resolution
  (testing "resolve nonexistent class create should return nil (assembla #262)"
    (is (nil? (resolve 'NonExistentClass.)))))

(deftest test-no-recur-across-try
  (testing "don't recur to function from inside try"
    (is (thrown? Exception (eval '(fn [x] (try (recur 1)))))))
  (testing "don't recur to loop from inside try"
    (is (thrown? Exception (eval '(loop [x] (try (recur 1)))))))
  (testing "don't get confused about what the recur is targeting"
    (is (thrown? Exception (eval '(loop [x] (try (fn [x]) (recur 1)))))))
  (testing "don't allow recur accross binding"
    (is (thrown? Exception (eval '(fn [x] (binding [+ *] (recur 1)))))))
  (testing "allow loop/recur inside try"
    (is (try
          (eval '(try (loop [x 3] (if (zero? x) x (recur (dec x))))))
          (catch Exception _))))
  (testing "allow fn/recur inside try"
    (is (try
          (eval '(try
                   ((fn [x]
                      (if (zero? x)
                        x
                        (recur (dec x))))
                    3)))
          (catch Exception _)))))

(deftest test-numeric-dispatch
  (is (= "(int, int)" (TestDispatch/someMethod (int 1) (int 1))))
  (is (= "(int, long)" (TestDispatch/someMethod (int 1) (long 1))))
  (is (= "(long, long)" (TestDispatch/someMethod (long 1) (long 1)))))

