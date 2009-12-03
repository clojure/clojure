;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka


(ns clojure.test-clojure.compilation
  (:use clojure.test))

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
 
