;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka


(ns clojure.test-clojure.other-functions
  (:use clojure.test))

; http://clojure.org/other_functions

; [= not= (tests in data_structures.clj and elsewhere)]


(deftest test-identity
  ; exactly 1 argument needed
  (is (thrown? IllegalArgumentException (identity)))
  (is (thrown? IllegalArgumentException (identity 1 2)))

  (are [x] (= (identity x) x)
      nil
      false true
      0 42
      0.0 3.14
      2/3
      0M 1M
      \c
      "" "abc"
      'sym
      :kw
      () '(1 2)
      [] [1 2]
      {} {:a 1 :b 2}
      #{} #{1 2} )

  ; evaluation
  (are [x y] (= (identity x) y)
      (+ 1 2) 3
      (> 5 0) true ))


(deftest test-name
  (are [x y] (= x (name y))
       "foo" :foo
       "bar" 'bar
       "quux" "quux"))

(deftest test-fnil
  (let [f1 (fnil vector :a)
        f2 (fnil vector :a :b)
        f3 (fnil vector :a :b :c)]
    (are [result input] (= result [(apply f1 input) (apply f2 input) (apply f3 input)])
         [[1 2 3 4] [1 2 3 4] [1 2 3 4]]  [1 2 3 4]
         [[:a 2 3 4] [:a 2 3 4] [:a 2 3 4]] [nil 2 3 4]
         [[:a nil 3 4] [:a :b 3 4] [:a :b 3 4]] [nil nil 3 4]
         [[:a nil nil 4] [:a :b nil 4] [:a :b :c 4]] [nil nil nil 4]
         [[:a nil nil nil] [:a :b nil nil] [:a :b :c nil]] [nil nil nil nil]))
  (are [x y] (= x y)
       ((fnil + 0) nil 42) 42
       ((fnil conj []) nil 42) [42]
       (reduce #(update-in %1 [%2] (fnil inc 0)) {} 
               ["fun" "counting" "words" "fun"])
       {"words" 1, "counting" 1, "fun" 2}
       (reduce #(update-in %1 [(first %2)] (fnil conj []) (second %2)) {} 
               [[:a 1] [:a 2] [:b 3]])
       {:b [3], :a [1 2]}))

; time assert comment doc

; partial
; comp
; complement
; constantly

; Printing
; pr prn print println newline
; pr-str prn-str print-str println-str [with-out-str (vars.clj)]

; Regex Support
; re-matcher re-find re-matches re-groups re-seq

