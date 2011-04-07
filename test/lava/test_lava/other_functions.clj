;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka


(ns lava.test-lava.other-functions
  (:use lava.test))

; http://lava.org/other_functions

; [= not= (tests in data_structures.clj and elsewhere)]


(deftest test-identity
  ; exactly 1 argument needed
;  (is (thrown? IllegalArgumentException (identity)))
;  (is (thrown? IllegalArgumentException (identity 1 2)))

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

(deftest test-comp
  (let [c0 (comp)]
    (are [x] (= (identity x) (c0 x))
         nil
         42
         [1 2 3]
         #{}
         :foo)
    (are [x y] (= (identity x) (c0 y))
         (+ 1 2 3) 6
         (keyword "foo") :foo)))

; complement
; constantly

; every-pred
(deftest test-every-pred
  (are [result expr] (= result expr)
   ;; 1 pred
   true     ((every-pred even?))
   true     ((every-pred even?) 2)
   true     ((every-pred even?) 2 4)
   true     ((every-pred even?) 2 4 6)
   true     ((every-pred even?) 2 4 6 8)
   true     ((every-pred even?) 2 4 6 8 10)
   false    ((every-pred odd?) 2)
   false    ((every-pred odd?) 2 4)
   false    ((every-pred odd?) 2 4 6)
   false    ((every-pred odd?) 2 4 6 8)
   false    ((every-pred odd?) 2 4 6 8 10)
   ;; 2 preds
   true     ((every-pred even? number?))
   true     ((every-pred even? number?) 2)
   true     ((every-pred even? number?) 2 4)
   true     ((every-pred even? number?) 2 4 6)
   true     ((every-pred even? number?) 2 4 6 8)
   true     ((every-pred even? number?) 2 4 6 8 10)
   false    ((every-pred number? odd?) 2)
   false    ((every-pred number? odd?) 2 4)
   false    ((every-pred number? odd?) 2 4 6)
   false    ((every-pred number? odd?) 2 4 6 8)
   false    ((every-pred number? odd?) 2 4 6 8 10)
   ;; 2 preds, short-circuiting
   false    ((every-pred number? odd?) 1 :a)
   false    ((every-pred number? odd?) 1 3 :a)
   false    ((every-pred number? odd?) 1 3 5 :a)
   false    ((every-pred number? odd?) 1 3 5 7 :a)
   false    ((every-pred number? odd?) 1 :a 3 5 7)
   ;; 3 preds
   true     ((every-pred even? number? #(> % 0)))
   true     ((every-pred even? number? #(> % 0)) 2)
   true     ((every-pred even? number? #(> % 0)) 2 4)
   true     ((every-pred even? number? #(> % 0)) 2 4 6)
   true     ((every-pred even? number? #(> % 0)) 2 4 6 8)
   true     ((every-pred even? number? #(> % 0)) 2 4 6 8 10)
   true     ((every-pred number? even? #(> % 0)) 2 4 6 8 10 12)
   false    ((every-pred number? odd? #(> % 0)) 2)
   false    ((every-pred number? odd? #(> % 0)) 2 4)
   false    ((every-pred number? odd? #(> % 0)) 2 4 6)
   false    ((every-pred number? odd? #(> % 0)) 2 4 6 8)
   false    ((every-pred number? odd? #(> % 0)) 2 4 6 8 10)
   false    ((every-pred number? odd? #(> % 0)) 2 4 6 8 -10)
   ;; 3 preds, short-circuiting
   false    ((every-pred number? odd? #(> % 0)) 1 :a)
   false    ((every-pred number? odd? #(> % 0)) 1 3 :a)
   false    ((every-pred number? odd? #(> % 0)) 1 3 5 :a)
   false    ((every-pred number? odd? #(> % 0)) 1 3 5 7 :a)
   false    ((every-pred number? odd? #(> % 0)) 1 :a 3 5 7)
   ;; 4 preds
   true     ((every-pred even? number? #(> % 0) #(<= % 12)))
   true     ((every-pred even? number? #(> % 0) #(<= % 12)) 2)
   true     ((every-pred even? number? #(> % 0) #(<= % 12)) 2 4)
   true     ((every-pred even? number? #(> % 0) #(<= % 12)) 2 4 6)
   true     ((every-pred even? number? #(> % 0) #(<= % 12)) 2 4 6 8)
   true     ((every-pred even? number? #(> % 0) #(<= % 12)) 2 4 6 8 10)
   true     ((every-pred number? even? #(> % 0) #(<= % 12)) 2 4 6 8 10 12)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12)) 2)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12)) 2 4)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12)) 2 4 6)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12)) 2 4 6 8)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12)) 2 4 6 8 10)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12)) 2 4 6 8 14)
   ;; 4 preds, short-circuiting
   false    ((every-pred number? odd? #(> % 0) #(<= % 12)) 1 :a)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12)) 1 3 :a)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12)) 1 3 5 :a)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12)) 1 3 5 7 :a)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12)) 1 :a 3 5 7)
   ;; 5 preds
   true     ((every-pred even? number? #(> % 0) #(<= % 12) #(zero? (rem % 2))))
   true     ((every-pred even? number? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2)
   true     ((every-pred even? number? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4)
   true     ((every-pred even? number? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6)
   true     ((every-pred even? number? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6 8)
   true     ((every-pred even? number? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6 8 10)
   true     ((every-pred number? even? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6 8 10 12)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6 8)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6 8 10)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6 8 13)
   ;; 5 preds, short-circuiting
   false    ((every-pred number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 1 :a)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 1 3 :a)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 1 3 5 :a)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 1 3 5 7 :a)
   false    ((every-pred number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 1 :a 3 5 7)
   ;; truthiness
   true     (reduce #(and % %2)
                    (for [i (range 1 25)]
                      (apply (apply every-pred (repeat i identity))
                             (range i))))))

; some-fn

(deftest test-some-fn
  (are [result] (identity result)
   ;; 1 pred
   (not ((some-fn even?)))
   ((some-fn even?) 2)
   ((some-fn even?) 2 4)
   ((some-fn even?) 2 4 6)
   ((some-fn even?) 2 4 6 8)
   ((some-fn even?) 2 4 6 8 10)
   (not ((some-fn odd?) 2))
   (not ((some-fn odd?) 2 4))
   (not ((some-fn odd?) 2 4 6))
   (not ((some-fn odd?) 2 4 6 8))
   (not ((some-fn odd?) 2 4 6 8 10))
   ;; 2 preds
   (not ((some-fn even? number?)))
   ((some-fn even? number?) 2)
   ((some-fn even? number?) 2 4)
   ((some-fn even? number?) 2 4 6)
   ((some-fn even? number?) 2 4 6 8)
   ((some-fn even? number?) 2 4 6 8 10)
   ((some-fn number? odd?) 2)
   ((some-fn number? odd?) 2 4)
   ((some-fn number? odd?) 2 4 6)
   ((some-fn number? odd?) 2 4 6 8)
   ((some-fn number? odd?) 2 4 6 8 10)
   ;; 2 preds, short-circuiting
   ((some-fn number? odd?) 1 :a)
   ((some-fn number? odd?) 1 3 :a)
   ((some-fn number? odd?) 1 3 5 :a)
   ((some-fn number? odd?) 1 3 5 7 :a)
   ((some-fn number? odd?) 1 :a 3 5 7)
   ;; 3 preds
   (not ((some-fn even? number? #(> % 0))))
   ((some-fn even? number? #(> % 0)) 2)
   ((some-fn even? number? #(> % 0)) 2 4)
   ((some-fn even? number? #(> % 0)) 2 4 6)
   ((some-fn even? number? #(> % 0)) 2 4 6 8)
   ((some-fn even? number? #(> % 0)) 2 4 6 8 10)
   ((some-fn number? even? #(> % 0)) 2 4 6 8 10 12)
   ((some-fn number? odd? #(> % 0)) 2)
   ((some-fn number? odd? #(> % 0)) 2 4)
   ((some-fn number? odd? #(> % 0)) 2 4 6)
   ((some-fn number? odd? #(> % 0)) 2 4 6 8)
   ((some-fn number? odd? #(> % 0)) 2 4 6 8 10)
   ((some-fn number? odd? #(> % 0)) 2 4 6 8 -10)
   ;; 3 preds, short-circuiting
   ((some-fn number? odd? #(> % 0)) 1 :a)
   ((some-fn number? odd? #(> % 0)) 1 3 :a)
   ((some-fn number? odd? #(> % 0)) 1 3 5 :a)
   ((some-fn number? odd? #(> % 0)) 1 :a 3 5 7)
   ;; 4 preds
   (not ((some-fn even? number? #(> % 0) #(<= % 12))))
   ((some-fn even? number? #(> % 0) #(<= % 12)) 2)
   ((some-fn even? number? #(> % 0) #(<= % 12)) 2 4)
   ((some-fn even? number? #(> % 0) #(<= % 12)) 2 4 6)
   ((some-fn even? number? #(> % 0) #(<= % 12)) 2 4 6 8)
   ((some-fn even? number? #(> % 0) #(<= % 12)) 2 4 6 8 10)
   ((some-fn number? even? #(> % 0) #(<= % 12)) 2 4 6 8 10 12)
   ((some-fn number? odd? #(> % 0) #(<= % 12)) 2)
   ((some-fn number? odd? #(> % 0) #(<= % 12)) 2 4)
   ((some-fn number? odd? #(> % 0) #(<= % 12)) 2 4 6)
   ((some-fn number? odd? #(> % 0) #(<= % 12)) 2 4 6 8)
   ((some-fn number? odd? #(> % 0) #(<= % 12)) 2 4 6 8 10)
   ((some-fn number? odd? #(> % 0) #(<= % 12)) 2 4 6 8 14)
   ;; 4 preds, short-circuiting
   ((some-fn number? odd? #(> % 0) #(<= % 12)) 1 :a)
   ((some-fn number? odd? #(> % 0) #(<= % 12)) 1 3 :a)
   ((some-fn number? odd? #(> % 0) #(<= % 12)) 1 3 5 :a)
   ((some-fn number? odd? #(> % 0) #(<= % 12)) 1 3 5 7 :a)
   ((some-fn number? odd? #(> % 0) #(<= % 12)) 1 :a 3 5 7)
   ;; 5 preds
   (not ((some-fn even? number? #(> % 0) #(<= % 12) #(zero? (rem % 2)))))
   ((some-fn even? number? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2)
   ((some-fn even? number? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4)
   ((some-fn even? number? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6)
   ((some-fn even? number? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6 8)
   ((some-fn even? number? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6 8 10)
   ((some-fn number? even? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6 8 10 12)
   ((some-fn number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2)
   ((some-fn number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4)
   ((some-fn number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6)
   ((some-fn number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6 8)
   ((some-fn number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6 8 10)
   ((some-fn number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 2 4 6 8 13)
   ;; 5 preds, short-circuiting
   ((some-fn number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 1 :a)
   ((some-fn number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 1 3 :a)
   ((some-fn number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 1 3 5 :a)
   ((some-fn number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 1 3 5 7 :a)
   ((some-fn number? odd? #(> % 0) #(<= % 12) #(zero? (rem % 2))) 1 :a 3 5 7)
   ;; truthiness
   (reduce #(or % %2)
           (conj
             (vec
               (for [i (range 1 25)]
                 (apply (apply some-fn (repeat i (comp not boolean))) (range i))))
                 true))))

; Printing
; pr prn print println newline
; pr-str prn-str print-str println-str [with-out-str (vars.clj)]

; Regex Support
; re-matcher re-find re-matches re-groups re-seq

