;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka, Mike Hinchey, Stuart Halloway

;;
;;  Test "flow control" constructs.
;;

(ns clojure.test-clojure.control
  (:use clojure.test
        [clojure.test-helper :only (exception)]))

;; *** Helper functions ***

(defn maintains-identity [f]
  (are [x] (= (f x) x)
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
      #{} #{1 2} ))


; http://clojure.org/special_forms
; http://clojure.org/macros

(deftest test-do
  (are [x y] (= x y)
      ; no params => nil
      (do) nil
      
      ; return last
      (do 1) 1
      (do 1 2) 2
      (do 1 2 3 4 5) 5
      
      ; evaluate and return last
      (let [a (atom 0)]
        (do (reset! a (+ @a 1))   ; 1
            (reset! a (+ @a 1))   ; 2
            (reset! a (+ @a 1))   ; 3
            @a))  3 )

  ; identity (= (do x) x)
  (maintains-identity (fn [_] (do _))) )


;; loop/recur
(deftest test-loop
  (are [x y] (= x y)
       1 (loop []
           1)
       3 (loop [a 1]
           (if (< a 3)
             (recur (inc a))
             a))
       [2 4 6] (loop [a []
                      b [1 2 3]]
                 (if (seq b)
                   (recur (conj a (* 2 (first b)))
                          (next b))
                   a))
       [6 4 2] (loop [a ()
                      b [1 2 3]]
                 (if (seq b)
                   (recur (conj a (* 2 (first b)))
                          (next b))
                   a))
       )
  )


;; throw, try

; if: see logic.clj

(deftest test-when
  (are [x y] (= x y)
       1 (when true 1)
       nil (when true)
       nil (when false)
       nil (when false (exception))
       ))

(deftest test-when-not
  (are [x y] (= x y)
       1 (when-not false 1)
       nil (when-not true)
       nil (when-not false)
       nil (when-not true (exception))
       ))

(deftest test-if-not
  (are [x y] (= x y)
       1 (if-not false 1)
       1 (if-not false 1 (exception))
       nil (if-not true 1)
       2 (if-not true 1 2)
       nil (if-not true (exception))
       1 (if-not true (exception) 1)
       ))

(deftest test-when-let
  (are [x y] (= x y)
       1 (when-let [a 1]
           a)
       2 (when-let [[a b] '(1 2)]
           b)
       nil (when-let [a false]
             (exception))
       ))

(deftest test-if-let
  (are [x y] (= x y)
       1 (if-let [a 1]
           a)
       2 (if-let [[a b] '(1 2)]
           b)
       nil (if-let [a false]
             (exception))
       1 (if-let [a false]
           a 1)
       1 (if-let [[a b] nil]
             b 1)
       1 (if-let [a false]
           (exception)
           1)
       ))

(deftest test-when-first
  (are [x y] (= x y)
       1 (when-first [a [1 2]]
           a)
       2 (when-first [[a b] '((1 2) 3)]
           b)
       nil (when-first [a nil]
             (exception))
       ))


(deftest test-cond
  (are [x y] (= x y)
      (cond) nil

      (cond nil true) nil
      (cond false true) nil
      
      (cond true 1 true (exception)) 1
      (cond nil 1 false 2 true 3 true 4) 3
      (cond nil 1 false 2 true 3 true (exception)) 3 )

  ; false
  (are [x]  (= (cond x :a true :b) :b)
      nil false )

  ; true
  (are [x]  (= (cond x :a true :b) :a)
      true
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
  (are [x y] (= x y)
      (cond (> 3 2) (+ 1 2) true :result true (exception)) 3
      (cond (< 3 2) (+ 1 2) true :result true (exception)) :result )

  ; identity (= (cond true x) x)
  (maintains-identity (fn [_] (cond true _))) )


(deftest test-condp
  (are [x] (= :pass x)
       (condp = 1
         1 :pass
         2 :fail)
       (condp = 1
         2 :fail
         1 :pass)
       (condp = 1
         2 :fail
         :pass)
       (condp = 1
         :pass)
       (condp = 1
         2 :fail
         ;; doc of condp says result-expr is returned
         ;; shouldn't it say similar to cond: "evaluates and returns
         ;; the value of the corresponding expr and doesn't evaluate any of the
         ;; other tests or exprs."
         (identity :pass))
       (condp + 1
         1 :>> #(if (= % 2) :pass :fail))
       (condp + 1
         1 :>> #(if (= % 3) :fail :pass))
       )
  (is (thrown? IllegalArgumentException
               (condp = 1)
               ))
  (is (thrown? IllegalArgumentException
               (condp = 1
                 2 :fail)
               ))
  )


; [for, doseq (for.clj)]

(deftest test-dotimes
  ;; dotimes always returns nil
  (is (= nil (dotimes [n 1] n)))
  ;; test using an atom since dotimes is for modifying
  ;; test executes n times
  (is (= 3
         (let [a (atom 0)]
           (dotimes [n 3]
             (swap! a inc))
           @a)
         ))
  ;; test all values of n
  (is (= [0 1 2]
         (let [a (atom [])]
           (dotimes [n 3]
             (swap! a conj n))
           @a)))
  (is (= []
         (let [a (atom [])]
           (dotimes [n 0]
             (swap! a conj n))
           @a)))
  )

(deftest test-while
  (is (= nil (while nil (throw (Exception. "never")))))
  (is (= [0 nil]
         ;; a will dec to 0
         ;; while always returns nil
         (let [a (atom 3)
               w (while (pos? @a)
                   (swap! a dec))]
           [@a w])))
  (is (thrown? Exception (while true (throw (Exception. "expected to throw")))))
  )

; locking, monitor-enter, monitor-exit

; case 
(deftest test-case
  (testing "can match many kinds of things"
    (let [two 2
          test-fn
          #(case %
                 1 :number
                 "foo" :string
                 \a :char
                 pow :symbol
                 :zap :keyword
                 (2 \b "bar") :one-of-many
                 [1 2] :sequential-thing
                 {:a 2} :map
                 {:r 2 :d 2} :droid
                 #{2 3 4 5} :set
                 [1 [[[2]]]] :deeply-nested
                 nil :nil
                 :default)]
      (are [result input] (= result (test-fn input))
           :number 1
           :string "foo"
           :char \a
           :keyword :zap
           :symbol 'pow
           :one-of-many 2
           :one-of-many \b
           :one-of-many "bar"
           :sequential-thing [1 2]
           :sequential-thing (list 1 2)
           :sequential-thing [1 two]
           :map {:a 2}
           :map {:a two}
           :set #{2 3 4 5}
           :set #{two 3 4 5}
           :default #{2 3 4 5 6}
           :droid {:r 2 :d 2}
           :deeply-nested [1 [[[two]]]]
           :nil nil
           :default :anything-not-appearing-above)))
  (testing "throws IllegalArgumentException if no match"
    (is (thrown-with-msg?
          IllegalArgumentException #"No matching clause: 2"
          (case 2 1 :ok))))
  (testing "sorting doesn't matter"
    (let [test-fn
          #(case %
                {:b 2 :a 1} :map
                #{3 2 1} :set
                :default)]
      (are [result input] (= result (test-fn input))
           :map {:a 1 :b 2}
           :map (sorted-map :a 1 :b 2)
           :set #{3 2 1}
           :set (sorted-set 2 1 3))))
  (testing "test constants are *not* evaluated"
    (let [test-fn
          ;; never write code like this...
          #(case %
                 (throw (RuntimeException. "boom")) :piece-of-throw-expr
                 :no-match)]
      (are [result input] (= result (test-fn input))
           :piece-of-throw-expr 'throw
           :piece-of-throw-expr '[RuntimeException. "boom"]
           :no-match nil))))
