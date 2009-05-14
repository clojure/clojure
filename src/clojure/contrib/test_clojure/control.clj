;;  Copyright (c) Frantisek Sodomka. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  Test "flow control" constructs.
;;

(ns clojure.contrib.test-clojure.control
  (:use clojure.contrib.test-is
        [clojure.contrib.test-clojure.test-utils :only (exception)]))

;; *** Helper functions ***

(defn maintains-identity [f]
  (are (= (f _) _)
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
  (are (= _1 _2)
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


; loop/recur
; throw, try

; [if (logic.clj)], if-not, if-let
; when, when-not, when-let, when-first


(deftest test-cond
  (are (= _1 _2)
      (cond) nil

      (cond nil true) nil
      (cond false true) nil
      
      (cond true 1 true (exception)) 1
      (cond nil 1 false 2 true 3 true 4) 3
      (cond nil 1 false 2 true 3 true (exception)) 3 )

  ; false
  (are (= (cond _ :a true :b) :b)
      nil false )

  ; true
  (are (= (cond _ :a true :b) :a)
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
  (are (= _1 _2)
      (cond (> 3 2) (+ 1 2) true :result true (exception)) 3
      (cond (< 3 2) (+ 1 2) true :result true (exception)) :result )

  ; identity (= (cond true x) x)
  (maintains-identity (fn [_] (cond true _))) )


; condp

; [for, doseq (for.clj)]

; dotimes, while

; locking, monitor-enter, monitor-exit

