;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka

;;
;;  Created 1/29/2009

(ns clojure.test-clojure.logic
  (:use clojure.test
        [clojure.test-helper :only (exception)]))


;; *** Tests ***

(deftest test-if
  ; true/false/nil
  (are [x y] (= x y)
      (if true :t) :t
      (if true :t :f) :t
      (if true :t (exception)) :t

      (if false :t) nil
      (if false :t :f) :f
      (if false (exception) :f) :f

      (if nil :t) nil
      (if nil :t :f) :f
      (if nil (exception) :f) :f )

  ; zero/empty is true
  (are [x] (= (if x :t :f) :t)
      (byte 0)
      (short 0)
      (int 0)
      (long 0)
      (bigint 0)
      (float 0)
      (double 0)
      (bigdec 0)

      0/2
      ""
      #""
      (symbol "")

      ()
      []
      {}
      #{}
      (into-array []) )

  ; anything except nil/false is true
  (are [x]  (= (if x :t :f) :t)
      (byte 2)
      (short 2)
      (int 2)
      (long 2)
      (bigint 2)
      (float 2)
      (double 2)
      (bigdec 2)

      2/3
      \a
      "abc"
      #"a*b"
      'abc
      :kw

      '(1 2)
      [1 2]
      {:a 1 :b 2}
      #{1 2}
      (into-array [1 2])

      (new java.util.Date) ))


(deftest test-nil-punning
  (are [x y]  (= (if x :no :yes) y)
    (first []) :yes
    (next [1]) :yes
    (rest [1]) :no

    (butlast [1]) :yes

    (seq nil) :yes
    (seq []) :yes

    (sequence nil) :no
    (sequence []) :no

    (lazy-seq nil) :no
    (lazy-seq []) :no

    (filter #(> % 10) [1 2 3]) :no
    (map identity []) :no
    (apply concat []) :no

    (concat) :no
    (concat []) :no

    (reverse nil) :no
    (reverse []) :no

    (sort nil) :no
    (sort []) :no ))


(deftest test-and
  (are [x y] (= x y)
      (and) true
      (and true) true
      (and nil) nil
      (and false) false

      (and true nil) nil
      (and true false) false

      (and 1 true :kw 'abc "abc") "abc"

      (and 1 true :kw nil 'abc "abc") nil
      (and 1 true :kw nil (exception) 'abc "abc") nil

      (and 1 true :kw 'abc "abc" false) false
      (and 1 true :kw 'abc "abc" false (exception)) false ))


(deftest test-or
  (are [x y] (= x y)
      (or) nil
      (or true) true
      (or nil) nil
      (or false) false

      (or nil false true) true
      (or nil false 1 2) 1
      (or nil false "abc" :kw) "abc"

      (or false nil) nil
      (or nil false) false
      (or nil nil nil false) false

      (or nil true false) true
      (or nil true (exception) false) true
      (or nil false "abc" (exception)) "abc" ))


(deftest test-not
;  (is (thrown? IllegalArgumentException (not)))
  (are [x] (= (not x) true)
      nil
      false )
  (are [x]  (= (not x) false)
      true

      ; numbers
      0
      0.0
      42
      1.2
      0/2
      2/3

      ; characters
      \space
      \tab
      \a

      ; strings
      ""
      "abc"

      ; regexes
      #""
      #"a*b"

      ; symbols
      (symbol "")
      'abc

      ; keywords
      :kw

      ; collections/arrays
      ()
      '(1 2)
      []
      [1 2]
      {}
      {:a 1 :b 2}
      #{}
      #{1 2}
      (into-array [])
      (into-array [1 2])

      ; Java objects
      (new java.util.Date) ))

(deftest test-some?
  (are [expected x] (= expected (some? x))
       false nil
       true false
       true 0
       true "abc"
       true []))
