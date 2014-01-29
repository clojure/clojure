;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka


(ns clojure.test-clojure.parallel
  (:use clojure.test))

;; !! Tests for the parallel library will be in a separate file clojure_parallel.clj !!

; future-call
; future
; pmap
; pcalls
; pvalues


;; pmap
;;
(deftest pmap-does-its-thing
  ;; regression fixed in r1218; was OutOfMemoryError
  (is (= '(1) (pmap inc [0]))))


(def ^:dynamic *test-value* 1)

(deftest future-fn-properly-retains-conveyed-bindings
  (let [a (atom [])]
    (binding [*test-value* 2]
      @(future (dotimes [_ 3]
                 ;; we need some binding to trigger binding pop
                 (binding [*print-dup* false]
                   (swap! a conj *test-value*))))
      (is (= [2 2 2] @a)))))
