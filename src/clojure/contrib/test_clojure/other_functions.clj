;;  Copyright (c) Frantisek Sodomka. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.

(ns clojure.contrib.test-clojure.other-functions
  (:use clojure.contrib.test-is))

; http://clojure.org/other_functions

; [= not= (tests in data_structures.clj and elsewhere)]


(deftest test-identity
  ; exactly 1 argument needed
  (is (thrown? IllegalArgumentException (identity)))
  (is (thrown? IllegalArgumentException (identity 1 2)))

  (are (= (identity _) _)
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
  (are (= (identity _1) _2)
      (+ 1 2) 3
      (> 5 0) true ))


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

