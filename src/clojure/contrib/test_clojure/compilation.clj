;;  Copyright (c) Frantisek Sodomka. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.

(ns clojure.contrib.test-clojure.compilation
  (:use clojure.contrib.test-is))

; http://clojure.org/compilation

; compile
; gen-class, gen-interface


(deftest test-compiler-metadata
  (let [m ^#'when]
    (are (= _1 _2)
        (list? (:arglists m)) true

        (string? (:doc m)) true
        (string? (:file m)) true

        (integer? (:line m)) true
        (> (:line m) 0) true

        (:macro m) true
        (:name m) 'when )))


