;;  Copyright (c) Frantisek Sodomka. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.

(ns clojure.contrib.test-clojure.clojure-set
  (:use clojure.contrib.test-is)
  (:require [clojure.set :as set]))


(deftest test-union
  (are (= _1 _2)
      (set/union) #{}

      ; identity
      (set/union #{}) #{}
      (set/union #{1}) #{1}
      (set/union #{1 2 3}) #{1 2 3}

      ; 2 sets, union with empty set
      (set/union #{} #{}) #{}
      (set/union #{} #{1}) #{1}
      (set/union #{} #{1 2 3}) #{1 2 3}
      (set/union #{1} #{}) #{1}
      (set/union #{1 2 3} #{}) #{1 2 3}

      ; 2 sets
      (set/union #{1} #{2}) #{1 2}
      (set/union #{1} #{1 2}) #{1 2}
      (set/union #{2} #{1 2}) #{1 2}
      (set/union #{1 2} #{3}) #{1 2 3}
      (set/union #{1 2} #{2 3}) #{1 2 3}

      ; 3 sets, union with empty set(s)
      (set/union #{} #{} #{}) #{}
      (set/union #{1} #{} #{}) #{1}
      (set/union #{} #{1} #{}) #{1}
      (set/union #{} #{} #{1}) #{1}
      (set/union #{1 2} #{2 3} #{}) #{1 2 3}

      ; 3 sets
      (set/union #{1 2} #{3 4} #{5 6}) #{1 2 3 4 5 6}
      (set/union #{1 2} #{2 3} #{1 3 4}) #{1 2 3 4}

      ; different data types
      (set/union #{1 2} #{:a :b} #{nil} #{false true} #{\c "abc"} #{[] [1 2]}
        #{{} {:a 1}} #{#{} #{1 2}})
          #{1 2 :a :b nil false true \c "abc" [] [1 2] {} {:a 1} #{} #{1 2}}

      ; different types of sets
      (set/union (hash-set) (hash-set 1 2) (hash-set 2 3))
          (hash-set 1 2 3)
      (set/union (sorted-set) (sorted-set 1 2) (sorted-set 2 3))
          (sorted-set 1 2 3)
      (set/union (hash-set) (hash-set 1 2) (hash-set 2 3)
        (sorted-set) (sorted-set 4 5) (sorted-set 5 6))
          (hash-set 1 2 3 4 5 6)  ; also equals (sorted-set 1 2 3 4 5 6)
))


; intersection
; difference
;
; select
; project
; rename-keys
; rename
; index
; map-invert
; join

