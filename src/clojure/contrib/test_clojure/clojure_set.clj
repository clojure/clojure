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

      ; 2 sets, at least one is empty
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

      ; 3 sets, some are empty
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


(deftest test-intersection
  ; at least one argument is needed
  (is (thrown? IllegalArgumentException (set/intersection)))
  
  (are (= _1 _2)
      ; identity
      (set/intersection #{}) #{}
      (set/intersection #{1}) #{1}
      (set/intersection #{1 2 3}) #{1 2 3}
      
      ; 2 sets, at least one is empty
      (set/intersection #{} #{}) #{}
      (set/intersection #{} #{1}) #{}
      (set/intersection #{} #{1 2 3}) #{}
      (set/intersection #{1} #{}) #{}
      (set/intersection #{1 2 3} #{}) #{}

      ; 2 sets
      (set/intersection #{1 2} #{1 2}) #{1 2}
      (set/intersection #{1 2} #{3 4}) #{}
      (set/intersection #{1 2} #{1}) #{1}
      (set/intersection #{1 2} #{2}) #{2}
      (set/intersection #{1 2 4} #{2 3 4 5}) #{2 4}

      ; 3 sets, some are empty
      (set/intersection #{} #{} #{}) #{}
      (set/intersection #{1} #{} #{}) #{}
      (set/intersection #{1} #{1} #{}) #{}
      (set/intersection #{1} #{} #{1}) #{}
      (set/intersection #{1 2} #{2 3} #{}) #{}

      ; 3 sets
      (set/intersection #{1 2} #{2 3} #{5 2}) #{2}
      (set/intersection #{1 2 3} #{1 3 4} #{1 3}) #{1 3}
      (set/intersection #{1 2 3} #{3 4 5} #{8 2 3}) #{3}

      ; different types of sets
      (set/intersection (hash-set 1 2) (hash-set 2 3)) #{2}
      (set/intersection (sorted-set 1 2) (sorted-set 2 3)) #{2}
      (set/intersection
        (hash-set 1 2) (hash-set 2 3)
        (sorted-set 1 2) (sorted-set 2 3)) #{2} ))


; difference
;
; select
; project
; rename-keys
; rename
; index
; map-invert
; join

