;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Author: Frantisek Sodomka


(ns clojure.test-clojure.clojure-set
  (:use clojure.test)
  (:require [clojure.set :as set]))

(deftest test-union
  (are [x y] (= x y)
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
  
  (are [x y] (= x y)
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

(deftest test-difference
  (are [x y] (= x y)
      ; identity
      (set/difference #{}) #{}
      (set/difference #{1}) #{1}
      (set/difference #{1 2 3}) #{1 2 3}

      ; 2 sets
      (set/difference #{1 2} #{1 2}) #{}
      (set/difference #{1 2} #{3 4}) #{1 2}
      (set/difference #{1 2} #{1}) #{2}
      (set/difference #{1 2} #{2}) #{1}
      (set/difference #{1 2 4} #{2 3 4 5}) #{1}

       ; 3 sets
      (set/difference #{1 2} #{2 3} #{5 2}) #{1}
      (set/difference #{1 2 3} #{1 3 4} #{1 3}) #{2}
      (set/difference #{1 2 3} #{3 4 5} #{8 2 3}) #{1} ))

(deftest test-select
  (are [x y] (= x y)
    (set/select integer? #{}) #{}
    (set/select integer? #{1 2}) #{1 2}
    (set/select integer? #{1 2 :a :b :c}) #{1 2}
    (set/select integer? #{:a :b :c}) #{}) )

(def compositions
  #{{:name "Art of the Fugue" :composer "J. S. Bach"}
    {:name "Musical Offering" :composer "J. S. Bach"}
    {:name "Requiem" :composer "Giuseppe Verdi"}
    {:name "Requiem" :composer "W. A. Mozart"}})

(deftest test-project
  (are [x y] (= x y)
    (set/project compositions [:name]) #{{:name "Art of the Fugue"}
                                         {:name "Requiem"}
                                         {:name "Musical Offering"}}
    (set/project compositions [:composer]) #{{:composer "W. A. Mozart"}
                                             {:composer "Giuseppe Verdi"}
                                             {:composer "J. S. Bach"}}
    (set/project compositions [:year]) #{{}}
    (set/project #{{}} [:name]) #{{}} ))

(deftest test-rename
  (are [x y] (= x y)
    (set/rename compositions {:name :title}) #{{:title "Art of the Fugue" :composer "J. S. Bach"}
                                               {:title "Musical Offering" :composer "J. S. Bach"}
                                               {:title "Requiem" :composer "Giuseppe Verdi"}
                                               {:title "Requiem" :composer "W. A. Mozart"}}
    (set/rename compositions {:year :decade}) #{{:name "Art of the Fugue" :composer "J. S. Bach"}
                                                {:name "Musical Offering" :composer "J. S. Bach"}
                                                {:name "Requiem" :composer "Giuseppe Verdi"}
                                                {:name "Requiem" :composer "W. A. Mozart"}}
    (set/rename #{{}} {:year :decade}) #{{}}))

(deftest test-rename-keys
  (are [x y] (= x y)
       (set/rename-keys {:a "one" :b "two"} {:a :z}) {:z "one" :b "two"}
       (set/rename-keys {:a "one" :b "two"} {:a :z :c :y}) {:z "one" :b "two"}
       (set/rename-keys {:a "one" :b "two" :c "three"} {:a :b :b :a}) {:a "two" :b "one" :c "three"}))

(deftest test-index
  (are [x y] (= x y)
    (set/index  #{{:c 2} {:b 1} {:a 1 :b 2}} [:b]) {{:b 2} #{{:a 1 :b 2}}, {:b 1} #{{:b 1}} {} #{{:c 2}}}
  ))

(deftest test-join
  (are [x y] (= x y)
    (set/join compositions compositions) compositions
    (set/join compositions #{{:name "Art of the Fugue" :genre "Classical"}})
                           #{{:name "Art of the Fugue" :composer "J. S. Bach" :genre "Classical"}}
    ))

(deftest test-map-invert
  (are [x y] (= x y)
       (set/map-invert {:a "one" :b "two"}) {"one" :a "two" :b}))

(deftest test-subset?
  (are [sub super] (set/subset? sub super)
       #{} #{}
       #{} #{1}
       #{1} #{1}
       #{1 2} #{1 2}
       #{1 2} #{1 2 42}
       #{false} #{false}
       #{nil}   #{nil}
       #{nil}   #{nil false}
       #{1 2 nil} #{1 2 nil 4})
  (are [notsub super] (not (set/subset? notsub super))
       #{1} #{}
       #{2} #{1}
       #{1 3} #{1}
       #{nil} #{false}
       #{false} #{nil}
       #{false nil} #{nil}
       #{1 2 nil}   #{1 2}))

(deftest test-superset?
  (are [super sub] (set/superset? super sub)
       #{} #{}
       #{1} #{}
       #{1} #{1}
       #{1 2} #{1 2}
       #{1 2 42} #{1 2}
       #{false}  #{false}
       #{nil}    #{nil}
       #{false nil} #{false}
       #{1 2 4 nil false} #{1 2 nil})
  (are [notsuper sub] (not (set/superset? notsuper sub))
       #{} #{1}
       #{2} #{1}
       #{1} #{1 3}
       #{nil} #{false}
       #{false} #{nil}
       #{nil}   #{false nil}
       #{nil 2 3} #{false nil 2 3}))

