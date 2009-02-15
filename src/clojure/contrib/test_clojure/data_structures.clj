;;  Copyright (c) Frantisek Sodomka. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;

(ns clojure.contrib.test-clojure.data-structures
  (:use clojure.contrib.test-is))


(deftest test-list
  (are (list? _)
      ()
      '()
      (list)
      (list 1 2 3) )

  ; order is important
  (are (not (= _1 _2))
      (list 1 2) (list 2 1)
      (list 3 1 2) (list 1 2 3) )

  (are (= _1 _2)
      '() ()
      (list) '()
      (list 1) '(1)
      (list 1 2) '(1 2)

      ; nesting
      (list 1 (list 2 3) (list 3 (list 4 5 (list 6 (list 7)))))
        '(1 (2 3) (3 (4 5 (6 (7)))))

      ; different data structures
      (list true false nil)
        '(true false nil)
      (list 1 2.5 2/3 "ab" \x 'cd :kw)
        '(1 2.5 2/3 "ab" \x cd :kw)
      (list (list 1 2) [3 4] {:a 1 :b 2} #{:c :d})
        '((1 2) [3 4] {:a 1 :b 2} #{:c :d})

      ; evaluation
      (list (+ 1 2) [(+ 2 3) 'a] (list (* 2 3) 8))
        '(3 [5 a] (6 8))

      ; special cases
      (list nil) '(nil)
      (list 1 nil) '(1 nil)
      (list nil 2) '(nil 2)
      (list ()) '(())
      (list 1 ()) '(1 ())
      (list () 2) '(() 2) ))


(deftest test-hash-set
  (are (set? _)
      #{}
      #{1 2}
      (hash-set)
      (hash-set 1 2) )

  ; order isn't important
  (are (= _1 _2)
      #{1 2} #{2 1}
      #{3 1 2} #{1 2 3}
      (hash-set 1 2) (hash-set 2 1)
      (hash-set 3 1 2) (hash-set 1 2 3) )

  ; equal and unique
  (are (and (= (hash-set _)   #{_})
            (= (hash-set _ _) #{_}))
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

  (are (= _1 _2)
      ; equal classes
      (class #{}) (class (hash-set))
      (class #{1 2}) (class (hash-set 1 2))

      ; creating
      (hash-set) #{}
      (hash-set 1) #{1}
      (hash-set 1 2) #{1 2}

      ; nesting
      (hash-set 1 (hash-set 2 3) (hash-set 3 (hash-set 4 5 (hash-set 6 (hash-set 7)))))
        #{1 #{2 3} #{3 #{4 5 #{6 #{7}}}}}

      ; different data structures
      (hash-set true false nil)
        #{true false nil}
      (hash-set 1 2.5 2/3 "ab" \x 'cd :kw)
        #{1 2.5 2/3 "ab" \x 'cd :kw}
      (hash-set (list 1 2) [3 4] {:a 1 :b 2} #{:c :d})
        #{'(1 2) [3 4] {:a 1 :b 2} #{:c :d}}

      ; evaluation
      (hash-set (+ 1 2) [(+ 2 3) :a] (hash-set (* 2 3) 8))
        #{3 [5 :a] #{6 8}}

      ; special cases
      (hash-set nil) #{nil}
      (hash-set 1 nil) #{1 nil}
      (hash-set nil 2) #{nil 2}
      (hash-set #{}) #{#{}}
      (hash-set 1 #{}) #{1 #{}}
      (hash-set #{} 2) #{#{} 2} ))

