;;  Copyright (c) Frantisek Sodomka. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;

(ns clojure.contrib.test-clojure.sequences
  (:use clojure.contrib.test-is))

;; *** Helper functions ***

(defn exception []
  (throw (new Exception "Exception which should never occur")))

(defn diff [s1 s2]
  (seq (reduce disj (set s1) (set s2))))


;; *** Tests ***

(deftest test-seq
  (are (= _1 _2)
    (seq nil) nil
    (seq [nil]) '(nil)

    (seq ()) nil
    (seq []) nil
    (seq #{}) nil
    (seq {}) nil
    (seq "") nil
    (seq (into-array [])) nil

    (seq (list 1 2)) '(1 2)
    (seq [1 2]) '(1 2)
    (seq (sorted-set 1 2)) '(1 2)
    (seq (sorted-map :a 1 :b 2)) '([:a 1] [:b 2])
    (seq "abc") '(\a \b \c)
    (seq (into-array [1 2])) '(1 2) ))


(deftest test-cons
  (is (thrown? IllegalArgumentException (cons 1 2)))
  (are (= _1 _2)
    (cons 1 nil) '(1)
    (cons nil nil) '(nil)

    (cons \a nil) '(\a)
    (cons \a "") '(\a)
    (cons \a "bc") '(\a \b \c)

    (cons 1 ()) '(1)
    (cons 1 '(2 3)) '(1 2 3)

    (cons 1 []) [1]
    (cons 1 [2 3]) [1 2 3]

    (cons 1 #{}) '(1)
    (cons 1 (sorted-set 2 3)) '(1 2 3)

    (cons 1 (into-array [])) '(1)
    (cons 1 (into-array [2 3])) '(1 2 3) ))


(deftest test-first
  (is (thrown? IllegalArgumentException (first)))
  (is (thrown? IllegalArgumentException (first true)))
  (is (thrown? IllegalArgumentException (first false)))
  (is (thrown? IllegalArgumentException (first 1)))
  (is (thrown? IllegalArgumentException (first 1 2)))
  (is (thrown? IllegalArgumentException (first \a)))
  (is (thrown? IllegalArgumentException (first 's)))
  (is (thrown? IllegalArgumentException (first :k)))
  (are (= _1 _2)
    (first nil) nil

    ; string
    (first "") nil
    (first "a") \a
    (first "abc") \a

    ; list
    (first ()) nil
    (first '(1)) 1
    (first '(1 2 3)) 1

    (first '(nil)) nil
    (first '(1 nil)) 1
    (first '(nil 2)) nil
    (first '(())) ()
    (first '(() nil)) ()
    (first '(() 2 nil)) ()

    ; vector
    (first []) nil
    (first [1]) 1
    (first [1 2 3]) 1

    (first [nil]) nil
    (first [1 nil]) 1
    (first [nil 2]) nil
    (first [[]]) []
    (first [[] nil]) []
    (first [[] 2 nil]) []

    ; set
    (first #{}) nil
    (first #{1}) 1
    ;(first #{1 2 3}) 1

    (first #{nil}) nil
    ;(first #{1 nil}) 1
    ;(first #{nil 2}) nil
    (first #{#{}}) #{}
    ;(first #{#{} nil}) #{}
    ;(first #{#{} 2 nil}) #{}

    ; map
    (first {}) nil
    (first (sorted-map :a 1)) '(:a 1)
    (first (sorted-map :a 1 :b 2 :c 3)) '(:a 1)

    ; array
    (first (into-array [])) nil
    (first (into-array [1])) 1
    (first (into-array [1 2 3])) 1
    (first (to-array [nil])) nil
    (first (to-array [1 nil])) 1
    (first (to-array [nil 2])) nil ))


(deftest test-next
  (is (thrown? IllegalArgumentException (next)))
  (is (thrown? IllegalArgumentException (next true)))
  (is (thrown? IllegalArgumentException (next false)))
  (is (thrown? IllegalArgumentException (next 1)))
  (is (thrown? IllegalArgumentException (next 1 2)))
  (is (thrown? IllegalArgumentException (next \a)))
  (is (thrown? IllegalArgumentException (next 's)))
  (is (thrown? IllegalArgumentException (next :k)))
  (are (= _1 _2)
    (next nil) nil

    ; string
    (next "") nil
    (next "a") nil
    (next "abc") '(\b \c)

    ; list
    (next ()) nil
    (next '(1)) nil
    (next '(1 2 3)) '(2 3)

    (next '(nil)) nil
    (next '(1 nil)) '(nil)
    (next '(1 ())) '(())
    (next '(nil 2)) '(2)
    (next '(())) nil
    (next '(() nil)) '(nil)
    (next '(() 2 nil)) '(2 nil)

    ; vector
    (next []) nil
    (next [1]) nil
    (next [1 2 3]) [2 3]

    (next [nil]) nil
    (next [1 nil]) [nil]
    (next [1 []]) [[]]
    (next [nil 2]) [2]
    (next [[]]) nil
    (next [[] nil]) [nil]
    (next [[] 2 nil]) [2 nil]

    ; set
    (next #{}) nil
    (next #{1}) nil
    ;(next #{1 2 3}) 1

    (next #{nil}) nil
    ;(next #{1 nil}) 1
    ;(next #{nil 2}) nil
    (next #{#{}}) nil
    ;(next #{#{} nil}) #{}
    ;(next #{#{} 2 nil}) #{}

    ; map
    (next {}) nil
    (next (sorted-map :a 1)) nil
    (next (sorted-map :a 1 :b 2 :c 3)) '((:b 2) (:c 3))

    ; array
    (next (into-array [])) nil
    (next (into-array [1])) nil
    (next (into-array [1 2 3])) '(2 3)

    (next (to-array [nil])) nil
    (next (to-array [1 nil])) '(nil)
    ;(next (to-array [1 (into-array [])])) (list (into-array []))
    (next (to-array [nil 2])) '(2)
    (next (to-array [(into-array [])])) nil
    (next (to-array [(into-array []) nil])) '(nil)
    (next (to-array [(into-array []) 2 nil])) '(2 nil) ))


;; (ffirst coll) = (first (first coll))
;;
(deftest test-ffirst
  (is (thrown? IllegalArgumentException (ffirst)))
  (are (= _1 _2)
    (ffirst nil) nil

    (ffirst ()) nil
    (ffirst '((1 2) (3 4))) 1

    (ffirst []) nil
    (ffirst [[1 2] [3 4]]) 1

    (ffirst {}) nil
    (ffirst {:a 1}) :a

    (ffirst #{}) nil
    (ffirst #{[1 2]}) 1 ))


;; (fnext coll) = (first (next coll)) = (second coll)
;;
(deftest test-fnext
  (is (thrown? IllegalArgumentException (fnext)))
  (are (= _1 _2)
    (fnext nil) nil

    (fnext ()) nil
    (fnext '(1)) nil
    (fnext '(1 2 3 4)) 2

    (fnext []) nil
    (fnext [1]) nil
    (fnext [1 2 3 4]) 2

    (fnext {}) nil
    (fnext (sorted-map :a 1)) nil
    (fnext (sorted-map :a 1 :b 2)) [:b 2]

    (fnext #{}) nil
    (fnext #{1}) nil
    (fnext (sorted-set 1 2 3 4)) 2 ))


;; (nfirst coll) = (next (first coll))
;;
(deftest test-nfirst
  (is (thrown? IllegalArgumentException (nfirst)))
  (are (= _1 _2)
    (nfirst nil) nil

    (nfirst ()) nil
    (nfirst '((1 2 3) (4 5 6))) '(2 3)

    (nfirst []) nil
    (nfirst [[1 2 3] [4 5 6]]) '(2 3)

    (nfirst {}) nil
    (nfirst {:a 1}) '(1)

    (nfirst #{}) nil
    (nfirst #{[1 2]}) '(2) ))


;; (nnext coll) = (next (next coll))
;;
(deftest test-nnext
  (is (thrown? IllegalArgumentException (nnext)))
  (are (= _1 _2)
    (nnext nil) nil

    (nnext ()) nil
    (nnext '(1)) nil
    (nnext '(1 2)) nil
    (nnext '(1 2 3 4)) '(3 4)

    (nnext []) nil
    (nnext [1]) nil
    (nnext [1 2]) nil
    (nnext [1 2 3 4]) '(3 4)

    (nnext {}) nil
    (nnext (sorted-map :a 1)) nil
    (nnext (sorted-map :a 1 :b 2)) nil
    (nnext (sorted-map :a 1 :b 2 :c 3 :d 4)) '([:c 3] [:d 4])

    (nnext #{}) nil
    (nnext #{1}) nil
    (nnext (sorted-set 1 2)) nil
    (nnext (sorted-set 1 2 3 4)) '(3 4) ))


; distinct was broken for nil & false:
;   fixed in rev 1278:
;   http://code.google.com/p/clojure/source/detail?r=1278
;
(deftest test-distinct
  (are (= _1 _2)
      (distinct ()) ()
      (distinct '(1)) '(1)
      (distinct '(1 2 3)) '(1 2 3)
      (distinct '(1 2 3 1 1 1)) '(1 2 3)
      (distinct '(1 1 1 2)) '(1 2)
      (distinct '(1 2 1 2)) '(1 2)

      (distinct []) ()
      (distinct [1]) '(1)
      (distinct [1 2 3]) '(1 2 3)
      (distinct [1 2 3 1 2 2 1 1]) '(1 2 3)
      (distinct [1 1 1 2]) '(1 2)
      (distinct [1 2 1 2]) '(1 2)

      (distinct "") ()
      (distinct "a") '(\a)
      (distinct "abc") '(\a \b \c)
      (distinct "abcabab") '(\a \b \c)
      (distinct "aaab") '(\a \b)
      (distinct "abab") '(\a \b) )

  (are (= (distinct [_ _]) [_])   ; (distinct [x x]) = [x]
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


(deftest test-interpose
  (are (= _1 _2)
    (interpose 0 []) ()
    (interpose 0 [1]) '(1)
    (interpose 0 [1 2]) '(1 0 2)
    (interpose 0 [1 2 3]) '(1 0 2 0 3) ))


(deftest test-interleave
  (are (= _1 _2)
    (interleave [1 2] [3 4]) '(1 3 2 4)

    (interleave [1] [3 4]) '(1 3)
    (interleave [1 2] [3]) '(1 3)

    (interleave [] [3 4]) ()
    (interleave [1 2] []) ()
    (interleave [] []) () ))


(deftest test-zipmap
  (are (= _1 _2)
    (zipmap [:a :b] [1 2]) {:a 1 :b 2}

    (zipmap [:a] [1 2]) {:a 1}
    (zipmap [:a :b] [1]) {:a 1}

    (zipmap [] [1 2]) {}
    (zipmap [:a :b] []) {}
    (zipmap [] []) {} ))


(deftest test-concat
  (are (= _1 _2)
    (concat) ()

    (concat []) ()
    (concat [1 2]) '(1 2)

    (concat [1 2] [3 4]) '(1 2 3 4)
    (concat [] [3 4]) '(3 4)
    (concat [1 2] []) '(1 2)
    (concat [] []) ()

    (concat [1 2] [3 4] [5 6]) '(1 2 3 4 5 6) ))


(deftest test-cycle
  (are (= _1 _2)
    (cycle []) ()

    (take 3 (cycle [1])) '(1 1 1)
    (take 5 (cycle [1 2 3])) '(1 2 3 1 2)

    (take 3 (cycle [nil])) '(nil nil nil) ))


(deftest test-partition
  (are (= _1 _2)
    (partition 2 [1 2 3]) '((1 2))
    (partition 2 [1 2 3 4]) '((1 2) (3 4))
    (partition 2 []) ()

    (partition 2 3 [1 2 3 4 5 6 7]) '((1 2) (4 5))
    (partition 2 3 [1 2 3 4 5 6 7 8]) '((1 2) (4 5) (7 8))
    (partition 2 3 []) ()

    (partition 1 []) ()
    (partition 1 [1 2 3]) '((1) (2) (3))

    (partition 5 [1 2 3]) ()

;    (partition 0 [1 2 3]) (repeat nil)   ; infinite sequence of nil
    (partition -1 [1 2 3]) ()
    (partition -2 [1 2 3]) () ))


(deftest test-reverse
  (are (= _1 _2)
    (reverse []) ()
    (reverse [1]) '(1)
    (reverse [1 2 3]) '(3 2 1) ))


(deftest test-take
  (are (= _1 _2)
    (take 1 [1 2 3 4 5]) '(1)
    (take 3 [1 2 3 4 5]) '(1 2 3)
    (take 5 [1 2 3 4 5]) '(1 2 3 4 5)
    (take 9 [1 2 3 4 5]) '(1 2 3 4 5)

    (take 0 [1 2 3 4 5]) ()
    (take -1 [1 2 3 4 5]) ()
    (take -2 [1 2 3 4 5]) () ))


(deftest test-drop
  (are (= _1 _2)
    (drop 1 [1 2 3 4 5]) '(2 3 4 5)
    (drop 3 [1 2 3 4 5]) '(4 5)
    (drop 5 [1 2 3 4 5]) ()
    (drop 9 [1 2 3 4 5]) ()

    (drop 0 [1 2 3 4 5]) '(1 2 3 4 5)
    (drop -1 [1 2 3 4 5]) '(1 2 3 4 5)
    (drop -2 [1 2 3 4 5]) '(1 2 3 4 5) ))


(deftest test-take-nth
  (are (= _1 _2)
     (take-nth 1 [1 2 3 4 5]) '(1 2 3 4 5)
     (take-nth 2 [1 2 3 4 5]) '(1 3 5)
     (take-nth 3 [1 2 3 4 5]) '(1 4)
     (take-nth 4 [1 2 3 4 5]) '(1 5)
     (take-nth 5 [1 2 3 4 5]) '(1)
     (take-nth 9 [1 2 3 4 5]) '(1)

     ; infinite seq of 1s = (repeat 1)
     ;(take-nth 0 [1 2 3 4 5])
     ;(take-nth -1 [1 2 3 4 5])
     ;(take-nth -2 [1 2 3 4 5])
  ))


(deftest test-take-while
  (are (= _1 _2)
    (take-while pos? []) ()
    (take-while pos? [1 2 3 4]) '(1 2 3 4)
    (take-while pos? [1 2 3 -1]) '(1 2 3)
    (take-while pos? [1 -1 2 3]) '(1)
    (take-while pos? [-1 1 2 3]) ()
    (take-while pos? [-1 -2 -3]) () ))


(deftest test-drop-while
  (are (= _1 _2)
    (drop-while pos? []) ()
    (drop-while pos? [1 2 3 4]) ()
    (drop-while pos? [1 2 3 -1]) '(-1)
    (drop-while pos? [1 -1 2 3]) '(-1 2 3)
    (drop-while pos? [-1 1 2 3]) '(-1 1 2 3)
    (drop-while pos? [-1 -2 -3]) '(-1 -2 -3) ))


(deftest test-butlast
  (are (= _1 _2)
    (butlast []) nil
    (butlast [1]) nil
    (butlast [1 2 3]) '(1 2) ))


(deftest test-drop-last
  (are (= _1 _2)
    ; as butlast
    (drop-last []) ()
    (drop-last [1]) ()
    (drop-last [1 2 3]) '(1 2)

    ; as butlast, but lazy
    (drop-last 1 []) ()
    (drop-last 1 [1]) ()
    (drop-last 1 [1 2 3]) '(1 2)

    (drop-last 2 []) ()
    (drop-last 2 [1]) ()
    (drop-last 2 [1 2 3]) '(1)

    (drop-last 5 []) ()
    (drop-last 5 [1]) ()
    (drop-last 5 [1 2 3]) ()

    (drop-last 0 []) ()
    (drop-last 0 [1]) '(1)
    (drop-last 0 [1 2 3]) '(1 2 3)

    (drop-last -1 []) ()
    (drop-last -1 [1]) '(1)
    (drop-last -1 [1 2 3]) '(1 2 3)

    (drop-last -2 []) ()
    (drop-last -2 [1]) '(1)
    (drop-last -2 [1 2 3]) '(1 2 3) ))


(deftest test-split-at
  (is (vector? (split-at 2 [])))
  (is (vector? (split-at 2 [1 2 3])))

  (are (= _1 _2)
    (split-at 2 []) [() ()]
    (split-at 2 [1 2 3 4 5]) [(list 1 2) (list 3 4 5)]

    (split-at 5 [1 2 3]) [(list 1 2 3) ()]
    (split-at 0 [1 2 3]) [() (list 1 2 3)]
    (split-at -1 [1 2 3]) [() (list 1 2 3)]
    (split-at -5 [1 2 3]) [() (list 1 2 3)] ))


(deftest test-split-with
  (is (vector? (split-with pos? [])))
  (is (vector? (split-with pos? [1 2 -1 0 3 4])))

  (are (= _1 _2)
    (split-with pos? []) [() ()]
    (split-with pos? [1 2 -1 0 3 4]) [(list 1 2) (list -1 0 3 4)]

    (split-with pos? [-1 2 3 4 5]) [() (list -1 2 3 4 5)]
    (split-with number? [1 -2 "abc" \x]) [(list 1 -2) (list "abc" \x)] ))


(deftest test-vals
  (are (= _1 _2)      ; other than map data structures
      (vals ()) nil
      (vals []) nil
      (vals #{}) nil
      (vals "") nil )

  (are (= _1 _2)
      ; (class {:a 1}) => clojure.lang.PersistentArrayMap
      (vals {}) nil
      (vals {:a 1}) '(1)
      (diff (vals {:a 1 :b 2}) '(1 2)) nil              ; (vals {:a 1 :b 2}) '(1 2)

      ; (class (sorted-map :a 1)) => clojure.lang.PersistentTreeMap
      (vals (sorted-map)) nil
      (vals (sorted-map :a 1)) '(1)
      (diff (vals (sorted-map :a 1 :b 2)) '(1 2)) nil   ; (vals (sorted-map :a 1 :b 2)) '(1 2)

      ; (class (hash-map :a 1)) => clojure.lang.PersistentHashMap
      (vals (hash-map)) nil
      (vals (hash-map :a 1)) '(1)
      (diff (vals (hash-map :a 1 :b 2)) '(1 2)) nil ))  ; (vals (hash-map :a 1 :b 2)) '(1 2)


(deftest test-keys
  (are (= _1 _2)      ; other than map data structures
      (keys ()) nil
      (keys []) nil
      (keys #{}) nil
      (keys "") nil )

  (are (= _1 _2)
      ; (class {:a 1}) => clojure.lang.PersistentArrayMap
      (keys {}) nil
      (keys {:a 1}) '(:a)
      (diff (keys {:a 1 :b 2}) '(:a :b)) nil              ; (keys {:a 1 :b 2}) '(:a :b)

      ; (class (sorted-map :a 1)) => clojure.lang.PersistentTreeMap
      (keys (sorted-map)) nil
      (keys (sorted-map :a 1)) '(:a)
      (diff (keys (sorted-map :a 1 :b 2)) '(:a :b)) nil   ; (keys (sorted-map :a 1 :b 2)) '(:a :b)

      ; (class (hash-map :a 1)) => clojure.lang.PersistentHashMap
      (keys (hash-map)) nil
      (keys (hash-map :a 1)) '(:a)
      (diff (keys (hash-map :a 1 :b 2)) '(:a :b)) nil ))  ; (keys (hash-map :a 1 :b 2)) '(:a :b)


(deftest test-empty?
  (are (empty? _)
    nil
    ()
    []
    {}
    #{}
    ""
    (into-array []) )

  (are (not (empty? _))
    '(1 2)
    [1 2]
    {:a 1 :b 2}
    #{1 2}
    "abc"
    (into-array [1 2]) ))


;; pmap
;;
(deftest pmap-does-its-thing
  ;; regression fixed in r1218; was OutOfMemoryError
  (is (= '(1) (pmap inc [0]))))


;; equality
;;
(deftest vectors-equal-other-seqs-by-items-equality
  ;; regression fixed in r1208; was not equal
  (is (= '() []))
  (is (= '(1) [1])))

