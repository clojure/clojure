;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka
; Contributors: Stuart Halloway

(ns clojure.test-clojure.sequences
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer (defspec)])
  (:import clojure.lang.IReduce))

;; *** Tests ***

; TODO:
; apply, map, filter, remove
; and more...

(deftest test-reduce-from-chunked-into-unchunked
  (= [1 2 \a \b] (into [] (concat [1 2] "ab"))))
 
(deftest test-reduce
  (let [int+ (fn [a b] (+ (int a) (int b)))
        arange (range 1 100) ;; enough to cross nodes
        avec (into [] arange)
        alist (into () arange)
        obj-array (into-array arange)
        int-array (into-array Integer/TYPE (map #(Integer. (int %)) arange))
        long-array (into-array Long/TYPE arange)
        float-array (into-array Float/TYPE arange)
        char-array (into-array Character/TYPE (map char arange))
        double-array (into-array Double/TYPE arange)
        byte-array (into-array Byte/TYPE (map byte arange))
        int-vec (into (vector-of :int) arange)
        long-vec (into (vector-of :long) arange)
        float-vec (into (vector-of :float) arange)
        char-vec (into (vector-of :char) (map char arange))
        double-vec (into (vector-of :double) arange)
        byte-vec (into (vector-of :byte) (map byte arange))
        all-true (into-array Boolean/TYPE (repeat 10 true))]
    (is (== 4950
           (reduce + arange)
           (reduce + avec)
           (.reduce ^IReduce avec +)
           (reduce + alist)
           (reduce + obj-array)
           (reduce + int-array)
           (reduce + long-array)
           (reduce + float-array)
           (reduce int+ char-array)
           (reduce + double-array)
           (reduce int+ byte-array)
           (reduce + int-vec)
           (reduce + long-vec)
           (reduce + float-vec)
           (reduce int+ char-vec)
           (reduce + double-vec)
           (reduce int+ byte-vec)))
    (is (== 4951
           (reduce + 1 arange)
           (reduce + 1 avec)
           (.reduce ^IReduce avec + 1)
           (reduce + 1 alist)
           (reduce + 1 obj-array)
           (reduce + 1 int-array)
           (reduce + 1 long-array)
           (reduce + 1 float-array)
           (reduce int+ 1 char-array)
           (reduce + 1 double-array)
           (reduce int+ 1 byte-array)
           (reduce + 1 int-vec)
           (reduce + 1 long-vec)
           (reduce + 1 float-vec)
           (reduce int+ 1 char-vec)
           (reduce + 1 double-vec)
           (reduce int+ 1 byte-vec)))
    (is (= true
           (reduce #(and %1 %2) all-true)
           (reduce #(and %1 %2) true all-true)))))

(deftest test-into-IReduceInit
  (let [iri (reify clojure.lang.IReduceInit
              (reduce [_ f start]
                (reduce f start (range 5))))]
    (is (= [0 1 2 3 4] (into [] iri)))))

;; CLJ-1237 regression test
(deftest reduce-with-varying-impls
  (is (= 1000000
         (->> (repeat 500000 (cons 1 [1]))
              (apply concat)
              (reduce +))))

  (is (= 4500000
         (->> (range 100000)
              (mapcat (fn [_] (java.util.ArrayList. (range 10))))
              (reduce +)))))

(deftest test-equality
  ; lazy sequences
  (are [x y] (= x y)
      ; fixed SVN 1288 - LazySeq and EmptyList equals/equiv
      ; http://groups.google.com/group/clojure/browse_frm/thread/286d807be9cae2a5#
      (map inc nil) ()
      (map inc ()) ()
      (map inc []) ()
      (map inc #{}) ()
      (map inc {}) ()
      (sequence (map inc) (range 10)) (range 1 11)
      (range 1 11) (sequence (map inc) (range 10))))


(deftest test-lazy-seq
  (are [x] (seq? x)
      (lazy-seq nil)
      (lazy-seq [])
      (lazy-seq [1 2]))

  (is (not (.equals (lazy-seq [3]) (lazy-seq [3N]))))

  (are [x y] (= x y)
      (lazy-seq nil) ()
      (lazy-seq [nil]) '(nil)

      (lazy-seq ()) ()
      (lazy-seq []) ()
      (lazy-seq #{}) ()
      (lazy-seq {}) ()
      (lazy-seq "") ()
      (lazy-seq (into-array [])) ()

      (lazy-seq [3]) [3N]
      (lazy-seq (list 1 2)) '(1 2)
      (lazy-seq [1 2]) '(1 2)
      (lazy-seq (sorted-set 1 2)) '(1 2)
      (lazy-seq (sorted-map :a 1 :b 2)) '([:a 1] [:b 2])
      (lazy-seq "abc") '(\a \b \c)
      (lazy-seq (into-array [1 2])) '(1 2) ))


(deftest test-seq
  (is (not (seq? (seq []))))
  (is (seq? (seq [1 2])))
  (is (not (.equals (seq [3]) (seq [3N]))))
  
  (are [x y] (= x y)
    (seq nil) nil
    (seq [nil]) '(nil)

    (seq ()) nil
    (seq []) nil
    (seq #{}) nil
    (seq {}) nil
    (seq "") nil
    (seq (into-array [])) nil

    (seq [3]) [3N]
    (seq (list 1 2)) '(1 2)
    (seq [1 2]) '(1 2)
    (seq (sorted-set 1 2)) '(1 2)
    (seq (sorted-map :a 1 :b 2)) '([:a 1] [:b 2])
    (seq "abc") '(\a \b \c)
    (seq (into-array [1 2])) '(1 2) ))


(deftest test-cons
  (is (thrown? IllegalArgumentException (cons 1 2)))
  (are [x y] (= x y)
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


(deftest test-empty
  (are [x y] (and (= (empty x) y)
                  (= (class (empty x)) (class y)))
      nil nil

      () ()
      '(1 2) ()

      [] []
      [1 2] []

      {} {}
      {:a 1 :b 2} {}

      (sorted-map) (sorted-map)
      (sorted-map :a 1 :b 2) (sorted-map)

      #{} #{}
      #{1 2} #{}

      (sorted-set) (sorted-set)
      (sorted-set 1 2) (sorted-set)

      (seq ()) nil      ; (seq ()) => nil
      (seq '(1 2)) ()

      (seq []) nil      ; (seq []) => nil
      (seq [1 2]) ()

      (seq "") nil      ; (seq "") => nil
      (seq "ab") ()

      (lazy-seq ()) ()
      (lazy-seq '(1 2)) ()

      (lazy-seq []) ()
      (lazy-seq [1 2]) ()

      ; non-coll, non-seq => nil
      42 nil
      1.2 nil
      "abc" nil ))

;Tests that the comparator is preserved
;The first element should be the same in each set if preserved.
(deftest test-empty-sorted
  (let [inv-compare (comp - compare)]
    (are [x y] (= (first (into (empty x) x)) 
		  (first y))
	 (sorted-set 1 2 3) (sorted-set 1 2 3)
	 (sorted-set-by inv-compare 1 2 3) (sorted-set-by inv-compare 1 2 3)

	 (sorted-map 1 :a 2 :b 3 :c) (sorted-map 1 :a 2 :b 3 :c)
	 (sorted-map-by inv-compare 1 :a 2 :b 3 :c) (sorted-map-by inv-compare 1 :a 2 :b 3 :c))))


(deftest test-not-empty
  ; empty coll/seq => nil
  (are [x] (= (not-empty x) nil)
      ()
      []
      {}
      #{}
      (seq ())
      (seq [])
      (lazy-seq ())
      (lazy-seq []) )

  ; non-empty coll/seq => identity
  (are [x] (and (= (not-empty x) x)
                (= (class (not-empty x)) (class x)))
      '(1 2)
      [1 2]
      {:a 1}
      #{1 2}
      (seq '(1 2))
      (seq [1 2])
      (lazy-seq '(1 2))
      (lazy-seq [1 2]) ))


(deftest test-first
  ;(is (thrown? Exception (first)))
  (is (thrown? IllegalArgumentException (first true)))
  (is (thrown? IllegalArgumentException (first false)))
  (is (thrown? IllegalArgumentException (first 1)))
  ;(is (thrown? IllegalArgumentException (first 1 2)))
  (is (thrown? IllegalArgumentException (first \a)))
  (is (thrown? IllegalArgumentException (first 's)))
  (is (thrown? IllegalArgumentException (first :k)))
  (are [x y] (= x y)
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
    (first (sorted-set 1 2 3)) 1

    (first #{nil}) nil
    (first (sorted-set 1 nil)) nil
    (first (sorted-set nil 2)) nil
    (first #{#{}}) #{}
    (first (sorted-set #{} nil)) nil
    ;(first (sorted-set #{} 2 nil)) nil

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
 ; (is (thrown? IllegalArgumentException (next)))
  (is (thrown? IllegalArgumentException (next true)))
  (is (thrown? IllegalArgumentException (next false)))
  (is (thrown? IllegalArgumentException (next 1)))
  ;(is (thrown? IllegalArgumentException (next 1 2)))
  (is (thrown? IllegalArgumentException (next \a)))
  (is (thrown? IllegalArgumentException (next 's)))
  (is (thrown? IllegalArgumentException (next :k)))
  (are [x y] (= x y)
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
    (next (sorted-set 1 2 3)) '(2 3)

    (next #{nil}) nil
    (next (sorted-set 1 nil)) '(1)
    (next (sorted-set nil 2)) '(2)
    (next #{#{}}) nil
    (next (sorted-set #{} nil)) '(#{})
    ;(next (sorted-set #{} 2 nil)) #{}

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


(deftest test-last
  (are [x y] (= x y)
      (last nil) nil

      ; list
      (last ()) nil
      (last '(1)) 1
      (last '(1 2 3)) 3

      (last '(nil)) nil
      (last '(1 nil)) nil
      (last '(nil 2)) 2
      (last '(())) ()
      (last '(() nil)) nil
      (last '(() 2 nil)) nil

      ; vector
      (last []) nil
      (last [1]) 1
      (last [1 2 3]) 3

      (last [nil]) nil
      (last [1 nil]) nil
      (last [nil 2]) 2
      (last [[]]) []
      (last [[] nil]) nil
      (last [[] 2 nil]) nil

      ; set
      (last #{}) nil
      (last #{1}) 1
      (last (sorted-set 1 2 3)) 3

      (last #{nil}) nil
      (last (sorted-set 1 nil)) 1
      (last (sorted-set nil 2)) 2
      (last #{#{}}) #{}
      (last (sorted-set #{} nil)) #{}
      ;(last (sorted-set #{} 2 nil)) nil

      ; map
      (last {}) nil
      (last (sorted-map :a 1)) [:a 1]
      (last (sorted-map :a 1 :b 2 :c 3)) [:c 3]

      ; string
      (last "") nil
      (last "a") \a
      (last "abc") \c

      ; array
      (last (into-array [])) nil
      (last (into-array [1])) 1
      (last (into-array [1 2 3])) 3
      (last (to-array [nil])) nil
      (last (to-array [1 nil])) nil
      (last (to-array [nil 2])) 2 ))


;; (ffirst coll) = (first (first coll))
;;
(deftest test-ffirst
;  (is (thrown? IllegalArgumentException (ffirst)))
  (are [x y] (= x y)
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
;  (is (thrown? IllegalArgumentException (fnext)))
  (are [x y] (= x y)
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
;  (is (thrown? IllegalArgumentException (nfirst)))
  (are [x y] (= x y)
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
;  (is (thrown? IllegalArgumentException (nnext)))
  (are [x y] (= x y)
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


(deftest test-nth
  ; maps, sets are not supported
  (is (thrown? UnsupportedOperationException (nth {} 0)))
  (is (thrown? UnsupportedOperationException (nth {:a 1 :b 2} 0)))
  (is (thrown? UnsupportedOperationException (nth #{} 0)))
  (is (thrown? UnsupportedOperationException (nth #{1 2 3} 0)))

  ; out of bounds
  (is (thrown? IndexOutOfBoundsException (nth '() 0)))
  (is (thrown? IndexOutOfBoundsException (nth '(1 2 3) 5)))
  (is (thrown? IndexOutOfBoundsException (nth '() -1)))
  (is (thrown? IndexOutOfBoundsException (nth '(1 2 3) -1)))

  (is (thrown? IndexOutOfBoundsException (nth [] 0)))
  (is (thrown? IndexOutOfBoundsException (nth [1 2 3] 5)))
  (is (thrown? IndexOutOfBoundsException (nth [] -1)))
  (is (thrown? IndexOutOfBoundsException (nth [1 2 3] -1)))  ; ???

  (is (thrown? IndexOutOfBoundsException (nth (into-array []) 0)))
  (is (thrown? IndexOutOfBoundsException (nth (into-array [1 2 3]) 5)))
  (is (thrown? IndexOutOfBoundsException (nth (into-array []) -1)))
  (is (thrown? IndexOutOfBoundsException (nth (into-array [1 2 3]) -1)))

  (is (thrown? StringIndexOutOfBoundsException (nth "" 0)))
  (is (thrown? StringIndexOutOfBoundsException (nth "abc" 5)))
  (is (thrown? StringIndexOutOfBoundsException (nth "" -1)))
  (is (thrown? StringIndexOutOfBoundsException (nth "abc" -1)))

  (is (thrown? IndexOutOfBoundsException (nth (java.util.ArrayList. []) 0)))
  (is (thrown? IndexOutOfBoundsException (nth (java.util.ArrayList. [1 2 3]) 5)))
  (is (thrown? IndexOutOfBoundsException (nth (java.util.ArrayList. []) -1)))       ; ???
  (is (thrown? IndexOutOfBoundsException (nth (java.util.ArrayList. [1 2 3]) -1)))  ; ???

  (are [x y] (= x y)
      (nth '(1) 0) 1
      (nth '(1 2 3) 0) 1
      (nth '(1 2 3 4 5) 1) 2
      (nth '(1 2 3 4 5) 4) 5
      (nth '(1 2 3) 5 :not-found) :not-found

      (nth [1] 0) 1
      (nth [1 2 3] 0) 1
      (nth [1 2 3 4 5] 1) 2
      (nth [1 2 3 4 5] 4) 5
      (nth [1 2 3] 5 :not-found) :not-found

      (nth (into-array [1]) 0) 1
      (nth (into-array [1 2 3]) 0) 1
      (nth (into-array [1 2 3 4 5]) 1) 2
      (nth (into-array [1 2 3 4 5]) 4) 5
      (nth (into-array [1 2 3]) 5 :not-found) :not-found

      (nth "a" 0) \a
      (nth "abc" 0) \a
      (nth "abcde" 1) \b
      (nth "abcde" 4) \e
      (nth "abc" 5 :not-found) :not-found

      (nth (java.util.ArrayList. [1]) 0) 1
      (nth (java.util.ArrayList. [1 2 3]) 0) 1
      (nth (java.util.ArrayList. [1 2 3 4 5]) 1) 2
      (nth (java.util.ArrayList. [1 2 3 4 5]) 4) 5
      (nth (java.util.ArrayList. [1 2 3]) 5 :not-found) :not-found )

  ; regex Matchers
  (let [m (re-matcher #"(a)(b)" "ababaa")]
    (re-find m) ; => ["ab" "a" "b"]
    (are [x y] (= x y)
        (nth m 0) "ab"
        (nth m 1) "a"
        (nth m 2) "b"
        (nth m 3 :not-found) :not-found
        (nth m -1 :not-found) :not-found )
    (is (thrown? IndexOutOfBoundsException (nth m 3)))
    (is (thrown? IndexOutOfBoundsException (nth m -1))))

  (let [m (re-matcher #"c" "ababaa")]
    (re-find m) ; => nil
    (are [x y] (= x y)
        (nth m 0 :not-found) :not-found
        (nth m 2 :not-found) :not-found
        (nth m -1 :not-found) :not-found )
    (is (thrown? IllegalStateException (nth m 0)))
    (is (thrown? IllegalStateException (nth m 2)))
    (is (thrown? IllegalStateException (nth m -1)))))


; distinct was broken for nil & false:
;   fixed in rev 1278:
;   http://code.google.com/p/clojure/source/detail?r=1278
;
(deftest test-distinct
  (are [x y] (= x y)
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

  (are [x] (= (distinct [x x]) [x])   
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
  (are [x y] (= x y)
    (interpose 0 []) ()
    (interpose 0 [1]) '(1)
    (interpose 0 [1 2]) '(1 0 2)
    (interpose 0 [1 2 3]) '(1 0 2 0 3) ))


(deftest test-interleave
  (are [x y] (= x y)
    (interleave [1 2] [3 4]) '(1 3 2 4)

    (interleave [1] [3 4]) '(1 3)
    (interleave [1 2] [3]) '(1 3)

    (interleave [] [3 4]) ()
    (interleave [1 2] []) ()
    (interleave [] []) ()

    (interleave [1]) '(1)

    (interleave) () ))


(deftest test-zipmap
  (are [x y] (= x y)
    (zipmap [:a :b] [1 2]) {:a 1 :b 2}

    (zipmap [:a] [1 2]) {:a 1}
    (zipmap [:a :b] [1]) {:a 1}

    (zipmap [] [1 2]) {}
    (zipmap [:a :b] []) {}
    (zipmap [] []) {} ))


(deftest test-concat
  (are [x y] (= x y)
    (concat) ()

    (concat []) ()
    (concat [1 2]) '(1 2)

    (concat [1 2] [3 4]) '(1 2 3 4)
    (concat [] [3 4]) '(3 4)
    (concat [1 2] []) '(1 2)
    (concat [] []) ()

    (concat [1 2] [3 4] [5 6]) '(1 2 3 4 5 6) ))


(deftest test-cycle
  (are [x y] (= x y)
    (cycle []) ()

    (take 3 (cycle [1])) '(1 1 1)
    (take 5 (cycle [1 2 3])) '(1 2 3 1 2)

    (take 3 (cycle [nil])) '(nil nil nil)

    (transduce (take 5) + (cycle [1])) 5
    (transduce (take 5) + 2 (cycle [1])) 7
    (transduce (take 5) + (cycle [3 7])) 23
    (transduce (take 5) + 2 (cycle [3 7])) 25

    (take 2 (cycle (map #(/ 42 %) '(2 1 0)))) '(21 42)
    (first (next (cycle (map #(/ 42 %) '(2 1 0))))) 42
    (into [] (take 2) (cycle (map #(/ 42 %) '(2 1 0)))) '(21 42)))


(deftest test-partition
  (are [x y] (= x y)
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
    (partition -2 [1 2 3]) () )

    ;; reduce
    (is (= [1 2 4 8 16] (map #(reduce * (repeat % 2)) (range 5))))
    (is (= [3 6 12 24 48] (map #(reduce * 3 (repeat % 2)) (range 5))))

    ;; equality and hashing
    (is (= (repeat 5 :x) (repeat 5 :x)))
    (is (= (repeat 5 :x) '(:x :x :x :x :x)))
    (is (= (hash (repeat 5 :x)) (hash '(:x :x :x :x :x))))
    (is (= (assoc (array-map (repeat 1 :x) :y) '(:x) :z) {'(:x) :z}))
    (is (= (assoc (hash-map (repeat 1 :x) :y) '(:x) :z) {'(:x) :z})))


(deftest test-iterate
      (are [x y] (= x y)
           (take 0 (iterate inc 0)) ()
           (take 1 (iterate inc 0)) '(0)
           (take 2 (iterate inc 0)) '(0 1)
           (take 5 (iterate inc 0)) '(0 1 2 3 4) )

      ;; test other fns
      (is (= '(:foo 42 :foo 42) (take 4 (iterate #(if (= % :foo) 42 :foo) :foo))))
      (is (= '(1 false true true) (take 4 (iterate #(instance? Boolean %) 1))))
      (is (= '(256 128 64 32 16 8 4 2 1 0) (take 10 (iterate #(quot % 2) 256))))
      (is (= '(0 true) (take 2 (iterate zero? 0))))
      (is (= 2 (first (next (next (iterate inc 0))))))
      (is (= [1 2 3] (into [] (take 3) (next (iterate inc 0)))))

      ;; reduce via transduce
      (is (= (transduce (take 5) + (iterate #(* 2 %) 2)) 62))
      (is (= (transduce (take 5) + 1 (iterate #(* 2 %) 2)) 63)) )


(deftest test-reverse
  (are [x y] (= x y)
    (reverse nil) ()    ; since SVN 1294
    (reverse []) ()
    (reverse [1]) '(1)
    (reverse [1 2 3]) '(3 2 1) ))


(deftest test-take
  (are [x y] (= x y)
    (take 1 [1 2 3 4 5]) '(1)
    (take 3 [1 2 3 4 5]) '(1 2 3)
    (take 5 [1 2 3 4 5]) '(1 2 3 4 5)
    (take 9 [1 2 3 4 5]) '(1 2 3 4 5)

    (take 0 [1 2 3 4 5]) ()
    (take -1 [1 2 3 4 5]) ()
    (take -2 [1 2 3 4 5]) () ))


(deftest test-drop
  (are [x y] (= x y)
    (drop 1 [1 2 3 4 5]) '(2 3 4 5)
    (drop 3 [1 2 3 4 5]) '(4 5)
    (drop 5 [1 2 3 4 5]) ()
    (drop 9 [1 2 3 4 5]) ()

    (drop 0 [1 2 3 4 5]) '(1 2 3 4 5)
    (drop -1 [1 2 3 4 5]) '(1 2 3 4 5)
    (drop -2 [1 2 3 4 5]) '(1 2 3 4 5) ))


(deftest test-take-nth
  (are [x y] (= x y)
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
  (are [x y] (= x y)
    (take-while pos? []) ()
    (take-while pos? [1 2 3 4]) '(1 2 3 4)
    (take-while pos? [1 2 3 -1]) '(1 2 3)
    (take-while pos? [1 -1 2 3]) '(1)
    (take-while pos? [-1 1 2 3]) ()
    (take-while pos? [-1 -2 -3]) () ))


(deftest test-drop-while
  (are [x y] (= x y)
    (drop-while pos? []) ()
    (drop-while pos? [1 2 3 4]) ()
    (drop-while pos? [1 2 3 -1]) '(-1)
    (drop-while pos? [1 -1 2 3]) '(-1 2 3)
    (drop-while pos? [-1 1 2 3]) '(-1 1 2 3)
    (drop-while pos? [-1 -2 -3]) '(-1 -2 -3) ))


(deftest test-butlast
  (are [x y] (= x y)
    (butlast []) nil
    (butlast [1]) nil
    (butlast [1 2 3]) '(1 2) ))


(deftest test-drop-last
  (are [x y] (= x y)
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

  (are [x y] (= x y)
    (split-at 2 []) [() ()]
    (split-at 2 [1 2 3 4 5]) [(list 1 2) (list 3 4 5)]

    (split-at 5 [1 2 3]) [(list 1 2 3) ()]
    (split-at 0 [1 2 3]) [() (list 1 2 3)]
    (split-at -1 [1 2 3]) [() (list 1 2 3)]
    (split-at -5 [1 2 3]) [() (list 1 2 3)] ))


(deftest test-split-with
  (is (vector? (split-with pos? [])))
  (is (vector? (split-with pos? [1 2 -1 0 3 4])))

  (are [x y] (= x y)
    (split-with pos? []) [() ()]
    (split-with pos? [1 2 -1 0 3 4]) [(list 1 2) (list -1 0 3 4)]

    (split-with pos? [-1 2 3 4 5]) [() (list -1 2 3 4 5)]
    (split-with number? [1 -2 "abc" \x]) [(list 1 -2) (list "abc" \x)] ))


(deftest test-repeat
  ;(is (thrown? IllegalArgumentException (repeat)))

  ; infinite sequence => use take
  (are [x y] (= x y)
      (take 0 (repeat 7)) ()
      (take 1 (repeat 7)) '(7)
      (take 2 (repeat 7)) '(7 7)
      (take 5 (repeat 7)) '(7 7 7 7 7) )

  ; limited sequence
  (are [x y] (= x y)
      (repeat 0 7) ()
      (repeat 1 7) '(7)
      (repeat 2 7) '(7 7)
      (repeat 5 7) '(7 7 7 7 7)

      (repeat -1 7) ()
      (repeat -3 7) () )

  ; test different data types
  (are [x] (= (repeat 3 x) (list x x x))
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

(defspec longrange-equals-range 100
  (prop/for-all [start gen/int
                 end gen/int
                 step gen/s-pos-int]
                (= (clojure.lang.Range/create start end step)
                   (clojure.lang.LongRange/create start end step))))

(deftest test-range
  (are [x y] (= x y)
      (take 100 (range)) (range 100)

      (range 0) ()   ; exclusive end!
      (range 1) '(0)
      (range 5) '(0 1 2 3 4)

      (range -1) ()
      (range -3) ()

      (range 2.5) '(0 1 2)
      (range 7/3) '(0 1 2)

      (range 0 3) '(0 1 2)
      (range 0 1) '(0)
      (range 0 0) ()
      (range 0 -3) ()

      (range 3 6) '(3 4 5)
      (range 3 4) '(3)
      (range 3 3) ()
      (range 3 1) ()
      (range 3 0) ()
      (range 3 -2) ()

      (range -2 5) '(-2 -1 0 1 2 3 4)
      (range -2 0) '(-2 -1)
      (range -2 -1) '(-2)
      (range -2 -2) ()
      (range -2 -5) ()

      (take 3 (range 3 9 0)) '(3 3 3)
      (take 3 (range 9 3 0)) '(9 9 9)
      (range 0 0 0) ()
      (range 3 9 1) '(3 4 5 6 7 8)
      (range 3 9 2) '(3 5 7)
      (range 3 9 3) '(3 6)
      (range 3 9 10) '(3)
      (range 3 9 -1) ()
      (range 10 10 -1) ()
      (range 10 9 -1) '(10)
      (range 10 8 -1) '(10 9)
      (range 10 7 -1) '(10 9 8)
      (range 10 0 -2) '(10 8 6 4 2)

      (take 100 (range)) (take 100 (iterate inc 0))

      (range 1/2 5 1/3) '(1/2 5/6 7/6 3/2 11/6 13/6 5/2 17/6 19/6 7/2 23/6 25/6 9/2 29/6)
      (range 0.5 8 1.2) '(0.5 1.7 2.9 4.1 5.3 6.5 7.7)
      (range 0.5 -4 -2) '(0.5 -1.5 -3.5)
      (take 3 (range Long/MAX_VALUE Double/POSITIVE_INFINITY)) '(9223372036854775807 9223372036854775808N 9223372036854775809N)

      (reduce + (take 100 (range))) 4950
      (reduce + 0 (take 100 (range))) 4950
      (reduce + (range 100)) 4950
      (reduce + 0 (range 100)) 4950
      (reduce + (range 0.0 100.0)) 4950.0
      (reduce + 0 (range 0.0 100.0)) 4950.0

      (reduce + (iterator-seq (.iterator (range 100)))) 4950
      (reduce + (iterator-seq (.iterator (range 0.0 100.0 1.0)))) 4950.0 ))

(defn unlimited-range-create [& args]
  (let [[arg1 arg2 arg3] args]
    (case (count args)
      1 (clojure.lang.Range/create arg1)
      2 (clojure.lang.Range/create arg1 arg2)
      3 (clojure.lang.Range/create arg1 arg2 arg3))))

(deftest test-longrange-corners
  (let [lmax Long/MAX_VALUE
        lmax-1 (- Long/MAX_VALUE 1)
        lmax-2 (- Long/MAX_VALUE 2)
        lmax-31 (- Long/MAX_VALUE 31)
        lmax-32 (- Long/MAX_VALUE 32)
        lmax-33 (- Long/MAX_VALUE 33)
        lmin Long/MIN_VALUE
        lmin+1 (+ Long/MIN_VALUE 1)
        lmin+2 (+ Long/MIN_VALUE 2)
        lmin+31 (+ Long/MIN_VALUE 31)
        lmin+32 (+ Long/MIN_VALUE 32)
        lmin+33 (+ Long/MIN_VALUE 33)]
    (doseq [range-args [ [lmax-2 lmax]
                         [lmax-33 lmax]
                         [lmax-33 lmax-31]
                         [lmin+2 lmin -1]
                         [lmin+33 lmin -1]
                         [lmin+33 lmin+31 -1]
                         [lmin lmax lmax]
                         [lmax lmin lmin]
                         [-1 lmax lmax]
                         [1 lmin lmin]]]
    (is (= (apply unlimited-range-create range-args)
           (apply range range-args))
        (apply str "from (range " (concat (interpose " " range-args) ")"))))))

(deftest test-empty?
  (are [x] (empty? x)
    nil
    ()
    (lazy-seq nil)    ; => ()
    []
    {}
    #{}
    ""
    (into-array []) )

  (are [x] (not (empty? x))
    '(1 2)
    (lazy-seq [1 2])
    [1 2]
    {:a 1 :b 2}
    #{1 2}
    "abc"
    (into-array [1 2]) ))


(deftest test-every?
  ; always true for nil or empty coll/seq
  (are [x] (= (every? pos? x) true)
      nil
      () [] {} #{}
      (lazy-seq [])
      (into-array []) )

  (are [x y] (= x y)
      true (every? pos? [1])
      true (every? pos? [1 2])
      true (every? pos? [1 2 3 4 5])

      false (every? pos? [-1])
      false (every? pos? [-1 -2])
      false (every? pos? [-1 -2 3])
      false (every? pos? [-1 2])
      false (every? pos? [1 -2])
      false (every? pos? [1 2 -3])
      false (every? pos? [1 2 -3 4]) )

  (are [x y] (= x y)
      true (every? #{:a} [:a :a])
;!      false (every? #{:a} [:a :b])   ; Issue 68: every? returns nil instead of false
;!      false (every? #{:a} [:b :b])   ; http://code.google.com/p/clojure/issues/detail?id=68
  ))


(deftest test-not-every?
  ; always false for nil or empty coll/seq
  (are [x] (= (not-every? pos? x) false)
      nil
      () [] {} #{}
      (lazy-seq [])
      (into-array []) )

  (are [x y] (= x y)
      false (not-every? pos? [1])
      false (not-every? pos? [1 2])
      false (not-every? pos? [1 2 3 4 5])

      true (not-every? pos? [-1])
      true (not-every? pos? [-1 -2])
      true (not-every? pos? [-1 -2 3])
      true (not-every? pos? [-1 2])
      true (not-every? pos? [1 -2])
      true (not-every? pos? [1 2 -3])
      true (not-every? pos? [1 2 -3 4]) )

  (are [x y] (= x y)
      false (not-every? #{:a} [:a :a])
      true (not-every? #{:a} [:a :b])
      true (not-every? #{:a} [:b :b]) ))


(deftest test-not-any?
  ; always true for nil or empty coll/seq
  (are [x] (= (not-any? pos? x) true)
      nil
      () [] {} #{}
      (lazy-seq [])
      (into-array []) )

  (are [x y] (= x y)
      false (not-any? pos? [1])
      false (not-any? pos? [1 2])
      false (not-any? pos? [1 2 3 4 5])

      true (not-any? pos? [-1])
      true (not-any? pos? [-1 -2])

      false (not-any? pos? [-1 -2 3])
      false (not-any? pos? [-1 2])
      false (not-any? pos? [1 -2])
      false (not-any? pos? [1 2 -3])
      false (not-any? pos? [1 2 -3 4]) )

  (are [x y] (= x y)
      false (not-any? #{:a} [:a :a])
      false (not-any? #{:a} [:a :b])
      true (not-any? #{:a} [:b :b]) ))


(deftest test-some
  ;; always nil for nil or empty coll/seq
  (are [x] (= (some pos? x) nil)
       nil
       () [] {} #{}
       (lazy-seq [])
       (into-array []))
  
  (are [x y] (= x y)
       nil (some nil nil)
       
       true (some pos? [1])
       true (some pos? [1 2])
       
       nil (some pos? [-1])
       nil (some pos? [-1 -2])
       true (some pos? [-1 2])
       true (some pos? [1 -2])
       
       :a (some #{:a} [:a :a])
       :a (some #{:a} [:b :a])
       nil (some #{:a} [:b :b])
       
       :a (some #{:a} '(:a :b))
       :a (some #{:a} #{:a :b})
       ))

(deftest test-flatten-present
  (are [expected nested-val] (= (flatten nested-val) expected)
       ;simple literals
       [] nil
       [] 1
       [] 'test
       [] :keyword
       [] 1/2
       [] #"[\r\n]"
       [] true
       [] false
       ;vectors
       [1 2 3 4 5] [[1 2] [3 4 [5]]]
       [1 2 3 4 5] [1 2 3 4 5]
       [#{1 2} 3 4 5] [#{1 2} 3 4 5]
       ;sets
       [] #{}
       [] #{#{1 2} 3 4 5}
       [] #{1 2 3 4 5}
       [] #{#{1 2} 3 4 5}
       ;lists
       [] '()
       [1 2 3 4 5] `(1 2 3 4 5)
       ;maps
       [] {:a 1 :b 2}
       [:a 1 :b 2] (sort-by key {:a 1 :b 2})
       [] {[:a :b] 1 :c 2}
       [:a :b 1 :c 2] (sort-by val {[:a :b] 1 :c 2})
       [:a 1 2 :b 3] (sort-by key {:a [1 2] :b 3})
       ;Strings
       [] "12345"
       [\1 \2 \3 \4 \5] (seq "12345")
       ;fns
       [] count
       [count even? odd?] [count even? odd?]))

(deftest test-group-by
  (is (= (group-by even? [1 2 3 4 5]) 
	 {false [1 3 5], true [2 4]})))

(deftest test-partition-by
  (are [test-seq] (= (partition-by (comp even? count) test-seq)
		     [["a"] ["bb" "cccc" "dd"] ["eee" "f"] ["" "hh"]])
       ["a" "bb" "cccc" "dd" "eee" "f" "" "hh"]
       '("a" "bb" "cccc" "dd" "eee" "f" "" "hh"))
  (is (=(partition-by #{\a \e \i \o \u} "abcdefghijklm")
       [[\a] [\b \c \d] [\e] [\f \g \h] [\i] [\j \k \l \m]])))

(deftest test-frequencies
  (are [expected test-seq] (= (frequencies test-seq) expected)
       {\p 2, \s 4, \i 4, \m 1} "mississippi"
       {1 4 2 2 3 1} [1 1 1 1 2 2 3]
       {1 4 2 2 3 1} '(1 1 1 1 2 2 3)))

(deftest test-reductions
  (is (= (reductions + nil)
         [0]))
  (is (= (reductions + [1 2 3 4 5])
	 [1 3 6 10 15]))
  (is (= (reductions + 10 [1 2 3 4 5])
	 [10 11 13 16 20 25])))

(deftest test-reductions-obeys-reduced
  (is (= [0 :x]
         (reductions (constantly (reduced :x))
                     (range))))
  (is (= [:x]
         (reductions (fn [acc x] x)
                     (reduced :x)
                     (range))))
  (is (= [2 6 12 12]
         (reductions (fn [acc x]
                       (if (= x :stop)
                         (reduced acc)
                         (+ acc x)))
                     [2 4 6 :stop 8 10]))))

(deftest test-rand-nth-invariants
  (let [elt (rand-nth [:a :b :c :d])]
    (is (#{:a :b :c :d} elt))))

(deftest test-partition-all
  (is (= (partition-all 4 [1 2 3 4 5 6 7 8 9])
	 [[1 2 3 4] [5 6 7 8] [9]]))
  (is (= (partition-all 4 2 [1 2 3 4 5 6 7 8 9])
	 [[1 2 3 4] [3 4 5 6] [5 6 7 8] [7 8 9] [9]])))

(deftest test-shuffle-invariants
  (is (= (count (shuffle [1 2 3 4])) 4))
  (let [shuffled-seq (shuffle [1 2 3 4])]
    (is (every? #{1 2 3 4} shuffled-seq))))

(deftest test-ArrayIter
  (are [arr expected]
    (let [iter (clojure.lang.ArrayIter/createFromObject arr)]
      (loop [accum []]
        (if (.hasNext iter)
          (recur (conj accum (.next iter)))
          (is (= expected accum)))))
    nil []
    (object-array ["a" "b" "c"]) ["a" "b" "c"]
    (boolean-array [false true false]) [false true false]
    (byte-array [1 2]) [(byte 1) (byte 2)]
    (short-array [1 2]) [1 2]
    (int-array [1 2]) [1 2]
    (long-array [1 2]) [1 2]
    (float-array [2.0 -2.5]) [2.0 -2.5]
    (double-array [1.2 -3.5]) [1.2 -3.5]
    (char-array [\H \i]) [\H \i]))

(deftest CLJ-1633
  (is (= ((fn [& args] (apply (fn [a & b] (apply list b)) args)) 1 2 3) '(2 3))))

(deftest test-subseq
  (let [s1 (range 100)
        s2 (into (sorted-set) s1)]
    (is (= s1 (seq s2)))
    (doseq [i (range 100)]
      (is (= s1 (concat (subseq s2 < i) (subseq s2 >= i))))
      (is (= (reverse s1) (concat (rsubseq s2 >= i) (rsubseq s2 < i)))))))
