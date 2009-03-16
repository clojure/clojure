;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Application examples for algebraic data types
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns clojure.contrib.types.examples
  (:use [clojure.contrib.types
	 :only (deftype defadt match)])
  (:require [clojure.contrib.generic.collection :as gc]))

;
; Multisets implemented as maps to integers
;

; The most basic type definition. A more elaborate version could add
; a constructor that verifies that its argument is a map with integer values.
(deftype ::multiset multiset)

; Some set operations generalized to multisets
; Note that the multiset constructor is nowhere called explicitly, as the
; map operations all preserve the metadata.
(defmethod gc/conj ::multiset
  ([ms x]
   (assoc ms x (inc (get ms x 0))))
  ([ms x & xs]
    (reduce gc/conj (gc/conj ms x) xs)))

(defmulti union (fn [& sets] (type (first sets))))

(defmethod union clojure.lang.IPersistentSet
  [& sets]
  (apply clojure.set/union sets))

; Note: a production-quality implementation should accept standard sets
; and perhaps other collections for its second argument.
(defmethod union ::multiset
  ([ms] ms)
  ([ms1 ms2]
     (letfn [(add-item [ms [item n]]
		       (assoc ms item (+ n (get ms item 0))))]
       (reduce add-item ms1 ms2)))
  ([ms1 ms2 & mss]
     (reduce union (union ms1 ms2) mss)))

; Let's use it:
(gc/conj #{} :a :a :b :c)
(gc/conj (multiset {}) :a :a :b :c)

(union #{:a :b} #{:b :c})
(union (multiset {:a 1 :b 1}) (multiset {:b 1 :c 2}))

;
; A simple tree structure defined as an algebraic data type
;
(defadt ::tree
  empty-tree
  (leaf value)
  (node left-tree right-tree))

(def a-tree (node (leaf :a) 
		  (node (leaf :b)
			(leaf :c))))

(defn depth
  [t]
  (match t
    empty-tree  0
    (leaf _)    1
    (node l r)  (inc (max (depth l) (depth r)))))

(depth empty-tree)
(depth a-tree)

; Algebraic data types with multimethods: map on a tree
(defmethod gc/map ::tree
  [f t]
  (match t
    empty-tree  empty-tree
    (leaf v)    (leaf (f v))
    (node l r)  (node (gc/map f l) (gc/map f r))))

(gc/map str a-tree)

;
; Nonsense examples to illustrate all the features of match
;
(defadt ::foo
  (bar a b c))

(defn foo-to-int
  [a-foo]
  (match a-foo
    (bar x x x)  x
    (bar 0 x y)  (+ x y)
    (bar 1 2 3)  -1
    (bar a b 1)  (* a b)
    :else        42))

(foo-to-int (bar 0 0 0))
(foo-to-int (bar 0 5 6))
(foo-to-int (bar 1 2 3))
(foo-to-int (bar 3 3 1))
(foo-to-int (bar 0 3 1))
(foo-to-int (bar 10 20 30))
