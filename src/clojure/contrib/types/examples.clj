;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Application examples for data types
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns
  #^{:author "Konrad Hinsen"
     :skip-wiki true
     :doc "Examples for data type definitions"}
  clojure.contrib.types.examples
  (:use [clojure.contrib.types
	 :only (deftype defadt match)])
  (:require [clojure.contrib.generic.collection :as gc])
  (:require [clojure.contrib.generic.functor :as gf]))

;
; Multisets implemented as maps to integers
;

; The most basic type definition. A more elaborate version could add
; a constructor that verifies that its argument is a map with integer values.
(deftype ::multiset multiset
  "Multiset (demo implementation)")

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
(depth (leaf 42))
(depth a-tree)

; Algebraic data types with multimethods: fmap on a tree
(defmethod gf/fmap ::tree
  [f t]
  (match t
    empty-tree  empty-tree
    (leaf v)    (leaf (f v))
    (node l r)  (node (gf/fmap f l) (gf/fmap f r))))

(gf/fmap str a-tree)

;
; Nonsense examples to illustrate all the features of match
; for type constructors.
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

(foo-to-int (bar 0 0 0))    ; 0
(foo-to-int (bar 0 5 6))    ; 11
(foo-to-int (bar 1 2 3))    ; -1
(foo-to-int (bar 3 3 1))    ; 9
(foo-to-int (bar 0 3 1))    ; 4
(foo-to-int (bar 10 20 30)) ; 42

;
; Match can also be used for lists, vectors, and maps. Note that since
; algebraic data types are represented as maps, they can be matched
; either with their type constructor and positional arguments, or
; with a map template.
;

; Tree depth once again with map templates
(defn depth
  [t]
  (match t
    empty-tree  0
    {:value _}  1
    {:left-tree l :right-tree r}  (inc (max (depth l) (depth r)))))

(depth empty-tree)
(depth (leaf 42))
(depth a-tree)

; Match for lists, vectors, and maps:

(for [x ['(1 2 3)
	 [1 2 3]
	 {:x 1 :y 2 :z 3}
	 '(1 1 1)
	 [2 1 2]
	 {:x 1 :y 1 :z 2}]]
  (match x
    '(a a a)  	 'list-of-three-equal-values
    '(a b c)  	 'list
    [a a a]   	 'vector-of-three-equal-values
    [a b a]   	 'vector-of-three-with-first-and-last-equal
    [a b c]      'vector
    {:x a :y z}  'map-with-x-equal-y
    {}           'any-map))
