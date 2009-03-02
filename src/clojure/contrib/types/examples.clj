;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Application examples for algebraic data types
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns clojure.contrib.types.examples
  (:use [clojure.contrib.types :only (deftype match get-value get-values)]))

;
; A simple tree structure
;
(deftype tree
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

;
; Algebraic data types with multimethods: Haskell-style functors
;
(defmulti fmap (fn [f s] (type s)))

; Sequences
(defmethod fmap clojure.lang.ISeq
  [f s]
  (map f s))

; Vectors
(defmethod fmap clojure.lang.IPersistentVector
  [f v]
  (into [] (map f v)))

; Maps
(defmethod fmap clojure.lang.IPersistentMap
  [f m]
  (into {} (for [[k v] m] [k (f v)])))

; Trees
(defmethod fmap tree
  [f t]
  (match t
    empty-tree  empty-tree
    (leaf v)    (leaf (f v))
    (node l r)  (node (fmap f l) (fmap f r))))

(fmap str '(:a :b :c))
(fmap str [:a :b :c])
(fmap str {:a 1 :b 2 :c 3})
(fmap str a-tree)

;
; Nonsense examples to illustrate all the features of match
;
(deftype foo
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

;
; Value accessors are defined only for algebraic data types that have
; exactly one constructor. get-values is defined if there is at least
; one argument in the constructor; it returns a vector of values.
; get-value is defined only for exactly one argument, it returns
; the value directly.
;

(get-value (bar 1 2 3))  ; fails
(get-values (bar 1 2 3))

(deftype sum-type
  (sum x))

(get-value (sum 42))
(get-values (sum 42))
