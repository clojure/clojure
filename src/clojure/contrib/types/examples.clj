;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Application examples for algebraic data types
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns clojure.contrib.types.examples
  (:use clojure.contrib.types))

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
  [#^tree t]
  (match t
    empty-tree  0
    (leaf n)    1
    (node l r)  (inc (max (depth l) (depth r)))))

(depth empty-tree)
(depth a-tree)

;
; Algebraic data types with multimethods: Haskell-style functors
;
(defmulti fmap (fn [f s] (class s)))

; Sequences
(defmethod fmap clojure.lang.ISeq
  [f s]
  (map f s))

; Vectors
(defmethod fmap clojure.lang.IPersistentVector
  [f v]
  (into [] (map f v)))

; Trees
(defmethod fmap tree
  [f t]
  (match t
    empty-tree  empty-tree
    (leaf v)    (leaf (f v))
    (node l r)  (node (fmap f l) (fmap f r))))

(fmap str '(:a :b :c))
(fmap str [:a :b :c])
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
