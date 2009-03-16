;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Application examples for algebraic data types
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns clojure.contrib.types.examples
  (:use [clojure.contrib.types
	 :only (deftype defadt match)]))

;
; Multisets implemented as maps to integers
;

; The most basic type definition. A more elaborate version could add
; a constructor that verifies that its argument is a map with integer values.
(deftype ::multiset multiset)

; Some set operations generalized to multisets
; Note that the multiset constructor is not called anywhere, as the
; map operations all preserve the metadata.
(defmulti my-conj (fn [& args] (type (first args))))

(defmethod my-conj :default
  [& args]
  (apply clojure.core/conj args))

(defmethod my-conj ::multiset
  ([ms x]
   (assoc ms x (inc (get ms x 0))))
  ([ms x & xs]
    (reduce my-conj (my-conj ms x) xs)))

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
(my-conj #{} :a :a :b :c)
(my-conj (multiset {}) :a :a :b :c)

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
(defmethod fmap ::tree
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
