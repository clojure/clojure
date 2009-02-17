;;; seq_utils.clj -- Sequence utilities for Clojure

;; by Stuart Sierra, http://stuartsierra.com/
;; last updated January 10, 2009

;; Copyright (c) Stuart Sierra, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; Change Log
;;
;; January 10, 2009 (Stuart Sierra):
;;
;; * BREAKING CHANGE: "includes?" now takes collection as first
;;   argument.  This is more consistent with Clojure collection
;;   functions; see discussion at http://groups.google.com/group/clojure/browse_thread/thread/8b2c8dc96b39ddd7/a8866d34b601ff43


(ns clojure.contrib.seq-utils)


;; 'flatten' written by Rich Hickey,
;; see http://groups.google.com/group/clojure/msg/385098fabfcaad9b
(defn flatten
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat sequence.
  (flatten nil) returns nil."
  [x]
  (filter (complement sequential?)
          (rest (tree-seq sequential? seq x))))

(defn separate
  "Returns a vector:
   [ (filter f s), (filter (complement f) s) ]"
  [f s]
  [(filter f s) (filter (complement f) s)])

(defn includes?
  "Returns true if coll contains something equal (with =) to x,
  in linear time."
  [coll x]
  (if (some (fn [y] (= y x)) coll)
    true false))

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.

  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

;; group-by written by Rich Hickey;
;; see http://paste.lisp.org/display/64190
(defn group-by 
  "Returns a sorted map of the elements of coll keyed by the result of
  f on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll."
  [f coll]
  (reduce
   (fn [ret x]
     (let [k (f x)]
       (assoc ret k (conj (get ret k []) x))))
   (sorted-map) coll))

;; partition-by written by Rich Hickey;
;; see http://paste.lisp.org/display/64190
(defn partition-by 
  "Applies f to each value in coll, splitting it each time f returns
   a new value.  Returns a lazy seq of lazy seqs."
  [f coll]
  (when-let [s (seq coll)]
    (let [fv (f (first s))
          run (take-while #(= fv (f %)) s)]
      (lazy-seq
       (cons run (partition-by f (drop (count run) s)))))))

(defn frequencies
  "Returns a map from distinct items in coll to the number of times
  they appear."
  [coll]
  (reduce (fn [counts x]
              (assoc counts x (inc (get counts x 0))))
          {} coll))

;; recursive sequence helpers by Christophe Grand
;; see http://clj-me.blogspot.com/2009/01/recursive-seqs.html
(defmacro rec-cat 
 "Similar to lazy-cat but binds the resulting sequence using the supplied 
  binding-form, allowing recursive expressions. The first collection 
  expression must not be recursive and must yield a non-nil seq."
 [binding-form expr & rec-exprs]
  `(let [rec-rest# (atom nil)
         result# (lazy-cat ~expr (force @rec-rest#))
         ~binding-form result#]
     (swap! rec-rest# (constantly (delay (lazy-cat ~@rec-exprs))))
     result#))
         
(defmacro rec-cons 
 "Similar to lazy-cons but binds the resulting sequence using the supplied 
  binding-form, allowing recursive expressions. The first expression must 
  not be recursive."
 [binding-form expr & rec-exprs]
  `(let [rec-rest# (atom nil)
         result# (lazy-seq (cons ~expr (force @rec-rest#)))
         ~binding-form result#]
     (swap! rec-rest# (constantly (delay (lazy-cat ~@rec-exprs))))
     result#))
     
;; reductions by Chris Houser
;; see http://groups.google.com/group/clojure/browse_thread/thread/3edf6e82617e18e0/58d9e319ad92aa5f?#58d9e319ad92aa5f
(defn reductions
  "Returns a lazy seq of the intermediate values of the reduction (as
  per reduce) of coll by f, starting with init."
  ([f coll]
   (if (seq coll)
     (rec-cons self (first coll) (map f self (rest coll)))
     (cons (f) nil)))
  ([f init coll]
   (rec-cons self init (map f self coll))))

(defn rotations
  "Returns a lazy seq of all rotations of a seq"
  [x]
  (if (seq x)
    (map
     (fn [n _]
       (lazy-cat (drop n x) (take n x)))
     (iterate inc 0) x)
    (list nil)))

(defn partition-all 
  "Lazily break s into chunks of length n (or less, for the
  final chunk)."
  [n s]
  (lazy-seq
   (when (seq s)
     (cons (take n s)
           (partition-all n (drop n s))))))

(defn shuffle
  "Return a random permutation of coll"
  [coll]
  (let [l (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle l)
    (seq l)))

(defn rand-elt [s]
  "Return a random element of this seq"
  (nth s (rand-int (count s))))
