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
(defmacro rec-seq 
 "Similar to lazy-seq but binds the resulting seq to the supplied 
  binding-name, allowing for recursive expressions."
 [binding-name & body]
  (list* '#^{:once true :super-name "clojure/lang/LazySeq"} fn* binding-name [] body))
  
(defmacro rec-cat 
 "Similar to lazy-cat but binds the resulting sequence to the supplied 
  binding-name, allowing for recursive expressions."
 [binding-name & exprs]
  `(rec-seq ~binding-name (lazy-cat ~@exprs)))
         
     
;; reductions by Chris Houser
;; see http://groups.google.com/group/clojure/browse_thread/thread/3edf6e82617e18e0/58d9e319ad92aa5f?#58d9e319ad92aa5f
(defn reductions
  "Returns a lazy seq of the intermediate values of the reduction (as
  per reduce) of coll by f, starting with init."
  ([f coll]
   (if (seq coll)
     (rec-seq self (cons (first coll) (map f self (rest coll))))
     (cons (f) nil)))
  ([f init coll]
   (rec-seq self (cons init (map f self coll)))))

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
  "Returns a lazy sequence of lists like clojure.core/partition, but may
  include lists with fewer than n items at the end."
  ([n coll]
     (partition-all n n coll))
  ([n step coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (cons (take n s) (partition-all n step (drop step s)))))))
  
(defn shuffle
  "Return a random permutation of coll"
  [coll]
  (let [l (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle l)
    (seq l)))

(defn rand-elt
  "Return a random element of this seq"
  [s]
  (nth s (rand-int (count s))))
