;;; seq_utils.clj -- Sequence utilities for Clojure

;; by Stuart Sierra, http://stuartsierra.com/
;; last updated August 12, 2008

;; Copyright (c) 2008 Stuart Sierra. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns clojure.contrib.seq-utils)


;; 'flatten' written by Rich Hickey,
;; see http://groups.google.com/group/clojure/msg/385098fabfcaad9b
(defn flatten
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat sequence."
  [x]
  (let [s? #(instance? clojure.lang.Sequential %)]
    (filter (complement s?) (tree-seq s? seq x))))

(defn separate
  "Returns a vector:
   [ (filter f s), (filter (complement f) s) ]"
  [f s]
  [(filter f s) (filter (complement f) s)])

(defn includes?
  "Returns true if s contains something equal (with =) to x."
  [x s]
  (if (some (fn [y] (= y x)) s)
    true false))

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.

  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

;; group-by written by Rich Hickey;
;; see http://paste.lisp.org/display/64190
(defn group-by [f coll]
  "Returns a sorted map of the elements of coll keyed by the result of
  f on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll."
  (reduce
   (fn [ret x]
     (let [k (f x)]
       (assoc ret k (conj (get ret k []) x))))
   (sorted-map) coll))

;; partition-by written by Rich Hickey;
;; see http://paste.lisp.org/display/64190
(defn partition-by [f coll]
  "Applies f to each value in coll, splitting it each time f returns
   a new value.  Returns a lazy seq of lazy seqs."
  (when-let [s (seq coll)]
    (let [fv (f (first s))
          ends (drop-while #(= fv (f %)) (rest s))
          tw (fn this [s]
               (when-not (identical? s ends)
                 (lazy-cons (first s) (this (rest s)))))]
      (lazy-cons (tw s) (partition-by f ends)))))
