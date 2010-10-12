;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns 
  ^{:author "Stuart Halloway",
    :doc "Non-core data functions."}
  clojure.data
  (:require [clojure.set :as set]))

(defn- atom-diff
  "Internal helper for diff."
  [a b]
  (if (= a b) [nil nil a] [a b nil]))

;; for big things a sparse vector class would be better
(defn- vectorize
  "Convert an associative-by-numeric-index collection into
   an equivalent vector, with nil for any missing keys"
  [m]
  (when (seq m)
    (reduce
     (fn [result [k v]] (assoc result k v))
     (vec (repeat (apply max (keys m))  nil))
     m)))

(declare diff)

(defprotocol ^{:added "1.3"} EqualityPartition
  "Implementation detail. Subject to change."
  (^{:added "1.3"} equality-partition [x] "Implementation detail. Subject to change."))

(defprotocol ^{:added "1.3"} Diff
  "Implementation detail. Subject to change."
  (^{:added "1.3"} diff-similar [a b] "Implementation detail. Subject to change."))

(extend nil
        Diff
        {:diff-similar atom-diff})

(extend Object
        Diff
        {:diff-similar atom-diff}
        EqualityPartition
        {:equality-partition (fn [x] (if (.. x getClass isArray) :sequential :atom))})

(defn- diff-associative
  "Diff associative things a and b, comparing only keys in ks."
  [a b ks]
  (reduce
   (fn [diff1 diff2]
     (map merge diff1 diff2))
   [nil nil nil]
   (map
    (fn [k] (map #(when % {k %}) (diff (get a k) (get b k))))
    ks)))

(extend-protocol EqualityPartition
  nil
  (equality-partition [x] :atom)
  
  java.util.Set
  (equality-partition [x] :set)

  java.util.List
  (equality-partition [x] :sequential)
  
  java.util.Map
  (equality-partition [x] :map))

(extend-protocol Diff
  java.util.Set
  (diff-similar [a b]
    [(not-empty (set/difference a b))
     (not-empty (set/difference b a))
     (not-empty (set/intersection a b))])
  
  java.util.List
  (diff-similar [a b]
    (vec (map vectorize (diff-associative
                         (if (vector? a) a (vec a))
                         (if (vector? b) b (vec b))
                         (range (max (count a) (count b)))))))
  
  java.util.Map
  (diff-similar [a b]
    (diff-associative a b (set/union (keys a) (keys b)))))

(defn diff
  "Recursively compares a and b, returning a tuple of
  [things-only-in-a things-only-in-b things-in-both].
  Comparison rules:

  * Maps are subdiffed where keys match and values differ.
  * Sets are never subdiffed.
  * All sequential things are treated as associative collections
    by their indexes, with results returned as vectors.
  * Everything else (including strings!) is treated as
    an atom and compared for equality."
  {:added "1.3"}
  [a b]
  (if (= (equality-partition a) (equality-partition b))
    (diff-similar a b)
    (atom-diff a b)))
  
