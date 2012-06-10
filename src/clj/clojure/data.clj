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

(declare diff)

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

(defn- diff-associative-key
  "Diff associative things a and b, comparing only the key k."
  [a b k]
  (let [va (get a k)
        vb (get b k)
        [a* b* ab] (diff va vb)
        in-a (contains? a k)
        in-b (contains? b k)
        same (and in-a in-b
                  (or (not (nil? ab))
                      (and (nil? va) (nil? vb))))]
    [(when (and in-a (or (not (nil? a*)) (not same))) {k a*})
     (when (and in-b (or (not (nil? b*)) (not same))) {k b*})
     (when same {k ab})
     ]))

(defn- diff-associative
  "Diff associative things a and b, comparing only keys in ks."
  [a b ks]
  (reduce
   (fn [diff1 diff2]
     (doall (map merge diff1 diff2)))
   [nil nil nil]
   (map
    (partial diff-associative-key a b)
    ks)))

(defn- diff-sequential
  [a b]
  (vec (map vectorize (diff-associative
                       (if (vector? a) a (vec a))
                       (if (vector? b) b (vec b))
                       (range (max (count a) (count b)))))))

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
        {:diff-similar (fn [a b] ((if (.. a getClass isArray) diff-sequential atom-diff) a b))}
        EqualityPartition
        {:equality-partition (fn [x] (if (.. x getClass isArray) :sequential :atom))})

(extend-protocol EqualityPartition
  nil
  (equality-partition [x] :atom)
  
  java.util.Set
  (equality-partition [x] :set)

  java.util.List
  (equality-partition [x] :sequential)
  
  java.util.Map
  (equality-partition [x] :map))

(defn- as-set-value
  [s]
  (if (set? s) s (into #{} s)))

(extend-protocol Diff
  java.util.Set
  (diff-similar
   [a b]
   (let [aval (as-set-value a)
         bval (as-set-value b)]
     [(not-empty (set/difference aval bval))
      (not-empty (set/difference bval aval))
      (not-empty (set/intersection aval bval))]))
  
  java.util.List
  (diff-similar [a b]
    (diff-sequential a b))
  
  java.util.Map
  (diff-similar [a b]
    (diff-associative a b (set/union (keys a) (keys b)))))

(defn diff
  "Recursively compares a and b, returning a tuple of
  [things-only-in-a things-only-in-b things-in-both].
  Comparison rules:

  * For equal a and b, return [nil nil a].
  * Maps are subdiffed where keys match and values differ.
  * Sets are never subdiffed.
  * All sequential things are treated as associative collections
    by their indexes, with results returned as vectors.
  * Everything else (including strings!) is treated as
    an atom and compared for equality."
  {:added "1.3"}
  [a b]
  (if (= a b)
    [nil nil a]
    (if (= (equality-partition a) (equality-partition b))
      (diff-similar a b)
      (atom-diff a b))))
  
