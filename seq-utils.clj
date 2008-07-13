;;; seq-utils.clj -- Sequence utilities for Clojure

;; by Stuart Sierra <mail@stuartsierra.com>
;; April 8, 2008

;; Copyright (c) 2008 Stuart Sierra. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(clojure/in-ns 'seq-utils)
(clojure/refer 'clojure)


;; 'flatten' written by Rich Hickey,
;; see http://groups.google.com/group/clojure/msg/385098fabfcaad9b
(defn flatten
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat sequence."
  [x]
  (let [s? #(instance? clojure.lang.Sequential %)]
    (filter (complement s?) (tree-seq s? seq x))))

(defn batch
  "Returns a sequence of sequences, each containing 'size' elements
  from s.  DEPRECATED in favor of clojure/partition, added to boot.clj
  in r865."
  [size s]
  (when s
    (lazy-cons (take size s) (batch size (drop size s)))))

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
