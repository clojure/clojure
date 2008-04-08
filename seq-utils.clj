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
  "Divides 'coll' into sequences each containing 'size' elements,
  returns a sequence of those sequences.  The last sequence may
  have fewer than 'size' elements."
  [size coll]
  (when coll
    (lazy-cons (take size coll) (batch size (drop size coll)))))

(defn separate
  "Separates elements in 'coll' into two sequences, one for which 
  (f item) is logical true, the other for which (f item) is logical
  false.  Returns a vector containing two (lazy) sequences:
     [ (true-items) (false-items) ]"
  [f coll]
  [(filter f coll) (filter (complement f) coll)])

(defn includes?
  "Returns true if 'coll' (a sequential collection) contains 'value',
  false otherwise.  Uses '=' to test equality.  May scan the entire
  collection."
  [value coll]
  (if (some (fn [x] (= x value)) coll)
    true false))
