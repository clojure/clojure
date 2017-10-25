;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;;; walk.clj - generic tree walker with replacement

;; by Stuart Sierra
;; December 15, 2008

;; CHANGE LOG:
;;
;; * December 15, 2008: replaced 'walk' with 'prewalk' & 'postwalk'
;;
;; * December 9, 2008: first version


(ns 
  ^{:author "Stuart Sierra",
     :doc "This file defines a generic tree walker for Clojure data
structures.  It takes any data structure (list, vector, map, set,
seq), calls a function on every element, and uses the return value
of the function in place of the original.  This makes it fairly
easy to write recursive search-and-replace functions, as shown in
the examples.

Note: \"walk\" supports all Clojure data structures EXCEPT maps
created with sorted-map-by.  There is no (obvious) way to retrieve
the sorting function."}
  clojure.walk)

(defn walk
  "Traverses form, an arbitrary data structure.  inner and outer are
  functions.  Applies inner to each element of form, building up a
  data structure of the same type, then applies outer to the result.
  Recognizes all Clojure data structures. Consumes seqs as with doall."

  {:added "1.1"}
  [inner outer form]
  (cond
   (list? form) (outer (apply list (map inner form)))
   (instance? clojure.lang.IMapEntry form) (outer (vec (map inner form)))
   (seq? form) (outer (doall (map inner form)))
   (instance? clojure.lang.IRecord form)
     (outer (reduce (fn [r x] (conj r (inner x))) form form))
   (coll? form) (outer (into (empty form) (map inner form)))
   :else (outer form)))

(defn postwalk
  "Performs a depth-first, post-order traversal of form.  Calls f on
  each sub-form, uses f's return value in place of the original.
  Recognizes all Clojure data structures. Consumes seqs as with doall."
  {:added "1.1"}
  [f form]
  (walk (partial postwalk f) f form))

(defn prewalk
  "Like postwalk, but does pre-order traversal."
  {:added "1.1"}
  [f form]
  (walk (partial prewalk f) identity (f form)))


;; Note: I wanted to write:
;;
;; (defn walk
;;   [f form]
;;   (let [pf (partial walk f)]
;;     (if (coll? form)
;;       (f (into (empty form) (map pf form)))
;;       (f form))))
;;
;; but this throws a ClassCastException when applied to a map.


(defn postwalk-demo
  "Demonstrates the behavior of postwalk by printing each form as it is
  walked.  Returns form."
  {:added "1.1"}
  [form]
  (postwalk (fn [x] (print "Walked: ") (prn x) x) form))

(defn prewalk-demo
  "Demonstrates the behavior of prewalk by printing each form as it is
  walked.  Returns form."
  {:added "1.1"}
  [form]
  (prewalk (fn [x] (print "Walked: ") (prn x) x) form))

(defn keywordize-keys
  "Recursively transforms all map keys from strings to keywords."
  {:added "1.1"}
  [m]
  (let [f (fn [[k v]] (if (string? k) [(keyword k) v] [k v]))]
    ;; only apply to maps
    (postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))

(defn stringify-keys
  "Recursively transforms all map keys from keywords to strings."
  {:added "1.1"}
  [m]
  (let [f (fn [[k v]] (if (keyword? k) [(name k) v] [k v]))]
    ;; only apply to maps
    (postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))

(defn prewalk-replace
  "Recursively transforms form by replacing keys in smap with their
  values.  Like clojure/replace but works on any data structure.  Does
  replacement at the root of the tree first."
  {:added "1.1"}
  [smap form]
  (prewalk (fn [x] (if (contains? smap x) (smap x) x)) form))

(defn postwalk-replace
  "Recursively transforms form by replacing keys in smap with their
  values.  Like clojure/replace but works on any data structure.  Does
  replacement at the leaves of the tree first."
  {:added "1.1"}
  [smap form]
  (postwalk (fn [x] (if (contains? smap x) (smap x) x)) form))

(defn macroexpand-all
  "Recursively performs all possible macroexpansions in form."
  {:added "1.1"}
  [form]
  (prewalk (fn [x] (if (seq? x) (macroexpand x) x)) form))

