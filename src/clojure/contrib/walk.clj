;;; walk.clj - generic tree walker with replacement

;; by Stuart Sierra, http://stuartsierra.com/
;; December 9, 2008

;; Copyright (c) 2008 Stuart Sierra. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; This file defines a generic tree walker for Clojure data
;; structures.  It takes any data structure (list, vector, map, set,
;; seq), calls a function on every element, and uses the return value
;; of the function in place of the original.  This makes it fairly
;; easy to write recursive search-and-replace functions, as shown in
;; the examples.
;;
;; Note: "walk" supports all Clojure data structures EXCEPT maps
;; created with sorted-map-by.  There is no (obvious) way to retrieve
;; the sorting function.


(ns clojure.contrib.walk)

(defn walk
  "Performs a depth-first, post-order traversal of form.  Calls f on
  each sub-form, uses f's return value in place of the original.
  Recognizes all Clojure data structures except sorted-map-by.
  Consumes seqs as with doall."
  [f form]
  (let [pf (partial walk f)]
    (cond
     (list? form) (f (apply list (map pf form)))
     (seq? form) (f (doall (map pf form)))
     (vector? form) (f (vec (map pf form)))
     (map? form) (f (into (if (sorted? form) (sorted-map) {})
                          (map pf form)))
     (set? form) (f (into (if (sorted? form) (sorted-set) #{})
                          (map pf form)))
     :else (f form))))


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


(defn pr-walk
  "Demonstrates the behavior of walk by printing each form as it is
  walked.  Returns form."
  [form]
  (walk (fn [x] (print "Walked: ") (prn x) x) form))

(defn keywordize-keys
  "Recursively transforms all map keys from strings to keywords."
  [m]
  (let [f (fn [[k v]] (if (string? k) [(keyword k) v] [k v]))]
    ;; only apply to maps
    (walk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))

(defn stringify-keys
  "Recursively transforms all map keys from keywords to strings."
  [m]
  (let [f (fn [[k v]] (if (keyword? k) [(name k) v] [k v]))]
    ;; only apply to maps
    (walk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))

(defn walk-replace
  "Recursively transforms form by replacing keys in smap with their
  values.  Like clojure/replace but works on any data structure."
  [smap form]
  (walk (fn [x] (if (contains? smap x) (smap x) x)) form))
