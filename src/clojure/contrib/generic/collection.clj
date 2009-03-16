;; Generic interfaces for collection-related functions

;; by Konrad Hinsen
;; last updated March 16, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.generic.collection
  "Generic collection function interface

   NOTE: This library is VERY experimental. It WILL change significantly
   with future release.

   This library defines generic versions of common collection-related functions
   such as map or conj as multimethods that can be defined for any type."
  (:refer-clojure :exclude [assoc conj dissoc empty get into map seq]))

;
; assoc
;
(defmulti assoc
  "Returns a new collection in which the values corresponding to the
   given keys are updated by the given values. Each type of collection
   can have specific restrictions on the possible keys."
   {:arglists '([coll & key-val-pairs])}
   (fn [coll & items] (type coll)))

(defmethod assoc :default
  [map & key-val-pairs]
  (apply clojure.core/assoc map key-val-pairs))

; assoc-in

;
; conj
;
(defmulti conj (fn [coll & xs] (type coll)))

(defmethod conj :default
  [coll & xs]
  (apply clojure.core/conj coll xs))

;
; diassoc
;
(defmulti dissoc
  "Returns a new collection in which the entries corresponding to the
   given keys are removed. Each type of collection can have specific
   restrictions on the possible keys."
   {:arglists '([coll & keys])}
   (fn [coll & keys] (type coll)))

(defmethod dissoc :default
  [map & keys]
  (apply clojure.core/dissoc map keys))

;
; empty
;
(defmulti empty
  "Returns an empty collection of the same kind as the argument"
   {:arglists '([coll])}
   type)

(defmethod empty :default
  [coll]
  (clojure.core/empty coll))

;
; get
;
(defmulti get
  "Returns the element of coll referred to by key. Each type of collection
   can have specific restrictions on the possible keys."
   {:arglists '([coll key] [coll key not-found])}
  (fn [coll & args] (type coll)))

(defmethod get :default
  ([coll key]
     (clojure.core/get coll key))
  ([coll key not-found]
     (clojure.core/get coll key not-found)))

;
; into
;
; This is a literal copy of into from clojure.core, but it is
; evaluated with the functions from this namespace here!
(declare seq)
(defn into
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined."
  [to from]
  (let [ret to items (seq from)]
    (if items
      (recur (conj ret (first items)) (next items))
      ret)))

;
; map
;
(defmulti map
  "Applies function f to each element of coll and returns a collection
   of the same kind as coll."
   {:arglists '([f coll])}
   (fn [f coll] (type coll)))

(defmethod map clojure.lang.ISeq
  [f coll]
  (clojure.core/map f coll))

(defmethod map clojure.lang.IPersistentVector
  [f v]
  (clojure.core/into (clojure.core/empty v) (clojure.core/map f v)))

(defmethod map clojure.lang.IPersistentMap
  [f m]
  (clojure.core/into (clojure.core/empty m) (for [[k v] m] [k (f v)])))

(defmethod map clojure.lang.IPersistentSet
  [f s]
  (clojure.core/into (clojure.core/empty s) (clojure.core/map f s)))

;
; seq
;
(defmulti seq
  "Returns a seq on the object s."
  {:arglists '([s])}
  type)

(defmethod seq :default
  [s]
  (clojure.core/seq s))
