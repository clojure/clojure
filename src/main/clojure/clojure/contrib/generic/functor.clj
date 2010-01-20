;; Generic interface for functors

;; by Konrad Hinsen
;; last updated May 3, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns
  #^{:author "Konrad Hinsen"
     :doc "Generic functor interface (fmap)"}
  clojure.contrib.generic.functor)


(defmulti fmap
  "Applies function f to each item in the data structure s and returns
   a structure of the same kind."
   {:arglists '([f s])}
   (fn [f s] (type s)))

(defmethod fmap clojure.lang.IPersistentList
  [f v]
  (into (empty v) (map f v)))

(defmethod fmap clojure.lang.IPersistentVector
  [f v]
  (into (empty v) (map f v)))

(defmethod fmap clojure.lang.IPersistentMap
  [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))

(defmethod fmap clojure.lang.IPersistentSet
  [f s]
  (into (empty s) (map f s)))
