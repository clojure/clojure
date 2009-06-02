;; PersistentFnMap.clj: implementation for clojure.contrib.fnmap

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; Thanks to Meikel Brandmeyer for his work on lazymap, which made
;; this implementation easier.


(ns clojure.contrib.fnmap.PersistentFnMap
  (:gen-class :extends clojure.lang.APersistentMap
              :state state
              :init init
              :constructors {[clojure.lang.IPersistentMap] [],
                             [clojure.lang.IPersistentMap clojure.lang.IPersistentMap] [clojure.lang.IPersistentMap]}))

(defn -init
  ([theMap] [[] theMap])
  ([theMap metadata] [[metadata] theMap]))

(defn create [getter setter]
  (clojure.contrib.fnmap.PersistentFnMap.
   {::getter getter ::setter setter}))

;; IPersistentMap

(defn -assoc [this key value]
  (clojure.contrib.fnmap.PersistentFnMap.
   ((::setter (. this state)) (. this state) key value)))

;; Associative

(defn- -containsKey [this key]
  (not (nil? ((::getter (. this state)) this key))))

(defn- -entryAt [this key]
  (clojure.lang.MapEntry. key ((::getter (. this state)) (. this state) key)))


(defn -valAt [this key]
  ((::getter (. this state)) (. this state) key))

;; Iterable

(defn -iterator [this]
  (.. this state iterator))

;; IPersistentCollection

(defn -count [this]
  (count (. this state)))

(defn -seq [this]
  (seq (. this state)))

(defn -cons [this that]
  (.. this state (cons this that)))

(defn -empty [this]
  (.. this state empty))

