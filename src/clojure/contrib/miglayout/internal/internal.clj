;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  clojure.contrib.miglayout.internal
;;
;;  Internal functions for 'clojure.contrib.miglayout
;;
;;  scgilardi (gmail)
;;  Created 13 October 2008

(ns clojure.contrib.miglayout.internal
  (:import (java.awt Component)))

(defn format-constraints
  "Returns a string representing all the constraints for one keyword-item
  or component formatted for miglayout. In Clojure, the constraints may be
  specified using strings, keywords, vectors, and/or maps."
  [& constraints]
  (loop [[c & cs] constraints
         v []]
    (if c
      (recur cs (concat v [", "]
        (cond (or (string? c) (keyword? c))
              [c]
              (vector? c)
              (interpose " " c)
              (map? c)
              (apply concat (interpose [", "] (map #(interpose " " %) c)))
              :else
              (throw
               (IllegalArgumentException.
                (format "unrecognized constraint: %s (%s)" c (class c)))))))
      (apply str (map #((if (keyword? %) name str) %) (rest v))))))

(defn keyword-item?
  "Returns true if x is a keyword-item"
  [x]
  (#{:layout :column :row} x))

(defn component?
  "Returns true if x is a java.awt.Component"
  [x]
  (instance? Component x))

(defn constraint?
  "Returns true if x is not a keyword-item or component"
  [x]
  (not
   (or (keyword-item? x)
       (component? x))))

(defn parse-item-constraints
  "Iterates over args and builds a map containing :keywords, a map of from
  keyword-item to constraints string and :components, a vector of vectors
  each associating a component with its constraints string. :components is
  a vector because ordering of components matters."
  [& args]
  (loop [[item & args] args
         item-constraints {:keyword-items {} :components []}]
    (if item
      (let [[constraints args] (split-with constraint? args)]
        (recur args
          (update-in
           item-constraints
           [(if (component? item) :components :keyword-items)]
           #(conj % [item (apply format-constraints constraints)]))))
      item-constraints)))
