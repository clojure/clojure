;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  clojure.contrib.miglayout.internal
;;
;;  Internal functions for 'clojure.contrib.miglayout
;;
;;  scgilardi (gmail)
;;  Created 13 October 2008

(ns clojure.contrib.miglayout.internal
  (:import (java.awt Component))
  (:use (clojure.contrib except fcase)))

(defn format-constraint
  "Returns a vector of vectors representing one or more constraints
  separated by commas. Constraints may be specified in Clojure using
  strings, keywords, vectors, and/or maps."
  [c]
  [[", "]
   (fcase #(%1 %2) c
     string?  [c]
     keyword? [c]
     vector?  (interpose " " c)
     map?     (apply concat (interpose [", "] (map #(interpose " " %) c)))
     (throwf IllegalArgumentException
             "unrecognized constraint: %s (%s)" c (class c)))])

(defn the-str
  "Returns the string for x--its name if it's a keyword."
  [x]
  ((if (keyword? x) name str) x))

(defn format-constraints
  "Returns a string representing all the constraints for one keyword-item
  or component formatted for miglayout."
  [& constraints]
  (apply str
         (map the-str
              (rest (reduce concat []
                            (mapcat format-constraint constraints))))))

(defn component?
  "Returns true if x is a java.awt.Component"
  [x]
  (instance? Component x))

(defn constraint?
  "Returns true if x is not a keyword-item or component"
  [x]
  (not
   (or (component? x)
       (#{:layout :column :row} x))))

(defn parse-item-constraints
  "Iterates over args and builds a map containing :keywords, a map of from
  keyword-item to constraints string and :components, a vector of vectors
  each associating a component with its constraints string. :components is
  a vector because ordering of components matters."
  [& args]
  (loop [[item & args] args
         item-constraints {:components [] :keyword-items {}}]
    (if item
      (let [[constraints args] (split-with constraint? args)]
        (recur args
          (update-in
           item-constraints
           [(if (component? item) :components :keyword-items)]
           conj [item (apply format-constraints constraints)])))
      item-constraints)))
