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
  (:import (java.awt Container Component)
           clojure.lang.RT)
  (:use (clojure.contrib
         [except :only (throwf)]
         [fcase :only (fcase)]
         [java-utils :only (as-str)])))

(declare format-constraints)

(defn new-instance
  "Returns a new instance of MigLayout with the specified constraints"
  [layout column row]
  (doto (.newInstance (RT/classForName "net.miginfocom.swing.MigLayout"))
    (.setLayoutConstraints layout)
    (.setColumnConstraints column)
    (.setRowConstraints row)))

(defn add-components
  "Adds components with constraints to a container"
  [#^Container container components]
  (doseq [[#^Component component constraints] components]
    (.add container component constraints))
  container)

(defn format-constraint
  "Returns a vector of vectors representing one or more constraints
  separated by commas. Constraints may be specified in Clojure using
  strings, keywords, vectors, maps, and/or sets."
  [c]
  [[", "]
   (fcase #(%1 %2) c
     string?  [c]
     keyword? [c]
     vector?  (interpose " " c)
     map?     (apply concat (interpose [", "] (map #(interpose " " %) c)))
     set?     (apply concat (interpose [", "] (map format-constraints c)))
     (throwf IllegalArgumentException
             "unrecognized constraint: %s (%s)" c (class c)))])

(defn format-constraints
  "Returns a string representing all the constraints for one keyword-item
  or component formatted for miglayout."
  [& constraints]
  (let [formatted
        (apply str
          (map as-str
            (rest (reduce concat []
              (mapcat format-constraint constraints)))))]
    ;(prn formatted)
    formatted))

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
  "Iterates over args and builds a map containing values associated with
  :keywords and :components. The value for :keywords is a map from keyword
  items to constraints strings. The value for :components is a vector of
  vectors each associating a component with its constraints string."
  [& args]
  (loop [[item & args] args
         item-constraints {:keywords {} :components []}]
    (if item
      (let [[constraints args] (split-with constraint? args)]
        (recur args
          (update-in
           item-constraints
           [(if (component? item) :components :keywords)]
           conj [item (apply format-constraints constraints)])))
      item-constraints)))
