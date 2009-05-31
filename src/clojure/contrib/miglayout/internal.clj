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
  (:import (clojure.lang RT Reflector)
           java.awt.Component
           javax.swing.JComponent)
  (:use (clojure.contrib
         [core :only (new-by-name)]
         [except :only (throwf)]
         [fcase :only (fcase)]
         [java-utils :only (as-str)])))

(def MigLayout "net.miginfocom.swing.MigLayout")
(def LayoutCallback "net.miginfocom.layout.LayoutCallback")
(def ConstraintParser "net.miginfocom.layout.ConstraintParser")

(declare format-constraints)

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
;;  (prn formatted)
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

(defn parse-component-constraint
  "Parses a component constraint string returning a CC object"
  [constraint]
  (Reflector/invokeStaticMethod
   ConstraintParser "parseComponentConstraint" (into-array [constraint])))

(defn add-components
  "Adds components with constraints to a container"
  [#^JComponent container components]
  (loop [[[#^Component component constraint] & components] components
         id-map nil]
    (if component
      (let [cc (parse-component-constraint constraint)]
        (.add container component cc)
        (recur
         components
         (if-let [id (.getId cc)]
           (assoc id-map (keyword id) component)
           id-map)))
      (doto container (.putClientProperty ::components id-map)))))

(defn get-components
  "Returns a map from id to component for all components with an id"
  [#^JComponent container]
  (.getClientProperty container ::components))

(defn do-layout
  "Attaches a MigLayout layout manager to container and adds components
  with constraints"
  [#^JComponent container layout column row components]
  (doto container
    (.setLayout (new-by-name MigLayout layout column row))
    (add-components components)))
