;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  clojure.contrib.miglayout
;;
;;  Clojure support for the MiGLayout layout manager
;;  http://www.miglayout.com/
;;
;;  Example:
;;
;;    (use '[clojure.contrib.miglayout.test :as mlt :only ()])
;;    (dotimes [i 5] (mlt/run-test i))
;;
;;  scgilardi (gmail)
;;  Created 5 October 2008

(ns 
  #^{:author "Stephen C. Gilardi",
     :doc "Clojure support for the MiGLayout layout manager
http://www.miglayout.com/

Example:

  (use '[clojure.contrib.miglayout.test :as mlt :only ()])
  (dotimes [i 5] (mlt/run-test i))

"}
  clojure.contrib.miglayout
  (:import (java.awt Container Component)
           (clojure.lang RT))
  (:use clojure.contrib.miglayout.internal))

(defn miglayout
  "Adds java.awt.Components to a java.awt.Container with constraints
  formatted for the MiGLayout layout manager.

  Arguments: container [item constraint*]*

    - container: the container for the specified components, its layout
      manager will be set to a new instance of MigLayout

    - an inline series of items and constraints--each item may be followed
      by zero or more constraints.

  Item:

    - An item is either a Component or one of the keywords :layout
     :column or :row. Constraints for a keyword item affect the entire
      layout.

  Constraint: string, keyword, vector, map, or set

    - A string specifies one or more constraints each with zero or more
      arguments.
    - A keyword specifies a single constraint without arguments
    - A vector specifies a single constraint with one or more arguments
    - A map specifies one or more constraints as keys, each mapped to a
      single argument
    - A set groups two or more constraints, each a string, keyword,
      vector, map, or set"
  [#^Container container & args]
  (let [item-constraints (apply parse-item-constraints args)
        {:keys [keywords components]} item-constraints
        {:keys [layout column row]} keywords
        class (RT/classForName "net.miginfocom.swing.MigLayout")
        layout-manager (.newInstance class)]
    (doto layout-manager
      (.setLayoutConstraints layout)
      (.setColumnConstraints column)
      (.setRowConstraints row))
    (.setLayout container layout-manager)
    (doseq [[#^Component component constraints] components]
      (.add container component constraints))
    container))
