;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  miglayout.clj
;;
;;  Clojure support for the MiGLayout layout manager
;;  http://www.miglayout.com/
;;
;;  Example:
;;
;;    (require '[clojure.contrib.miglayout.test :as mlt])
;;    (doseq i (range 3) (mlt/run-test i))
;;
;;  scgilardi (gmail)
;;  Created 5 October 2008

(ns clojure.contrib.miglayout
  (:import (java.awt Container Component)
           (net.miginfocom.swing MigLayout))
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

  Constraints:

    - The set of constraints for each item is presented to MiGLayout as a
      single string with each constraint and its arguments separated from
      any subsequent constraint by a comma.

  Constraint: string, keyword, vector, or map

    - A string specifies one or more constraints each with zero or more
      arguments. If it specifies more than one constraint, the string must
      include commas to separate them.
    - A keyword specifies a single constraint without arguments
    - A vector specifies a single constraint with one or more arguments
    - A map specifies one or more constraints as keys, each mapped to a
      single argument"
  [#^Container container & args]
  (loop [[item & args] args
         item-constraints {:layout {} :component []}]
    (if item
      (let [[constraints args] (split-with constraint? args)]
        (recur args
         (update-in
          item-constraints
          [(if (component? item) :component :layout)]
          #(conj % [item (apply format-constraints constraints)]))))
      (do
        (.setLayout
         container
         (MigLayout.
          (get-in item-constraints [:layout :layout])
          (get-in item-constraints [:layout :column])
          (get-in item-constraints [:layout :row])))
        (doseq [component constraints] (:component item-constraints)
          (.add container component constraints))
        container))))
