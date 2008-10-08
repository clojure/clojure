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
  (:import (net.miginfocom.swing MigLayout)))

(defn miglayout
  "Adds java.awt.Components to a java.awt.Container with constraints
  formatted for the MiGLayout layout manager.

  Arguments:
    - container is the container for the subsequent components
    - an optional map mapping any or all of the keys :layout, :column
      or :row to a string that specifies the corresponding constraints for
      the whole layout.
    - a series of compononents, each followed by zero or more component
      constraints.

  Component constraints may be supplied as strings, keywords, vectors, or
  maps. The set of constraints for a single component is presented to
  MiGLayout as a single string with each constraint and its arguments
  separated from any subsequent constraint by a commas. Keywords appear
  without their leading colons.

  Component constraints:
    - A string specifies one or more constraints each with zero or more
      arguments. If it specifies more than one constraint, the string must
      include commas to separate them.
    - A keyword specifies a single constraint without arguments
    - A vector specifies a single constraint with zero or more arguments
    - A map specifiess one or more constraints as keys, each mapped to a
      single argument."
  [container & args]
  (let [[f & r :as a] args
        [constraints args] (if (map? f) [f r] [nil a])
        the-str #((if (keyword? %) name str) %)]
    (.setLayout container
      (MigLayout.
       (str (:layout constraints))
       (str (:column constraints))
       (str (:row constraints))))
    (loop [component (first args)
           constraints nil
           [arg & args] (rest args)]
      (cond (string? arg)
            (recur component
                   (str constraints ", " arg)
                   args)
            (keyword? arg)
            (recur component
                   (str constraints ", " (name arg))
                   args)
            (vector? arg)
            (recur component
                   (apply str constraints ", "
                     (map the-str
                       (interpose " " arg)))
                   args)
            (map? arg)
            (recur component
                   (apply str constraints ", "
                     (map the-str
                       (apply concat
                         (interpose [", "]
                           (map #(interpose " " %) arg)))))
                   args)
            (or (instance? java.awt.Component arg) (nil? arg))
            (do
              (if constraints
                (.add container component (subs constraints 2))
                (.add container component))
              (if arg
                (recur arg nil args)
                container))
            :else
            (throw (IllegalArgumentException.
                    (format "unrecognized argument: %s (%s)" arg (class arg))))))))
