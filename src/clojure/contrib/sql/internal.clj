;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  internal definitions for clojure.contrib.sql
;;
;;  scgilardi (gmail)
;;  Created 3 October 2008

(ns clojure.contrib.sql.internal)

(def *db* {:connection nil :level 0})

(defn connection
  "Returns the current database connection or throws an exception."
  []
  (or (:connection *db*)
      (throw (Exception. "no current database connection"))))

(defn the-str
  "Returns the name or string representation of x"
  [x]
  (if (instance? clojure.lang.Named x)
    (name x)
    (str x)))

(defn properties
  "Converts a map from keywords or symbols to values into a
  java.util.Properties object that maps the same keys to the values with
  all represented as strings."
  [m]
  (let [p (java.util.Properties.)]
    (when m
      (loop [[key & keys] (keys m)
             [val & vals] (vals m)]
        (.setProperty p (the-str key) (the-str val))
        (when keys
          (recur keys vals))))
    p))
