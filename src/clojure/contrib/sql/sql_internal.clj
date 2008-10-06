;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  internal definitions for 'clojure.contrib.sql
;;
;;  scgilardi (gmail)
;;  Created 3 October 2008

(defn properties
  "Converts a Clojure map from keywords or symbols to values into a
  java.util.Properties object that maps the names of the keywords or
  symbols to the String representation of the values"
  [m]
  (let [p (java.util.Properties.)]
    (when m
      (loop [[key & keys] (keys m)
             [val & vals] (vals m)]
        (.setProperty p (name key) (str val))
        (when keys
          (recur keys vals))))
    p))

(defn- the-str
  "Returns the String represented by the String, Keyword, or Symbol x"
  [x]
  (if (instance? String x)
    x
    (name x)))
