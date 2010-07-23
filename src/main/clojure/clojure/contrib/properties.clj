;   Copyright (c) Stuart Halloway & Contributors, April 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.contrib.properties
  (:use [clojure.contrib.string :only (as-str)]
        [clojure.contrib.io :only (file)])
  (:import (java.util Properties)
           (java.io FileInputStream FileOutputStream)))

(defn get-system-property 
  "Get a system property."
  ([stringable]
   (System/getProperty (as-str stringable)))
  ([stringable default]
   (System/getProperty (as-str stringable) default)))

(defn set-system-properties
  "Set some system properties. Nil clears a property."
  [settings]
  (doseq [[name val] settings]
    (if val
      (System/setProperty (as-str name) (as-str val))
      (System/clearProperty (as-str name)))))

(defmacro with-system-properties
  "setting => property-name value

  Sets the system properties to the supplied values, executes the body, and
  sets the properties back to their original values. Values of nil are
  translated to a clearing of the property."
  [settings & body]
  `(let [settings# ~settings
         current# (reduce (fn [coll# k#]
			    (assoc coll# k# (get-system-property k#)))
			  {}
			  (keys settings#))]
     (set-system-properties settings#)       
     (try
      ~@body
      (finally
       (set-system-properties current#)))))


; Not there is no corresponding props->map. Just destructure!
(defn ^Properties as-properties
  "Convert any seq of pairs to a java.utils.Properties instance.
   Uses as-str to convert both keys and values into strings."
  {:tag Properties}
  [m]
  (let [p (Properties.)]
    (doseq [[k v] m]
      (.setProperty p (as-str k) (as-str v)))
    p))

(defn read-properties
  "Read properties from file-able."
  [file-able]
  (with-open [f (java.io.FileInputStream. (file file-able))]
    (doto (Properties.)
      (.load f))))

(defn write-properties
  "Write properties to file-able."
  {:tag Properties}
  ([m file-able] (write-properties m file-able nil))
  ([m file-able comments]
    (with-open [^FileOutputStream f (FileOutputStream. (file file-able))]
      (doto (as-properties m)
        (.store f ^String comments)))))
