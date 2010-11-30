;   Copyright (c) Stuart Halloway & Contributors, April 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;;
;; CHANGELOG
;;
;; Functions deprecated in 1.2, have been removed.
;; Some have migrated to clojure.java.io and others to c.c.reflections.

(ns 
  ^{:author "Stuart Halloway, Stephen C. Gilardi, Shawn Hoover, Perry Trolard, Stuart Sierra",
     :doc "A set of utilties for dealing with Java stuff like files and properties.

   Design goals:

   (1) Ease-of-use. These APIs should be convenient. Performance is secondary.

   (2) Duck typing. I hate having to think about the difference between
       a string that names a file, and a File. Ditto for a ton of other 
       wrapper classes in the Java world (URL, InternetAddress). With these
       APIs you should be able to think about domain equivalence, not type
       equivalence.

   (3) No bossiness. I am not marking any of these functions as private
       the docstrings will tell you the intended usage but do what works for you. 					

   Feedback welcome!

   If something in this module violates the principle of least surprise, please 
   let me (Stu) and the Clojure community know via the mailing list.
   Contributors:

   Stuart Halloway
   Stephen C. Gilardi
   Shawn Hoover
   Perry Trolard
   Stuart Sierra
"}
  clojure.contrib.java-utils
  (:import [java.io File FileOutputStream]
	   [java.util Properties]
           [java.net URI URL])
  (:use [clojure.java.io :only [file]]))

(defn get-system-property 
  "Get a system property."
  ([stringable]
   (System/getProperty (name stringable)))
  ([stringable default]
   (System/getProperty (name stringable) default)))

(defn set-system-properties
  "Set some system properties. Nil clears a property."
  [settings]
  (doseq [[name val] settings]
    (if val
      (System/setProperty (name name) (name val))
      (System/clearProperty (name name)))))

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
   Uses name to convert both keys and values into strings."
  {:tag Properties}
  [m]
  (let [p (Properties.)]
    (doseq [[k v] m]
      (.setProperty p (name k) (name v)))
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
