;   Copyright (c) Stuart Halloway & Contributors, April 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;   Design goals:
;
;   (1) Ease-of-use. These APIs should be convenient. Performance is secondary.
;
;   (2) Duck typing. I hate having to think about the difference between
;       a string that names a file, and a File. Ditto for a ton of other 
;       wrapper classes in the Java world (URL, InternetAddress). With these
;       APIs you should be able to think about domain equivalence, not type
;       equivalence.
;
;   (3) No bossiness. I am not marking any of these functions as private;
;       the docstrings will tell you the intended usage but do what works for you. 					
;
;   Feedback welcome!
;
;   If something in this module violates the principle of least surprise, please 
;   let me (Stu) and the Clojure community know via the mailing list.
;
;   Contributors:
;
;   Stuart Halloway
;   Stephen C. Gilardi
;   Shawn Hoover

(ns clojure.contrib.java-utils
  (:import [java.io File]))

(defmulti relative-path-string 
  "Interpret a String or java.io.File as a relative path string. 
   Building block for clojure.contrib.java-utils/file."
  class)

(defmethod relative-path-string String [s]
  (relative-path-string (File. s)))

(defmethod relative-path-string File [f]
  (if (.isAbsolute f)
    (throw (IllegalArgumentException. (str f " is not a relative path")))
    (.getPath f)))

(defmulti as-file 
  "Interpret a String or a java.io.File as a File. Building block
   for clojure.contrib.java-utils/file, which you should prefer
   in most cases."
  class)
(defmethod as-file String [s] (File. s))
(defmethod as-file File [f] f)

(defn file
  "Returns a java.io.File from string or file args."
  ([arg]                      
     (as-file arg))
  ([parent child]             
     (File. (as-file parent) (relative-path-string child)))
  ([parent child & more]
     (reduce file (file parent child) more)))

(defn the-str
  "Returns the name or string representation of x"
  [x]
  (if (instance? clojure.lang.Named x)
    (name x)
    (str x)))

(defn get-system-property [stringable]
  (System/getProperty (the-str stringable)))

(defn set-system-properties
  [settings]
  "Set some system properties. Nil clears a property."
  (doseq [[name val] settings]
    (if val
      (System/setProperty (the-str name) val)
      (System/clearProperty (the-str name)))))

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



  
     