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
;   Perry Trolard
;   Stuart Sierra

(ns 
  #^{:author "Stuart Halloway, Stephen C. Gilardi, Shawn Hoover, Perry Trolard, Stuart Sierra",
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
"}
  clojure.contrib.java-utils
  (:import [java.io File FileOutputStream]
	   [java.util Properties]
           [java.net URI URL]))

(defmulti relative-path-string 
  "Interpret a String or java.io.File as a relative path string. 
   Building block for clojure.contrib.java-utils/file."
  class)

(defmethod relative-path-string String [#^String s]
  (relative-path-string (File. s)))

(defmethod relative-path-string File [#^File f]
  (if (.isAbsolute f)
    (throw (IllegalArgumentException. (str f " is not a relative path")))
    (.getPath f)))

(defmulti #^File as-file 
  "Interpret a String or a java.io.File as a File. Building block
   for clojure.contrib.java-utils/file, which you should prefer
   in most cases."
  class)
(defmethod as-file String [#^String s] (File. s))
(defmethod as-file File [f] f)

(defn #^File file
  "Returns a java.io.File from string or file args."
  ([arg]                      
     (as-file arg))
  ([parent child]             
     (File. #^File (as-file parent) #^String (relative-path-string child)))
  ([parent child & more]
     (reduce file (file parent child) more)))

(defn as-str
  "Like clojure.core/str, but if an argument is a keyword or symbol,
  its name will be used instead of its literal representation.

  Example:
     (str :foo :bar)     ;;=> \":foo:bar\"
     (as-str :foo :bar)  ;;=> \"foobar\" 

  Note that this does not apply to keywords or symbols nested within
  data structures; they will be rendered as with str.

  Example:
     (str {:foo :bar})     ;;=> \"{:foo :bar}\"
     (as-str {:foo :bar})  ;;=> \"{:foo :bar}\" "
  ([] "")
  ([x] (if (instance? clojure.lang.Named x)
         (name x)
         (str x)))
  ([x & ys]
     ((fn [#^StringBuilder sb more]
        (if more
          (recur (. sb  (append (as-str (first more)))) (next more))
          (str sb)))
      (new StringBuilder #^String (as-str x)) ys)))

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
(defn #^Properties as-properties
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
    (with-open [#^FileOutputStream f (FileOutputStream. (file file-able))]
      (doto (as-properties m)
        (.store f #^String comments)))))

(defmulti
  #^{:doc "Coerces argument (URL, URI, or String) to a java.net.URL."
     :arglists '([arg])}
  as-url type)

(defmethod as-url URL [x] x)

(defmethod as-url URI [#^URI x] (.toURL x))

(defmethod as-url String [#^String x] (URL. x))

(defmethod as-url File [#^File x] (.toURL x))
