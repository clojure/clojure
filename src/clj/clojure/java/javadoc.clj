;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns 
  ^{:author "Christophe Grand, Stuart Sierra",
     :doc "A repl helper to quickly open javadocs."}
  clojure.java.javadoc
  (:use [clojure.java.browse :only (browse-url)])
  (:require [clojure.string :as str])
  (:import
   (java.io File)))

(def ^{:dynamic true :deprecated "1.13"} *feeling-lucky-url* nil)
(def ^{:dynamic true :deprecated "1.13"} *feeling-lucky* false)

(def ^:dynamic *local-javadocs* (ref (list)))
 
(def ^:dynamic *core-java-api*
  (let [java-version (System/getProperty "java.specification.version")]
    (str "https://docs.oracle.com/en/java/javase/" java-version "/docs/api/%s/")))

(def ^:dynamic *remote-javadocs*
 (ref (sorted-map
       "com.google.common." "https://guava.dev/releases/snapshot-jre/api/docs/"
       "java." *core-java-api*
       "javax." *core-java-api*
       "org.ietf.jgss." *core-java-api*
       "org.omg." *core-java-api*
       "org.w3c.dom." *core-java-api*
       "org.xml.sax." *core-java-api*
       "org.apache.commons.codec." "https://commons.apache.org/proper/commons-codec/apidocs/"
       "org.apache.commons.io." "https://commons.apache.org/proper/commons-io/apidocs/"
       "org.apache.commons.lang." "https://commons.apache.org/proper/commons-lang/javadocs/api-2.6/"
       "org.apache.commons.lang3." "https://commons.apache.org/proper/commons-lang/apidocs/")))

(defn add-local-javadoc
  "Adds to the list of local Javadoc paths."
  {:added "1.2"}
  [path]
  (dosync (commute *local-javadocs* conj path)))

(defn add-remote-javadoc
  "Adds to the list of remote Javadoc URLs.  package-prefix is the
  beginning of the package name that has docs at this URL."
  {:added "1.2"}
  [package-prefix url]
  (dosync (commute *remote-javadocs* assoc package-prefix url)))

(defn- fill-in-module-name [^String url ^String classname]
  ;; The getModule method was introduced in JDK 9, and did not exist
  ;; in earlier JDK versions.  Avoid calling it unless its result is
  ;; needed.
  (if (.contains url "%s")
    (let [klass (Class/forName classname)
          module-name (.getName (.getModule klass))]
      (format url module-name))
    url))

(defn javadoc-url
  "Searches for a URL for the given class name.  Tries
  *local-javadocs* first, then *remote-javadocs*.  Returns a string."
  {:tag String,
   :added "1.2"}
  [^String classname]
  (let [classname (str/replace classname #"\$.*" "")
        file-path (.replace classname \. File/separatorChar)
        url-path (.replace classname \. \/)]
    (if-let [file ^File (first
                           (filter #(.exists ^File %)
                             (map #(File. (str %) (str file-path ".html"))
                               @*local-javadocs*)))]
      (-> file .toURI str)
      ;; If no local file, try remote URLs:
      (some (fn [[prefix url]]
                  (when (.startsWith classname prefix)
                    (str (fill-in-module-name url classname)
                         url-path ".html")))
            @*remote-javadocs*))))

(defn javadoc
  "Opens a browser window displaying the javadoc for the argument.
  Tries *local-javadocs* first, then *remote-javadocs*."
  {:added "1.2"}
  [class-or-object]
  (let [^Class c (if (instance? Class class-or-object) 
                    class-or-object 
                    (class class-or-object))]
    (if-let [url (javadoc-url (.getName c))]
      (browse-url url)
      (println "Could not find Javadoc for" c))))
