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
  (:use [clojure.java.browse :only (browse-url)] )
  (:import
   (java.io File)))

(def ^:dynamic *feeling-lucky-url* "http://www.google.com/search?btnI=I%27m%20Feeling%20Lucky&q=allinurl:")
(def ^:dynamic *feeling-lucky* true)

(def ^:dynamic *local-javadocs* (ref (list)))
 
(def ^:dynamic *core-java-api*
  (case (System/getProperty "java.specification.version")
    "1.8" "http://docs.oracle.com/javase/8/docs/api/"
    "9" "http://docs.oracle.com/javase/9/docs/api/"
    "10" "http://docs.oracle.com/javase/10/docs/api/"
    "11" "https://docs.oracle.com/en/java/javase/11/docs/api/java.base/"
    "http://docs.oracle.com/javase/8/docs/api/"))

(def ^:dynamic *remote-javadocs*
 (ref (sorted-map
       "com.google.common." "http://google.github.io/guava/releases/23.0/api/docs/"
       "java." *core-java-api*
       "javax." *core-java-api*
       "org.ietf.jgss." *core-java-api*
       "org.omg." *core-java-api*
       "org.w3c.dom." *core-java-api*
       "org.xml.sax." *core-java-api*
       "org.apache.commons.codec." "http://commons.apache.org/proper/commons-codec/apidocs/"
       "org.apache.commons.io." "http://commons.apache.org/proper/commons-io/javadocs/api-release/"
       "org.apache.commons.lang." "http://commons.apache.org/proper/commons-lang/javadocs/api-2.6/"
       "org.apache.commons.lang3." "http://commons.apache.org/proper/commons-lang/javadocs/api-release/")))

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

(defn- javadoc-url
  "Searches for a URL for the given class name.  Tries
  *local-javadocs* first, then *remote-javadocs*.  Returns a string."
  {:tag String,
   :added "1.2"}
  [^String classname]
  (let [file-path (.replace classname \. File/separatorChar)
        url-path (.replace classname \. \/)]
    (if-let [file ^File (first
                           (filter #(.exists ^File %)
                             (map #(File. (str %) (str file-path ".html"))
                               @*local-javadocs*)))]
      (-> file .toURI str)
      ;; If no local file, try remote URLs:
      (or (some (fn [[prefix url]]
                  (when (.startsWith classname prefix)
                    (str url url-path ".html")))
            @*remote-javadocs*)
        ;; if *feeling-lucky* try a web search
        (when *feeling-lucky* (str *feeling-lucky-url* url-path ".html"))))))

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
