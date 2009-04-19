;;; find_namespaces.clj: search for ns declarations in dirs, JARs, or CLASSPATH

;; by Stuart Sierra, http://stuartsierra.com/
;; April 19, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns clojure.contrib.find-namespaces
  (:require [clojure.contrib.classpath :as cp]
            [clojure.contrib.jar :as jar])
  (import (java.io File FileReader BufferedReader PushbackReader
                   InputStreamReader)
          (java.util.jar JarFile)))


;;; Finding namespaces in a directory tree

(defn clojure-source-file?
  "Returns true if file is a normal file with a .clj extension."
  [#^File file]
  (and (.isFile file)
       (.endsWith (.getName file) ".clj")))

(defn find-clojure-sources-in-dir
  "Searches recursively under dir for Clojure source files (.clj).
  Returns a sequence of File objects, in breadth-first sort order."
  [#^File dir]
  ;; Use sort by absolute path to get breadth-first search.
  (sort-by #(.getAbsolutePath %)
           (filter clojure-source-file? (file-seq dir))))

(defn read-ns-decl
  "Attempts to read a (ns ...) declaration from rdr, and returns the
  unevaluated form.  Returns nil if read fails, or if the first form
  is not a ns declaration."
  [#^PushbackReader rdr]
  (try (let [form (read rdr)]
         (when (and (list? form) (= 'ns (first form)))
           form))
       (catch Exception e nil)))

(defn read-file-ns-decl
  "Attempts to read a (ns ...) declaration from file, and returns the
  unevaluated form.  Returns nil if read fails, or if the first form
  is not a ns declaration."
  [#^File file]
  (with-open [rdr (PushbackReader. (BufferedReader. (FileReader. file)))]
    (read-ns-decl rdr)))

(defn find-ns-decls-in-dir
  "Searches dir recursively for (ns ...) declarations in Clojure
  source files; returns the unevaluated ns declarations."
  [#^File dir]
  (filter identity (map read-file-ns-decl (find-clojure-sources-in-dir dir))))

(defn find-namespaces-in-dir
  "Searches dir recursively for (ns ...) declarations in Clojure
  source files; returns the symbol names of the declared namespaces."
  [#^File dir]
  (map second (find-ns-decls-in-dir dir)))


;;; Finding namespaces in JAR files

(defn clojure-sources-in-jar
  "Returns a sequence of filenames ending in .clj found in the JAR file."
  [#^JarFile jar-file]
  (filter #(.endsWith % ".clj") (jar/filenames-in-jar jar-file)))

(defn read-ns-decl-from-jarfile-entry
  "Attempts to read a (ns ...) declaration from the named entry in the
  JAR file, and returns the unevaluated form.  Returns nil if the read
  fails, or if the first form is not a ns declaration."
  [#^JarFile jarfile #^String entry-name]
  (with-open [rdr (PushbackReader.
                   (BufferedReader.
                    (InputStreamReader.
                     (.getInputStream jarfile (.getEntry jarfile entry-name)))))]
    (read-ns-decl rdr)))

(defn find-ns-decls-in-jarfile
  "Searches the JAR file for Clojure source files containing (ns ...)
  declarations; returns the unevaluated ns declarations."
  [#^JarFile jarfile]
  (filter identity
          (map #(read-ns-decl-from-jarfile-entry jarfile %)
               (clojure-sources-in-jar jarfile))))

(defn find-namespaces-in-jarfile
  "Searches the JAR file for Clojure source files containing (ns ...)
  declarations.  Returns a sequence of the symbol names of the
  declared namespaces."
  [#^JarFile jarfile]
  (map second (find-ns-decls-in-jarfile jarfile)))


;;; Finding namespaces anywhere on CLASSPATH

(defn find-ns-decls-on-classpath
  "Searches CLASSPATH (both directories and JAR files) for Clojure
  source files containing (ns ...) declarations.  Returns a sequence
  of the unevaluated ns declaration forms."
  []
  (concat
   (mapcat find-ns-decls-in-dir (cp/classpath-directories))
   (mapcat find-ns-decls-in-jarfile (cp/classpath-jarfiles))))

(defn find-namespaces-on-classpath
  "Searches CLASSPATH (both directories and JAR files) for Clojure
  source files containing (ns ...) declarations.  Returns a sequence
  of the symbol names of the declared namespaces."
  []
  (map second (find-ns-decls-on-classpath)))
