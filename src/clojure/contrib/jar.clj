;;; jar.clj: utilities for working with Java JAR files

;; by Stuart Sierra, http://stuartsierra.com/
;; April 19, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns clojure.contrib.jar
  (:import (java.io File)
           (java.util.jar JarFile)))

(defn jar-file?
  "Returns true if file is a normal file with a .jar or .JAR extension."
  [#^File file]
  (and (.isFile file)
       (or (.endsWith (.getName file) ".jar")
           (.endsWith (.getName file) ".JAR"))))

(defn filenames-in-jar
  "Returns a sequence of Strings naming the non-directory entries in
  the JAR file."
  [#^JarFile jar-file]
  (map #(.getName %)
       (filter #(not (.isDirectory %))
               (enumeration-seq (.entries jar-file)))))
