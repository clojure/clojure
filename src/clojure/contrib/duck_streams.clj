;;; duck_streams.clj -- duck-typed I/O streams for Clojure

;; by Stuart Sierra, http://stuartsierra.com/
;; December 19, 2008

;; Copyright (c) Stuart Sierra, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; This file defines "duck-typed" I/O utility functions for Clojure.
;; The 'reader' and 'writer' functions will open and return an
;; instance of java.io.BufferedReader and java.io.PrintWriter,
;; respectively, for a variety of argument types -- filenames as
;; strings, URLs, java.io.File's, etc.  'reader' even works on http
;; URLs.
;;
;; Note: this is not really "duck typing" as implemented in languages
;; like Ruby.  A better name would have been "do-what-I-mean-streams"
;; or "just-give-me-a-stream", but ducks are funnier.


;; CHANGE LOG
;;
;; December 19, 2008: rewrote reader and writer as multimethods; added
;; slurp*, file, and read-lines
;;
;; April 8, 2008: first version



(ns clojure.contrib.duck-streams
    (:import 
     (java.io Reader InputStream InputStreamReader FileReader
              BufferedReader File PrintWriter OutputStream
              OutputStreamWriter BufferedWriter Writer FileWriter)
     (java.net URI URL MalformedURLException)))



(defmacro #^{:private true} bufr [reader]
  `(new java.io.BufferedReader ~reader))

(defmulti #^{:tag BufferedReader
             :doc "Attempts to coerce its argument into an open
  java.io.BufferedReader.  Argument may be an instance of Reader,
  BufferedReader, InputStream, File, URI, URL, or String.

  If argument is a String, it tries to resolve it first as a URI, then
  as a local file name.  URIs with a 'file' protocol are converted to
  local file names.

  Should be used inside with-open to ensure the Reader is properly
  closed."}
  reader class)

(defmethod reader BufferedReader [x] x)

(defmethod reader BufferedReader [x] (bufr x))

(defmethod reader InputStream [x] (bufr (InputStreamReader. x)))

(defmethod reader File [#^File x] (bufr (FileReader. x)))

(defmethod reader URL [#^URL x]
  (if (= "file" (.getProtocol x))
    (bufr (FileReader. (.getPath x)))
    (bufr (InputStreamReader. (.openStream x)))))

(defmethod reader URI [#^URI x] (reader (.toURL x)))

(defmethod reader String [#^String x]
  (try (let [url (URL. x)]
         (reader url))
       (catch MalformedURLException e
         (bufr (FileReader. #^File (file x))))))

(defmethod reader :default [x]
  (throw (Exception. (str "Cannot open <" (pr-str x) "> as a reader."))))



(defmacro #^{:private true} bufw [writer]
  `(new java.io.PrintWriter (new java.io.BufferedWriter ~writer)))

(defmulti #^{:tag PrintWriter
             :doc "Attempts to coerce its argument into an open java.io.PrintWriter
  wrapped around a java.io.BufferedWriter.  Argument may be an
  instance of Writer, PrintWriter, BufferedWriter, OutputStream, File,
  URI, URL, or String.

  If argument is a String, it tries to resolve it first as a URI, then
  as a local file name.  URIs with a 'file' protocol are converted to
  local file names.

  Should be used inside with-open to ensure the Writer is properly
  closed."}
  writer class)

(defmethod writer PrintWriter [x] x)

(defmethod writer BufferedWriter [#^BufferedWriter x] (PrintWriter. x))

(defmethod writer Writer [x] (bufw x)) ; includes FileWriter

(defmethod writer File [#^File x] (bufw (FileWriter. x)))

(defmethod writer URL [#^URL x]
  (if (= "file" (.getProtocol x))
    (bufw (FileWriter. (.getPath x)))
    (throw (Exception. (str "Cannot write to non-file URL <" x ">")))))

(defmethod writer URI [#^URI x] (writer (.toURL x)))

(defmethod writer String [#^String x]
  (try (let [url (URL. x)]
         (writer url))
       (catch MalformedURLException err
         (bufw (FileWriter. #^File (file x))))))

(defmethod writer :default [x]
  (throw (Exception. (str "Cannot open <" (pr-str x) "> as a writer."))))



(defn write-lines
  "Writes lines (a seq) to f, separated by newlines.  f is opened with
  writer."
  [f lines]
  (with-open [#^PrintWriter writer (writer f)]
    (loop [lines lines]
      (when-let [line (first lines)]
        (.write writer (str line))
        (.println writer)
        (recur (rest lines))))))

(defn read-lines
  "Like clojure.core/line-seq but opens f with reader.  Automatically
  closes the reader AFTER YOU CONSUME THE ENTIRE SEQUENCE."
  [f]
  (let [read-line (fn this [#^BufferedReader rdr]
                    (if-let [line (.readLine rdr)]
                        (lazy-cons line (this rdr))
                      (.close rdr)))]
    (read-line (reader f))))

(defn slurp*
  "Like clojure.core/slurp but opens f with reader."
  [f]
  (with-open [#^BufferedReader r (reader f)]
      (let [sb (StringBuilder.)]
        (loop [c (.read r)]
          (if (neg? c)
            (str sb)
            (do (.append sb (char c))
                (recur (.read r))))))))

(defn spit
  "Opposite of slurp.  Opens f with writer, writes content, then
  closes f."
  [f content]
  (with-open [#^PrintWriter w (writer f)]
      (.print w content)))

(defn #^File file
  "Concatenates args as strings returns a java.io.File.  Replaces all
  / and \\ with File/separatorChar.  Replaces ~ at the start of the
  path with the user.home system property."
  [& args]
  (let [#^String s (apply str args)
        s (.replace s \/ File/separatorChar)
        s (.replace s \\ File/separatorChar)
        s (if (.startsWith s "~")
            (str (System/getProperty "user.home")
                 File/separatorChar (subs s 1))
            s)]
    (File. s)))
