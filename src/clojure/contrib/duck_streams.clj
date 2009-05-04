;;; duck_streams.clj -- duck-typed I/O streams for Clojure

;; by Stuart Sierra, http://stuartsierra.com/
;; May 3, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
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
;; May 3, 2009: renamed file to file-str, for compatibility with
;; clojure.contrib.java-utils.  reader/writer no longer use this
;; function.
;;
;; February 16, 2009: (lazy branch) fixed read-lines to work with lazy
;; Clojure.
;;
;; January 10, 2009: added *default-encoding*, so streams are always
;; opened as UTF-8.
;;
;; December 19, 2008: rewrote reader and writer as multimethods; added
;; slurp*, file, and read-lines
;;
;; April 8, 2008: first version



(ns 
  #^{:author "Stuart Sierra",
     :doc "This file defines \"duck-typed\" I/O utility functions for Clojure.
           The 'reader' and 'writer' functions will open and return an
           instance of java.io.BufferedReader and java.io.PrintWriter,
           respectively, for a variety of argument types -- filenames as
           strings, URLs, java.io.File's, etc.  'reader' even works on http
           URLs.

           Note: this is not really \"duck typing\" as implemented in languages
           like Ruby.  A better name would have been \"do-what-I-mean-streams\"
           or \"just-give-me-a-stream\", but ducks are funnier."} 
    clojure.contrib.duck-streams
    (:import 
     (java.io Reader InputStream InputStreamReader PushbackReader
              BufferedReader File PrintWriter OutputStream
              OutputStreamWriter BufferedWriter Writer
              FileInputStream FileOutputStream)
     (java.net URI URL MalformedURLException)))


(def *default-encoding* "UTF-8")

(defn #^File file-str
  "Concatenates args as strings and returns a java.io.File.  Replaces
  all / and \\ with File/separatorChar.  Replaces ~ at the start of
  the path with the user.home system property."
  [& args]
  (let [#^String s (apply str args)
        s (.replaceAll (re-matcher #"[/\\]" s) File/separator)
        s (if (.startsWith s "~")
            (str (System/getProperty "user.home")
                 File/separator (subs s 1))
            s)]
    (File. s)))


(defmulti #^{:tag BufferedReader
             :doc "Attempts to coerce its argument into an open
  java.io.BufferedReader.  Argument may be an instance of Reader,
  BufferedReader, InputStream, File, URI, URL, or String.

  If argument is a String, it tries to resolve it first as a URI, then
  as a local file name.  URIs with a 'file' protocol are converted to
  local file names.  Uses *default-encoding* as the text encoding.

  Should be used inside with-open to ensure the Reader is properly
  closed."}
  reader class)

(defmethod reader Reader [x]
  (BufferedReader. x))

(defmethod reader InputStream [x]
  (BufferedReader. (InputStreamReader. x *default-encoding*)))

(defmethod reader File [#^File x]
  (reader (FileInputStream. x)))

(defmethod reader URL [#^URL x]
  (reader (if (= "file" (.getProtocol x))
            (FileInputStream. (.getPath x))
            (.openStream x))))

(defmethod reader URI [#^URI x]
  (reader (.toURL x)))

(defmethod reader String [#^String x]
  (try (let [url (URL. x)]
         (reader url))
       (catch MalformedURLException e
         (reader (File. x)))))

(defmethod reader :default [x]
  (throw (Exception. (str "Cannot open " (pr-str x) " as a reader."))))



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

(defmethod writer BufferedWriter [#^BufferedWriter x]
  (PrintWriter. x))

(defmethod writer Writer [x]
  ;; Writer includes sub-classes such as FileWriter
  (PrintWriter. (BufferedWriter. x)))   

(defmethod writer OutputStream [x]
  (PrintWriter.
   (BufferedWriter.
    (OutputStreamWriter. x *default-encoding*))))

(defmethod writer File [#^File x]
  (writer (FileOutputStream. x)))

(defmethod writer URL [#^URL x]
  (if (= "file" (.getProtocol x))
    (writer (File. (.getPath x)))
    (throw (Exception. (str "Cannot write to non-file URL <" x ">")))))

(defmethod writer URI [#^URI x]
  (writer (.toURL x)))

(defmethod writer String [#^String x]
  (try (let [url (URL. x)]
         (writer url))
       (catch MalformedURLException err
         (writer (File. x)))))

(defmethod writer :default [x]
  (throw (Exception. (str "Cannot open <" (pr-str x) "> as a writer."))))


(defn write-lines
  "Writes lines (a seq) to f, separated by newlines.  f is opened with
  writer, and automatically closed at the end of the sequence."
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
                    (lazy-seq
                     (if-let [line (.readLine rdr)]
                       (cons line (this rdr))
                       (.close rdr))))]
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

(defn pwd
  "Returns current working directory as a String.  (Like UNIX 'pwd'.)
  Note: In Java, you cannot change the current working directory."
  []
  (System/getProperty "user.dir"))



(defmacro with-out-writer
  "Opens a writer on f, binds it to *out*, and evalutes body."
  [f & body]
  `(with-open [stream# (writer ~f)]
     (binding [*out* stream#]
       ~@body)))

(defmacro with-in-reader
  "Opens a PushbackReader on f, binds it to *in*, and evaluates body."
  [f & body]
  `(with-open [stream# (PushbackReader. (reader ~f))]
     (binding [*in* stream#]
       ~@body)))
