;;; duck_streams.clj -- duck-typed I/O streams for Clojure

;; by Stuart Sierra, http://stuartsierra.com/
;; May 13, 2009

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
;; May 13, 2009: added functions to open writers for appending
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
              FileInputStream FileOutputStream ByteArrayOutputStream)
     (java.net URI URL MalformedURLException)))


(def
 #^{:doc "Name of the default encoding to use when reading & writing.
  Default is UTF-8."}
 *default-encoding* "UTF-8")

(def
 #^{:doc "Size, in bytes or characters, of the buffer used when
  copying streams."}
 *buffer-size* 1024)

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
  closed."
             :arglists '([x])}
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


(def
 #^{:doc "If true, writer and spit will open files in append mode.
 Defaults to false.  Use append-writer or append-spit."}
 *append-to-writer* false)


(defmulti #^{:tag PrintWriter
             :doc "Attempts to coerce its argument into an open java.io.PrintWriter
  wrapped around a java.io.BufferedWriter.  Argument may be an
  instance of Writer, PrintWriter, BufferedWriter, OutputStream, File,
  URI, URL, or String.

  If argument is a String, it tries to resolve it first as a URI, then
  as a local file name.  URIs with a 'file' protocol are converted to
  local file names.

  Should be used inside with-open to ensure the Writer is properly
  closed."
             :arglists '([x])}
  writer class)

(defn- assert-not-appending []
  (when *append-to-writer*
    (throw (Exception. "Cannot change an open stream to append mode."))))

(defmethod writer PrintWriter [x]
  (assert-not-appending)
  x)

(defmethod writer BufferedWriter [#^BufferedWriter x]
  (assert-not-appending)
  (PrintWriter. x))

(defmethod writer Writer [x]
  (assert-not-appending)
  ;; Writer includes sub-classes such as FileWriter
  (PrintWriter. (BufferedWriter. x)))   

(defmethod writer OutputStream [x]
  (assert-not-appending)
  (PrintWriter.
   (BufferedWriter.
    (OutputStreamWriter. x *default-encoding*))))

(defmethod writer File [#^File x]
  (let [stream (FileOutputStream. x *append-to-writer*)]
    (binding [*append-to-writer* false]
      (writer stream))))

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


(defn append-writer
  "Like writer but opens file for appending.  Does not work on streams
  that are already open."
  [x]
  (binding [*append-to-writer* true]
    (writer x)))


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

(defn append-spit
  "Like spit but appends to file."
  [f content]
  (with-open [#^PrintWriter w (append-writer f)]
    (.print w content)))

(defn pwd
  "Returns current working directory as a String.  (Like UNIX 'pwd'.)
  Note: In Java, you cannot change the current working directory."
  []
  (System/getProperty "user.dir"))



(defmacro with-out-writer
  "Opens a writer on f, binds it to *out*, and evalutes body.
  Anything printed within body will be written to f."
  [f & body]
  `(with-open [stream# (writer ~f)]
     (binding [*out* stream#]
       ~@body)))

(defmacro with-out-append-writer
  "Like with-out-writer but appends to file."
  [f & body]
  `(with-open [stream# (append-writer ~f)]
     (binding [*out* stream#]
       ~@body)))

(defmacro with-in-reader
  "Opens a PushbackReader on f, binds it to *in*, and evaluates body."
  [f & body]
  `(with-open [stream# (PushbackReader. (reader ~f))]
     (binding [*in* stream#]
       ~@body)))

(defmulti
  #^{:doc "Copies input to output.  Returns nil.
  Input may be an InputStream, Reader, or File.
  Output may be an OutputStream, Writer, or File.

  Does not close any streams except those it opens itself 
  (on a File).

  Writing a File fails if the parent directory does not exist."
     :arglists '([input output])}
  copy
  (fn [input output] [(type input) (type output)]))

(defmethod copy [InputStream OutputStream] [input output]
  (let [buffer (make-array Byte/TYPE *buffer-size*)]
    (loop []
      (let [size (.read input buffer)]
        (when (pos? size)
          (do (.write output buffer 0 size)
              (recur)))))))

(defmethod copy [InputStream Writer] [input output]
  (let [buffer (make-array Byte/TYPE *buffer-size*)]
    (loop []
      (let [size (.read input buffer)]
        (when (pos? size)
          (let [chars (.toCharArray (String. buffer *default-encoding*))]
            (do (.write output chars)
                (recur))))))))

(defmethod copy [InputStream File] [input output]
  (with-open [out (FileOutputStream. output)]
    (copy input out)))

(defmethod copy [Reader OutputStream] [input output]
  (let [buffer (make-array Character/TYPE *buffer-size*)]
    (loop []
      (let [size (.read input buffer)]
        (when (pos? size)
          (let [bytes (.getBytes (String. buffer 0 size) *default-encoding*)]
            (do (.write output bytes)
                (recur))))))))

(defmethod copy [Reader Writer] [input output]
  (let [buffer (make-array Character/TYPE *buffer-size*)]
    (loop []
      (let [size (.read input buffer)]
        (when (pos? size)
          (do (.write output buffer 0 size)
              (recur)))))))

(defmethod copy [Reader File] [input output]
  (with-open [out (FileOutputStream. output)]
    (copy input out)))

(defmethod copy [File OutputStream] [input output]
  (with-open [in (FileInputStream. input)]
    (copy in output)))

(defmethod copy [File Writer] [input output]
  (with-open [in (FileInputStream. input)]
    (copy in output)))

(defmethod copy [File File] [input output]
  (with-open [in (FileInputStream. input)
              out (FileOutputStream. output)]
    (copy in out)))


(def
 #^{:doc "Type object for a Java primitive byte array."}
 *byte-array-type* (class (make-array Byte/TYPE 0)))

(defn make-parents
  "Creates all parent directories of file."
  [#^File file]
  (.mkdirs (.getParentFile file)))

(defmulti
  #^{:doc "Converts argument into a Java byte array.  Argument may be
  a String, File, InputStream, or Reader.  If the argument is already
  a byte array, returns it."
    :arglists '([arg])}
  to-byte-array type)

(defmethod to-byte-array *byte-array-type* [x] x)

(defmethod to-byte-array String [x]
  (.getBytes x *default-encoding*))

(defmethod to-byte-array File [x]
  (with-open [input (FileInputStream. x)
              buffer (ByteArrayOutputStream.)]
    (copy input buffer)
    (.toByteArray buffer)))

(defmethod to-byte-array InputStream [x]
  (let [buffer (ByteArrayOutputStream.)]
    (copy x buffer)
    (.toByteArray buffer)))

(defmethod to-byte-array Reader [x]
  (.getBytes (slurp* x) *default-encoding*))

