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
;; clojure.contrib.java.  reader/writer no longer use this
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
    clojure.contrib.io
    (:import 
     (java.io Reader InputStream InputStreamReader PushbackReader
              BufferedReader File PrintWriter OutputStream
              OutputStreamWriter BufferedWriter Writer
              FileInputStream FileOutputStream ByteArrayOutputStream
              StringReader ByteArrayInputStream
              BufferedInputStream BufferedOutputStream)
     (java.net URI URL MalformedURLException Socket)))


(def
 #^{:doc "Name of the default encoding to use when reading & writing.
  Default is UTF-8."
    :tag "java.lang.String"}
 *default-encoding* "UTF-8")

(def
 #^{:doc "Size, in bytes or characters, of the buffer used when
  copying streams."}
 *buffer-size* 1024)

(def
 #^{:doc "Type object for a Java primitive byte array."}
 *byte-array-type* (class (make-array Byte/TYPE 0)))


(defn #^File file-str
  "Concatenates args as strings and returns a java.io.File.  Replaces
  all / and \\ with File/separatorChar.  Replaces ~ at the start of
  the path with the user.home system property."
  [& args]
  (let [#^String s (apply str args)
        s (.replace s \\ File/separatorChar)
        s (.replace s \/ File/separatorChar)
        s (if (.startsWith s "~")
            (str (System/getProperty "user.home")
                 File/separator (subs s 1))
            s)]
    (File. s)))


(defmulti #^{:tag BufferedInputStream
             :doc "Attempts to coerce its argument into an open
  java.io.BufferedInputStream.  Argument may be an instance of
  BufferedInputStream, InputStream, File, URI, URL, Socket, or String.

  If argument is a String, it tries to resolve it first as a URI, then
  as a local file name.  URIs with a 'file' protocol are converted to
  local file names. If this fails, a final attempt is made to resolve
  the string as a resource on the CLASSPATH.

  Should be used inside with-open to ensure the InputStream is properly
  closed."
             :arglists '([x])}
  input-stream class)

(defmethod input-stream BufferedInputStream [x]
  x)

(defmethod input-stream InputStream [x]
  (BufferedInputStream. x))

(defmethod input-stream File [#^File x]
  (input-stream (FileInputStream. x)))

(defmethod input-stream URL [#^URL x]
  (input-stream (if (= "file" (.getProtocol x))
                  (FileInputStream. (.getPath x))
                  (.openStream x))))

(defmethod input-stream URI [#^URI x]
  (input-stream (.toURL x)))

(defmethod input-stream String [#^String x]
  (try (let [url (URL. x)]
         (input-stream url))
       (catch MalformedURLException e
         (input-stream (File. x)))))

(defmethod input-stream Socket [#^Socket x]
  (input-stream (.getInputStream x)))

(defmethod input-stream :default [x]
  (throw (Exception. (str "Cannot open " (pr-str x) " as an InputStream."))))


(defmulti #^{:tag BufferedReader
             :doc "Attempts to coerce its argument into an open
  java.io.BufferedReader.  Argument may be an instance of Reader,
  BufferedReader, InputStream, File, URI, URL, Socket, or String.

  If argument is a String, it tries to resolve it first as a URI, then
  as a local file name.  URIs with a 'file' protocol are converted to
  local file names.  If this fails, a final attempt is made to resolve
  the string as a resource on the CLASSPATH.

  Uses *default-encoding* as the text encoding.

  Should be used inside with-open to ensure the Reader is properly
  closed."
             :arglists '([x])}
  reader class)

(defmethod reader BufferedReader [x]
  x)

(defmethod reader Reader [x]
  (BufferedReader. x))

(defmethod reader InputStream [#^InputStream x]
  (reader (InputStreamReader. x *default-encoding*)))

(defmethod reader :default [x]
  ; input-stream throws if it can't hanlde x.
  (reader (input-stream x)))

(def
 #^{:doc "If true, writer, output-stream and spit will open files in append mode.
 Defaults to false.  Instead of binding this var directly, use append-writer,
 append-output-stream or append-spit."
    :tag "java.lang.Boolean"}
 *append* false)

(defn- assert-not-appending []
  (when *append*
    (throw (Exception. "Cannot change an open stream to append mode."))))

(defmulti #^{:tag OutputStream
             :doc "Attempts to coerce its argument into an open
  java.io.OutputStream or java.io.BufferedOutputStream. Argument may
  be an instance of OutputStream, File, URI, URL, Socket, or String.

  If argument is a String, it tries to resolve it first as a URI, then
  as a local file name.  URIs with a 'file' protocol are converted to
  local file names.

  Should be used inside with-open to ensure the OutputStream is
  properly closed."
             :arglists '([x])}
  output-stream class)

(defmethod output-stream BufferedOutputStream [#^BufferedOutputStream x]
  (assert-not-appending)
  x)

(defmethod output-stream OutputStream [#^OutputStream x]
  (assert-not-appending)
  (BufferedOutputStream. x))

(defmethod output-stream File [#^File x]
  (let [stream (FileOutputStream. x *append*)]
    (binding [*append* false]
      (output-stream stream))))

(defmethod output-stream URL [#^URL x]
  (if (= "file" (.getProtocol x))
    (output-stream (File. (.getPath x)))
    (throw (Exception. (str "Can not write to non-file URL <" x ">")))))

(defmethod output-stream URI [#^URI x]
  (output-stream (.toURL x)))

(defmethod output-stream String [#^String x]
  (try (let [url (URL. x)]
         (output-stream url))
       (catch MalformedURLException err
         (output-stream (File. x)))))

(defmethod output-stream Socket [#^Socket x]
  (output-stream (.getOutputStream x)))

(defmethod output-stream :default [x]
  (throw (Exception. (str "Cannot open <" (pr-str x) "> as an output stream."))))

(defn append-output-stream
  "Like output-stream but opens file for appending.  Does not work on streams
  that are already open."
  [x]
  (binding [*append* true]
    (output-stream x)))


(defmulti #^{:tag PrintWriter
             :doc "Attempts to coerce its argument into an open java.io.PrintWriter
  wrapped around a java.io.BufferedWriter.  Argument may be an
  instance of Writer, PrintWriter, BufferedWriter, OutputStream, File,
  URI, URL, Socket, or String.

  If argument is a String, it tries to resolve it first as a URI, then
  as a local file name.  URIs with a 'file' protocol are converted to
  local file names.

  Should be used inside with-open to ensure the Writer is properly
  closed."
             :arglists '([x])}
  writer class)

(defmethod writer PrintWriter [x]
  (assert-not-appending)
  x)

(defmethod writer BufferedWriter [#^BufferedWriter x]
  (assert-not-appending)
  (PrintWriter. x))

(defmethod writer Writer [x]
  (assert-not-appending)
  ;; Writer includes sub-classes such as FileWriter
  (writer (BufferedWriter. x)))

(defmethod writer OutputStream [#^OutputStream x]
  (assert-not-appending)
  (writer (OutputStreamWriter. x *default-encoding*)))

(defmethod writer File [#^File x]
  (let [stream (FileOutputStream. x *append*)]
    (binding [*append* false]
      (writer stream))))

(defmethod writer String [#^String x]
  (try (let [url (URL. x)]
         (writer url))
       (catch MalformedURLException err
         (writer (File. x)))))

(defmethod writer :default [x]
  (writer (output-stream x)))

(defn append-writer
  "Like writer but opens file for appending.  Does not work on streams
  that are already open."
  [x]
  (binding [*append* true]
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

(defn #^String slurp*
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
  Input may be an InputStream, Reader, File, byte[], or String.
  Output may be an OutputStream, Writer, or File.

  Does not close any streams except those it opens itself 
  (on a File).

  Writing a File fails if the parent directory does not exist."
     :arglists '([input output])}
  copy
  (fn [input output] [(type input) (type output)]))

(defmethod copy [InputStream OutputStream] [#^InputStream input #^OutputStream output]
  (let [buffer (make-array Byte/TYPE *buffer-size*)]
    (loop []
      (let [size (.read input buffer)]
        (when (pos? size)
          (do (.write output buffer 0 size)
              (recur)))))))

(defmethod copy [InputStream Writer] [#^InputStream input #^Writer output]
  (let [#^"[B" buffer (make-array Byte/TYPE *buffer-size*)]
    (loop []
      (let [size (.read input buffer)]
        (when (pos? size)
          (let [chars (.toCharArray (String. buffer 0 size *default-encoding*))]
            (do (.write output chars)
                (recur))))))))

(defmethod copy [InputStream File] [#^InputStream input #^File output]
  (with-open [out (FileOutputStream. output)]
    (copy input out)))

(defmethod copy [Reader OutputStream] [#^Reader input #^OutputStream output]
  (let [#^"[C" buffer (make-array Character/TYPE *buffer-size*)]
    (loop []
      (let [size (.read input buffer)]
        (when (pos? size)
          (let [bytes (.getBytes (String. buffer 0 size) *default-encoding*)]
            (do (.write output bytes)
                (recur))))))))

(defmethod copy [Reader Writer] [#^Reader input #^Writer output]
  (let [#^"[C" buffer (make-array Character/TYPE *buffer-size*)]
    (loop []
      (let [size (.read input buffer)]
        (when (pos? size)
          (do (.write output buffer 0 size)
              (recur)))))))

(defmethod copy [Reader File] [#^Reader input #^File output]
  (with-open [out (FileOutputStream. output)]
    (copy input out)))

(defmethod copy [File OutputStream] [#^File input #^OutputStream output]
  (with-open [in (FileInputStream. input)]
    (copy in output)))

(defmethod copy [File Writer] [#^File input #^Writer output]
  (with-open [in (FileInputStream. input)]
    (copy in output)))

(defmethod copy [File File] [#^File input #^File output]
  (with-open [in (FileInputStream. input)
              out (FileOutputStream. output)]
    (copy in out)))

(defmethod copy [String OutputStream] [#^String input #^OutputStream output]
  (copy (StringReader. input) output))

(defmethod copy [String Writer] [#^String input #^Writer output]
  (copy (StringReader. input) output))

(defmethod copy [String File] [#^String input #^File output]
  (copy (StringReader. input) output))

(defmethod copy [*byte-array-type* OutputStream] [#^"[B" input #^OutputStream output]
  (copy (ByteArrayInputStream. input) output))

(defmethod copy [*byte-array-type* Writer] [#^"[B" input #^Writer output]
  (copy (ByteArrayInputStream. input) output))

(defmethod copy [*byte-array-type* File] [#^"[B" input #^Writer output]
  (copy (ByteArrayInputStream. input) output))


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

(defmethod to-byte-array String [#^String x]
  (.getBytes x *default-encoding*))

(defmethod to-byte-array File [#^File x]
  (with-open [input (FileInputStream. x)
              buffer (ByteArrayOutputStream.)]
    (copy input buffer)
    (.toByteArray buffer)))

(defmethod to-byte-array InputStream [#^InputStream x]
  (let [buffer (ByteArrayOutputStream.)]
    (copy x buffer)
    (.toByteArray buffer)))

(defmethod to-byte-array Reader [#^Reader x]
  (.getBytes (slurp* x) *default-encoding*))

(defmulti relative-path-string 
  "Interpret a String or java.io.File as a relative path string. 
   Building block for clojure.contrib.java/file."
  class)

(defmethod relative-path-string String [#^String s]
  (relative-path-string (File. s)))

(defmethod relative-path-string File [#^File f]
  (if (.isAbsolute f)
    (throw (IllegalArgumentException. (str f " is not a relative path")))
    (.getPath f)))

(defmulti #^File as-file 
  "Interpret a String or a java.io.File as a File. Building block
   for clojure.contrib.java/file, which you should prefer
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

(defn delete-file
  "Delete file f. Raise an exception if it fails unless silently is true."
  [f & [silently]]
  (or (.delete (file f))
      silently
      (throw (java.io.IOException. (str "Couldn't delete " f)))))

(defn delete-file-recursively
  "Delete file f. If it's a directory, recursively delete all its contents.
Raise an exception if any deletion fails unless silently is true."
  [f & [silently]]
  (let [f (file f)]
    (if (.isDirectory f)
      (doseq [child (.listFiles f)]
        (delete-file-recursively child silently)))
    (delete-file f silently)))

(defmulti
  #^{:doc "Coerces argument (URL, URI, or String) to a java.net.URL."
     :arglists '([arg])}
  as-url type)

(defmethod as-url URL [x] x)

(defmethod as-url URI [#^URI x] (.toURL x))

(defmethod as-url String [#^String x] (URL. x))

(defmethod as-url File [#^File x] (.toURL x))
