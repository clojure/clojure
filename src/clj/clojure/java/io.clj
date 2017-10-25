;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns 
  ^{:author "Stuart Sierra, Chas Emerick, Stuart Halloway",
     :doc "This file defines polymorphic I/O utility functions for Clojure."}
    clojure.java.io
    (:require clojure.string)
    (:import 
     (java.io Reader InputStream InputStreamReader PushbackReader
              BufferedReader File OutputStream
              OutputStreamWriter BufferedWriter Writer
              FileInputStream FileOutputStream ByteArrayOutputStream
              StringReader ByteArrayInputStream
              BufferedInputStream BufferedOutputStream
              CharArrayReader Closeable)
     (java.net URI URL MalformedURLException Socket URLDecoder URLEncoder)))

(def
    ^{:doc "Type object for a Java primitive byte array."
      :private true
      }
 byte-array-type (class (make-array Byte/TYPE 0)))

(def
    ^{:doc "Type object for a Java primitive char array."
      :private true}
 char-array-type (class (make-array Character/TYPE 0)))

(defprotocol ^{:added "1.2"} Coercions
  "Coerce between various 'resource-namish' things."
  (^{:tag java.io.File, :added "1.2"} as-file [x] "Coerce argument to a file.")
  (^{:tag java.net.URL, :added "1.2"} as-url [x] "Coerce argument to a URL."))

(defn- escaped-utf8-urlstring->str [s]
  (-> (clojure.string/replace s "+" (URLEncoder/encode "+" "UTF-8"))
      (URLDecoder/decode "UTF-8")))

(extend-protocol Coercions
  nil
  (as-file [_] nil)
  (as-url [_] nil)
  
  String
  (as-file [s] (File. s))
  (as-url [s] (URL. s))  
  
  File
  (as-file [f] f)
  (as-url [f] (.toURL (.toURI f)))

  URL
  (as-url [u] u)
  (as-file [u]
    (if (= "file" (.getProtocol u))
      (as-file (escaped-utf8-urlstring->str
                (.replace (.getFile u) \/ File/separatorChar)))
      (throw (IllegalArgumentException. (str "Not a file: " u)))))

  URI
  (as-url [u] (.toURL u))
  (as-file [u] (as-file (as-url u))))

(defprotocol ^{:added "1.2"} IOFactory
  "Factory functions that create ready-to-use, buffered versions of
   the various Java I/O stream types, on top of anything that can
   be unequivocally converted to the requested kind of stream.

   Common options include
   
     :append    true to open stream in append mode
     :encoding  string name of encoding to use, e.g. \"UTF-8\".

   Callers should generally prefer the higher level API provided by
   reader, writer, input-stream, and output-stream."
  (^{:added "1.2"} make-reader [x opts] "Creates a BufferedReader. See also IOFactory docs.")
  (^{:added "1.2"} make-writer [x opts] "Creates a BufferedWriter. See also IOFactory docs.")
  (^{:added "1.2"} make-input-stream [x opts] "Creates a BufferedInputStream. See also IOFactory docs.")
  (^{:added "1.2"} make-output-stream [x opts] "Creates a BufferedOutputStream. See also IOFactory docs."))

(defn ^Reader reader
  "Attempts to coerce its argument into an open java.io.Reader.
   Default implementations always return a java.io.BufferedReader.

   Default implementations are provided for Reader, BufferedReader,
   InputStream, File, URI, URL, Socket, byte arrays, character arrays,
   and String.

   If argument is a String, it tries to resolve it first as a URI, then
   as a local file name.  URIs with a 'file' protocol are converted to
   local file names.

   Should be used inside with-open to ensure the Reader is properly
   closed."
  {:added "1.2"}
  [x & opts]
  (make-reader x (when opts (apply hash-map opts))))

(defn ^Writer writer
  "Attempts to coerce its argument into an open java.io.Writer.
   Default implementations always return a java.io.BufferedWriter.

   Default implementations are provided for Writer, BufferedWriter,
   OutputStream, File, URI, URL, Socket, and String.

   If the argument is a String, it tries to resolve it first as a URI, then
   as a local file name.  URIs with a 'file' protocol are converted to
   local file names.

   Should be used inside with-open to ensure the Writer is properly
   closed."
  {:added "1.2"}
  [x & opts]
  (make-writer x (when opts (apply hash-map opts))))

(defn ^InputStream input-stream
  "Attempts to coerce its argument into an open java.io.InputStream.
   Default implementations always return a java.io.BufferedInputStream.

   Default implementations are defined for InputStream, File, URI, URL,
   Socket, byte array, and String arguments.

   If the argument is a String, it tries to resolve it first as a URI, then
   as a local file name.  URIs with a 'file' protocol are converted to
   local file names.

   Should be used inside with-open to ensure the InputStream is properly
   closed."
  {:added "1.2"}
  [x & opts]
  (make-input-stream x (when opts (apply hash-map opts))))

(defn ^OutputStream output-stream
  "Attempts to coerce its argument into an open java.io.OutputStream.
   Default implementations always return a java.io.BufferedOutputStream.

   Default implementations are defined for OutputStream, File, URI, URL,
   Socket, and String arguments.

   If the argument is a String, it tries to resolve it first as a URI, then
   as a local file name.  URIs with a 'file' protocol are converted to
   local file names.

   Should be used inside with-open to ensure the OutputStream is
   properly closed."
  {:added "1.2"}
  [x & opts]
  (make-output-stream x (when opts (apply hash-map opts))))

(defn- ^Boolean append? [opts]
  (boolean (:append opts)))

(defn- ^String encoding [opts]
  (or (:encoding opts) "UTF-8"))

(defn- buffer-size [opts]
  (or (:buffer-size opts) 1024))

(def default-streams-impl
  {:make-reader (fn [x opts] (make-reader (make-input-stream x opts) opts))
   :make-writer (fn [x opts] (make-writer (make-output-stream x opts) opts))
   :make-input-stream (fn [x opts]
                        (throw (IllegalArgumentException.
                                (str "Cannot open <" (pr-str x) "> as an InputStream."))))
   :make-output-stream (fn [x opts]
                         (throw (IllegalArgumentException.
                                 (str "Cannot open <" (pr-str x) "> as an OutputStream."))))})

(defn- inputstream->reader
  [^InputStream is opts]
  (make-reader (InputStreamReader. is (encoding opts)) opts))

(defn- outputstream->writer
  [^OutputStream os opts]
  (make-writer (OutputStreamWriter. os (encoding opts)) opts))

(extend BufferedInputStream
  IOFactory
  (assoc default-streams-impl
    :make-input-stream (fn [x opts] x)
    :make-reader inputstream->reader))

(extend InputStream
  IOFactory
  (assoc default-streams-impl
    :make-input-stream (fn [x opts] (BufferedInputStream. x))
    :make-reader inputstream->reader))

(extend Reader
  IOFactory
  (assoc default-streams-impl
    :make-reader (fn [x opts] (BufferedReader. x))))

(extend BufferedReader
  IOFactory
  (assoc default-streams-impl
    :make-reader (fn [x opts] x)))

(extend Writer
  IOFactory
  (assoc default-streams-impl
    :make-writer (fn [x opts] (BufferedWriter. x))))

(extend BufferedWriter
  IOFactory
  (assoc default-streams-impl
    :make-writer (fn [x opts] x)))

(extend OutputStream
  IOFactory
  (assoc default-streams-impl
    :make-output-stream (fn [x opts] (BufferedOutputStream. x))
    :make-writer outputstream->writer))

(extend BufferedOutputStream
  IOFactory
  (assoc default-streams-impl
    :make-output-stream (fn [x opts] x)
    :make-writer outputstream->writer))

(extend File
  IOFactory
  (assoc default-streams-impl
    :make-input-stream (fn [^File x opts] (make-input-stream (FileInputStream. x) opts))
    :make-output-stream (fn [^File x opts] (make-output-stream (FileOutputStream. x (append? opts)) opts))))

(extend URL
  IOFactory
  (assoc default-streams-impl
    :make-input-stream (fn [^URL x opts]
                         (make-input-stream
                          (if (= "file" (.getProtocol x))
                            (FileInputStream. (as-file x))
                            (.openStream x)) opts))
    :make-output-stream (fn [^URL x opts]
                          (if (= "file" (.getProtocol x))
                            (make-output-stream (as-file x) opts)
                            (throw (IllegalArgumentException. (str "Can not write to non-file URL <" x ">")))))))

(extend URI
  IOFactory
  (assoc default-streams-impl
    :make-input-stream (fn [^URI x opts] (make-input-stream (.toURL x) opts))
    :make-output-stream (fn [^URI x opts] (make-output-stream (.toURL x) opts))))

(extend String
  IOFactory
  (assoc default-streams-impl
    :make-input-stream (fn [^String x opts]
                         (try
                          (make-input-stream (URL. x) opts)
                          (catch MalformedURLException e
                            (make-input-stream (File. x) opts))))
    :make-output-stream (fn [^String x opts]
                          (try
                           (make-output-stream (URL. x) opts)
                           (catch MalformedURLException err
                             (make-output-stream (File. x) opts))))))

(extend Socket
  IOFactory
  (assoc default-streams-impl
    :make-input-stream (fn [^Socket x opts] (make-input-stream (.getInputStream x) opts))
    :make-output-stream (fn [^Socket x opts] (make-output-stream (.getOutputStream x) opts))))

(extend byte-array-type
  IOFactory
  (assoc default-streams-impl
    :make-input-stream (fn [x opts] (make-input-stream (ByteArrayInputStream. x) opts))))

(extend char-array-type
  IOFactory
  (assoc default-streams-impl
    :make-reader (fn [x opts] (make-reader (CharArrayReader. x) opts))))

(extend Object
  IOFactory
  default-streams-impl)

(extend nil
  IOFactory
  (assoc default-streams-impl
    :make-reader (fn [x opts]
                   (throw (IllegalArgumentException.
                           (str "Cannot open <" (pr-str x) "> as a Reader."))))
    :make-writer (fn [x opts]
                   (throw (IllegalArgumentException.
                           (str "Cannot open <" (pr-str x) "> as a Writer."))))))

(defmulti
  ^{:doc "Internal helper for copy"
     :private true
     :arglists '([input output opts])}
  do-copy
  (fn [input output opts] [(type input) (type output)]))

(defmethod do-copy [InputStream OutputStream] [^InputStream input ^OutputStream output opts]
  (let [buffer (make-array Byte/TYPE (buffer-size opts))]
    (loop []
      (let [size (.read input buffer)]
        (when (pos? size)
          (do (.write output buffer 0 size)
              (recur)))))))

(defmethod do-copy [InputStream Writer] [^InputStream input ^Writer output opts]
  (let [^"[C" buffer (make-array Character/TYPE (buffer-size opts))
        in (InputStreamReader. input (encoding opts))]
    (loop []
      (let [size (.read in buffer 0 (alength buffer))]
        (if (pos? size)
          (do (.write output buffer 0 size)
              (recur)))))))

(defmethod do-copy [InputStream File] [^InputStream input ^File output opts]
  (with-open [out (FileOutputStream. output)]
    (do-copy input out opts)))

(defmethod do-copy [Reader OutputStream] [^Reader input ^OutputStream output opts]
  (let [^"[C" buffer (make-array Character/TYPE (buffer-size opts))
        out (OutputStreamWriter. output (encoding opts))]
    (loop []
      (let [size (.read input buffer)]
        (if (pos? size)
          (do
            (.write out buffer 0 size)
            (recur))
          (.flush out))))))

(defmethod do-copy [Reader Writer] [^Reader input ^Writer output opts]
  (let [^"[C" buffer (make-array Character/TYPE (buffer-size opts))]
    (loop []
      (let [size (.read input buffer)]
        (when (pos? size)
          (do (.write output buffer 0 size)
              (recur)))))))

(defmethod do-copy [Reader File] [^Reader input ^File output opts]
  (with-open [out (FileOutputStream. output)]
    (do-copy input out opts)))

(defmethod do-copy [File OutputStream] [^File input ^OutputStream output opts]
  (with-open [in (FileInputStream. input)]
    (do-copy in output opts)))

(defmethod do-copy [File Writer] [^File input ^Writer output opts]
  (with-open [in (FileInputStream. input)]
    (do-copy in output opts)))

(defmethod do-copy [File File] [^File input ^File output opts]
  (with-open [in (-> input FileInputStream. .getChannel)
              out (-> output FileOutputStream. .getChannel)]
    (let [sz (.size in)]
      (loop [pos 0]
        (let [bytes-xferred (.transferTo in pos (- sz pos) out)
              pos (+ pos bytes-xferred)]
          (when (< pos sz)
            (recur pos)))))))

(defmethod do-copy [String OutputStream] [^String input ^OutputStream output opts]
  (do-copy (StringReader. input) output opts))

(defmethod do-copy [String Writer] [^String input ^Writer output opts]
  (do-copy (StringReader. input) output opts))

(defmethod do-copy [String File] [^String input ^File output opts]
  (do-copy (StringReader. input) output opts))

(defmethod do-copy [char-array-type OutputStream] [input ^OutputStream output opts]
  (do-copy (CharArrayReader. input) output opts))

(defmethod do-copy [char-array-type Writer] [input ^Writer output opts]
  (do-copy (CharArrayReader. input) output opts))

(defmethod do-copy [char-array-type File] [input ^File output opts]
  (do-copy (CharArrayReader. input) output opts))

(defmethod do-copy [byte-array-type OutputStream] [^"[B" input ^OutputStream output opts]
  (do-copy (ByteArrayInputStream. input) output opts))

(defmethod do-copy [byte-array-type Writer] [^"[B" input ^Writer output opts]
  (do-copy (ByteArrayInputStream. input) output opts))

(defmethod do-copy [byte-array-type File] [^"[B" input ^Writer output opts]
  (do-copy (ByteArrayInputStream. input) output opts))

(defn copy
  "Copies input to output.  Returns nil or throws IOException.
  Input may be an InputStream, Reader, File, byte[], or String.
  Output may be an OutputStream, Writer, or File.

  Options are key/value pairs and may be one of

    :buffer-size  buffer size to use, default is 1024.
    :encoding     encoding to use if converting between
                  byte and char streams.   

  Does not close any streams except those it opens itself 
  (on a File)."
  {:added "1.2"}
  [input output & opts]
  (do-copy input output (when opts (apply hash-map opts))))

(defn ^String as-relative-path
  "Take an as-file-able thing and return a string if it is
   a relative path, else IllegalArgumentException."
  {:added "1.2"}
  [x]
  (let [^File f (as-file x)]
    (if (.isAbsolute f)
      (throw (IllegalArgumentException. (str f " is not a relative path")))
      (.getPath f))))

(defn ^File file
  "Returns a java.io.File, passing each arg to as-file.  Multiple-arg
   versions treat the first argument as parent and subsequent args as
   children relative to the parent."
  {:added "1.2"}
  ([arg]                      
     (as-file arg))
  ([parent child]             
     (File. ^File (as-file parent) ^String (as-relative-path child)))
  ([parent child & more]
     (reduce file (file parent child) more)))

(defn delete-file
  "Delete file f. Raise an exception if it fails unless silently is true."
  {:added "1.2"}
  [f & [silently]]
  (or (.delete (file f))
      silently
      (throw (java.io.IOException. (str "Couldn't delete " f)))))

(defn make-parents
  "Given the same arg(s) as for file, creates all parent directories of
   the file they represent."
  {:added "1.2"}
  [f & more]
  (when-let [parent (.getParentFile ^File (apply file f more))]
    (.mkdirs parent)))

(defn ^URL resource
  "Returns the URL for a named resource. Use the context class loader
   if no loader is specified."
  {:added "1.2"}
  ([n] (resource n (.getContextClassLoader (Thread/currentThread))))
  ([n ^ClassLoader loader] (.getResource loader n)))
