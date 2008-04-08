;;; duck-streams.clj -- duck-typed I/O streams for Clojure

;; by Stuart Sierra <mail@stuartsierra.com>
;; April 8, 2008

;; Copyright (c) 2008 Stuart Sierra. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; This file defines "duck-typed" I/O utility functions for Clojure.
;; The 'reader' and 'writer' functions will open and return an
;; instance of java.io.BufferedReader and java.io.PrintWriter,
;; respectively, for a variety of argument types -- filenames as
;; strings, URLs, java.io.File's, etc.  These functions are not very
;; efficient, because they have to perform a number of 'instance?'
;; checks, but they are convenient when you just want to open a file
;; and don't want to deal with all the Java I/O classes.
;;
;; This file also defines two convenience functions, 'spit' (opposite
;; of 'slurp') and 'write-lines' (opposite of 'line-seq').


(clojure/in-ns 'duck-streams)
(clojure/refer 'clojure)

(import '(java.io Reader InputStream InputStreamReader FileReader
		  BufferedReader File PrintWriter OutputStream
		  OutputStreamWriter BufferedWriter Writer FileWriter)
	'(java.net URI URL MalformedURLException))

(defmacro bufr
  {:private true}
  [reader]
  `(new java.io.BufferedReader ~reader))

(defn reader
  "Attempts to coerce its argument into an open
  java.io.BufferedReader.  Argument may be an instance of Reader,
  BufferedReader, InputStream, File, URI, URL, or String.

  If argument is a String, it tries to resolve it first as a URI, then
  as a local file name.  URIs with a 'file' protocol are converted to
  local file names.

  Should be used inside with-open to ensure the Reader is properly
  closed."
  [x]
  (cond 
   (instance? BufferedReader x) x
   (instance? Reader x) (bufr x)
   (instance? InputStream x) (bufr (new InputStreamReader x))
   (instance? File x) (bufr (new FileReader #^File x))
   (instance? URL x) (if (= (. #^URL x (getProtocol)) "file")
                       (bufr (new FileReader (. #^URL x (getPath))))
                       (bufr (new InputStreamReader (. #^URL x (openStream)))))
   (instance? URI x) (reader (. #^URI x (toURL)))
   (instance? String x) (try (let [url (new URL x)]
                               (reader url))
                             (catch MalformedURLException err
                                    (bufr (new FileReader #^String x))))
   :else (throw (new Exception (str "Cannot coerce " (class x)
                                    " into a Reader.")))))

(defmacro bufw
  {:private true}
  [writer]
  `(new java.io.PrintWriter (new java.io.BufferedWriter ~writer)))

(defn writer
  "Attempts to coerce its argument into an open java.io.PrintWriter
  wrapped around a java.io.BufferedWriter.  Argument may be an
  instance of Writer, PrintWriter, BufferedWriter, OutputStream, File,
  URI, URL, or String.

  If argument is a String, it tries to resolve it first as a URI, then
  as a local file name.  URIs with a 'file' protocol are converted to
  local file names.

  Should be used inside with-open to ensure the Writer is properly
  closed."
  [x]
  (cond 
   (instance? PrintWriter x) x
   (instance? BufferedWriter x) (new PrintWriter x)
   (instance? Writer x) (bufw x)  ; includes FileWriter
   (instance? OutputStream x) (bufw (new OutputStreamWriter x))
   (instance? File x) (bufw (new FileWriter #^File x))
   (instance? URL x) (if (= (. #^URL x (getProtocol)) "file")
                       (bufw (new FileWriter (. #^URL x (getPath))))
                       (throw (new Exception (str "Cannot write to non-file URL <" x ">."))))
   (instance? URI x) (writer (. #^URI x (toURL)))
   (instance? String x) (try (let [url (new URL x)]
                               (writer url))
                             (catch MalformedURLException err
                                    (bufw (new FileWriter #^String x))))
   :else (throw (new Exception (str "Cannot coerce " (class x)
                                    " into a Writer.")))))

(defn write-lines
  "Opposite of 'line-seq'.  Writes lines (a seq) to writer (an open
  java.io.PrintWriter), separated by newlines."
  [writer lines]
  (let [line (first lines)]
    (when line
      (. writer (write (str line)))
      (. writer (println))
      (recur writer (rest lines)))))

(defn spit
  "Opposite of 'slurp'.  Writes 'contents' to the file named by
  'filename'."
  [filename contents]
  (with-open w (writer filename)
    (. w (print contents))))

