;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns 
  ^{:author "Chris Houser, Stuart Halloway",
    :doc "Conveniently launch a sub-process providing its stdin and
collecting its stdout"}
  clojure.java.shell
  (:use [clojure.java.io :only (as-file copy)])
  (:import (java.io OutputStreamWriter ByteArrayOutputStream StringWriter)
           (java.nio.charset Charset)))

(def *sh-dir* nil)
(def *sh-env* nil)

(defmacro with-sh-dir
  "Sets the directory for use with sh, see sh for details."
  {:added "1.2"}
  [dir & forms]
  `(binding [*sh-dir* ~dir]
     ~@forms))

(defmacro with-sh-env
  "Sets the environment for use with sh, see sh for details."
  {:added "1.2"}
  [env & forms]
  `(binding [*sh-env* ~env]
     ~@forms))
     
(defn- stream-seq
  "Takes an InputStream and returns a lazy seq of integers from the stream."
  [stream]
  (take-while #(>= % 0) (repeatedly #(.read stream))))

(defn- aconcat
  "Concatenates arrays of given type."
  [type & xs]
  (let [target (make-array type (apply + (map count xs)))]
    (loop [i 0 idx 0]
      (when-let [a (nth xs i nil)]
        (System/arraycopy a 0 target idx (count a))
        (recur (inc i) (+ idx (count a)))))
    target))

(defn- parse-args
  [args]
  (let [default-encoding (.name (Charset/defaultCharset))
        default-opts {:outenc default-encoding :inenc default-encoding :dir *sh-dir* :env *sh-env*}
        [cmd opts] (split-with string? args)]
    [cmd (merge default-opts (apply hash-map opts))]))

(defn- as-env-string 
  "Helper so that callers can pass a Clojure map for the :env to sh."
  [arg]
  (cond
   (nil? arg) nil
   (map? arg) (into-array String (map (fn [[k v]] (str (name k) "=" v)) arg))
   true arg))

(defn- stream-to-bytes
  [in]
  (with-open [bout (ByteArrayOutputStream.)]
    (copy in bout)
    (.toByteArray bout)))

(defn- stream-to-string
  ([in] (stream-to-string in (.name (Charset/defaultCharset))))
  ([in enc]
     (with-open [bout (StringWriter.)]
       (copy in bout :encoding enc)
       (.toString bout))))

(defn- stream-to-enc
  [stream enc]
  (if (= enc :bytes)
    (stream-to-bytes stream)
    (stream-to-string stream enc)))

(defn sh
  "Passes the given strings to Runtime.exec() to launch a sub-process.

  Options are

  :in      may be given followed by a String or byte array specifying input
           to be fed to the sub-process's stdin.
  :inenc   option may be given followed by a String, used as a character
           encoding name (for example \"UTF-8\" or \"ISO-8859-1\") to
           convert the input string specified by the :in option to the
           sub-process's stdin.  Defaults to the platform default encoding.
           If the :in option provides a byte array, then the bytes are passed
           unencoded, and this option is ignored.
  :outenc  option may be given followed by :bytes or a String. If a
           String is given, it will be used as a character encoding
           name (for example \"UTF-8\" or \"ISO-8859-1\") to convert
           the sub-process's stdout to a String which is returned.
           If :bytes is given, the sub-process's stdout will be stored
           in a byte array and returned.  Defaults to the platform default
           encoding.
  :env     override the process env with a map (or the underlying Java
           String[] if you are a masochist).
  :dir     override the process dir with a String or java.io.File.

  You can bind :env or :dir for multiple operations using with-sh-env
  and with-sh-dir.

  sh returns a map of
    :exit => sub-process's exit code
    :out  => sub-process's stdout (as byte[] or String)
    :err  => sub-process's stderr (as byte[] or String)"
  {:added "1.2"}
  [& args]
  (let [[cmd opts] (parse-args args)
        proc (.exec (Runtime/getRuntime) 
		    (into-array cmd) 
		    (as-env-string (:env opts))
		    (as-file (:dir opts)))
        in (:in opts)]
    (if in
      (future
       (if (instance? (class (byte-array 0)) in)
         (with-open [os (.getOutputStream proc)]
           (.write os in))
         (with-open [osw (OutputStreamWriter. (.getOutputStream proc) (:inenc opts))]
           (.write osw in))))
      (.close (.getOutputStream proc)))
    (with-open [stdout (.getInputStream proc)
                stderr (.getErrorStream proc)]
      (let [out (stream-to-enc stdout (:outenc opts))
            err (stream-to-string stderr)
            exit-code (.waitFor proc)]
        {:exit exit-code :out out :err err}))))

(comment

(println (sh "ls" "-l"))
(println (sh "ls" "-l" "/no-such-thing"))
(println (sh "sed" "s/[aeiou]/oo/g" :in "hello there\n"))
(println (sh "cat" :in "x\u25bax\n"))
(println (sh "echo" "x\u25bax"))
(println (sh "echo" "x\u25bax" :outenc "ISO-8859-1")) ; reads 4 single-byte chars
(println (sh "cat" "myimage.png" :outenc :bytes)) ; reads binary file into bytes[]
(println (sh "cmd" "/c dir 1>&2"))

)
