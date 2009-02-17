;   Copyright (c) Chris Houser, Jan 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; :dir and :env options added by Stuart Halloway

; Conveniently launch a sub-process providing to its stdin and
; collecting its stdout

(ns clojure.contrib.shell-out
  (:import (java.io InputStreamReader OutputStreamWriter)))

(def *sh-dir* nil)
(def *sh-env* nil)

(defmacro with-sh-dir [dir & forms]
  "Sets the directory for use with sh, see sh for details."
  `(binding [*sh-dir* ~dir]
     ~@forms))

(defmacro with-sh-env [env & forms]
  "Sets the environment for use with sh, see sh for details."
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
  "Takes a seq of 'sh' arguments and returns a map of option keywords
  to option values."
  [args]
  (loop [[arg :as args] args opts {:cmd [] :out "UTF-8" :dir *sh-dir* :env *sh-env*}]
    (if-not args
      opts
      (if (keyword? arg)
        (recur (nnext args) (assoc opts arg (second args)))
        (recur (next args) (update-in opts [:cmd] conj arg))))))

(defn- as-env-key [arg]
  "Helper so that callers can use symbols, keywords, or strings
   when building an environment map."
  (cond
   (symbol? arg) (name arg)
   (keyword? arg) (name arg)
   (string? arg) arg))

(defn- as-file [arg]
  "Helper so that callers can pass a String for the :dir to sh."   
  (cond
   (string? arg) (java.io.File. arg)
   (nil? arg) nil
   (instance? java.io.File arg) arg))
   
(defn- as-env-string [arg]
  "Helper so that callers can pass a Clojure map for the :env to sh." 
  (cond
   (nil? arg) nil
   (map? arg) (into-array String (map (fn [[k v]] (str (as-env-key k) "=" v)) arg))
   true arg))

(defn sh
  "Passes the given strings to Runtime.exec() to launch a sub-process.

  Options are

  :in    may be given followed by a String specifying text to be fed to the 
         sub-process's stdin.  
  :out   option may be given followed by :bytes or a String. If a String 
         is given, it will be used as a character encoding name (for 
         example \"UTF-8\" or \"ISO-8859-1\") to convert the 
         sub-process's stdout to a String which is returned.
         If :bytes is given, the sub-process's stdout will be stored in 
         a byte array and returned. 
  :return-map
         when followed by boolean true returns a map of
           :exit => sub-process's exit code
           :out  => sub-process's stdout (as byte[] or String)
           :err  => sub-process's stderr (as byte[] or String)
  :env   override the process env with a map (or the underlying Java
         String[] if you are masochist).
  :dir   override the process dir with a String or java.io.File.

  You can bind :env or :dir for multiple operations using with-sh-env
  and with-sh-dir."
  [& args]
  (let [opts (parse-args args)
        proc (.exec (Runtime/getRuntime) 
		    (into-array (:cmd opts)) 
		    (as-env-string (:env opts))
		    (as-file (:dir opts)))
        in-stream (.getInputStream proc)]
    (when (:in opts)
      (with-open [osw (OutputStreamWriter. (.getOutputStream proc))]
        (.write osw (:in opts))))
    (let [stdout (.getInputStream proc)
          stderr (.getErrorStream proc)
          [[out err] combine-fn]
            (if (= (:out opts) :bytes)
              [(for [strm [stdout stderr]]
                (into-array Byte/TYPE (map byte (stream-seq strm))))
               #(aconcat Byte/TYPE %1 %2)]
              [(for [strm [stdout stderr]]
                (apply str (map char (stream-seq 
                                       (InputStreamReader. strm (:out opts))))))
                 str])
           exit-code  (.waitFor proc)]
      (if (:return-map opts)
        {:exit exit-code :out out :err err}
        (combine-fn out err)))))

(comment

(println (sh "ls" "-l"))
(println (sh "ls" "-l" "/no-such-thing"))
(println (sh "sed" "s/[aeiou]/oo/g" :in "hello there\n"))
(println (sh "cat" :in "x\u25bax\n"))
(println (sh "echo" "x\u25bax"))
(println (sh "echo" "x\u25bax" :out "ISO-8859-1")) ; reads 4 single-byte chars
(println (sh "cat" "myimage.png" :out :bytes)) ; reads binary file into bytes[]

)
