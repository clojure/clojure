;   Copyright (c) Chris Houser, Jan 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Conveniently launch a sub-process providing to its stdin and
; collecting its stdout

(ns clojure.contrib.shell-out
  (:import (java.io InputStreamReader OutputStreamWriter)))

(defn- stream-seq
  "Takes an InputStream and returns a lazy seq of integers from the stream."
  [stream]
  (take-while #(>= % 0) (repeatedly #(.read stream))))

(defn- parse-args
  "Takes a seq of 'sh' arguments and returns a map of option keywords
  to option values."
  [args]
  (loop [[arg :as args] args opts {:cmd [] :out "UTF-8"}]
    (if-not args
      opts
      (if (keyword? arg)
        (recur (rrest args) (assoc opts arg (second args)))
        (recur (rest args) (update-in opts [:cmd] conj arg))))))

(defn sh
  "Passes the given strings to Runtime.exec() to launch a sub-process.  An
  :in option may given followed by a String specifying text to be fed
  to the sub-process's stdin.  An :out option may be given followed by
  :bytes or a String. If a String is given, it will be used as a
  character encoding name (for example \"UTF-8\" or \"ISO-8859-1\") to
  convert the sub-process's stdout to a String which is returned.  If
  :bytes is given, the sub-process's stdout will be stored in a byte
  array and returned."
  [& args]
  (let [opts (parse-args args)
        proc (.exec (Runtime/getRuntime) (into-array (:cmd opts)))
        in-stream (.getInputStream proc)]
    (when (:in opts)
      (with-open [osw (OutputStreamWriter. (.getOutputStream proc))]
        (.write osw (:in opts))))
    (let [stdout (.getInputStream proc)
          stderr (.getErrorStream proc)
          rtn (if (= (:out opts) :bytes)
                (into-array Byte/TYPE (map byte (concat (stream-seq stdout)
                                                        (stream-seq stderr))))
                (let [isr-out (InputStreamReader. stdout (:out opts))
                      isr-err (InputStreamReader. stderr (:out opts))]
                  (apply str (map char (concat (stream-seq isr-out)
                                               (stream-seq isr-err))))))]
      (.waitFor proc)
      rtn)))

(comment

(println (sh "ls" "-l"))
(println (sh "ls" "-l" "/no-such-thing"))
(println (sh "sed" "s/[aeiou]/oo/g" :in "hello there\n"))
(println (sh "cat" :in "x\u25bax\n"))
(println (sh "echo" "x\u25bax"))
(println (sh "echo" "x\u25bax" :out "ISO-8859-1")) ; reads 4 single-byte chars
(println (sh "cat" "myimage.png" :out :bytes)) ; reads binary file into bytes[]

)
