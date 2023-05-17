;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.java.process
  "A process invocation API wrapping the Java process API.

   The primary function here is 'start' which starts a process and handles the
   streams as directed. It returns a map that contains keys to access the streams
   (if available) and the Java Process object. It is also deref-able to wait for
   process exit.

   Helper functions are available to 'capture' the output of the process stdout
   and to wait for an 'ok?' non-error exit. The 'exec' function handles the common
   case of `start'ing a process, waiting for process exit, capture and return
   stdout."
  (:require
   [clojure.java.io :as jio]
   [clojure.string :as str])
  (:import
    [java.io StringWriter File]
    [java.lang ProcessBuilder ProcessBuilder$Redirect Process]
    [java.util List]
    [clojure.lang IDeref IBlockingDeref]))

(set! *warn-on-reflection* true)

;; this is built into Java 9, backfilled here for Java 8
(def ^:private ^File null-file
  (delay
    (jio/file
     (if (.startsWith (System/getProperty "os.name") "Windows")
       "NUL"
       "/dev/null"))))

(defn to-file
  "Coerce f to a file per clojure.java.io/file and return a ProcessBuilder.Redirect writing to the file.
  Set ':append' in opts to append. This can be passed to 'start' in :out or :err."
  {:added "1.12"}
  ^ProcessBuilder$Redirect [f & {:keys [append] :as opts}]
  (let [fo (jio/file f)]
    (if append
      (ProcessBuilder$Redirect/appendTo fo)
      (ProcessBuilder$Redirect/to fo))))

(defn from-file
  "Coerce f to a file per clojure.java.io/file and return a ProcessBuilder.Redirect reading from the file.
  This can be passed to 'start' in :in."
  {:added "1.12"}
  ^ProcessBuilder$Redirect [f]
  (ProcessBuilder$Redirect/from (jio/file f)))

(defn start
  "Starts an external command as args and optional leading opts map:

    :in - a ProcessBuilder.Redirect (default = :pipe) or :inherit
    :out - a ProcessBuilder.Redirect (default = :pipe) or :inherit :discard
    :err - a ProcessBuilder.Redirect (default = :pipe) or :inherit :discard :stdout
    :dir - directory to run the command from, default=\".\"
    :env - {env-var value} of environment variables (all strings)

  Returns an ILookup containing the java.lang.Process in :process and the
  streams :in :out :err. The map is also an IDeref that waits for process exit
  and returns the exit code."
  {:added "1.12"}
  [& opts+args]
  (let [[opts command] (if (map? (first opts+args))
                         [(first opts+args) (rest opts+args)]
                         [{} opts+args])
        {:keys [in out err dir env]
         :or {in :pipe, out :pipe, err :pipe, dir "."}} opts
        pb (ProcessBuilder. ^List command)
        to-redirect (fn to-redirect
                      [x]
                      (case x
                        :pipe ProcessBuilder$Redirect/PIPE
                        :inherit ProcessBuilder$Redirect/INHERIT
                        :discard (ProcessBuilder$Redirect/to @null-file)
                        ;; in Java 9+, just use ProcessBuilder$Redirect/DISCARD
                        x))]
    (.directory pb (jio/file (or dir ".")))
    (when in (.redirectInput pb ^ProcessBuilder$Redirect (to-redirect in)))
    (when out (.redirectOutput pb ^ProcessBuilder$Redirect (to-redirect out)))
    (cond
      (= err :stdout) (.redirectErrorStream pb true)
      err (.redirectError pb ^ProcessBuilder$Redirect (to-redirect err)))
    (when env
      (let [pb-env (.environment pb)]
        (run! (fn [[k v]] (.put pb-env k v)) env)))
    (let [proc (.start pb)
          m {:process proc
             :in (.getOutputStream proc)
             :out (.getInputStream proc)
             :err (.getErrorStream proc)}]
      (reify
        clojure.lang.ILookup
        (valAt [_ key] (get m key))
        (valAt [_ key not-found] (get m key not-found))

        IDeref
        (deref [_] (.waitFor proc))

        IBlockingDeref
        (deref [_ timeout unit] (.waitFor proc timeout unit))))))

(defn ok?
  "Given the map returned from 'start', wait for the process to exit
  and then return true on success"
  {:added "1.12"}
  [process-map]
  (zero? (.waitFor ^Process (:process process-map))))

(defn capture
  "Read from input-stream until EOF and return a String (or nil if 0 length).
  Takes same opts as clojure.java.io/copy - :buffer-size and :encoding"
  {:added "1.12"}
  [input-stream & opts]
  (let [writer (StringWriter.)]
    (apply jio/copy input-stream writer opts)
    (let [s (str/trim (.toString writer))]
      (when-not (zero? (.length s))
        s))))

(defn exec
  "Execute a command and on successful exit, return the captured output,
  else throw RuntimeException. Args are the same as 'start' and options
  if supplied override the default 'exec' settings."
  {:added "1.12"}
  [& opts+args]
  (let [[opts command] (if (map? (first opts+args))
                         [(first opts+args) (rest opts+args)]
                         [{} opts+args])
        opts (merge {:err :inherit} opts)]
    (let [state (apply start opts command)
          out-promise (promise)
          capture-fn #(deliver out-promise (capture (:out state)))]
      (doto (Thread. ^Runnable capture-fn) (.setDaemon true) (.start))
      (if (ok? state)
        @out-promise
        (throw (RuntimeException. (str "Process failed with exit=" (.exitValue ^Process (:process state)))))))))

(comment
  ;; shell out and inherit the i/o
  (start {:out :inherit, :err :stdout} "ls" "-l")

  ;; write out and err to files, wait for process to exit, return exit code
  @(start {:out (to-file "out") :err (to-file "err")} "ls" "-l")

  ;; capture output to string
  (-> (start "ls" "-l") :out capture)

  ;; with exec
  (exec "ls" "-l")

  ;; read input from file
  (exec {:in (from-file "deps.edn")} "wc" "-l")
  )
