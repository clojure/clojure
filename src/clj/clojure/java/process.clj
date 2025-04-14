;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.java.process
  "A process invocation API wrapping the Java process API.

   The primary function is 'start' which starts a process and handles the
   streams as directed. It returns the Process object. Use 'exit-ref' to wait
   for completion and receive the exit value, and ‘stdout', 'stderr', 'stdin'
   to access the process streams. The 'exec' function handles the common case
   to 'start' a process, wait for process exit, and return stdout."
  (:require
   [clojure.java.io :as jio])
  (:import
    [java.io File InputStream OutputStream]
    [java.lang ProcessBuilder ProcessBuilder$Redirect Process]
    [java.util List]
    [clojure.lang IDeref IBlockingDeref]
    [java.util.concurrent Executors ExecutorService ThreadFactory]))

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
  "Start an external command, defined in args.
  The process environment vars are inherited from the parent by
  default (use :clear-env to clear them).

  If needed, provide options in map as first arg:
    :in - a ProcessBuilder.Redirect (default = :pipe) or :inherit
    :out - a ProcessBuilder.Redirect (default = :pipe) or :inherit :discard
    :err - a ProcessBuilder.Redirect (default = :pipe) or :inherit :discard :stdout
    :dir - current directory when the process runs (default=\".\")
    :clear-env - if true, remove all inherited parent env vars
    :env - {env-var value} of environment variables to set (all strings)

  Returns the java.lang.Process."
  {:added "1.12"}
  ^Process [& opts+args]
  (let [[opts command] (if (map? (first opts+args))
                         [(first opts+args) (rest opts+args)]
                         [{} opts+args])
        {:keys [in out err dir env clear-env]
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
    (.directory pb (jio/file dir))
    (.redirectInput pb ^ProcessBuilder$Redirect (to-redirect in))
    (.redirectOutput pb ^ProcessBuilder$Redirect (to-redirect out))
    (if
      (= err :stdout) (.redirectErrorStream pb true)
      (.redirectError pb ^ProcessBuilder$Redirect (to-redirect err)))
    (when clear-env
      (.clear (.environment pb)))
    (when env
      (let [pb-env (.environment pb)]
        (run! (fn [[k v]] (.put pb-env k v)) env)))
    (.start pb)))

(defn stdin
  "Given a process, return the stdin of the external process (an OutputStream)"
  {:added "1.12"}
  ^OutputStream [^Process process]
  (.getOutputStream process))

(defn stdout
  "Given a process, return the stdout of the external process (an InputStream)"
  {:added "1.12"}
  ^InputStream [^Process process]
  (.getInputStream process))

(defn stderr
  "Given a process, return the stderr of the external process (an InputStream)"
  {:added "1.12"}
  ^InputStream [^Process process]
  (.getErrorStream process))

(defn exit-ref
  "Given a Process (the output of 'start'), return a reference that can be
  used to wait for process completion then returns the exit value."
  {:added "1.12"}
  [^Process process]
  (reify
    IDeref
    (deref [_] (long (.waitFor process)))

    IBlockingDeref
    (deref [_ timeout-ms timeout-val]
      (if (.waitFor process timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
        (long (.exitValue process))
        timeout-val))))

;; A thread factory for daemon threads
(defonce ^:private io-thread-factory
  (let [counter (atom 0)]
    (reify ThreadFactory
      (newThread [_ r]
        (doto (Thread. r)
          (.setName (str "Clojure Process IO " (swap! counter inc)))
          (.setDaemon true))))))

;; An ExecutorService for cached, daemon threads
(defonce ^:private io-executor
  (Executors/newCachedThreadPool ^ThreadFactory io-thread-factory))

(defn io-task
  {:skip-wiki true}
  [^Runnable f]
  (let [f (bound-fn* f)
        fut (.submit ^ExecutorService io-executor ^Callable f)]
    (reify
      clojure.lang.IDeref
      (deref [_] (#'clojure.core/deref-future fut))
      clojure.lang.IBlockingDeref
      (deref
        [_ timeout-ms timeout-val]
        (#'clojure.core/deref-future fut timeout-ms timeout-val))
      clojure.lang.IPending
      (isRealized [_] (.isDone fut))
      java.util.concurrent.Future
      (get [_] (.get fut))
      (get [_ timeout unit] (.get fut timeout unit))
      (isCancelled [_] (.isCancelled fut))
      (isDone [_] (.isDone fut))
      (cancel [_ interrupt?] (.cancel fut interrupt?)))))

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
    (let [proc (apply start opts command)
          captured (io-task #(slurp (stdout proc)))
          exit (deref (exit-ref proc))]
      (if (zero? exit)
        @captured
        (throw (RuntimeException. (str "Process failed with exit=" exit)))))))

(comment
  ;; shell out and inherit the i/o
  (start {:out :inherit, :err :stdout} "ls" "-l")

  ;; write out and err to files, wait for process to exit, return exit code
  @(exit-ref (start {:out (to-file "out") :err (to-file "err")} "ls" "-l"))

  ;; capture output to string
  (-> (start "ls" "-l") stdout slurp)

  ;; with exec
  (exec "ls" "-l")

  ;; read input from file
  (-> (exec {:in (from-file "deps.edn")} "wc" "-l") clojure.string/trim parse-long)
  )
