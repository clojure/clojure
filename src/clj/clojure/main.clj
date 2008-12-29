;; Copyright (c) Rich Hickey All rights reserved. The use and
;; distribution terms for this software are covered by the Eclipse Public
;; License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found
;; in the file epl-v10.html at the root of this distribution. By using this
;; software in any fashion, you are agreeing to be bound by the terms of
;; this license. You must not remove this notice, or any other, from this
;; software.

;; Originally contributed by Stephen C. Gilardi

(ns clojure.main
  (:import (clojure.lang Compiler Compiler$CompilerException
                         LineNumberingPushbackReader RT)))

(declare main)

(def #^{:private true}
     eof (Object.))

(defmacro with-bindings
  "Executes body in the context of thread-local bindings for several vars
  that often need to be set!: *ns* *warn-on-reflection* *print-meta*
  *print-length* *print-level* *compile-path* *command-line-args* *1
  *2 *3 *e"
  [& body]
  `(binding [*ns* *ns*
             *warn-on-reflection* *warn-on-reflection*
             *print-meta* *print-meta*
             *print-length* *print-length*
             *print-level* *print-level*
             *compile-path* (System/getProperty "clojure.compile.path" "classes")
             *command-line-args* *command-line-args*
             *1 nil
             *2 nil
             *3 nil
             *e nil]
     ~@body))

(defn- root-cause
  "Returns the initial cause of an exception or error by peeling off all of
  its wrappers"
  [throwable]
  (loop [cause throwable]
    (if-let [cause (.getCause cause)]
      (recur cause)
      cause)))

(defn skip-if-eol
  "If the next character on stream s is a newline, skips it, otherwise
  leaves the stream untouched. Returns :line-start, :stream-end, or :body
  to indicate the relative location of the next character on s. The stream
  must either be an instance of LineNumberingPushbackReader or duplicate
  its behavior of both supporting .unread and collapsing all of CR, LF, and
  CRLF to a single \\newline."
  [s]
  (let [c (.read s)]
    (cond
     (= c (int \newline)) :line-start
     (= c -1) :stream-end
     :else (do (.unread s c) :body))))

(defn skip-whitespace
  "Skips whitespace characters on stream s. Returns :line-start, :stream-end,
  or :body to indicate the relative location of the next character on s.
  Interprets comma as whitespace and semicolon as comment to end of line.
  Does not interpret #! as comment to end of line because only one
  character of lookahead is available. The stream must either be an
  instance of LineNumberingPushbackReader or duplicate its behavior of both
  supporting .unread and collapsing all of CR, LF, and CRLF to a single
  \\newline."
  [s]
  (loop [c (.read s)]
    (cond
     (= c (int \newline)) :line-start
     (= c -1) :stream-end
     (= c (int \;)) (do (.readLine s) :line-start)
     (or (Character/isWhitespace c) (= c (int \,))) (recur (.read s))
     :else (do (.unread s c) :body))))

(defn repl-exception
  "Returns CompilerExceptions in tact, but only the root cause of other
  throwables"
  [throwable]
  (if (instance? Compiler$CompilerException throwable)
    throwable
    (root-cause throwable)))

(defn repl
  "Generic, reusable, read-eval-print loop. Reads from *in* and writes to
  *out*. *in* must either be an instance of LineNumberingPushbackReader or
  duplicate its behavior of both supporting .unread and collapsing CR, LF,
  and CRLF into a single \\newline. Options are sequential keyword-value
  pairs. Available options and their defaults:

     - :init, function of no arguments, initialization hook
       default: #()

     - :need-prompt, function of no arguments, called before each
       read-eval-print except the first, the user will be prompted if it
       returns true.
       default: (if (instance? LineNumberingPushbackReader *in*)
                  #(.atLineStart *in*)
                  #(identity true))

     - :prompt, function of no arguments, prompts for more input.
       default: #(printf \"%s=> \" (ns-name *ns*))

     - :flush, function of no arguments, flushes output
       default: flush

     - :read, function of no arguments, returns the next object read from
       the input
       default: read

     - :eval, funtion of one argument, returns the evaluation of its
       argument
       default: eval

     - :print, function of one argument, prints its argument to the output
       default: prn

     - :caught, function of one argument, a throwable, called when
       read, eval, or print throws an exception or error
       default: #(.println *err* (repl-exception %))"
  [& options]
  (let [{:keys [init need-prompt prompt flush read eval print caught]
         :or {init        #()
              need-prompt (if (instance? LineNumberingPushbackReader *in*)
                            #(.atLineStart *in*)
                            #(identity true))
              prompt      #(printf "%s=> " (ns-name *ns*))
              flush       flush
              read        read
              eval        eval
              print       prn
              caught      #(.println *err* (repl-exception %))}}
        (apply hash-map options)]
    (with-bindings
      (init)
      (prompt)
      (flush)
      (loop [where (skip-whitespace *in*)]
        (when-not (= where :stream-end)
          (when (= where :body)
            (try
             (let [input (read)]
               (skip-if-eol *in*)
               (let [value (eval input)]
                 (print value)
                 (set! *3 *2)
                 (set! *2 *1)
                 (set! *1 value)))
             (catch Throwable e
               (caught e)
               (set! *e e))))
          (when (need-prompt)
            (prompt)
            (flush))
          (recur (skip-whitespace *in*)))))))

(defn load-script
  "Loads Clojure source from a file or resource given its path. Paths
  beginning with @ or @/ are considered relative to classpath."
  [path]
  (if (.startsWith path "@")
    (RT/loadResourceScript
     (.substring path (if (.startsWith path "@/") 2 1)))
    (Compiler/loadFile path)))

(defn- init-opt
  "Load a script"
  [path]
  (load-script path))

(defn- eval-opt
  "Evals expressions in str, prints each non-nil result using prn"
  [str]
  (with-in-str str
    (loop [input (read *in* false eof)]
      (when-not (= input eof)
        (let [value (eval input)]
          (when-not (nil? value)
            (prn value))
          (recur (read *in* false eof)))))))

(defn- init-dispatch
  "Returns the handler associated with an init opt"
  [opt]
  ({"-i"     init-opt
    "--init" init-opt
    "-e"     eval-opt
    "--eval" eval-opt} opt))

(defn- initialize
  "Common initialize routine for repl, script, and null opts"
  [args inits]
  (in-ns 'user)
  (set! *command-line-args* args)
  (doseq [[opt arg] inits]
    ((init-dispatch opt) arg)))

(defn- repl-opt
  "Start a repl with args and inits. Print greeting if no eval options were
  present"
  [[_ & args] inits]
  (when-not (some #(= eval-opt (init-dispatch (first %))) inits)
    (println "Clojure"))
  (repl :init #(initialize args inits))
  (prn)
  (System/exit 0))

(defn- script-opt
  "Run a script from a file, resource, or standard in with args and inits"
  [[path & args] inits]
  (with-bindings
    (initialize args inits)
    (if (= path "-")
      (load-reader *in*)
      (load-script path))))

(defn- null-opt
  "No repl or script opt present, just bind args and run inits"
  [args inits]
  (with-bindings
    (initialize args inits)))

(defn- help-opt
  "Print help text for main"
  [_ _]
  (println (:doc (meta (var main)))))

(defn- main-dispatch
  "Returns the handler associated with a main option"
  [opt]
  (or
   ({"-r"     repl-opt
     "--repl" repl-opt
     nil      null-opt
     "-h"     help-opt
     "--help" help-opt
     "-?"     help-opt} opt)
   script-opt))

(defn- legacy-repl
  "Called by the clojure.lang.Repl.main stub to run a repl with args
  specified the old way"
  [args]
  (let [[inits [sep & args]] (split-with (complement #{"--"}) args)]
    (repl-opt (concat ["-r"] args) (map vector (repeat "-i") inits))))

(defn- legacy-script
  "Called by the clojure.lang.Script.main stub to run a script with args
  specified the old way"
  [args]
  (let [[inits [sep & args]] (split-with (complement #{"--"}) args)]
    (null-opt args (map vector (repeat "-i") inits))))

(defn main
  "Usage: java -cp clojure.jar clojure.main [init-opt*] [main-opt] [arg*]

  With no options or args, runs an interactive Read-Eval-Print Loop

  init options:
    -i, --init path   Load a file or resource
    -e, --eval string Evaluate expressions in string; print non-nil values

  main options:
    -r, --repl        Run a repl
    path              Run a script from from a file or resource
    -                 Run a script from standard input
    -h, -?, --help    Print this help message and exit

  operation:

    - Establishes thread-local bindings for commonly set!-able vars
    - Enters the user namespace
    - Binds *command-line-args* to a seq of strings containing command line
      args that appear after any main option
    - Runs all init options in order
    - Runs a repl or script if requested

  The init options may be repeated and mixed freely, but must appear before
  any main option. The appearance of any eval option before running a repl
  suppresses the usual repl greeting message: \"Clojure\".

  Paths may be absolute or relative in the filesystem or relative to
  classpath. Classpath-relative paths have prefix of @ or @/"
  [& args]
  (try
   (if args
     (loop [[opt arg & more :as args] args inits []]
       (if (init-dispatch opt)
         (recur more (conj inits [opt arg]))
         ((main-dispatch opt) args inits)))
     (repl-opt nil nil))
   (catch Exception e
     (.printStackTrace e *err*)))
  (flush))
