;; Copyright (c) Rich Hickey All rights reserved. The use and
;; distribution terms for this software are covered by the Common Public
;; License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;; in the file CPL.TXT at the root of this distribution. By using this
;; software in any fashion, you are agreeing to be bound by the terms of
;; this license. You must not remove this notice, or any other, from this
;; software.

;; Originally contributed by Stephen C. Gilardi

(ns clojure.main
  (:gen-class)
  (:import (clojure.lang Compiler Compiler$CompilerException RT)))

(defmacro with-bindings
  "Executes body in the context of thread-local bindings for several vars
  that often need to be set!"
  [& body]
  (let [compile-path
        (System/getProperty "clojure.compile.path" "classes")]
    `(binding [*ns* *ns*
               *warn-on-reflection* *warn-on-reflection*
               *print-meta* *print-meta*
               *print-length* *print-length*
               *print-level* *print-level*
               *compile-path* ~compile-path
               *command-line-args* *command-line-args*
               *1 nil
               *2 nil
               *3 nil
               *e nil]
       ~@body)))

(defn- root-cause
  "Returns the initial cause of an exception or error by peeling off all of
  its wrappers"
  [throwable]
  (loop [cause throwable]
    (if-let [cause (.getCause cause)]
      (recur cause)
      cause)))

(defn repl-exception
  "Returns CompilerExceptions in tact, but only the root cause of other
  throwables"
  [throwable]
  (if (instance? Compiler$CompilerException throwable)
    throwable
    (root-cause throwable)))

(defn repl
  "Generic, reusable, read-eval-print loop. Options are sequential
  keyword-value pairs. Available options and their defaults:

     - :init, function of no arguments, initialization hook
       default: #()

     - :prompt, function of no arguments, prompts for more input
       default: #(printf \"%s=> \" (ns-name *ns*))

     - :flush, function of no arguments, flushes output
       default: flush

     - :read, function of one argument, returns the next object read from
       the input, or its argument iff the input is exhausted
       default: #(read *in* false %)

     - :eval, funtion of one argument, returns the evaluation of its
       argument
       default: eval

     - :print, function of one argument, prints its argument to the output
       default: println

     - :caught, function of one argument, a throwable, called when
       read, eval, or print throws an exception or error
       default: #(.println *err* (repl-exception %))"
  [& options]
  (let [{:keys [init prompt flush read eval print caught]
         :or {init    #()
              prompt  #(printf "%s=> " (ns-name *ns*))
              flush   flush
              read    #(read *in* false %)
              eval    eval
              print   prn
              caught  #(.println *err* (repl-exception %))}}
        (apply hash-map options)
        eof (Object.)]
    (with-bindings
     (init)
     (loop []
       (prompt)
       (flush)
       (when-not
        (= eof
           (try
            (let [input (read eof)]
              (if (= input eof)
                eof
                (let [value (eval input)]
                  (print value)
                  (set! *3 *2)
                  (set! *2 *1)
                  (set! *1 value))))
            (catch Throwable e
              (caught e)
              (set! *e e))))
        (recur))))))

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
  "Eval expr, print the result if it's not nil"
  [expr]
  (let [value (with-in-str expr (eval (read)))]
    (when-not (nil? value)
      (println value))))

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
  (prn))

(defn- script-opt
  "Run a script from a file, resource, or standard in with args and inits"
  [[path & args] inits]
  (with-bindings
   (initialize args inits)
   (if (= path "-")
     (load-reader *in*)
     (load-script path))))

(defn- null-opt
  "No repl or script opt present, just bind nil and run inits"
  [args inits]
  (with-bindings
   (initialize args inits)))

(defn- help-opt
  "Print help text for main"
  [_ _]
  (println
"Usage: java -jar clojure.jar [option*] [arg*]

  With no options or args, runs an interactive Read-Eval-Print Loop

init options:

  -i, --init path  Load a file or resource
  -e, --eval expr  Evaluate an expression and print its value if non-nil

main options:

  -r, --repl       Run a repl
  path             Run a script from from a file or resource
  -                Run a script from standard input
  -h, -?, --help   Print this help message and exit

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
  classpath. Classpath-relative paths have prefix of @ or @/"))

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

(defn- -main
  "Flexible main for Clojure"
  [& args]
  (try
   (if args
     (loop [[opt arg & more :as args] args inits []]
       (if (init-dispatch opt)
         (recur more (conj inits [opt arg]))
         ((main-dispatch opt) args inits)))
     (repl-opt nil nil))
   (catch Exception e
     (.printStackTrace e *err*))
   (finally
    (flush))))
