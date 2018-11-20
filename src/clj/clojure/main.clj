;; Copyright (c) Rich Hickey All rights reserved. The use and
;; distribution terms for this software are covered by the Eclipse Public
;; License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found
;; in the file epl-v10.html at the root of this distribution. By using this
;; software in any fashion, you are agreeing to be bound by the terms of
;; this license. You must not remove this notice, or any other, from this
;; software.

;; Originally contributed by Stephen C. Gilardi

(ns ^{:doc "Top-level main function for Clojure REPL and scripts."
       :author "Stephen C. Gilardi and Rich Hickey"}
  clojure.main
  (:refer-clojure :exclude [with-bindings])
  (:require [clojure.spec.alpha :as spec])
  (:import (java.io StringReader)
           (clojure.lang Compiler Compiler$CompilerException
                         LineNumberingPushbackReader RT LispReader$ReaderException))
  ;;(:use [clojure.repl :only (demunge root-cause stack-element-str)])
  )

(declare main)

;;;;;;;;;;;;;;;;;;; redundantly copied from clojure.repl to avoid dep ;;;;;;;;;;;;;;

(defn demunge
  "Given a string representation of a fn class,
  as in a stack trace element, returns a readable version."
  {:added "1.3"}
  [fn-name]
  (clojure.lang.Compiler/demunge fn-name))

(defn root-cause
  "Returns the initial cause of an exception or error by peeling off all of
  its wrappers"
  {:added "1.3"}
  [^Throwable t]
  (loop [cause t]
    (if (and (instance? clojure.lang.Compiler$CompilerException cause)
             (not= (.source ^clojure.lang.Compiler$CompilerException cause) "NO_SOURCE_FILE"))
      cause
      (if-let [cause (.getCause cause)]
        (recur cause)
        cause))))

;;;;;;;;;;;;;;;;;;; end of redundantly copied from clojure.repl to avoid dep ;;;;;;;;;;;;;;

(def ^:private core-namespaces
  #{"clojure.core" "clojure.core.reducers" "clojure.core.protocols" "clojure.data" "clojure.datafy"
    "clojure.edn" "clojure.instant" "clojure.java.io" "clojure.main" "clojure.pprint" "clojure.reflect"
    "clojure.repl" "clojure.set" "clojure.spec.alpha" "clojure.spec.gen.alpha" "clojure.spec.test.alpha"
    "clojure.string" "clojure.template" "clojure.uuid" "clojure.walk" "clojure.xml" "clojure.zip"})

(defn- core-class?
  [^String class-name]
  (and (not (nil? class-name))
       (or (.startsWith class-name "clojure.lang.")
           (contains? core-namespaces (second (re-find #"^([^$]+)\$" class-name))))))

(defn stack-element-str
  "Returns a (possibly unmunged) string representation of a StackTraceElement"
  {:added "1.3"}
  [^StackTraceElement el]
  (let [file (.getFileName el)
        clojure-fn? (and file (or (.endsWith file ".clj")
                                  (.endsWith file ".cljc")
                                  (= file "NO_SOURCE_FILE")))]
    (str (if clojure-fn?
           (demunge (.getClassName el))
           (str (.getClassName el) "." (.getMethodName el)))
         " (" (.getFileName el) ":" (.getLineNumber el) ")")))
;;;;;;;;;;;;;;;;;;; end of redundantly copied from clojure.repl to avoid dep ;;;;;;;;;;;;;;


(defmacro with-bindings
  "Executes body in the context of thread-local bindings for several vars
  that often need to be set!: *ns* *warn-on-reflection* *math-context*
  *print-meta* *print-length* *print-level* *compile-path*
  *command-line-args* *1 *2 *3 *e"
  [& body]
  `(binding [*ns* *ns*
             *warn-on-reflection* *warn-on-reflection*
             *math-context* *math-context*
             *print-meta* *print-meta*
             *print-length* *print-length*
             *print-level* *print-level*
             *print-namespace-maps* true
             *data-readers* *data-readers*
             *default-data-reader-fn* *default-data-reader-fn*
             *compile-path* (System/getProperty "clojure.compile.path" "classes")
             *command-line-args* *command-line-args*
             *unchecked-math* *unchecked-math*
             *assert* *assert*
             clojure.spec.alpha/*explain-out* clojure.spec.alpha/*explain-out*
             *1 nil
             *2 nil
             *3 nil
             *e nil]
     ~@body))

(defn repl-prompt
  "Default :prompt hook for repl"
  []
  (printf "%s=> " (ns-name *ns*)))

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
     (or (Character/isWhitespace (char c)) (= c (int \,))) (recur (.read s))
     :else (do (.unread s c) :body))))

(defn renumbering-read
  "Reads from reader, which must be a LineNumberingPushbackReader, while capturing
  the read string. If the read is successful, reset the line number and re-read.
  The line number on re-read is the passed line-number unless :line or
  :clojure.core/eval-file meta are explicitly set on the read value."
  {:added "1.10"}
  ([opts ^LineNumberingPushbackReader reader line-number]
   (let [pre-line (.getLineNumber reader)
         [pre-read s] (read+string opts reader)
         {:keys [clojure.core/eval-file line]} (meta pre-read)
         re-reader (doto (LineNumberingPushbackReader. (StringReader. s))
                     (.setLineNumber (if (and line (or eval-file (not= pre-line line))) line line-number)))]
     (read opts re-reader))))

(defn repl-read
  "Default :read hook for repl. Reads from *in* which must either be an
  instance of LineNumberingPushbackReader or duplicate its behavior of both
  supporting .unread and collapsing all of CR, LF, and CRLF into a single
  \\newline. repl-read:
    - skips whitespace, then
      - returns request-prompt on start of line, or
      - returns request-exit on end of stream, or
      - reads an object from the input stream, then
        - skips the next input character if it's end of line, then
        - returns the object."
  [request-prompt request-exit]
  (or ({:line-start request-prompt :stream-end request-exit}
       (skip-whitespace *in*))
      (let [input (renumbering-read {:read-cond :allow} *in* 1)]
        (skip-if-eol *in*)
        input)))

(defn repl-exception
  "Returns the root cause of throwables"
  [throwable]
  (root-cause throwable))

(defn- file-name
  "Helper to get just the file name part of a path or nil"
  [^String full-path]
  (when full-path
    (try
      (.getName (java.io.File. full-path))
      (catch Throwable t))))

(defn- java-loc->source
  "Convert Java class name and method symbol to source symbol, either a
  Clojure function or Java class and method."
  [clazz method]
  (if (#{'invoke 'invokeStatic} method)
    (let [degen #(.replaceAll ^String % "--.*$" "")
          [ns-name fn-name & nested] (->> (str clazz) (.split #"\$") (map demunge) (map degen))]
      (symbol ns-name (String/join "$" ^"[Ljava.lang.String;" (into-array String (cons fn-name nested)))))
    (symbol (name clazz) (name method))))

(defn ex-triage
  "Returns an analysis of the phase, error, cause, and location of an error that occurred
  based on Throwable data, as returned by Throwable->map. All attributes other than phase
  are optional:
    :clojure.error/phase - keyword phase indicator, one of:
      :read-source :compile-syntax-check :compilation :macro-syntax-check :macroexpansion
      :execution :read-eval-result :print-eval-result
    :clojure.error/source - file name (no path)
    :clojure.error/line - integer line number
    :clojure.error/column - integer column number
    :clojure.error/symbol - symbol being expanded/compiled/invoked
    :clojure.error/class - cause exception class symbol
    :clojure.error/cause - cause exception message
    :clojure.error/spec - explain-data for spec error"
  {:added "1.10"}
  [datafied-throwable]
  (let [{:keys [via trace phase] :or {phase :execution}} datafied-throwable
        {:keys [type message data]} (last via)
        {:clojure.spec.alpha/keys [problems fn], :clojure.spec.test.alpha/keys [caller]} data
        {:clojure.error/keys [source] :as top-data} (:data (first via))]
    (assoc
      (case phase
        :read-source
        (let [{:clojure.error/keys [line column]} data]
          (cond-> (merge (-> via second :data) top-data)
            source (assoc :clojure.error/source (file-name source))
            (#{"NO_SOURCE_FILE" "NO_SOURCE_PATH"} source) (dissoc :clojure.error/source)
            message (assoc :clojure.error/cause message)))

        (:compile-syntax-check :compilation :macro-syntax-check :macroexpansion)
        (cond-> top-data
          source (assoc :clojure.error/source (file-name source))
          (#{"NO_SOURCE_FILE" "NO_SOURCE_PATH"} source) (dissoc :clojure.error/source)
          type (assoc :clojure.error/class type)
          message (assoc :clojure.error/cause message)
          problems (assoc :clojure.error/spec data))

        (:read-eval-result :print-eval-result)
        (let [[source method file line] (-> trace first)]
          (cond-> top-data
            line (assoc :clojure.error/line line)
            file (assoc :clojure.error/source file)
            (and source method) (assoc :clojure.error/symbol (java-loc->source source method))
            type (assoc :clojure.error/class type)
            message (assoc :clojure.error/cause message)))

        :execution
        (let [[source method file line] (->> trace (drop-while #(core-class? (name (first %)))) first)
              file (first (remove #(or (nil? %) (#{"NO_SOURCE_FILE" "NO_SOURCE_PATH"} %)) [(:file caller) file]))
              err-line (or (:line caller) line)]
          (cond-> {:clojure.error/class type}
            err-line (assoc :clojure.error/line err-line)
            message (assoc :clojure.error/cause message)
            (or fn (and source method)) (assoc :clojure.error/symbol (or fn (java-loc->source source method)))
            file (assoc :clojure.error/source file)
            problems (assoc :clojure.error/spec data))))
      :clojure.error/phase phase)))

(defn ex-str
  "Returns a string from exception data, as produced by ex-triage.
  The first line summarizes the exception phase and location.
  The subsequent lines describe the cause."
  {:added "1.10"}
  [{:clojure.error/keys [phase source line column symbol class cause spec]
    :as triage-data}]
  (let [loc (str (or source "REPL") ":" (or line 1) (if column (str ":" column) ""))
        class-name (name (or class ""))
        simple-class (if class (or (first (re-find #"([^.])++$" class-name)) class-name))
        cause-type (if (contains? #{"Exception" "RuntimeException"} simple-class)
                     "" ;; omit, not useful
                     (str " (" simple-class ")"))]
    (case phase
      :read-source
      (format "Syntax error reading source at (%s).%n%s%n" loc cause)

      :macro-syntax-check
      (format "Syntax error macroexpanding %sat (%s).%n%s"
              (if symbol (str symbol " ") "")
              loc
              (if spec
                (with-out-str
                  (spec/explain-out
                    (if (= spec/*explain-out* spec/explain-printer)
                      (update spec :clojure.spec.alpha/problems
                              (fn [probs] (map #(dissoc % :in) probs)))
                      spec)))
                (format "%s%n" cause)))

      :macroexpansion
      (format "Unexpected error%s macroexpanding %sat (%s).%n%s%n"
              cause-type
              (if symbol (str symbol " ") "")
              loc
              cause)

      :compile-syntax-check
      (format "Syntax error%s compiling %sat (%s).%n%s%n"
              cause-type
              (if symbol (str symbol " ") "")
              loc
              cause)

      :compilation
      (format "Unexpected error%s compiling %sat (%s).%n%s%n"
              cause-type
              (if symbol (str symbol " ") "")
              loc
              cause)

      :read-eval-result
      (format "Error reading eval result%s at %s (%s).%n%s%n" cause-type symbol loc cause)

      :print-eval-result
      (format "Error printing return value%s at %s (%s).%n%s%n" cause-type symbol loc cause)

      :execution
      (if spec
        (format "Execution error - invalid arguments to %s at (%s).%n%s"
                symbol
                loc
                (with-out-str
                  (spec/explain-out
                    (if (= spec/*explain-out* spec/explain-printer)
                      (update spec :clojure.spec.alpha/problems
                              (fn [probs] (map #(dissoc % :in) probs)))
                      spec))))
        (format "Execution error%s at %s(%s).%n%s%n"
                cause-type
                (if symbol (str symbol " ") "")
                loc
                cause)))))

(defn repl-caught
  "Default :caught hook for repl"
  [e]
  (binding [*out* *err*]
    (print (-> e Throwable->map ex-triage ex-str))
    (flush)))

(def ^{:doc "A sequence of lib specs that are applied to `require`
by default when a new command-line REPL is started."} repl-requires
  '[[clojure.repl :refer (source apropos dir pst doc find-doc)]
    [clojure.java.javadoc :refer (javadoc)]
    [clojure.pprint :refer (pp pprint)]])

(defmacro with-read-known
  "Evaluates body with *read-eval* set to a \"known\" value,
   i.e. substituting true for :unknown if necessary."
  [& body]
  `(binding [*read-eval* (if (= :unknown *read-eval*) true *read-eval*)]
     ~@body))

(defn repl
  "Generic, reusable, read-eval-print loop. By default, reads from *in*,
  writes to *out*, and prints exception summaries to *err*. If you use the
  default :read hook, *in* must either be an instance of
  LineNumberingPushbackReader or duplicate its behavior of both supporting
  .unread and collapsing CR, LF, and CRLF into a single \\newline. Options
  are sequential keyword-value pairs. Available options and their defaults:

     - :init, function of no arguments, initialization hook called with
       bindings for set!-able vars in place.
       default: #()

     - :need-prompt, function of no arguments, called before each
       read-eval-print except the first, the user will be prompted if it
       returns true.
       default: (if (instance? LineNumberingPushbackReader *in*)
                  #(.atLineStart *in*)
                  #(identity true))

     - :prompt, function of no arguments, prompts for more input.
       default: repl-prompt

     - :flush, function of no arguments, flushes output
       default: flush

     - :read, function of two arguments, reads from *in*:
         - returns its first argument to request a fresh prompt
           - depending on need-prompt, this may cause the repl to prompt
             before reading again
         - returns its second argument to request an exit from the repl
         - else returns the next object read from the input stream
       default: repl-read

     - :eval, function of one argument, returns the evaluation of its
       argument
       default: eval

     - :print, function of one argument, prints its argument to the output
       default: prn

     - :caught, function of one argument, a throwable, called when
       read, eval, or print throws an exception or error
       default: repl-caught"
  [& options]
  (let [cl (.getContextClassLoader (Thread/currentThread))]
    (.setContextClassLoader (Thread/currentThread) (clojure.lang.DynamicClassLoader. cl)))
  (let [{:keys [init need-prompt prompt flush read eval print caught]
         :or {init        #()
              need-prompt (if (instance? LineNumberingPushbackReader *in*)
                            #(.atLineStart ^LineNumberingPushbackReader *in*)
                            #(identity true))
              prompt      repl-prompt
              flush       flush
              read        repl-read
              eval        eval
              print       prn
              caught      repl-caught}}
        (apply hash-map options)
        request-prompt (Object.)
        request-exit (Object.)
        read-eval-print
        (fn []
          (try
            (let [read-eval *read-eval*
                  input (try
                          (with-read-known (read request-prompt request-exit))
                          (catch LispReader$ReaderException e
                            (throw (ex-info nil {:clojure.error/phase :read-source} e))))]
             (or (#{request-prompt request-exit} input)
                 (let [value (binding [*read-eval* read-eval] (eval input))]
                   (set! *3 *2)
                   (set! *2 *1)
                   (set! *1 value)
                   (try
                     (print value)
                     (catch Throwable e
                       (throw (ex-info nil {:clojure.error/phase :print-eval-result} e)))))))
           (catch Throwable e
             (caught e)
             (set! *e e))))]
    (with-bindings
     (try
      (init)
      (catch Throwable e
        (caught e)
        (set! *e e)))
     (prompt)
     (flush)
     (loop []
       (when-not 
       	 (try (identical? (read-eval-print) request-exit)
	  (catch Throwable e
	   (caught e)
	   (set! *e e)
	   nil))
         (when (need-prompt)
           (prompt)
           (flush))
         (recur))))))

(defn load-script
  "Loads Clojure source from a file or resource given its path. Paths
  beginning with @ or @/ are considered relative to classpath."
  [^String path]
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
  (let [eof (Object.)
        reader (LineNumberingPushbackReader. (java.io.StringReader. str))]
      (loop [input (with-read-known (read reader false eof))]
        (when-not (= input eof)
          (let [value (eval input)]
            (when-not (nil? value)
              (prn value))
            (recur (with-read-known (read reader false eof))))))))

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

(defn- main-opt
  "Call the -main function from a namespace with string arguments from
  the command line."
  [[_ main-ns & args] inits]
  (with-bindings
    (initialize args inits)
    (apply (ns-resolve (doto (symbol main-ns) require) '-main) args)))

(defn- repl-opt
  "Start a repl with args and inits. Print greeting if no eval options were
  present"
  [[_ & args] inits]
  (when-not (some #(= eval-opt (init-dispatch (first %))) inits)
    (println "Clojure" (clojure-version)))
  (repl :init (fn []
                (initialize args inits)
                (apply require repl-requires)))
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
     "-m"     main-opt
     "--main" main-opt
     nil      null-opt
     "-h"     help-opt
     "--help" help-opt
     "-?"     help-opt} opt)
   script-opt))

(defn- legacy-repl
  "Called by the clojure.lang.Repl.main stub to run a repl with args
  specified the old way"
  [args]
  (println "WARNING: clojure.lang.Repl is deprecated.
Instead, use clojure.main like this:
java -cp clojure.jar clojure.main -i init.clj -r args...")
  (let [[inits [sep & args]] (split-with (complement #{"--"}) args)]
    (repl-opt (concat ["-r"] args) (map vector (repeat "-i") inits))))

(defn- legacy-script
  "Called by the clojure.lang.Script.main stub to run a script with args
  specified the old way"
  [args]
  (println "WARNING: clojure.lang.Script is deprecated.
Instead, use clojure.main like this:
java -cp clojure.jar clojure.main -i init.clj script.clj args...")
  (let [[inits [sep & args]] (split-with (complement #{"--"}) args)]
    (null-opt args (map vector (repeat "-i") inits))))

(defn main
  "Usage: java -cp clojure.jar clojure.main [init-opt*] [main-opt] [arg*]

  With no options or args, runs an interactive Read-Eval-Print Loop

  init options:
    -i, --init path     Load a file or resource
    -e, --eval string   Evaluate expressions in string; print non-nil values

  main options:
    -m, --main ns-name  Call the -main function from a namespace with args
    -r, --repl          Run a repl
    path                Run a script from a file or resource
    -                   Run a script from standard input
    -h, -?, --help      Print this help message and exit

  operation:

    - Establishes thread-local bindings for commonly set!-able vars
    - Enters the user namespace
    - Binds *command-line-args* to a seq of strings containing command line
      args that appear after any main option
    - Runs all init options in order
    - Calls a -main function or runs a repl or script if requested

  The init options may be repeated and mixed freely, but must appear before
  any main option. The appearance of any eval option before running a repl
  suppresses the usual repl greeting message: \"Clojure ~(clojure-version)\".

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
   (finally 
     (flush))))

