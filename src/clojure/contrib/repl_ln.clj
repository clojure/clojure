;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  A repl with that provides support for lines and line numbers in the
;;  input stream.
;;
;;  scgilardi (gmail)
;;  Created 28 November 2008

(ns clojure.contrib.repl-ln
  (:gen-class)
  (:import (clojure.lang Compiler LineNumberingPushbackReader RT Var)
           (java.io InputStreamReader OutputStreamWriter PrintWriter)
           java.util.Date)
  (:require clojure.main)
  (:use [clojure.contrib.def
         :only (defmacro- defonce- defstruct- defvar-)]))

;; Private

(declare repl)

(defstruct- repl-info
  :name :started :name-fmt :prompt-fmt :serial :thread :depth)

(defvar- +name-formats+
  {"%S" "%1$d" "%T" "%2$d" "%D" "%3$d"}
  "For set-name, maps our dynamic value codes to arg positions in
  the call to format in repl-name")

(defvar- +prompt-formats+
  {"%S" "%1$d" "%T" "%2$d" "%D" "%3$d" "%L" "%4$d" "%N" "%5$s"}
  "For set-prompt, maps our dynamic value codes to arg positions in
  the call to format in repl-prompt")

(defvar- +info-format+
  ["Name:       %s"
   "Started:    %s"
   "Name-fmt:   \"%s\""
   "Prompt-fmt: \"%s\""
   "Serial:     %d"
   "Thread:     %d"
   "Depth:      %d"
   "Line:       %d"])

(defvar- +info-defaults+
  (struct-map repl-info
    :name-fmt   "repl-%S"
    :prompt-fmt "%S:%L %N=> "
    :depth      0)
  "Default/root values for repl info")

(defonce- *serial-number* (atom 0)
  "Serial number counter")

(defonce- *info* +info-defaults+
  "Public info for this repl")

(defonce- *private* {}
  "Private info for this repl")

(defmacro- assoc!
  "Replaces the map thread-locally bound to map-var with a copy that
  includes updated and/or new values from keys and vals."
  [map-var & key-vals]
  `(set! ~map-var (assoc ~map-var ~@key-vals)))

(defn- repl-name
  "Returns the repl name based on this repl's name-fmt"
  []
  (let [{:keys [name-fmt]} *private*
        {:keys [serial thread depth]} *info*]
    (format name-fmt serial thread depth)))

(defn- prompt-hook
  []
  (let [prompt (*private* :prompt)]
    (var-set Compiler/LINE (.getLineNumber *in*))
    (prompt)))

(defn- process-inits
  "Processes initial pairs of args of the form:

    -i     filepath, or
    --init filepath

  by loading the referenced files. Returns a seq of the remaining args."
  [args]
  (loop [[init filename & more :as args] args]
    (if (#{"-i" "--init"} init)
      (do
        (clojure.main/load-script filename)
        (recur more))
      args)))

(defn- process-command-line
  "Args are strings passed in from the command line. Loads any requested
  init files and binds *command-line-args* to a seq of the remaining args"
  [args]
  (set! *command-line-args* (process-inits args)))

(defn stream-repl
  "Repl entry point that provides convenient overriding of input, output,
  and err streams via sequential keyword-value pairs. Default values
  for :in, :out, and :err are streams associated with System/in,
  System/out, and System/err using UTF-8 encoding. Also supports all the
  options provided by clojure.contrib.repl-ln/repl."
  [& options]
  (let [enc RT/UTF8
        {:keys [in out err]
         :or {in (LineNumberingPushbackReader.
                  (InputStreamReader. System/in enc))
              out (OutputStreamWriter. System/out enc)
              err (PrintWriter. (OutputStreamWriter. System/err enc))}}
        (apply hash-map options)]
    (binding [*in* in *out* out *err* err]
      (apply repl options))))

(defn- -main
  "Main entry point, starts a repl enters the user namespace and processes
  command line args."
  [& args]
  (repl :init
        (fn []
          (println "Clojure")
          (in-ns 'user)
          (process-command-line args))))

;; Public

(defn repl-prompt
  "Returns the current repl prompt based on this repl's prompt-fmt"
  []
  (let [{:keys [prompt-fmt]} *private*
        {:keys [serial thread depth]} *info*
        line (.getLineNumber *in*)
        namespace (ns-name *ns*)]
    (format prompt-fmt serial thread depth line namespace)))

(defn set-repl-name
  "Sets the repl name format to the string name-fmt. Include the following
  codes in the name to make the corresponding dynamic values part of it:

    %S - repl serial number
    %T - thread id
    %D - nesting depth in this thread

  With no arguments, resets the repl name to its default: \"repl-%S\""
  ([]
     (set-repl-name (+info-defaults+ :name-fmt)))
  ([name-fmt]
     (assoc! *info* :name-fmt name-fmt)
     (loop [[[code fmt] & more] (seq +name-formats+)
            name-fmt name-fmt]
       (if code
         (recur more (.replace name-fmt code fmt))
         (assoc! *private* :name-fmt name-fmt)))
     (let [name (repl-name)]
       (assoc! *info* :name name)
       (var-set Compiler/SOURCE name))
     nil))

(defn set-repl-prompt
  "Sets the repl prompt. Include the following codes in the prompt to make
  the corresponding dynamic values part of it:

    %S - repl serial number
    %T - thread id
    %D - nesting depth in this thread
    %L - input line number
    %N - namespace name

  With no arguments, resets the repl pompt to its default: \"%S:%L %N=> \""
  ([]
     (set-repl-prompt (+info-defaults+ :prompt-fmt)))
  ([prompt-fmt]
     (assoc! *info* :prompt-fmt prompt-fmt)
     (loop [[[code fmt] & more] (seq +prompt-formats+)
            prompt-fmt prompt-fmt]
       (if code
         (recur more (.replace prompt-fmt code fmt))
         (assoc! *private* :prompt-fmt prompt-fmt)))
     nil))

(defn repl-info
  "Returns a map of info about the current repl"
  []
  (let [line (.getLineNumber *in*)]
    (assoc *info* :line line)))

(defn print-repl-info
  "Prints info about the current repl"
  []
  (let [{:keys [name started name-fmt prompt-fmt serial thread depth line]}
        (repl-info)]
    (printf
     (apply str (interleave +info-format+ (repeat "\n")))
     name started name-fmt prompt-fmt serial thread depth line)))

(defn repl
  "A repl that supports line numbers. For definitions and evaluations made
  at the repl, the repl-name and line number will be reported as the
  origin. Use set-repl-name and set-repl-prompt to customize the repl name
  and prompt. This repl supports all of the keyword arguments documented
  for clojure.main/repl with the following change and additions:

       - :prompt has a new default
         default: #(clojure.core/print (repl-prompt))

       - :name-fmt, Name format string
         default: the name-fmt of the parent repl, or \"repl-%S\"

       - :prompt-fmt, Prompt format string
         default: the prompt-fmt of the parent repl, or \"%S:%L %N=> \""
  [& options]
  (let [{:keys [init need-prompt prompt flush read eval print caught
                name-fmt prompt-fmt]
         :or {init        #()
              need-prompt (if (instance? LineNumberingPushbackReader *in*)
                            #(.atLineStart *in*)
                            #(identity true))
              prompt      #(clojure.core/print (repl-prompt))
              flush       flush
              read        read
              eval        eval
              print       prn
              caught      #(.println *err* (clojure.main/repl-exception %))
              name-fmt    (*info* :name-fmt)
              prompt-fmt  (*info* :prompt-fmt)}}
              (apply hash-map options)]
    (try
     (Var/pushThreadBindings
      {Compiler/SOURCE (var-get Compiler/SOURCE)
       Compiler/LINE (var-get Compiler/LINE)
       (var *info*) *info*
       (var *private*) {}})
     (assoc! *info*
             :started (Date.)
             :serial (swap! *serial-number* inc)
             :thread (.getId (Thread/currentThread))
             :depth (inc (*info* :depth)))
     (assoc! *private*
             :prompt prompt)
     (set-repl-name name-fmt)
     (set-repl-prompt prompt-fmt)
     (clojure.main/repl
      :init init
      :need-prompt need-prompt
      :prompt prompt-hook
      :flush flush
      :read read
      :eval eval
      :print print
      :caught caught)
     (finally
      (Var/popThreadBindings)
      (prn)))))
