;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
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
           (java.util Date))
  (:require clojure.main)
  (:use (clojure.contrib cond def fcase)))

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
    :name-fmt "repl-%S"
    :prompt-fmt "%S:%L %N=> "
    :depth 0)
  "Default/root values for repl info")

(defvar- +special-character+
  { (int \return)  :eol
    (int \newline) :eol
    (int \,)       :ws
    (int \;)       :cte
    -1             :eos }
  "Maps interesting character codes to keywords representing their type")

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

(defn- next-char
  "Reads the next character in s and either returns it or one of the
  following keywords if the character is of the corresponding type:
    :ws  whitespace
    :eol end-of-line
    :eos end-of-stream
    :cte comment-to-end character"
  [s]
  (let [c (.read s)]
    (cond-let [type]
      (+special-character+ c) type
      (Character/isWhitespace c) :ws
      :else c)))

(defn- skip-to-end
  "Skips characters on stream s until an end of stream or end of line"
  [s]
  (loop [c (next-char s)]
    (if (#{:eol :eos} c)
      c
      (recur (next-char s)))))

(defn- find-readable-this-line
  "Skips characters on stream s until end of stream, end of line, or a
  character of interest to the Reader. Returns :eos on end of stream, :eol
  on end of line, :eol or :eos after skipping to end of line or end of
  stream on semicolon, or :readable otherwise. Before returning :readable,
  the readable character is pushed back onto the stream."
  [s]
  (loop [c (next-char s)]
    (case c
     :eol c
     :eos c
     :cte (skip-to-end s)
     :ws (recur (next-char s))
     (do
       (.unread s c)
       :readable))))

(defn- read-hook
  "Read hook for clojure.main/repl that keeps the compiler's line number in
  sync with that of our input stream, prompts only when there is nothing
  interesting remaining to read on the previous input line, and calls the
  Reader only when there's something interesting to read on the current
  line."
  [eof]
  (let [{:keys [prompt flush read]} *private*]
    (loop [c (find-readable-this-line *in*)]
      (case c
       :eos eof
       :eol
       (do
         (prompt)
         (flush)
         (recur (find-readable-this-line *in*)))
       :readable
       (do
         (var-set Compiler/LINE (.getLineNumber *in*))
         (read eof))))))

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

       - :in,:out,:err input, output, and error streams
         default: System/in, System/out, System/err

       - :encoding java.nio.charset.Charset, encoding for in, out, err
         default: RT/UTF8

       - :name-fmt, Name format string
         default: the name-fmt of the parent repl, or \"repl-%S\"

       - :prompt-fmt, Prompt format string
         default: the prompt-fmt of the parent repl, or \"%S:%L %N=> \""
  [& options]
  (let [{:keys [init prompt flush read eval print caught in out err
                encoding name-fmt prompt-fmt]
         :or {init       #()
              prompt     #(clojure.core/print (repl-prompt))
              flush      flush
              read       #(read *in* false %)
              eval       eval
              print      prn
              caught     #(.println *err* (clojure.main/repl-exception %))
              in         System/in
              out        System/out
              err        System/err
              encoding   RT/UTF8}}
        (apply hash-map options)]
    (try
     (Var/pushThreadBindings
      {RT/IN (LineNumberingPushbackReader.
              (InputStreamReader. in encoding))
       RT/OUT (OutputStreamWriter. out encoding)
       RT/ERR (PrintWriter. (OutputStreamWriter. err encoding) true)
       Compiler/SOURCE (var-get Compiler/SOURCE)
       Compiler/LINE (var-get Compiler/LINE)
       (var *info*) *info*
       (var *private*) {}})
     (assoc! *info*
             :started (Date.)
             :serial (swap! *serial-number* inc)
             :thread (.getId (Thread/currentThread))
             :depth (inc (:depth *info*)))
     (assoc! *private*
             :prompt prompt
             :flush flush
             :read read)
     (set-repl-name (or name-fmt (:name-fmt *info*)))
     (set-repl-prompt (or prompt-fmt (:prompt-fmt *info*)))
     ;; unread newline to enable first prompt
     (.unread *in* (int \newline))
     (clojure.main/repl
      :init init
      :prompt #()
      :flush #()
      :read read-hook
      :eval eval
      :print print
      :caught caught)
     (finally
      (Var/popThreadBindings)
      (prn)))))
