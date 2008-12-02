;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  A repl with a custom "read" hook that provides support for line
;;  numbers.
;;
;;  scgilardi (gmail)
;;  Created 28 November 2008

(ns clojure.contrib.repl-ln
  (:import (clojure.lang Compiler LineNumberingPushbackReader RT Var)
           (java.io InputStreamReader OutputStreamWriter PrintWriter)
           (java.util Date))
  (:use clojure.contrib.def))

;; Private

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

(defonce- *serial-number* (ref 0)
  "Serial number counter")

(defonce- *info* +info-defaults+
  "Info for this repl")

(defn- set-info!
  "Replaces the value thread-locally bound to *info* with a new map with
  updated values. Args are sequential inline key value pairs."
  [& args]
  (set! *info* (apply assoc *info* args)))

(defn- repl-name
  "Returns the repl name based on *info*"
  []
  (let [{:keys [name-fmt-internal serial thread depth]} *info*]
    (format name-fmt-internal serial thread depth)))

(defn- repl-prompt
  "Returns the repl prompt based on *info*, line number, and namespace"
  []
  (let [{:keys [prompt-fmt-internal serial thread depth]} *info*
        line (.getLineNumber *in*)
        namespace (ns-name *ns*)]
    (format prompt-fmt-internal serial thread depth line namespace)))

(defn- whitespace?
  "Returns logical true if c is whitespace in Clojure"
  [c]
  (or (Character/isWhitespace c) (= c (int \,))))

(defn- eol?
  "Returns logical true if c is an eol character"
  [c]
  (#{\return \newline} (char c)))

(defn- skip-to-eol
  "Reads and skips everything until an eol character"
  [s]
  (loop [c (.read s)]
    (when-not (eol? c)
      (recur (.read s)))))
          
(defn- skip-whitespace
  "Reads and skips whitespace characters from stream s. Returns :eos on end
  of stream, :eol on end of line, :eol after skipping rest of line on
  semicolon, or false otherwise."
  [s]
  (loop [c (.read s)]
    (cond (= c -1) :eos
          (eol? c) :eol
          (= c (int \;)) (do (skip-to-eol s) :eol)
          (whitespace? c) (recur (.read s))
          :else (do (.unread s c) false))))

(defn- read-hook
  "Read hook that keeps the compiler's line number in sync with that of our
  input stream, prompts only when there is nothing remaining to read on the
  previous input line, and calls the Clojure reader only when there's
  something interesting to read on the current line."
  [eof]
  (loop [c (skip-whitespace *in*)]
    (cond (= c :eos) eof
          (= c :eol)
          (do
            (print (repl-prompt))
            (flush)
            (var-set Compiler/LINE (.getLineNumber *in*))
            (recur (skip-whitespace *in*)))
          :else (read *in* false eof))))

;; Public

(defn set-repl-name
  "Sets the repl name. Include the following codes in the name to make the
  corresponding dynamic values part of it:

    %S - repl serial number
    %T - thread id
    %D - nesting depth in this thread

  The default name is \"repl-%S\""
  [name-fmt]
  (set-info! :name-fmt name-fmt)
  (loop [[[code fmt] & more] (seq +name-formats+)
         name-fmt name-fmt]
    (if code
      (recur more (.replace name-fmt code fmt))
      (set-info! :name-fmt-internal name-fmt)))
  (let [name (repl-name)]
    (set-info! :name name)
    (var-set Compiler/SOURCE name))
  nil)

(defn set-repl-prompt
  "Sets the repl prompt. Include the following codes in the prompt to make
  the corresponding dynamic values part of it:

    %S - repl serial number
    %T - thread id
    %D - nesting depth in this thread
    %L - input line number
    %N - namespace name

  The default prompt is \"%S:%L %N=> \""
  [prompt-fmt]
  (set-info! :prompt-fmt prompt-fmt)
  (loop [[[code fmt] & more] (seq +prompt-formats+)
         prompt-fmt prompt-fmt]
    (if code
      (recur more (.replace prompt-fmt code fmt))
      (set-info! :prompt-fmt-internal prompt-fmt)))
  nil)

(defn repl-info
  "Returns a map of info about the current repl"
  []
  (let [line (.getLineNumber *in*)]
    (dissoc (assoc *info* :line line)
            :name-fmt-internal :prompt-fmt-internal)))

(defn print-repl-info
  "Prints info about the current repl"
  []
  (let [{:keys [name started name-fmt prompt-fmt serial thread depth line]}
        (repl-info)]
    (printf
     (apply str (interleave +info-format+ (repeat "\n")))
     name started name-fmt prompt-fmt serial thread depth line)))

(defn repl
  "A repl that supports line numbers. The default prompt displays repl
  serial number, line number, and namespace. The default repl name contains
  the repl serial number. Thrown exceptions display the repl name and line
  number; metadata for defs made at the repl identify their origin by repl
  name and line number. Use set-repl-name and set-repl-prompt to customize
  the repl name and prompt"
  ([]
     (repl System/in System/out System/err RT/UTF8))
  ([in out err encoding]
     (try
      (Var/pushThreadBindings
       {RT/IN (LineNumberingPushbackReader.
               (InputStreamReader. in encoding))
        RT/OUT (OutputStreamWriter. out encoding)
        RT/ERR (PrintWriter. (OutputStreamWriter. err encoding) true)
        Compiler/SOURCE (var-get Compiler/SOURCE)
        Compiler/LINE (var-get Compiler/LINE)
        (var *info*) *info*})
      (set-info!
       :started (Date.)
       :serial (dosync (alter *serial-number* inc))
       :thread (.getId (Thread/currentThread))
       :depth (inc (:depth *info*)))
      (set-repl-name (:name-fmt *info*))
      (set-repl-prompt (:prompt-fmt *info*))
      ;; unread newline to enable first prompt
      (.unread *in* (int \newline))
      (clojure.main/repl
       :prompt #()
       :flush #()
       :read read-hook)
      (finally
       (Var/popThreadBindings)
       (prn)))))
