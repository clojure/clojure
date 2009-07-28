;;; logging.clj -- delegated logging for Clojure
 
;; by Alex Taggart
;; July 27, 2009
 
;; Copyright (c) Alex Taggart, July 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns 
  #^{:author "Alex Taggart, Timothy Pratley",
     :doc "Logging macros which delegate to a specific logging 
  implementation. At runtime a specific implementation is selected from, in 
  order, Apache commons-logging, log4j, and finally java.util.logging.
  
  Logging levels are specified by clojure keywords corresponding to the 
  values used in log4j/commons-logging:
    :trace, :debug, :info, :warn, :error, :fatal  
  
  Logging occurs with the log macro which writes either directly or via an 
  agent.  By default direct logging is disabled, but can be enabled via the 
  *allow-direct-logging* boolean ref.  If log is invoked within a transaction it
  will always use an agent.
  
  The logging macros will not evaluate their 'message' unless the specific 
  logging level is in effect.
  
  Alternately, you can use the spy function when you have code that needs to be
  evaluated, and also want to output its result to the debug log,
  
  Unless otherwise specified, the current namespace (as identified by *ns*) will
  be used as the log-name (similar to how the java class name is usually used).
  
  Use the enabled? function to write conditional code against the logging level 
  (beyond simply whether or not to call log, which is handled automatically).
  
  You can redirect all java writes of System.out and System.err to the log 
  system by calling log-capture!.  To rebind *out* and *err* to the log system 
  invoke with-logs.  In both cases a log-name (e.g., 'com.example.captured') 
  needs to be specified to namespace the output."}
  clojure.contrib.logging)


(defstruct log-system
  :name     ; the name of the logging system used
  :get-log  ; fn [name] to obtain a log by string name
  :enabled? ; fn [log lvl] to check if a particular level is emabled
  :write)   ; fn [log lvl msg ex] to a log a message


(defmacro commons-logging
  "Creates a log-system struct using the Apache commons-logging API, 
  if present; otherwise nil."
  []
  (try
    (import (org.apache.commons.logging LogFactory Log))
    `(letfn [(get-log# [name#]
               (LogFactory/getLog name#))
             (enabled?# [log# level#]
               (condp = level#
                 :trace (.isTraceEnabled log#)
                 :debug (.isDebugEnabled log#)
                 :info  (.isInfoEnabled  log#)
                 :warn  (.isWarnEnabled  log#)
                 :error (.isErrorEnabled log#)
                 :fatal (.isFatalEnabled log#)))
             (write# [log# level# msg# e#]
               (condp = level#
                 :trace (.trace log# msg# e#)
                 :debug (.debug log# msg# e#)
                 :info  (.info  log# msg# e#)
                 :warn  (.warn  log# msg# e#)
                 :error (.error log# msg# e#)
                 :fatal (.fatal log# msg# e#)))]
      (struct log-system "commons-logging" get-log# enabled?# write#))
    (catch Exception e nil)))


(defmacro log4j-logging
  "Creates a log-system struct using the log4j API, if present; otherwise nil."
  []
  (try
    (import (org.apache.log4j Logger Level))
    `(let [levels# {:trace Level/TRACE
                    :debug Level/DEBUG
                    :info  Level/INFO
                    :warn  Level/WARN
                    :error Level/ERROR
                    :fatal Level/FATAL}]
      (letfn [(get-log# [name#] 
                (Logger/getLogger name#))
              (enabled?# [log# level#] 
                (.isEnabledFor log# (levels# level#)))
              (write# [log# level# msg# e#]
                (if-not e#
                  (.log log# (levels# level#) msg#)
                  (.log log# (levels# level#) msg# e#)))]
        (struct log-system "log4j-logging" get-log# enabled?# write#)))
    (catch Exception e nil)))


(defmacro java-logging
  "Creates a log-system struct using the java.util.logging API."
  []
  (try
    (import (java.util.logging Logger Level))
    `(let [levels# {:trace Level/FINEST
                    :debug Level/FINE
                    :info  Level/INFO
                    :warn  Level/WARNING
                    :error Level/SEVERE
                    :fatal Level/SEVERE}]
      (letfn [(get-log# [name#] 
                (Logger/getLogger name#))
              (enabled?# [log# level#] 
                (.isLoggable log# (levels# level#)))
              (write# [log# level# msg# e#]
                (if-not e#
                  (.log log# (levels# level#) (str msg#))
                  (.log log# (levels# level#) (str msg#) e#)))]
        (struct log-system "java-logging" get-log# enabled?# write#)))
    (catch Exception e nil)))


(defmacro direct-log
  "Logs the message immediately if the specific logging level is enabled. Use 
  the log macro in prefernce to this."
  [level message throwable log-name system]
  `(let [log# ((~system :get-log) ~log-name)]
    (if ((~system :enabled?) log# ~level)
      ((~system :write) log# ~level ~message ~throwable))))


(defmacro send-log
  "Sends the message to a logging agent, which will in turn write to the log if 
  the specific logging level is enabled. Use the log macro in preference to 
  this."
  [level message throwable log-name system-agent]
  (defn agent-log [sys# lvl# msg# th# ln#]
    (let [log# ((sys# :get-log) ln#)]
      (if ((sys# :enabled?) log# lvl#)
        ((sys# :write) log# lvl# (force msg#) th#))
      sys#))
  `(send-off ~system-agent 
      agent-log ~level (delay ~message) ~throwable ~log-name))


(def *log-system*
  (ref (some eval ['(commons-logging)
                   '(log4j-logging)
                   '(java-logging)])))
               
(def *log-system-agent* (agent *log-system*))

(def *allow-direct-logging* (ref false))


(defmacro log
  "Logs a message, either directly or via an agent."
  ([level message]
    `(log ~level ~message nil))
  ([level message throwable]
    `(log ~level ~message ~throwable (str *ns*)))
  ([level message throwable log-name]
    `(if (and @*allow-direct-logging*
                (not (clojure.lang.LockingTransaction/isRunning)))
        (direct-log ~level ~message ~throwable ~log-name @*log-system*)
        (send-log ~level ~message ~throwable ~log-name *log-system-agent*))))


(defn enabled?
  "Returns true if the specific logging level is enabled.  This function should 
  only be necessary if one needs to execute alternate code paths beyond whether 
  the log should be written to."
  ([level]
    (enabled? level (str *ns*)))
  ([level log-name]
    (let [sys @*log-system*]
      ((sys :enabled?) ((sys :get-log) log-name) level))))


(defmacro spy
  "Evaluates expr and outputs the form and its result to the debug log; returns 
  the result of expr."
  [expr]
  `(let [a# ~expr] (log :debug (str '~expr " => " a#)) a#))


(defn log-stream
  "Creates a logging output stream that will output to the log."
  [level log-name]
  (java.io.PrintStream.
    (proxy [java.io.ByteArrayOutputStream] []
      (flush []
        (proxy-super flush)
        (let [s (.trim (.toString this))]
          (proxy-super reset)
          (if (> (.length s) 0)
            (log level s nil log-name)))))
    true))


(def *old-std-streams* (ref nil))


(defn log-capture!
  "Captures System/out and System/err, redirecting all writes of those streams 
  to :info and :error logging, respectively. The specified log-name value will 
  be used for all redirected logging. NOTE: this will not redirect output of 
  *out* or *err*; for that use with-logs" 
  [log-name]
  (dosync
    (let [new-out (log-stream :info log-name)
          new-err (log-stream :error log-name)]
      ; don't overwrite the original values
      (if (nil? @*old-std-streams*)
        (ref-set *old-std-streams* {:out System/out :err System/err})) 
      (System/setOut new-out)
      (System/setErr new-err))))


(defn log-uncapture!
  "Restores System/out and System/err to their original values."
  []
  (dosync
    (when-let [{old-out :out old-err :err} @*old-std-streams*]
      (ref-set *old-std-streams* nil)
      (System/setOut old-out)
      (System/setErr old-err))))


(defmacro with-logs
  "Evaluates exprs in a context in which *out* and *err* are bound to :info and 
  :error logging, respectively."
  [log-name & body]
  `(let [new-out# (java.io.PrintWriter. 
                    (log-stream :info ~log-name) true)
         new-err# (java.io.PrintWriter. 
                    (log-stream :error ~log-name) true)]
    (binding [*out* new-out# 
              *err* new-err#]
      ~@body)))
