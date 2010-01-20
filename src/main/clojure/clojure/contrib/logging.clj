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
     :doc
  "Logging macros which delegate to a specific logging implementation. At
  macro-expansion-time a specific implementation is selected from, in order,
  Apache commons-logging, log4j, and finally java.util.logging.
  
  Logging levels are specified by clojure keywords corresponding to the
  values used in log4j and commons-logging:
    :trace, :debug, :info, :warn, :error, :fatal
  
  Logging occurs with the log macro, or the level-specific convenience macros,
  which write either directly or via an agent.  For performance reasons, direct
  logging is enabled by default, but setting the *allow-direct-logging* boolean
  atom to false will disable it. If logging is invoked within a transaction it
  will always use an agent.
  
  The log macros will not evaluate their 'message' unless the specific logging
  level is in effect. Alternately, you can use the spy macro when you have code
  that needs to be evaluated, and also want to output the code and its result to
  the debug log.
  
  Unless otherwise specified, the current namespace (as identified by *ns*) will
  be used as the log-ns (similar to how the java class name is usually used).
  Note: your log configuration should display the name that was passed to the
  logging implementation, and not perform stack-inspection, otherwise you'll see
  something like \"fn__72$impl_write_BANG__39__auto____81\" in your logs.
  
  Use the enabled? macro to write conditional code against the logging level
  (beyond simply whether or not to call log, which is handled automatically).
  
  You can redirect all java writes of System.out and System.err to the log
  system by calling log-capture!.  To rebind *out* and *err* to the log system
  invoke with-logs.  In both cases a log-ns (e.g., \"com.example.captured\")
  needs to be specified to namespace the output."}
  clojure.contrib.logging)

(declare *impl-name* impl-get-log impl-enabled? impl-write!)

;; Macros used so that implementation-specific functions all have the same meta.

(defmacro def-impl-name
  {:private true} [& body]
  `(def
    #^{:doc "The name of the logging implementation used."}
    *impl-name*
    ~@body))

(defmacro def-impl-get-log
  {:private true} [& body]
  `(def
    #^{:doc
  "Returns an implementation-specific log by string namespace. End-users should
  not need to call this function."
       :arglist '([~'log-ns])}
    impl-get-log
    (memoize ~@body)))

(defmacro def-impl-enabled?
  {:private true} [& body]
  `(def
    #^{:doc
  "Implementation-specific check if a particular level is enabled. End-users
  should not need to call this function."
       :arglist '([~'log ~'level])}
    impl-enabled?
    ~@body))

(defmacro def-impl-write!
  {:private true} [& body]
  `(def
    #^{:doc
  "Implementation-specific write of a log message. End-users should not need to
  call this function."
       :arglist '([~'log ~'level ~'message ~'throwable])}
    impl-write!
    ~@body))

(defmacro commons-logging
  "Defines the commons-logging-based implementations of the core logging
  functions. End-users should never need to call this macro."
  {:private true}
  []
  (try
    (import (org.apache.commons.logging LogFactory Log))
    `(do
      (def-impl-name "org.apache.commons.logging")
      (def-impl-get-log
        (fn [log-ns#]
          (org.apache.commons.logging.LogFactory/getLog #^String log-ns#)))
      (def-impl-enabled?
        (fn [#^org.apache.commons.logging.Log log# level#]
          (condp = level#
            :trace (.isTraceEnabled log#)
            :debug (.isDebugEnabled log#)
            :info  (.isInfoEnabled  log#)
            :warn  (.isWarnEnabled  log#)
            :error (.isErrorEnabled log#)
            :fatal (.isFatalEnabled log#))))
      (def-impl-write!
        (fn [#^org.apache.commons.logging.Log log# level# msg# e#]
          (condp = level#
            :trace (.trace log# msg# e#)
            :debug (.debug log# msg# e#)
            :info  (.info  log# msg# e#)
            :warn  (.warn  log# msg# e#)
            :error (.error log# msg# e#)
            :fatal (.fatal log# msg# e#))))
      true)
    (catch Exception e nil)))


(defmacro log4j-logging
  "Defines the log4j-based implementations of the core logging functions.
   End-users should never need to call this macro."
  {:private true}
  []
  (try
    (import (org.apache.log4j Logger Level))
    `(do
      (def-impl-name "org.apache.log4j")
      (def-impl-get-log
        (fn [log-ns#]
          (org.apache.log4j.Logger/getLogger #^String log-ns#)))
      (let [levels# {:trace org.apache.log4j.Level/TRACE
                     :debug org.apache.log4j.Level/DEBUG
                     :info  org.apache.log4j.Level/INFO
                     :warn  org.apache.log4j.Level/WARN
                     :error org.apache.log4j.Level/ERROR
                     :fatal org.apache.log4j.Level/FATAL}]
        (def-impl-enabled?
          (fn [#^org.apache.log4j.Logger log# level#]
            (.isEnabledFor log# (levels# level#))))
        (def-impl-write!
          (fn [#^org.apache.log4j.Logger log# level# msg# e#]
            (if-not e#
              (.log log# (levels# level#) msg#)
              (.log log# (levels# level#) msg# e#)))))
      true)
    (catch Exception e nil)))


(defmacro java-logging
  "Defines the java-logging-based implementations of the core logging
  functions. End-users should never need to call this macro."
  {:private true}
  []
  (try
    (import (java.util.logging Logger Level))
    `(do
      (def-impl-name "java.util.logging")
      (def-impl-get-log
        (fn [log-ns#]
          (java.util.logging.Logger/getLogger log-ns#)))
      (let [levels# {:trace java.util.logging.Level/FINEST
                     :debug java.util.logging.Level/FINE
                     :info  java.util.logging.Level/INFO
                     :warn  java.util.logging.Level/WARNING
                     :error java.util.logging.Level/SEVERE
                     :fatal java.util.logging.Level/SEVERE}]
        (def-impl-enabled?
          (fn [#^java.util.logging.Logger log# level#]
            (.isLoggable log# (levels# level#))))
        (def-impl-write!
          (fn [#^java.util.logging.Logger log# level# msg# e#]
            (if-not e#
              (.log log# #^java.util.logging.Level (levels# level#)
                         #^String (str msg#))
              (.log log# #^java.util.logging.Level (levels# level#)
                         #^String (str msg#) #^Throwable e#)))))
      true)
    (catch Exception e nil)))


;; Initialize implementation-specific functions
(or (commons-logging)
    (log4j-logging)
    (java-logging)
    (throw ; this should never happen in 1.5+
      (RuntimeException.
        "Valid logging implementation could not be found.")))


(def #^{:doc
  "The default agent used for performing logging durng a transaction or when
  direct logging is disabled."}
  *logging-agent* (agent nil))


(def #^{:doc
  "A boolean indicating whether direct logging (as opposed to via an agent) is
  allowed when not operating from within a transaction. Defaults to true."}
  *allow-direct-logging* (atom true))


(defn agent-write!
  "Writes the message immediately, and ignores the first argument. Used by the
  logging agent. End-users should never need to call this function."
  [_ log level message throwable]
  (impl-write! log level message throwable))


(defmacro log
  "Logs a message, either directly or via an agent. Also see the level-specific
  convenience macros."
  ([level message]
    `(log ~level ~message nil))
  ([level message throwable]
    `(log ~level ~message ~throwable ~(str *ns*)))
  ([level message throwable log-ns]
    `(let [log# (impl-get-log ~log-ns)]
      (if (impl-enabled? log# ~level)
        (if (and @*allow-direct-logging*
                 (not (clojure.lang.LockingTransaction/isRunning)))
          (impl-write! log# ~level ~message ~throwable)
          (send-off *logging-agent*
            agent-write! log# ~level ~message ~throwable))))))


(defmacro enabled?
  "Returns true if the specific logging level is enabled.  Use of this function
  should only be necessary if one needs to execute alternate code paths beyond
  whether the log should be written to."
  ([level]
    `(enabled? ~level ~(str *ns*)))
  ([level log-ns]
    `(impl-enabled? (impl-get-log ~log-ns) ~level)))


(defmacro spy
  "Evaluates expr and outputs the form and its result to the debug log; returns 
  the result of expr."
  [expr]
  `(let [a# ~expr] (log :debug (str '~expr " => " a#)) a#))


(defn log-stream
  "Creates a PrintStream that will output to the log. End-users should not need
  to invoke this function."
  [level log-ns]
  (java.io.PrintStream.
    (proxy [java.io.ByteArrayOutputStream] []
      (flush []
        (proxy-super flush)
        (let [s (.trim (.toString #^java.io.ByteArrayOutputStream this))]
          (proxy-super reset)
          (if (> (.length s) 0)
            (log level s nil log-ns)))))
    true))


(def #^{:doc
  "A ref used by log-capture! to maintain a reference to the original System.out
  and System.err streams."
  :private true}
  *old-std-streams* (ref nil))


(defn log-capture!
  "Captures System.out and System.err, redirecting all writes of those streams
  to :info and :error logging, respectively. The specified log-ns value will
  be used to namespace all redirected logging. NOTE: this will not redirect
  output of *out* or *err*; for that, use with-logs."
  [log-ns]
  (dosync
    (let [new-out (log-stream :info log-ns)
          new-err (log-stream :error log-ns)]
      ; don't overwrite the original values
      (if (nil? @*old-std-streams*)
        (ref-set *old-std-streams* {:out System/out :err System/err})) 
      (System/setOut new-out)
      (System/setErr new-err))))


(defn log-uncapture!
  "Restores System.out and System.err to their original values."
  []
  (dosync
    (when-let [{old-out :out old-err :err} @*old-std-streams*]
      (ref-set *old-std-streams* nil)
      (System/setOut old-out)
      (System/setErr old-err))))


(defmacro with-logs
  "Evaluates exprs in a context in which *out* and *err* are bound to :info and
  :error logging, respectively. The specified log-ns value will be used to
  namespace all redirected logging."
  [log-ns & body]
  (if (and log-ns (seq body))
    `(binding [*out* (java.io.OutputStreamWriter.
                       (log-stream :info ~log-ns))
               *err* (java.io.OutputStreamWriter.
                       (log-stream :error ~log-ns))]
      ~@body)))

(defmacro trace
  "Logs a message at the trace level."
  ([message]
    `(log :trace ~message))
  ([message throwable]
    `(log :trace ~message ~throwable)))

(defmacro debug
  "Logs a message at the debug level."
  ([message]
    `(log :debug ~message))
  ([message throwable]
    `(log :debug ~message ~throwable)))

(defmacro info
  "Logs a message at the info level."
  ([message]
    `(log :info ~message))
  ([message throwable]
    `(log :info ~message ~throwable)))

(defmacro warn
  "Logs a message at the warn level."
  ([message]
    `(log :warn ~message))
  ([message throwable]
    `(log :warn ~message ~throwable)))

(defmacro error
  "Logs a message at the error level."
  ([message]
    `(log :error ~message))
  ([message throwable]
    `(log :error ~message ~throwable)))

(defmacro fatal
  "Logs a message at the fatal level."
  ([message]
    `(log :fatal ~message))
  ([message throwable]
    `(log :fatal ~message ~throwable)))
