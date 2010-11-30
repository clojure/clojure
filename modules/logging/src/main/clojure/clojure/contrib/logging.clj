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
  ^{:author
  "Alex Taggart, with contributions and suggestions by Chris Dean, Phil
  Hagelberg, Richard Newman, and Timothy Pratley",
    :doc
  "Logging macros which delegate to a specific logging implementation. At
  runtime a specific implementation is selected from, in order, Apache
  commons-logging, slf4j, log4j, and finally java.util.logging.

  Logging levels are specified by clojure keywords corresponding to the
  values used in log4j and commons-logging:
    :trace, :debug, :info, :warn, :error, :fatal

  Logging occurs with the log macro, or the level-specific convenience macros,
  which write either directly or via an agent.  See log* for more details
  regarding direct vs agent logging.

  The log macros will not evaluate their 'message' unless the specific logging
  level is in effect. Alternately, you can use the spy macro when you have code
  that needs to be evaluated, and also want to output the code and its result to
  the log.

  Unless otherwise specified, the current namespace (as identified by *ns*) will
  be used as the log-ns (similar to how the java class name is usually used).
  Note: your log configuration should display the name that was passed to the
  logging implementation, and not perform stack-inspection, otherwise you'll see
  some ugly and unhelpful text in your logs.

  Use the enabled? macro to write conditional code against the logging level
  (beyond simply whether or not to call log, which is handled automatically).

  You can redirect all java writes of System.out and System.err to the log
  system by calling log-capture!.  To bind *out* and *err* to the log system
  invoke with-logs.  In both cases a log-ns (e.g., \"com.example.captured\")
  must be specified in order to namespace the output.

  For those new to using a java logging library, the following is a very basic
  configuration for log4j. Place it in a file called \"log4j.properties\"
  and place that file (and the log4j JAR) on the classpath.
    log4j.rootLogger=WARN, A1
    log4j.logger.user=DEBUG
    log4j.appender.A1=org.apache.log4j.ConsoleAppender
    log4j.appender.A1.layout=org.apache.log4j.PatternLayout
    log4j.appender.A1.layout.ConversionPattern=%d %-5p %c: %m%n
  The above will print messages to the console for :debug or higher if one is
  in the user namespace, and :warn or higher in all other namespaces."}
  clojure.contrib.logging
  [:use [clojure.pprint :only [code-dispatch pprint with-pprint-dispatch]]])

(defprotocol Log
  "The protocol through which macros will interact with an underlying logging
  implementation.  Implementations should at least support the six specified
  logging levels if they wish to benefit from the level-specific macros."
  (impl-enabled? [log level]
    "Implementation-specific check if a particular level is enabled. End-users
    should not need to call this.")
  (impl-write! [log level throwable message]
    "Implementation-specific write of a log message. End-users should not need
    to call this."))

(defprotocol LogFactory
  "The protocol through which macros will obtain an instance satisfying Log as
  well as providing information about the particular implementation being used.
  Implementations should be bound to *log-factory* in order to be picked up by
  this library."
  (impl-name [factory]
    "Returns some text identifying the underlying implementation.")
  (impl-get-log [factory log-ns]
    "Returns an implementation-specific Log by namespace. End-users should not
    need to call this."))

(def ^{:doc
  "The default agent used for performing logging when direct logging is
  disabled. See log* for details."}
  *logging-agent* (agent nil :error-mode :continue))

(def ^{:doc
  "The set of levels that will require using an agent when logging from within a
  running transaction. Defaults to #{:info :warn}. See log* for details."}
  *tx-agent-levels* #{:info :warn})

(def ^{:doc
  "Overrides the default rules for choosing between logging directly or via an
  agent. Defaults to nil. See log* for details."}
  *force* nil)

(defn log*
  "Attempts to log a message, either directly or via an agent; does not check if
  the level is enabled.

  For performance reasons, an agent will only be used when invoked within a
  running transaction, and only for logging levels specified by
  *tx-agent-levels*. This allows those entries to only be written once the
  transaction commits, and are discarded if it is retried or aborted.  As
  corollary, other levels (e.g., :debug, :error) will be written even from
  failed transactions though at the cost of repeat messages during retries.

  One can override the above by setting *force* to :direct or :agent; all
  subsequent writes will be direct or via an agent, respectively."
  [log level throwable message]
  (if (cond
        (nil? *force*) (and (clojure.lang.LockingTransaction/isRunning)
                         (*tx-agent-levels* level))
        (= *force* :agent) true
        (= *force* :direct) false)
    (send-off *logging-agent*
      (fn [_#] (impl-write! log level throwable message)))
    (impl-write! log level throwable message)))

(declare *log-factory*) ; default LogFactory instance for calling impl-get-log

(defmacro log
  "Evaluates and logs a message only if the specified level is enabled. See log*
  for more details."
  ([level message]
    `(log ~level nil ~message))
  ([level throwable message]
    `(log ~*ns* ~level ~throwable ~message))
  ([log-ns level throwable message]
    `(log *log-factory* ~log-ns ~level ~throwable ~message))
  ([log-factory log-ns level throwable message]
    `(let [log# (impl-get-log ~log-factory ~log-ns)]
       (if (impl-enabled? log# ~level)
         (log* log# ~level ~throwable ~message)))))

(defmacro logp
  "Logs a message using print style args. Can optionally take a throwable as its
  second arg. See level-specific macros, e.g., debug."
  {:arglists '([level message & more] [level throwable message & more])}
  [level x & more]
  (if (or (instance? String x) (nil? more)) ; optimize for common case
    `(log ~level (print-str ~x ~@more))
    `(let [log# (impl-get-log *log-factory* ~*ns*)]
       (if (impl-enabled? log# ~level)
         (if (instance? Throwable ~x) ; type check only when enabled
           (log* log# ~level ~x (print-str ~@more))
           (log* log# ~level (print-str ~x ~@more)))))))

(defmacro logf
  "Logs a message using a format string and args. Can optionally take a
  throwable as its second arg. See level-specific macros, e.g., debugf."
  {:arglists '([level fmt & fmt-args] [level throwable fmt & fmt-args])}
  [level x & more]
  (if (or (instance? String x) (nil? more)) ; optimize for common case
    `(log ~level (format ~x ~@more))
    `(let [log# (impl-get-log *log-factory* ~*ns*)]
       (if (impl-enabled? log# ~level)
         (if (instance? Throwable ~x) ; type check only when enabled
           (log* log# ~level ~x (format ~(first more) ~@(next more)))
           (log* log# ~level (format ~x ~@more)))))))

(defmacro enabled?
  "Returns true if the specific logging level is enabled.  Use of this function
  should only be necessary if one needs to execute alternate code paths beyond
  whether the log should be written to."
  ([level]
    `(enabled? ~level ~*ns*))
  ([level log-ns]
    `(impl-enabled? (impl-get-log *log-factory* ~log-ns) ~level)))

(defmacro spy
  "Evaluates expr and writes the form and its result to the log. Returns the
  result of expr. Defaults to debug log level."
  ([expr]
    `(spy :debug ~expr))
  ([level expr]
    `(let [a# ~expr]
       (log ~level
         (let [s# (with-out-str
                    (with-pprint-dispatch code-dispatch ; need a better way
                      (pprint '~expr)
                      (print "=> ")
                      (pprint a#)))]
           (.substring s# 0 (dec (count s#))))) ; trim off the trailing newline
       a#)))

(defn log-stream
  "Creates a PrintStream that will output to the log at the specified level."
  [level log-ns]
  (let [log (impl-get-log *log-factory* log-ns)]
    (java.io.PrintStream.
      (proxy [java.io.ByteArrayOutputStream] []
        (flush []
          ; deal with reflection in proxy-super
          (let [^java.io.ByteArrayOutputStream this this]
            (proxy-super flush)
            (let [message (.trim (.toString this))]
              (proxy-super reset)
              (if (> (.length message) 0)
                (log* log level nil message))))))
      true)))

(let [orig (atom nil)    ; holds original System.out and System.err
      monitor (Object.)] ; sync monitor for calling setOut/setErr
  (defn log-capture!
    "Captures System.out and System.err, piping all writes of those streams to
    the log. If unspecified, levels default to :info and :error, respectively.
    The specified log-ns value will be used to namespace all log entries.

    Note: use with-logs to redirect output of *out* or *err*.

    Warning: if the logging implementation is configured to output to System.out
    (as is the default with java.util.logging) then using this function will
    result in StackOverflowException when writing to the log."
    ; Implementation Notes:
    ; - only set orig when nil to preserve original out/err
    ; - no enabled? check before making streams since that may change later
    ([log-ns]
      (log-capture! log-ns :info :error))
    ([log-ns out-level err-level]
      (locking monitor
        (compare-and-set! orig nil [System/out System/err])
        (System/setOut  (log-stream out-level log-ns))
        (System/setErr (log-stream err-level log-ns)))))
  (defn log-uncapture!
    "Restores System.out and System.err to their original values."
    []
    (locking monitor
      (when-let [[out err :as v] @orig]
        (swap! orig (constantly nil))
        (System/setOut out)
        (System/setErr err)))))

(defmacro with-logs
  "Evaluates exprs in a context in which *out* and *err* write to the log. The
  specified log-ns value will be used to namespace all log entries.

  By default *out* and *err* write to :info and :error, respectively."
  {:arglists '([log-ns & body]
               [[log-ns out-level err-level] & body])}
  [arg & body]
  ; Implementation Notes:
  ; - no enabled? check before making writers since that may change later
  (let [[log-ns out-level err-level] (if (vector? arg)
                                       arg
                                       [arg :info :error])]
    (if (and log-ns (seq body))
      `(binding [*out* (java.io.OutputStreamWriter.
                         (log-stream ~out-level ~log-ns))
                 *err* (java.io.OutputStreamWriter.
                         (log-stream ~err-level ~log-ns))]
         ~@body))))


;; level-specific macros

(defmacro trace
  "Trace level logging using print-style args."
  {:arglists '([message & more] [throwable message & more])}
  [& args]
  `(logp :trace ~@args))

(defmacro debug
  "Debug level logging using print-style args."
  {:arglists '([message & more] [throwable message & more])}
  [& args]
  `(logp :debug ~@args))

(defmacro info
  "Info level logging using print-style args."
  {:arglists '([message & more] [throwable message & more])}
  [& args]
  `(logp :info ~@args))

(defmacro warn
  "Warn level logging using print-style args."
  {:arglists '([message & more] [throwable message & more])}
  [& args]
  `(logp :warn ~@args))

(defmacro error
  "Error level logging using print-style args."
  {:arglists '([message & more] [throwable message & more])}
  [& args]
  `(logp :error ~@args))

(defmacro fatal
  "Fatal level logging using print-style args."
  {:arglists '([message & more] [throwable message & more])}
  [& args]
  `(logp :fatal ~@args))

(defmacro tracef
  "Trace level logging using format."
  {:arglists '([fmt & fmt-args] [throwable fmt & fmt-args])}
  [& args]
  `(logf :trace ~@args))

(defmacro debugf
  "Debug level logging using format."
  {:arglists '([fmt & fmt-args] [throwable fmt & fmt-args])}
  [& args]
  `(logf :debug ~@args))

(defmacro infof
  "Info level logging using format."
  {:arglists '([fmt & fmt-args] [throwable fmt & fmt-args])}
  [& args]
  `(logf :info ~@args))

(defmacro warnf
  "Warn level logging using format."
  {:arglists '([fmt & fmt-args] [throwable fmt & fmt-args])}
  [& args]
  `(logf :warn ~@args))

(defmacro errorf
  "Error level logging using format."
  {:arglists '([fmt & fmt-args] [throwable fmt & fmt-args])}
  [& args]
  `(logf :error ~@args))

(defmacro fatalf
  "Fatal level logging using format."
  {:arglists '([fmt & fmt-args] [throwable fmt & fmt-args])}
  [& args]
  `(logf :fatal ~@args))


;; Implementations of Log and LogFactory protocols:

(defn commons-logging
  "Returns a commons-logging-based implementation of the LogFactory protocol, or
  nil if not available. End-users should not need to call this."
  []
  (try
    (Class/forName "org.apache.commons.logging.Log")
    (eval
      '(do
         (extend-type org.apache.commons.logging.Log
           Log
           (impl-enabled? [log# level#]
             (condp = level#
               :trace (.isTraceEnabled log#)
               :debug (.isDebugEnabled log#)
               :info  (.isInfoEnabled  log#)
               :warn  (.isWarnEnabled  log#)
               :error (.isErrorEnabled log#)
               :fatal (.isFatalEnabled log#)
               (throw (IllegalArgumentException. (str level#)))))
           (impl-write! [log# level# e# msg#]
             (condp = level#
               :trace (.trace log# msg# e#)
               :debug (.debug log# msg# e#)
               :info  (.info  log# msg# e#)
               :warn  (.warn  log# msg# e#)
               :error (.error log# msg# e#)
               :fatal (.fatal log# msg# e#)
               (throw (IllegalArgumentException. (str level#))))))
         (reify LogFactory
           (impl-name [_#]
             "org.apache.commons.logging")
           (impl-get-log [_# log-ns#]
             (org.apache.commons.logging.LogFactory/getLog (str log-ns#))))))
    (catch Exception e nil)))

(defn slf4j-logging
  "Returns a SLF4J-based implementation of the LogFactory protocol, or nil if
  not available. End-users should not need to call this."
  []
  (try
    (Class/forName "org.slf4j.Logger")
    (eval
      '(do
        (extend-type org.slf4j.Logger
          Log
          (impl-enabled? [log# level#]
            (condp = level#
              :trace (.isTraceEnabled log#)
              :debug (.isDebugEnabled log#)
              :info  (.isInfoEnabled  log#)
              :warn  (.isWarnEnabled  log#)
              :error (.isErrorEnabled log#)
              :fatal (.isErrorEnabled log#)
              (throw (IllegalArgumentException. (str level#)))))
          (impl-write! [^org.slf4j.Logger log# level# ^Throwable e# msg#]
            (let [^String msg# (str msg#)]
              (condp = level#
                :trace (.trace log# msg# e#)
                :debug (.debug log# msg# e#)
                :info  (.info  log# msg# e#)
                :warn  (.warn  log# msg# e#)
                :error (.error log# msg# e#)
                :fatal (.error log# msg# e#)
                (throw (IllegalArgumentException. (str level#)))))))
        (reify LogFactory
          (impl-name [_#]
            "org.slf4j")
          (impl-get-log [_# log-ns#]
            (org.slf4j.LoggerFactory/getLogger ^String (str log-ns#))))))
    (catch Exception e nil)))

(defn log4j-logging
  "Returns a log4j-based implementation of the LogFactory protocol, or nil if
  not available. End-users should not need to call this."
  []
  (try
    (Class/forName "org.apache.log4j.Logger")
    (eval
      '(let [levels# {:trace org.apache.log4j.Level/TRACE
                      :debug org.apache.log4j.Level/DEBUG
                      :info  org.apache.log4j.Level/INFO
                      :warn  org.apache.log4j.Level/WARN
                      :error org.apache.log4j.Level/ERROR
                      :fatal org.apache.log4j.Level/FATAL}]
         (extend-type org.apache.log4j.Logger
           Log
           (impl-enabled? [log# level#]
             (.isEnabledFor log#
               (or
                 (levels# level#)
                 (throw (IllegalArgumentException. (str level#))))))
           (impl-write! [log# level# e# msg#]
             (let [level# (or
                            (levels# level#)
                            (throw (IllegalArgumentException. (str level#))))]
               (if-not e#
                 (.log log# level# msg#)
                 (.log log# level# msg# e#)))))
         (reify LogFactory
           (impl-name [_#]
             "org.apache.log4j")
           (impl-get-log [_# log-ns#]
             (org.apache.log4j.Logger/getLogger ^String (str log-ns#))))))
    (catch Exception e nil)))

(defn java-util-logging
  "Returns a java.util.logging-based implementation of the LogFactory protocol,
  or nil if not available. End-users should not need to call this."
  []
  (try
    (Class/forName "java.util.logging.Logger")
    (eval
      '(let [levels# {:trace java.util.logging.Level/FINEST
                      :debug java.util.logging.Level/FINE
                      :info  java.util.logging.Level/INFO
                      :warn  java.util.logging.Level/WARNING
                      :error java.util.logging.Level/SEVERE
                      :fatal java.util.logging.Level/SEVERE}]
         (extend-type java.util.logging.Logger
           Log
           (impl-enabled? [log# level#]
             (.isLoggable log#
               (or
                 (levels# level#)
                 (throw (IllegalArgumentException. (str level#))))))
           (impl-write! [log# level# ^Throwable e# msg#]
             (let [^java.util.logging.Level level#
                   (or
                     (levels# level#)
                     (throw (IllegalArgumentException. (str level#))))
                   ^String msg# (str msg#)]
               (if e#
                 (.log log# level# msg# e#)
                 (.log log# level# msg#)))))
         (reify LogFactory
           (impl-name [_#]
             "java.util.logging")
           (impl-get-log [_# log-ns#]
             (java.util.logging.Logger/getLogger (str log-ns#))))))
    (catch Exception e nil)))

(defn find-factory
  "Returns the first LogFactory found that is available from commons-logging,
  slf4j-logging, log4j-logging, or java-util-logging. End-users should not need
  to call this."
  []
  (or (commons-logging)
    (slf4j-logging)
    (log4j-logging)
    (java-util-logging)
    (throw ; this should never happen in 1.5+
      (RuntimeException.
        "Valid logging implementation could not be found."))))

(def ^{:doc
  "An instance satisfying the LogFactory protocol. Used internally when needing
  to obtain an instance satisfying the Log protocol. Defaults to the value
  returned from find-factory. Can be rebound to provide alternate logging
  implementations"}
  *log-factory*
  (find-factory))
