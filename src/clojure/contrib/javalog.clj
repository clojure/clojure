;;; javalog.clj -- convenient access to java.util.logging in Clojure

;; by Stuart Sierra <mail@stuartsierra.com>
;; April 8, 2008

;; Copyright (c) Stuart Sierra, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; This file defines some convenience functions for using the Java
;; logging framework from Clojure.  It is oriented towards simple
;; development and debugging rather than complex production
;; environments.


;; NOTES ADDED December 3, 2008
;;
;; This uses Logger.GLOBAL_LOGGER_NAME, which was introduced in
;; Java 6.  This library will not work on Java 5.
;;
;; With improved syntax for calling Java from Clojure such as
;; "(.method object)" and "ClassName/staticMember", this library is
;; less useful than it once was.  Consider it DEPRECATED.  I now
;; recommend using the Java logging API directly instead of this
;; library.  Better yet, use Apache Commons Logging,
;; <http://commons.apache.org/logging/>, which has a nicer API.


(ns clojure.contrib.javalog
            (:import 
             (java.util.logging Logger Level ConsoleHandler
                                FileHandler SimpleFormatter)))

(def 
 #^{:tag Logger
    :doc "The current java.util.logging.Logger.  By default, the
          global logger, modified by 'with-logger'."}
 *logger*
 (. Logger
    (getLogger
     (. Logger GLOBAL_LOGGER_NAME))))

(defmacro log-level
  "Translates 'level' (a lower-case keyword) into a static field of
  java.util.logging.Level, by name.  

  Example: (log-level :severe)  =>  java.util.logging.Level.SEVERE

  If 'level' is not a keyword, it is assumed to be a user-defined
  instance of java.util.logging.Level and is returned unchanged."
  [level]
  (if (keyword? level)
    `(. java.util.logging.Level
        ~(symbol (. (name level) (toUpperCase))))
    level))

(defn root-logger
  "Returns the root Logger instance."
  ([] (root-logger *logger*))
  ([logger] (let [parent (. logger (getParent))]
              (if parent 
                (recur parent) 
                logger))))

(defn set-console-log-level 
  "Attempts to set the level of the current logger and the root
  ConsoleHandler to 'level' (a java.util.logging.Level).  Useful for
  debugging at the REPL."
  [level]
  (let [console-handler 
        (some (fn [h] (if (instance? ConsoleHandler h) h))
              (. (root-logger) (getHandlers)))]
    (if console-handler
      (do (. *logger* (setLevel level))
          (. console-handler (setLevel level)))
      (throw (new Exception "No ConsoleHandler on root logger.")))))

(defn add-log-file
  "Attaches a log file, using SimpleFormatter, with the given level,
  to the named logger.  'level' defaults to ALL.  Note: multiple
  invocations will create multiple log files, with numbers appended to
  the names."
  ([logger-name filename]
     (add-log-file logger-name filename (. Level ALL)))
  ([logger-name filename level]
     (let [logger (. Logger (getLogger logger-name))
           handler (new FileHandler filename)]
       (. handler (setFormatter (new SimpleFormatter)))
       (. handler (setLevel level))
       (. logger (addHandler handler)))))

(defmacro with-logger
  "Executes 'body' with *logger* bound to a logger with the given name
  and level.  'level' is expanded with 'log-level'."
  [logger-name level & body]
  `(binding [*logger* (. Logger (getLogger ~logger-name))]
     (. *logger* (setLevel (log-level ~level)))
     ~@body))

(defmacro log
  "Logs a message to *logger*.  'level' is expanded with 'log-level'.
  Example: (log :severe \"Bad argument: \" object)" 
  [level & strings]
  `(. *logger* (log (log-level ~level) (str ~@strings))))
