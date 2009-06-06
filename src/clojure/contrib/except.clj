;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  except.clj
;;
;;  Provides functions that make it easy to specify the class, cause, and
;;  message when throwing an Exception or Error. The optional message is
;;  formatted using clojure.core/format.
;;
;;  scgilardi (gmail)
;;  Created 07 July 2008

(ns 
  #^{:author "Stephen C. Gilardi",
     :doc "Provides functions that make it easy to specify the class, cause, and
message when throwing an Exception or Error. The optional message is
formatted using clojure.core/format."}
  clojure.contrib.except
  (:import (clojure.lang Reflector)))

(declare throwable)

(defn throwf
  "Throws an Exception or Error with an optional message formatted using
  clojure.core/format. All arguments are optional:

      class? cause? format? format-args*

  - class defaults to Exception, if present it must name a kind of
    Throwable
  - cause defaults to nil, if present it must be a Throwable
  - format is a format string for clojure.core/format
  - format-args are objects that correspond to format specifiers in
    format."
  [& args]
  (throw (throwable args)))

(defn throw-if
  "Throws an Exception or Error if test is true. args are those documented
  for throwf."
  [test & args]
  (when test
    (throw (throwable args))))

(defn throw-if-not
  "Throws an Exception or Error if test is false. args are those documented
  for throwf."
  [test & args]
  (when-not test
    (throw (throwable args))))

(defn throw-arg
  "Throws an IllegalArgumentException. All arguments are optional:

        cause? format? format-args*

  - cause defaults to nil, if present it must be a Throwable
  - format is a format string for clojure.core/format
  - format-args are objects that correspond to format specifiers in
    format."
  [& args]
  (throw (throwable (cons IllegalArgumentException args))))

(defn- throwable?
  "Returns true if x is a Throwable"
  [x]
  (instance? Throwable x))

(defn- throwable
  "Constructs a Throwable with optional cause and formatted message. Its
  stack trace will begin with our caller's caller. Args are as described
  for throwf except throwable accepts them as list rather than inline."
  [args]
  (let [[arg] args
        [class & args] (if (class? arg) args (cons Exception args))
        [arg] args
        [cause & args] (if (throwable? arg) args (cons nil args))
        message (when args (apply format args))
        ctor-args (into-array Object [message cause])
        throwable (Reflector/invokeConstructor class ctor-args)
        our-prefix "clojure.contrib.except$throwable"
        not-us? #(not (.startsWith (.getClassName %) our-prefix))
        raw-trace (.getStackTrace throwable)
        edited-trace (into-array StackTraceElement
                      (drop 3 (drop-while not-us? raw-trace)))]
    (.setStackTrace throwable edited-trace)
    throwable))
