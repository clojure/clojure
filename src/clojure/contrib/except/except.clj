;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  except.clj
;;
;;  scgilardi (gmail)
;;  Created 07 July 2008

(ns clojure.contrib.except
  (:import (clojure.lang Reflector)))

(declare throw-formatted)

(defn throwf
  "Throws a formatted Exception or Error with an optional message formatted
  like clojure/printf. All arguments are optional:

      class? format? format-args*

  - class defaults to Exception
  - format is a format string for clojure/format
  - format-args are objects that correspond format specifiers in format."
  [& args]
  (apply throw-formatted "clojure.contrib.except.throwf" args))

(defn throw-if
  "Throws a formatted Exception or Error if test is true. args are those
  documented for throwf."
  [test & args]
  (when test
    (apply throw-formatted "clojure.contrib.except.throw_if" args)))

;; throw-if-not is synonymous with assert, but clojure/assert exists

(defn throw-if-not
  "Throws a formatted Exception or Error if test is false. args are those
  documented for throwf."
  [test & args]
  (when-not test
    (apply throw-formatted "clojure.contrib.except.throw_if_not" args)))

(defn- throw-formatted
  "Internal helper for formatted exceptions. It builds the formatted message,
  creates the exception object, and edits the exception's stack trace to
  exclude frames that are internal to our implementation. The stack trace
  will start with the line in the caller that contains the throwf,
  throw-if, or throw-if-not call."
  [fn-prefix & args]
  (let [[class & [fmt & fmt-args]]
         (if (class? (first args)) args (cons Exception args))
        args (into-array (if fmt [(apply format fmt fmt-args)] []))
        exception (Reflector/invokeConstructor class args)
        raw-trace (.getStackTrace exception)
        not-our-fn? #(not (.startsWith (.getClassName %) fn-prefix))
        trace (into-array (rrest (drop-while not-our-fn? raw-trace)))]
    (.setStackTrace exception trace)
    (throw exception)))
