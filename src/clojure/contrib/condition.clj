;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  condition.clj
;;
;;  scgilardi (gmail)
;;  Created 09 June 2009

(ns #^{:author "Stephen C. Gilardi"
       :doc "Flexible raising and handling of conditions:

Functions:

              raise: raises a condition
       handler-case: dispatches raised conditions to appropriate handlers
  print-stack-trace: prints abbreviated or full condition stack traces

Data:

  A condition is a map containing values for these keys:

    - :type, a condition type specifier, typically a keyword
    - :stack-trace, a stack trace to the site of the raise
    - :message, a human-readable message (optional)
    - :cause, a wrapped exception or condition (optional)
    - other keys given as arguments to raise (optional)

Note: requires AOT compilation.

Based on an idea from Chouser:
http://groups.google.com/group/clojure/browse_frm/thread/da1285c538f22bb5"}
  clojure.contrib.condition
  (:require clojure.contrib.condition.Condition)
  (:import clojure.contrib.condition.Condition
           clojure.lang.IPersistentMap)
  (:use (clojure.contrib
         [def :only (defvar)]
         [seq-utils :only (separate)])))

(defvar *condition*
  "While a handler is running, bound to the condition being handled")

(defvar *selector*
  "While a handler is running, bound to the selector returned by the
  handler-case dispatch-fn for *condition*")

(defvar *condition-object*
  "While a handler is running, bound to the Condition object whose metadata
  is the condition")

(defvar *full-stack-traces* false
  "Bind to true to include clojure.{core,lang,main} frames in stack
  traces")

(defmacro raise
  "Raises a condition. With no arguments, re-raises the current condition.
  With one argument (a map), raises the argument. With two or more
  arguments, raises a map with keys and values from the arguments."
  ([]
     `(throw *condition-object*))
  ([m]
     `(throw (Condition. ~m)))
  ([key val & keyvals]
     `(raise (hash-map ~key ~val ~@keyvals))))

(defmacro handler-case
  "Executes body in a context where raised conditions can be handled.

  dispatch-fn accepts a raised condition (a map) and returns a selector
  used to choose a handler. Commonly, dispatch-fn will be :type to dispatch
  on the condition's :type value.

  Handlers are forms within body:

    (handle key
      ...)

  If a condition is raised, executes the body of the first handler whose
  key satisfies (isa? selector key). If no handlers match, re-raises the
  condition.

  While a handler is running, *condition* is bound to the condition being
  handled and *selector* is bound to to the value returned by dispatch-fn
  that matched the handler's key."
  [dispatch-fn & body]
  (let [[handlers code]
        (separate #(and (list? %) (= 'handle (first %))) body)]
    `(try
      ~@code
      (catch Condition c#
        (binding [*condition-object* c#
                  *condition* ^c#
                  *selector* (~dispatch-fn ^c#)]
          (cond
           ~@(mapcat
              (fn [[_ key & body]]
                `[(isa? *selector* ~key) (do ~@body)])
              handlers)
           :else (raise)))))))

(defmulti stack-trace-info
  "Returns header, stack-trace, and cause info from conditions and
  Throwables"
  class)

(defmethod stack-trace-info IPersistentMap
  [condition]
  [(format "condition: %s, %s" (:type condition)
           (dissoc condition :type :stack-trace :cause))
   (:stack-trace condition)
   (:cause condition)])

(defmethod stack-trace-info Condition
  [condition]
  (stack-trace-info ^condition))

(defmethod stack-trace-info Throwable
  [throwable]
  [(str throwable)
   (.getStackTrace throwable)
   (.getCause throwable)])

(defn print-stack-trace
  "Prints a stack trace for a condition or Throwable. Skips frames for
  classes in clojure.{core,lang,main} unless the *full-stack-traces* is
  bound to logical true"
  [x]
  (let [[header frames cause] (stack-trace-info x)]
    (printf "%s\n" header)
    (doseq [frame frames]
      (let [classname (.getClassName frame)]
        (if (or *full-stack-traces*
                (not (re-matches
                      #"clojure.(?:core|lang|main).*" classname)))
          (printf "        at %s/%s(%s:%s)\n"
                  classname
                  (.getMethodName frame)
                  (.getFileName frame)
                  (.getLineNumber frame)))))
    (when cause
      (printf "caused by: ")
      (recur cause))))
