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
;;  Flexible raising and handling of conditions. A condition is a map
;;  containing:
;;
;;    - keys and values specified as arguments to raise, and
;;    - a stack trace at key :stack-trace.
;;
;;  Note: requires AOT compilation.
;;
;;  scgilardi (gmail)
;;  Created 09 June 2009

(ns 
    #^{:author "Stephen C. Gilardi",
       :doc "Flexible raising and handling of conditions. A condition is a map
containing:
  - keys and values specified as arguments to raise, and
  - a stack trace at key :stack-trace.

Note: requires AOT compilation."}
  clojure.contrib.condition
  (:require clojure.contrib.condition.Condition)
  (:import clojure.contrib.condition.Condition))

(defmacro raise
  "Raises a condition with the supplied mappings.
  keyval => key val"
  [& keyvals]
  `(throw (Condition. (hash-map ~@keyvals))))

(defmacro handler-case
  "Executes body in a context in which any raised conditions can be handled.

  dispatch-fn accepts a raised condition: a map, and returns a value used
  to select a handler.

  The name specified by condition is bound to the condition within
  handlers.

  Handlers are forms within body:

    (handle key
      ...)

  If a condition is raised, executes the body of the first handler whose
  key satisfies (isa? selector key). If no handlers match, the condition is
  re-raised."
  [dispatch-fn condition & body]
  (let [selector (gensym "selector")]
    (loop [[form & forms] body
           m {:code [] :handlers []}]
      (if form
        (recur
         forms
         (if (and (list? form) (= (first form) 'handle))
           (let [[_ key & body] form
                 handler `[(isa? ~selector ~key) (do ~@body)]]
             (update-in m [:handlers] concat handler))
           (update-in m [:code] conj form)))
        (if (empty? (:handlers m))
          `(do ~@(:code m))
          `(try
            ~@(:code m)
            (catch Condition c#
              (let [~condition (meta c#)
                    ~selector (~dispatch-fn ~condition)]
                (cond ~@(:handlers m)
                      :else (throw c#))))))))))
