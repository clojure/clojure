;;; trace.clj -- simple call-tracing macros for Clojure

;; by Stuart Sierra, http://stuartsierra.com/
;; December 3, 2008

;; Copyright (c) Stuart Sierra, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; This file defines simple "tracing" macros to help you see what your
;; code is doing.


;; CHANGE LOG
;;
;; December 3, 2008:
;;
;;   * replaced *trace-out* with tracer
;;
;;   * made trace a function instead of a macro 
;;     (suggestion from Stuart Halloway)
;;
;;   * added trace-fn-call
;;
;; June 9, 2008: first version



(ns 
  ^{:author "Stuart Sierra, Michel Salim",
     :doc "This file defines simple \"tracing\" macros to help you see what your
code is doing."}
  clojure.contrib.trace)

(def
 ^{:doc "Current stack depth of traced function calls."}
 *trace-depth* 0)

(defn tracer
  "This function is called by trace.  Prints to standard output, but
  may be rebound to do anything you like.  'name' is optional."
  [name value]
  (println (str "TRACE" (when name (str " " name)) ": " value)))

(defn trace
  "Sends name (optional) and value to the tracer function, then
  returns value.  May be wrapped around any expression without
  affecting the result."
  ([value] (trace nil value))
  ([name value]
     (tracer name (pr-str value))
     value))

(defn trace-indent
  "Returns an indentation string based on *trace-depth*"
  []
  (apply str (take *trace-depth* (repeat "|    "))))

(defn trace-fn-call
  "Traces a single call to a function f with args.  'name' is the
  symbol name of the function."
  [name f args]
  (let [id (gensym "t")]
    (tracer id (str (trace-indent) (pr-str (cons name args))))
    (let [value (binding [*trace-depth* (inc *trace-depth*)]
                  (apply f args))]
      (tracer id (str (trace-indent) "=> " (pr-str value)))
      value)))

(defmacro deftrace
  "Use in place of defn; traces each call/return of this fn, including
  arguments.  Nested calls to deftrace'd functions will print a
  tree-like structure."
  [name & definition]
  `(do
     (def ~name)
     (let [f# (fn ~@definition)]
       (defn ~name [& args#]
         (trace-fn-call '~name f# args#)))))

(defmacro dotrace
  "Given a sequence of function identifiers, evaluate the body
   expressions in an environment in which the identifiers are bound to
   the traced functions.  Does not work on inlined functions,
   such as clojure.core/+"
  [fnames & exprs]
  `(binding [~@(interleave fnames
                           (for [fname fnames]
                             `(let [f# @(var ~fname)]
                                (fn [& args#]
                                  (trace-fn-call '~fname f# args#)))))]
     ~@exprs))
