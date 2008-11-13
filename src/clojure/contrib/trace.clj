;;; trace.clj -- simple call-tracing macros for Clojure

;; by Stuart Sierra, http://stuartsierra.com/
;; June 9, 2008

;; Copyright (c) 2008 Stuart Sierra. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; This file defines simple "tracing" macros to help you see what your
;; code is doing.


(ns clojure.contrib.trace)

(def
 #^{:doc "PrintStream for trace output.  Defaults to System.err."}
 *trace-out* (. System err))

(defmacro trace
  "Prints value of expr to standard error and returns it.  Can be
  inserted anywhere without affecting surrounding code.  Optional
  'name' argument can be used to identify what is being traced."
  ([expr]
     `(let [value# ~expr]
        (. *trace-out* (println
                        (str "TRACE: " (pr-str value#))))
        value#))
  ([name expr]
     `(let [value# ~expr]
        (. *trace-out* (println
                        (str "TRACE " ~name ": " (pr-str value#))))
        value#)))

(defmacro deftrace
  "Use in place of defn; traces each call/return of this fn, including
  arguments."
  [name & definition]
  `(let [f# (fn ~@definition)]
     (defn ~name [& args#]
       (let [id# (gensym "t")]  ; identifier for this invocation
         (. *trace-out*
            (println (str "TRACE " id# ": " ~(str name)
                          " called with " (pr-str args#))))
         (let [value# (apply f# args#)]  ; call original fn
           (. *trace-out*
              (println (str "TRACE " id# ": " ~(str name)
                            " returned " (pr-str value#))))
           value#)))))
