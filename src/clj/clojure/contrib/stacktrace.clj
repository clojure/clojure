;;; stacktrace.clj: print Clojure-centric stack traces

;; by Stuart Sierra, http://stuartsierra.com/
;; January 6, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns 
  #^{:author "Stuart Sierra",
     :doc "Print Clojure-centric stack traces"}
  clojure.contrib.stacktrace)

(defn root-cause
  "Returns the last 'cause' Throwable in a chain of Throwables."
  [tr]
  (if-let [cause (.getCause tr)]
    (recur cause)
    tr))

(defn print-trace-element
  "Prints a Clojure-oriented view of one element in a stack trace."
  [e]
  (let [class (.getClassName e)
	method (.getMethodName e)] 
    (let [match (re-matches #"^([A-Za-z0-9_.-]+)\$(\w+)__\d+$" class)]
      (if (and match (= "invoke" method))
	(apply printf "%s/%s" (rest match))
	(printf "%s.%s" class method))))
  (printf " (%s:%d)" (or (.getFileName e) "") (.getLineNumber e)))

(defn print-throwable
  "Prints the class and message of a Throwable."
  [tr]
  (printf "%s: %s" (.getName (class tr)) (.getMessage tr)))

(defn print-stack-trace
  "Prints a Clojure-oriented stack trace of tr, a Throwable.
  Prints a maximum of n stack frames (default: unlimited).
  Does not print chained exceptions (causes)."
  ([tr] (print-stack-trace tr nil))
  ([tr n]
     (let [st (.getStackTrace tr)]
       (print-throwable tr)
       (newline)
       (print " at ") 
       (print-trace-element (first st))
       (newline)
       (doseq [e (if (nil? n)
		   (rest st)
		   (take (dec n) (rest st)))]
	 (print "    ")
	 (print-trace-element e)
	 (newline)))))

(defn print-cause-trace
  "Like print-stack-trace but prints chained exceptions (causes)."
  ([tr] (print-cause-trace tr nil))
  ([tr n]
     (print-stack-trace tr n)
     (when-let [cause (.getCause tr)]
       (print "Caused by: " )
       (recur cause n))))

(defn e
  "REPL utility.  Prints a brief stack trace for the root cause of the
  most recent exception."  
  []
  (print-stack-trace (root-cause *e) 8))
