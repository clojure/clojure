;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  clojure.contrib.condition.example.clj
;;
;;  scgilardi (gmail)
;;  Created 09 June 2009

(ns clojure.contrib.condition.example
  (:use (clojure.contrib
         [condition
          :only (handler-case print-stack-trace raise *condition*)])))

(defn func [x y]
  "Raises an exception if x is negative"
  (when (neg? x)
    (raise :type :illegal-argument :arg 'x :value x))
  (+ x y))

(defn main
  []
  
  ;; simple handler
  
  (handler-case :type
    (println (func 3 4))
    (println (func -5 10))
    (handle :illegal-argument
            (print-stack-trace *condition*))
    (println 3))

  ;; multiple handlers
  
  (handler-case :type
    (println (func 4 1))
    (println (func -3 22))
    (handle :overflow
      (print-stack-trace *condition*))
    (handle :illegal-argument
      (print-stack-trace *condition*)))

  ;; nested handlers

  (handler-case :type
    (handler-case :type
      nil
      nil
      (println 1)
      (println 2)
      (println 3)
      (println (func 8 2))
      (println (func -6 17))
      ;; no handler for :illegal-argument
      (handle :overflow
        (println "nested")
        (print-stack-trace *condition*)))
    (println (func 3 4))
    (println (func -5 10))
    (handle :illegal-argument
      (println "outer")
      (print-stack-trace *condition*))))
