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
  (:use clojure.contrib.condition))

(defn func [x y]
  (if (neg? x)
    (raise :source ::Args :arg 'x :value x :message "shouldn't be negative")
    (+ x y)))

(defn main
  []
  
  ;; simple handler
  
  (handler-case :source
    (println (func 3 4))
    (println (func -5 10))
    (handle ::Args
      (printf "Bad argument: %s\n" *condition*)))

  ;; demonstrate nested handlers

  (handler-case :source
    (handler-case :source
      (println (func 8 2))
      (println (func -6 17))
      ;; no handler for ::Args
      (handle ::nested
        (printf "I'm nested: %s\n" *condition*)))
    (println (func 3 4))
    (println (func -5 10))
    (handle ::Args
      (printf "Bad argument: %s\n" *condition*))))
