;   Copyright (c) Christophe Grand, November 2008. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; misc agent utilities

;; note to other contrib members: feel free to add to this lib

(ns
    #^{:author "Christophe Grande",
       :doc "Miscellaneous agent utilities
 (note to other contrib members: feel free to add to this lib)",
}
  clojure.contrib.agent-utils)

(defmacro capture-and-send
 "Capture the current value of the specified vars and rebind 
  them on the agent thread before executing the action.
  
  Example:
    (capture-and-send [*out*] a f b c)"
    
 [vars agent action & args]
  (let [locals (map #(gensym (name %)) vars)]
    `(let [~@(interleave locals vars)
           action# (fn [& args#]
                     (binding [~@(interleave vars locals)]
                       (apply ~action args#)))]
       (send ~agent action# ~@args))))
