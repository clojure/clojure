;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Stuart Halloway

(ns clojure.test-clojure.helpers
  (:use clojure.test))

(defn temp-ns
  "Create and return a temporary ns, using clojure.core + uses"
  [& uses]
  (binding [*ns* *ns*]
    (in-ns (gensym))
    (apply clojure.core/use 'clojure.core uses)
    *ns*))

(defmacro eval-in-temp-ns [& forms]
  `(binding [*ns* *ns*]
     (in-ns (gensym))
     (clojure.core/use 'clojure.core)
     (eval
      '(do ~@forms))))

