;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Authors: Stuart Halloway, Frantisek Sodomka

(ns clojure.test-clojure.metadata
  (:use clojure.test))

(def public-namespaces
  '[clojure.core
    clojure.pprint
    clojure.inspector
    clojure.set
    clojure.stacktrace
    clojure.test
    clojure.walk
    clojure.xml
    clojure.zip])

(doseq [ns public-namespaces]
  (require ns))

(def public-vars
  (mapcat #(vals (ns-publics %)) public-namespaces))

(def public-vars-with-docstrings
  (filter (comp :doc meta) public-vars))

(deftest public-vars-with-docstrings-have-added
  (is (= [] (remove (comp :added meta) public-vars-with-docstrings))))
