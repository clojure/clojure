;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  clojure.contrib.test-clojure
;;
;;  Tests for the facilities provided by Clojure
;;
;;  scgilardi (gmail)
;;  Created 22 October 2008

(ns clojure.contrib.test-clojure
  (:use [clojure.contrib.test-is :only (run-tests)])
  (:gen-class))

(def test-names
     [:reader
      :printer
      :evaluation
      :predicates
      :logic
      :data-structures
      :numbers
      :sequences
      :for
      :agents
      :main
      ])

(def test-namespaces
     (map #(symbol (str "clojure.contrib.test-clojure." (name %)))
          test-names))

(defn run
  "Runs all defined tests"
  []
  (println "Loading tests...")
  (apply require :reload-all test-namespaces)
  (apply run-tests test-namespaces))

(defn -main
  "Run all defined tests from the command line"
  [& args]
  (run)
  (System/exit 0))
