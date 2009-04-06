;;  Copyright (c) Stuart Halloway. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  clojure.contrib.test-contrib
;;
;;  Tests for the facilities provided by clojure-contrib
;;
;;  stuart.halloway (gmail)

(ns clojure.contrib.test-contrib
  (:use [clojure.contrib.test-is :only (run-tests)])
  (:gen-class))

(def test-names [:complex-numbers :monads :pprint.pretty :pprint.cl-format
                 :str-utils :shell-out :test-graph :test-dataflow :test-java-utils])

(def test-namespaces
     (map #(symbol (str "clojure.contrib.test-contrib." (name %)))
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


