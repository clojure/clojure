;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  test.clj
;;
;;  A Clojure implementation of Datalog -- Tests
;;
;;  straszheimjeffrey (gmail)
;;  Created 11 Feburary 2009

(ns clojure.contrib.datalog.tests.test
  (:use [clojure.test :only (run-tests)])
  (:gen-class))

(def test-names [:test-util
                 :test-database
                 :test-literals
                 :test-rules
                 :test-magic
                 :test-softstrat])

(def test-namespaces
     (map #(symbol (str "clojure.contrib.datalog.tests." (name %)))
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


;; End of file
