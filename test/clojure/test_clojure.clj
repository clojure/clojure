;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;

;;  clojure.test-clojure
;;
;;  Tests for the facilities provided by Clojure
;;
;;  scgilardi (gmail)
;;  Created 22 October 2008

(ns clojure.test-clojure
  (:require [clojure.test :as t])
  (:gen-class))

(def test-names
     [:reader
      :printer
      :compilation
      :evaluation
      :special
      :macros
      :metadata
      :ns-libs
      :logic
      :predicates
      :control
      :data-structures
      :numbers
      :sequences
      :for
      :multimethods
      :other-functions
      :vars
      :refs
      :agents
      :atoms
      :parallel
      :java-interop
      :test
      :test-fixtures
      ;; libraries
      :clojure-set
      :clojure-xml
      :clojure-zip
      :protocols
      :genclass
      :main
      :vectors
      :annotations
      :pprint
      :serialization
      :rt
      :repl
      :java.io
      :string
      :java.javadoc
      :java.shell
      :transients
      :def
      ])

(def test-namespaces
     (map #(symbol (str "clojure.test-clojure." (name %)))
          test-names))

(defn run
  "Runs all defined tests"
  []
  (println "Loading tests...")
  (apply require :reload-all test-namespaces)
  (apply t/run-tests test-namespaces))

(defn run-ant
  "Runs all defined tests, prints report to *err*, throw if failures. This works well for running in an ant java task."
  []
  (let [rpt t/report]
    (binding [;; binding to *err* because, in ant, when the test target
              ;; runs after compile-clojure, *out* doesn't print anything
              *out* *err*
              t/*test-out* *err*
              t/report (fn report [m]
                         (if (= :summary (:type m))
                           (do (rpt m)
                               (if (or (pos? (:fail m)) (pos? (:error m)))
                                 (throw (new Exception (str (:fail m) " failures, " (:error m) " errors.")))))
                           (rpt m)))]
      (run))))

(defn -main
  "Run all defined tests from the command line"
  [& args]
  (run)
  (System/exit 0))
