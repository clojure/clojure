(ns clojure.test-clojure (:require clojure.test))

(def test-namespaces '[
clojure.test-clojure.agents
clojure.test-clojure.annotations
clojure.test-clojure.atoms
clojure.test-clojure.clojure-set
clojure.test-clojure.clojure-walk                       
clojure.test-clojure.clojure-xml
clojure.test-clojure.clojure-zip
clojure.test-clojure.compilation
clojure.test-clojure.control
clojure.test-clojure.data
clojure.test-clojure.data-structures
clojure.test-clojure.def
clojure.test-clojure.errors
clojure.test-clojure.evaluation
clojure.test-clojure.for
clojure.test-clojure.genclass.examples
clojure.test-clojure.genclass
clojure.test-clojure.java.io
clojure.test-clojure.java.javadoc
clojure.test-clojure.java.shell
clojure.test-clojure.java-interop
clojure.test-clojure.keywords
clojure.test-clojure.load
clojure.test-clojure.logic
clojure.test-clojure.macros
clojure.test-clojure.main
clojure.test-clojure.metadata
clojure.test-clojure.multimethods
clojure.test-clojure.ns-libs
clojure.test-clojure.numbers
clojure.test-clojure.other-functions
clojure.test-clojure.parallel
clojure.test-clojure.pprint
clojure.test-clojure.predicates
clojure.test-clojure.printer
clojure.test-clojure.protocols
clojure.test-clojure.protocols.hash-collisions
clojure.test-clojure.reader
clojure.test-clojure.reflect
clojure.test-clojure.refs
clojure.test-clojure.repl
clojure.test-clojure.rt
clojure.test-clojure.sequences
clojure.test-clojure.serialization
clojure.test-clojure.special
clojure.test-clojure.string
clojure.test-clojure.test
clojure.test-clojure.test-fixtures
clojure.test-clojure.transients
clojure.test-clojure.try-catch
clojure.test-clojure.vars
clojure.test-clojure.vectors
])

(apply require test-namespaces)

(let [results (apply clojure.test/run-tests test-namespaces)]
  (System/exit (+ (:error results) (:fail results))))
