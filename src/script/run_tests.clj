(ns lava.test-lava (:require lava.test))

(def test-namespaces '[
lava.test-lava.agents
lava.test-lava.annotations
lava.test-lava.atoms
lava.test-lava.lava-set
lava.test-lava.lava-walk                       
lava.test-lava.lava-xml
lava.test-lava.lava-zip
lava.test-lava.compilation
lava.test-lava.control
lava.test-lava.data
lava.test-lava.data-structures
lava.test-lava.def
lava.test-lava.errors
lava.test-lava.evaluation
lava.test-lava.for
lava.test-lava.genclass.examples
lava.test-lava.genclass
lava.test-lava.java.io
lava.test-lava.java.javadoc
lava.test-lava.java.shell
lava.test-lava.java-interop
lava.test-lava.keywords
lava.test-lava.load
lava.test-lava.logic
lava.test-lava.macros
lava.test-lava.main
lava.test-lava.metadata
lava.test-lava.multimethods
lava.test-lava.ns-libs
lava.test-lava.numbers
lava.test-lava.other-functions
lava.test-lava.parallel
lava.test-lava.pprint
lava.test-lava.predicates
lava.test-lava.printer
lava.test-lava.protocols
lava.test-lava.reader
lava.test-lava.reflect
lava.test-lava.refs
lava.test-lava.repl
lava.test-lava.rt
lava.test-lava.sequences
lava.test-lava.serialization
lava.test-lava.special
lava.test-lava.string
lava.test-lava.test
lava.test-lava.test-fixtures
lava.test-lava.transients
lava.test-lava.vars
lava.test-lava.vectors
])

(apply require test-namespaces)

(let [results (apply lava.test/run-tests test-namespaces)]
  (System/exit (+ (:error results) (:fail results))))
