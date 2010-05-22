(ns clojure.test-clojure.protocols.examples)

(defprotocol ExampleProtocol
  "example protocol used by clojure tests"

  (foo [a] "method with one arg")
  (bar [a b] "method with two args")
  (^String baz [a] [a b] "method with multiple arities")
  (with-quux [a] "method name with a hyphen"))

(definterface ExampleInterface
  (hinted [^int i])
  (hinted [^String s]))

