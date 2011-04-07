(ns lava.test-lava.protocols.examples)

(defprotocol ExampleProtocol
  "example protocol used by lava tests"

  (foo [a] "method with one arg")
  (bar [a b] "method with two args")
  (^String baz [a] [a b] "method with multiple arities")
  (with-quux [a] "method name with a hyphen"))

(definterface ExampleInterface
  (hinted [^int i])
  (hinted [^String s]))

