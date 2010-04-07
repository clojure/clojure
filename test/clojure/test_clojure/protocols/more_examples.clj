(ns clojure.test-clojure.protocols.more-examples)

(defprotocol SimpleProtocol
  "example protocol used by clojure tests. Note that
   foo collides with examples/ExampleProtocol."

  (foo [a] ""))
