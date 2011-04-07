(ns lava.test-lava.protocols.more-examples)

(defprotocol SimpleProtocol
  "example protocol used by lava tests. Note that
   foo collides with examples/ExampleProtocol."

  (foo [a] ""))
