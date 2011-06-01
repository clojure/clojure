(ns clojure.test-clojure.protocols.hash-collisions
  (:use clojure.test))

(defprotocol TestProtocol
  (method [this] "Test method"))

(deftype TestType1 [])

(deftype TestType2 [])

(def original-hash hash)

(defn alternate-hash [x]
  (if (or (= x TestType1) (= x TestType2))
    999 ;; artificial hash code, to cause a collision
    (original-hash x)))

(deftest protocols-with-hash-collisions
  (with-redefs [hash alternate-hash]
    (extend TestType1 TestProtocol {:method (constantly 1)})
    (extend TestType2 TestProtocol {:method (constantly 2)})
    (is (= 1 (method (TestType1.))))
    (is (= 2 (method (TestType2.))))))
