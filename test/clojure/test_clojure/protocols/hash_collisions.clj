(ns clojure.test-clojure.protocols.hash-collisions
  (:use clojure.test))

(defprotocol TestProtocolA
  (method-a [this] "Test method A"))

(defprotocol TestProtocolB
  (method-b [this] "Test method B"))

(deftype TestType1 [])
(deftype TestType2 [])
(deftype TestType3 [])
(deftype TestType4 [])
(deftype TestType5 [])
(deftype TestType6 [])
(deftype TestType7 [])
(deftype TestType8 [])
(deftype TestType9 [])
(deftype TestType10 [])
(deftype TestType11 [])
(deftype TestType12 [])
(deftype TestType13 [])
(deftype TestType14 [])
(deftype TestType15 [])

(def original-hash hash)

(defn colliding-hash
  "Mock hash function which returns identical hash codes for the
  classes TestType1 and TestType2, normal hashes for everything else."
  [x]
  (if (or (= x TestType1) (= x TestType2))
    999 ;; artificial hash code, to cause a collision
    (original-hash x)))

(deftest protocols-with-hash-collisions
  (with-redefs [hash colliding-hash]
    (extend TestType1 TestProtocolA {:method-a (constantly 1)})
    (extend TestType2 TestProtocolA {:method-a (constantly 2)})
    (is (= 1 (method-a (TestType1.))))
    (is (= 2 (method-a (TestType2.))))))

(defn no-min-hash-in-13-bits
  "Mock hash function which returns hash codes for the classes
  TestType1 through TestType15 such that they cannot be compressed
  into a 13-bit min-hash table. Returns normal hashes for everything
  else."
  [x]
  (cond
   (= x TestType1) 1
   (= x TestType2) 2
   (= x TestType3) 4
   (= x TestType4) 8
   (= x TestType5) 16
   (= x TestType6) 32
   (= x TestType7) 64
   (= x TestType8) 128
   (= x TestType9) 256
   (= x TestType10) 512
   (= x TestType11) 1024
   (= x TestType12) 2048
   (= x TestType13) 4096
   (= x TestType14) 8192
   (= x TestType15) 16384
   :else (original-hash x)))

(deftest protocols-with-no-min-hash-in-13-bits
  (with-redefs [hash no-min-hash-in-13-bits]
    (extend TestType1 TestProtocolB {:method-b (constantly 1)})
    (extend TestType2 TestProtocolB {:method-b (constantly 2)})
    (extend TestType3 TestProtocolB {:method-b (constantly 3)})
    (extend TestType4 TestProtocolB {:method-b (constantly 4)})
    (extend TestType5 TestProtocolB {:method-b (constantly 5)})
    (extend TestType6 TestProtocolB {:method-b (constantly 6)})
    (extend TestType7 TestProtocolB {:method-b (constantly 7)})
    (extend TestType8 TestProtocolB {:method-b (constantly 8)})
    (extend TestType9 TestProtocolB {:method-b (constantly 9)})
    (extend TestType10 TestProtocolB {:method-b (constantly 10)})
    (extend TestType11 TestProtocolB {:method-b (constantly 11)})
    (extend TestType12 TestProtocolB {:method-b (constantly 12)})
    (extend TestType13 TestProtocolB {:method-b (constantly 13)})
    (extend TestType14 TestProtocolB {:method-b (constantly 14)})
    (extend TestType15 TestProtocolB {:method-b (constantly 15)})
    (is (= 1 (method-b (TestType1.))))
    (is (= 2 (method-b (TestType2.))))
    (is (= 3 (method-b (TestType3.))))
    (is (= 4 (method-b (TestType4.))))
    (is (= 5 (method-b (TestType5.))))
    (is (= 6 (method-b (TestType6.))))
    (is (= 7 (method-b (TestType7.))))
    (is (= 8 (method-b (TestType8.))))
    (is (= 9 (method-b (TestType9.))))
    (is (= 10 (method-b (TestType10.))))
    (is (= 11 (method-b (TestType11.))))
    (is (= 12 (method-b (TestType12.))))
    (is (= 13 (method-b (TestType13.))))
    (is (= 14 (method-b (TestType14.))))
    (is (= 15 (method-b (TestType15.))))))
