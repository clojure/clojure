;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Stuart Halloway

(ns clojure.test-clojure.protocols
  (:use clojure.test clojure.test-clojure.protocols.examples))

(defn method-names
  "return sorted list of method names on a class"
  [c]
  (->> (.getMethods c)
     (map #(.getName %))
     (sort)))

(deftest protocols-test
  (testing "protocol fns throw IllegalArgumentException if no impl matches"
    (is (thrown-with-msg?
          IllegalArgumentException
          #"No implementation of method: :foo of protocol: #'clojure.test-clojure.protocols.examples/AnExampleProtocol found for class: nil"
          (foo nil))))
  (testing "protocols generate a corresponding interface using _ instead of - for method names"
    (is (= ["bar" "baz" "baz" "foo" "with_quux"] (method-names clojure.test_clojure.protocols.examples.AnExampleProtocol))))
  (testing "protocol will work with instances of its interface (use for interop, not in Clojure!)"
    (let [obj (proxy [clojure.test_clojure.protocols.examples.AnExampleProtocol] []
                (foo [] "foo!"))]
      (is (= "foo!" (.foo obj)) "call through interface")
      (is (= "foo!" (foo obj)) "call through protocol")))
  (testing "you can implement just part of a protocol if you want"
    (let [obj (reify AnExampleProtocol
                     (baz [a b] "two-arg baz!"))]
      (is (= "two-arg baz!" (baz obj nil)))
      (is (thrown? AbstractMethodError (baz obj))))))


