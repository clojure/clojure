;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.array-symbols
  (:use clojure.test)
  (:require [clojure.test-helper :as util]))

(set! *warn-on-reflection* true)

(deftest test-array-symbols
  (is (= 'java.lang.String/1 (read-string "java.lang.String/1")))
  (is (= 'String/1 (read-string "String/1")))
  (is (= 'int/2 (read-string "int/2")))
  (testing "array symbol resolution"
    (are [str-repr klass] (= (Class/forName str-repr) klass)
      "[Z" (resolve 'boolean/1)
      "[B" (resolve 'byte/1)
      "[C" (resolve 'char/1)
      "[S" (resolve 'short/1)
      "[F" (resolve 'float/1)
      "[D" (resolve 'double/1)
      "[I" (resolve 'int/1)
      "[J" (resolve 'long/1)
      "[[J" (resolve 'long/2)
      "[Ljava.lang.Object;" (resolve 'Object/1)
      "[Ljava.lang.String;" (resolve 'String/1)
      "[[Ljava.lang.String;" (resolve 'String/2))
    (is (thrown? ClassNotFoundException (resolve 'ThisIsNotAClassThatCouldBeFound138/2)))
    (is (thrown? ClassNotFoundException (resolve 'foo.bar.ThisIsNotAClassThatCouldBeFound138/2))))
  (testing "array hints"
    (util/should-not-reflect
     (let [^long/1 a (long-array [1 2 3 4 99 100])]
       (java.util.Arrays/binarySearch a 99))))
  (testing "syntax quote"
    (is (= `byte/1 'byte/1))
    (is (= `byte/9 'byte/9))
    (is (= `java.util.UUID/1 'java.util.UUID/1))
    (is (= `String/1 'java.lang.String/1)))
  (testing "resolution"
    (is (= (eval 'long/1) (class (make-array Long/TYPE 0))))
    (is (= (resolve 'long/1) (class (make-array Long/TYPE 0))))
    (is (= (resolve 'String/1) (class (make-array String 0))))
    (is (= (resolve 'java.lang.String/1) (class (make-array String 0))))
    (is (= (resolve 'java.util.UUID/1) (class (make-array java.util.UUID 0))))
    (is (= (resolve 'String/2)
           (class (into-array (class (make-array String 0)) [(into-array String ["a" "b"])])))))
  (testing "value position"
    (is (= (class (make-array String 0)) String/1))
    (is (= [(class (make-array String 0))] [String/1])))
  (testing "printing"
    (is (= "long/1" (print-str long/1)))
    (is (= "byte/1" (print-str (class (make-array Byte/TYPE 0)))))
    (is (= "java.lang.String/2" (print-str String/2)))
    (is (= "[[java.lang.String/2]]" (print-str [[String/2]])))
    (is (= "java.util.UUID/4" (print-str java.util.UUID/4)))
    (is (= "[[[[[[[[[[Ljava.lang.Object;" (print-str (Class/forName "[[[[[[[[[[Ljava.lang.Object;"))))
    (is (= "java.lang.Object/9" (print-str (Class/forName "[[[[[[[[[Ljava.lang.Object;")))))
  (testing "error conditions"
    (is (thrown? Exception (read-string "String/1:")))
    (is (thrown? Exception (read-string "String/0")))
    (is (thrown? Exception (read-string "String/42")))
    (is (thrown? Exception (eval '(deftype Foo/2 [a]))))))

(definterface ArrayClassSymbolFoo (^String/1 bar []))
(definterface ArrayClassSymbolFooAsHint (^ArrayClassSymbolFoo/1 baz []))

(deftest test-definterface-acs
  (testing "definterface"
    (let [obj (reify ArrayClassSymbolFoo (bar [this] (into-array String ["a"])))]
      (is (= ["a"] (seq (.bar obj)))))
    (let [obj (reify ArrayClassSymbolFooAsHint
                (baz [this]
                  (into-array ArrayClassSymbolFoo [(reify ArrayClassSymbolFoo
                                                     (bar [this] (into-array String ["a"])))])))]
      (is (= ["a"] (let [^ArrayClassSymbolFoo acsf (first (.baz obj))]
                    (seq (.bar acsf))))))))
