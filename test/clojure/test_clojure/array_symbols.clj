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
  (is (= 'int::2 (read-string "int::2")))
  (is (thrown? Exception (read-string "String::-2")))
  (is (thrown? Exception (read-string "String::foo")))
  (is (thrown? Exception (read-string "String::1:")))
  (is (thrown? Exception (eval '(def int::2 2))))
  (is (thrown? Exception (eval '(deftype Foo::2 [a]))))
  (is (thrown? Exception (eval '(.importClass *ns* 'int::2 java.util.UUID))))
  (is (thrown? IllegalArgumentException (.addAlias *ns* 'String::1 *ns*)))
  (testing "array symbol resolution"
    (are [str-repr klass] (= (Class/forName str-repr) klass)
      "[Z" (clojure.lang.Compiler$HostExpr/maybeArrayClass 'boolean::1)
      "[B" (clojure.lang.Compiler$HostExpr/maybeArrayClass 'byte::1)
      "[C" (clojure.lang.Compiler$HostExpr/maybeArrayClass 'char::1)
      "[S" (clojure.lang.Compiler$HostExpr/maybeArrayClass 'short::1)
      "[F" (clojure.lang.Compiler$HostExpr/maybeArrayClass 'float::1)
      "[D" (clojure.lang.Compiler$HostExpr/maybeArrayClass 'double::1)
      "[I" (clojure.lang.Compiler$HostExpr/maybeArrayClass 'int::1)
      "[J" (clojure.lang.Compiler$HostExpr/maybeArrayClass 'long::1)
      "[[J" (clojure.lang.Compiler$HostExpr/maybeArrayClass 'long::2)
      "[Ljava.lang.Object;" (clojure.lang.Compiler$HostExpr/maybeArrayClass 'Object::1)
      "[Ljava.lang.String;" (clojure.lang.Compiler$HostExpr/maybeArrayClass 'String::1)
      "[[Ljava.lang.String;" (clojure.lang.Compiler$HostExpr/maybeArrayClass 'String::2))
    (is (nil? (clojure.lang.Compiler$HostExpr/maybeArrayClass 'Object)))
    (is (nil? (clojure.lang.Compiler$HostExpr/maybeArrayClass 'foo/java.lang.String::1)))
    (is (nil? (clojure.lang.Compiler$HostExpr/maybeArrayClass 'ThisIsNotAClassThatCouldBeFound138::2)))
    (is (thrown? ClassNotFoundException
                 (clojure.lang.Compiler$HostExpr/maybeArrayClass 'foo.bar.ThisIsNotAClassThatCouldBeFound138::2))))
  (testing "array hints"
    (util/should-not-reflect
     (let [^long::1 a (long-array [1 2 3 4 99 100])]
       (java.util.Arrays/binarySearch a 99))))
  (testing "syntax quote"
    (is (= `byte::1 'byte::1))
    (is (= `byte::33 'byte::33))
    (is (= `java.util.UUID::1 'java.util.UUID::1))
    (is (= `String::1 'java.lang.String::1)))
  (testing "resolve"
    (is (= (resolve 'long::1) (class (make-array Long/TYPE 0))))
    (is (= (resolve 'String::1) (class (make-array String 0))))
    (is (= (resolve 'java.lang.String::1) (class (make-array String 0))))
    (is (= (resolve 'java.util.UUID::1) (class (make-array java.util.UUID 0))))
    (is (= (resolve 'String::2)
           (class (into-array (class (make-array String 0)) [(into-array String ["a" "b"])])))))
  (testing "value position"
    (is (= 42 (let [long::2 42] long::2)))
    (is (= (class (make-array String 0)) String::1))
    (is (= [(class (make-array String 0))] [String::1])))
  (testing "printing"
    (is (= "long::1" (print-str long::1)))
    (is (= "byte::1" (print-str (class (make-array Byte/TYPE 0)))))
    (is (= "java.lang.String::2" (print-str String::2)))
    (is (= "[[java.lang.String::2]]" (print-str [[String::2]])))
    (is (= "java.util.UUID::4" (print-str java.util.UUID::4)))))
