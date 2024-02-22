;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.array-symbols
  (:use clojure.test))

(deftest test-array-symbols
  (is (= 'int::2 (read-string "int::2")))
  (is (= '[int 2] (clojure.lang.Compiler$HostExpr/decodeArraySymbolComponents 'int::2)))
  (is (= '[java.util.UUID 2] (clojure.lang.Compiler$HostExpr/decodeArraySymbolComponents 'java.util.UUID::2)))
  (is (nil? (clojure.lang.Compiler$HostExpr/decodeArraySymbolComponents 'int2)))
  (is (thrown? Exception (eval '(def int::2 2))))
  (is (thrown? Exception (eval '(.importClass *ns* 'int::2 java.util.UUID))))
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
                 (clojure.lang.Compiler$HostExpr/maybeArrayClass 'foo.bar.ThisIsNotAClassThatCouldBeFound138::2)))))
