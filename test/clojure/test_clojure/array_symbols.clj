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
  (is (nil? (clojure.lang.Compiler$HostExpr/decodeArraySymbolComponents 'int2))))
