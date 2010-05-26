;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.java.javadoc
  (:use clojure.test
	[clojure.java.javadoc :as j])
  (:import (java.io File)))

(deftest javadoc-url-test
  (testing "for a core api"
    (binding [*feeling-lucky* false]
      (are [x y] (= x (#'j/javadoc-url y))
           nil "foo.Bar"
           (str *core-java-api* "java/lang/String.html") "java.lang.String")))
  (testing "for a remote javadoc"
    (binding [*remote-javadocs* (ref (sorted-map "java." "http://example.com/"))]
      (is (= "http://example.com/java/lang/Number.html" (#'j/javadoc-url "java.lang.Number"))))))
