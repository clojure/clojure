;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Test proxy classes that are AOT-compiled for the tests in
           clojure.test-clojure.java-interop."
      :author "Ambrose Bonnaire-Sergeant"}
  clojure.test-clojure.proxy.examples)

(definterface A
  (^int a [^String x])
  (^boolean a [^java.io.File x])
  (^boolean a [^Runnable x])
  (a [^Boolean x])
  (^int a [^Boolean x ^String y])
  (^int a [^String x ^String y])
  (^boolean a [^String x ^java.io.File y])
  (^boolean a [^String x ^Runnable y])
  (b [^String x])
  (c [^String x])
  (d [^String x]))

(def ^String proxy1-class-name
  (-> (proxy [A] [])
      class
      .getName))
