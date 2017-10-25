;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.api
  (:require [clojure.test.generative :refer (defspec)]
            [clojure.test-clojure.generators :as cgen])
  (:import clojure.lang.IFn
           clojure.java.api.Clojure
           clojure.lang.Var))

(set! *warn-on-reflection* true)

(defn roundtrip
  "Print an object and read it back with Clojure/read"
  [o]
  (binding [*print-length* nil
            *print-dup* nil
            *print-level* nil]
    (Clojure/read (pr-str o))))

(defn api-var-str
  [^Var v]
  (Clojure/var (str (.name (.ns v)))
               (str (.sym v))))

(defn api-var
  [^Var v]
  (Clojure/var (.name (.ns v))
               (.sym v)))

(defspec api-can-read
  roundtrip
  [^{:tag cgen/ednable} o]
  (when-not (= o %)
    (throw (ex-info "Value cannot roundtrip with Clojure/read" {:printed o :read %}))))

(defspec api-can-find-var
  api-var
  [^{:tag cgen/var} v]
  (when-not (= v %)
    (throw (ex-info "Var cannot roundtrip through Clojure/var" {:from v :to %}))))

(defspec api-can-find-var-str
  api-var-str
  [^{:tag cgen/var} v]
  (when-not (= v %)
    (throw (ex-info "Var cannot roundtrip strings through Clojure/var" {:from v :to %}))))


