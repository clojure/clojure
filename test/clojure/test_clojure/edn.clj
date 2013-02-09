;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Stuart Halloway


(ns clojure.test-clojure.edn
  (:require [clojure.test.generative :refer (defspec)]
            [clojure.test-clojure.generators :as cgen]
            [clojure.edn :as edn]))

(defn roundtrip
  "Print an object and read it back as edn. Returns rather than throws
   any exceptions."
  [o]
  (binding [*print-length* nil
            *print-dup* nil
            *print-level* nil]
    (try
     (-> o pr-str edn/read-string)
     (catch Throwable t t))))

(defspec types-that-should-roundtrip
  roundtrip
  [^{:tag cgen/ednable} o]
  (when-not (= o %)
    (throw (ex-info "Value cannot roundtrip, see ex-data" {:printed o :read %}))))

(defspec types-that-should-not-roundtrip
  roundtrip
  [^{:tag cgen/non-ednable} o]
  (when-not (instance? Throwable %)
    (throw (ex-info "edn/read should have thrown, see ex-data" {:printed o :read %}))))
