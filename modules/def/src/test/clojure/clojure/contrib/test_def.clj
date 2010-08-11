;; Tests for def.clj

;; by Stuart Halloway

;; Copyright (c) Stuart Halloway, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.test-def
  (:use clojure.test)
  (:require [clojure.contrib.def :as d]))

(defn sample-fn "sample-fn docstring" [])
(d/defalias aliased-fn sample-fn)
(defmacro sample-macro "sample-macro-docstring" [])
(d/defalias aliased-macro sample-macro)

(deftest defalias-preserves-metadata
  (let [preserved-meta #(-> % (meta) (select-keys [:doc :arglists :ns :file :macro]))]
    (are [x y] (= (preserved-meta (var x)) (preserved-meta (var y)))
         aliased-fn sample-fn
         aliased-macro sample-macro)))

