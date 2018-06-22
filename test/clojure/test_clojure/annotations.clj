;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Authors: Stuart Halloway, Rich Hickey

(ns clojure.test-clojure.annotations
  (:use clojure.test))

(case (System/getProperty "java.specification.version")
      "1.8" (load "annotations/java_8")
      "9" (load "annotations/java_9")
      "10" (load "annotations/java_9")
      nil)

