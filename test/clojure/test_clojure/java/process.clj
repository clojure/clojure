;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.java.process
  (:require
    [clojure.test :refer :all]
    [clojure.java.process :as p]
    [clojure.string :as str]))

(deftest test-stderr-redirect
  ;; capture to stdout and return string
  (is (not (str/blank? (p/exec "bash" "-c" "ls"))))

  ;; print to stderr, capture nil
  (is (nil? (p/exec "bash" "-c" "ls >&2")))

  ;; redirect, then capture to string
  (is (not (str/blank? (p/exec {:err :stdout} "bash" "-c" "ls >&2")))))


