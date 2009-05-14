;;  Copyright (c) Frantisek Sodomka. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.

(ns clojure.contrib.test-clojure.test-utils
  (:use [clojure.contrib.combinatorics :only (combinations)]))


(defn exception
  "Use this function to ensure that execution of a program doesn't
  reach certain point."
  []
  (throw (new Exception "Exception which should never occur")))

