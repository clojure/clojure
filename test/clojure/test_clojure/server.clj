;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Alex Miller

(ns clojure.test-clojure.server
    (:import java.util.Random)
    (:require [clojure.test :refer :all])
    (:require [clojure.core.server :as s]))

(defn check-invalid-opts
  [opts msg]
  (try
    (#'clojure.core.server/validate-opts opts)
    (is nil)
    (catch Exception e
      (is (= (ex-data e) opts))
      (is (= msg (.getMessage e))))))

(defn create-random-thread
  []
  (Thread.
    (fn []
      (let [random (new Random)]
      (while (not (.isInterrupted (Thread/currentThread)))
        (System/setProperty (Integer/toString (.nextInt random)) (Integer/toString (.nextInt random))))))))

(deftest test-validate-opts
  (check-invalid-opts {} "Missing required socket server property :name")
  (check-invalid-opts {:name "a" :accept 'clojure.core/+} "Missing required socket server property :port")
  (doseq [port [-1 "5" 999999]]
    (check-invalid-opts {:name "a" :port port :accept 'clojure.core/+} (str "Invalid socket server port: " port)))
  (check-invalid-opts {:name "a" :port 5555} "Missing required socket server property :accept"))

(deftest test-parse-props
  (let [thread (create-random-thread)]
    (.start thread)
    (Thread/sleep 1000)
    (try
      (is (>= (count
        (#'s/parse-props (System/getProperties))) 0))
      (finally (.interrupt thread)))))
