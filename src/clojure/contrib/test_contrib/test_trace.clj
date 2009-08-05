(ns clojure.contrib.test-contrib.test-trace
  (:use clojure.test
        [clojure.contrib trace str-utils]))

(deftrace call-myself [n]
  (when-not (< n 1)
    (call-myself (dec n))))

(deftest test-tracing-a-function-that-calls-itself
  (let [output (with-out-str (call-myself 1))]
    (is (re-find #"^TRACE t\d+: (call-myself 1)\nTRACE t\d+: |    (call-myself 0)\nTRACE t\d+: |    => nil\nTRACE t\d+: => nil$"
                 output))))