(ns clojure.contrib.test-contrib.expect-test.util
 (:use clojure.test))

(defmacro assert-called [fn-name called? & body]
  `(let [called-status?# (atom false)]
     (binding [~fn-name (fn [& args#] (swap! called-status?# (fn [& args#] true)))] ~@body)
     (is (= ~called? @called-status?#))))

