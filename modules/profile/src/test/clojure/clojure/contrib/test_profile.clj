(ns clojure.contrib.test-profile
  (:use clojure.test
	clojure.contrib.profile))

(deftest test-print-summary
  (testing "doesn't blow up with no data (assembla #31)"
    (is (= "Name      mean       min       max     count       sum\n"
           (with-out-str (print-summary {}))))))
