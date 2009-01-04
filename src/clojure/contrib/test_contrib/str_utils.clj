(ns clojure.contrib.test-contrib.str-utils
    (:use clojure.contrib.test-is
          clojure.contrib.str-utils))


(deftest test-re-gsub
  (let [re #"\%([0-9a-fA-F]{2})"
        replacement (fn [match] 
                      (char (Integer/parseInt 
                              (match 1) 16)))]
    (is (= (re-gsub re replacement "") ""))
    (is (= (re-gsub re replacement "%20") " "))
    (is (= (re-gsub re replacement "x%20") "x "))
    (is (= (re-gsub re replacement "x%20%0a") "x \n"))
    (is (= (re-gsub re replacement "x%20y") "x y"))
    (is (= (re-gsub re "?" "") ""))
    (is (= (re-gsub re "?" "%21") "?"))
    (is (= (re-gsub re "?" "x%22") "x?"))
    (is (= (re-gsub re "?" "x%23y") "x?y"))))

(deftest test-re-sub
  (let [re #"\%([0-9a-fA-F]{2})"
        replacement (fn [match] 
                      (char (Integer/parseInt 
                              (match 1) 16)))]
    (is (= (re-sub re replacement "") ""))
    (is (= (re-sub re replacement "%20") " "))
    (is (= (re-sub re replacement "x%20%20") "x %20"))
    (is (= (re-sub re replacement "x%20y") "x y"))
    (is (= (re-sub re "?" "") ""))
    (is (= (re-sub re "?" "%21") "?"))
    (is (= (re-sub re "?" "x%22%25") "x?%25"))
    (is (= (re-gsub re "?" "x%23y") "x?y"))))
