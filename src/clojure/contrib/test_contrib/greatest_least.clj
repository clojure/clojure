(ns clojure.contrib.test-contrib.greatest-least
  (:use clojure.contrib.greatest-least
        [clojure.contrib.test-is :only (is deftest run-tests)]))

(deftest test-greatest
  (is (nil? (greatest)) "greatest with no arguments is nil")
  (is (= 1 (greatest 1)))
  (is (= 2 (greatest 1 2)))
  (is (= 2 (greatest 2 1)))
  (is (= "b" (greatest "aa" "b"))))

(deftest test-greatest-by
  (is (nil? (greatest-by identity)) "greatest-by with no arguments is nil")
  (is (= "" (greatest-by count "")))
  (is (= "a" (greatest-by count "a" "")))
  (is (= "a" (greatest-by count "" "a")))
  (is (= "aa" (greatest-by count "aa" "b"))))

(deftest test-least
  (is (nil? (least)) "least with no arguments is nil")
  (is (= 1 (least 1)))
  (is (= 1 (least 1 2)))
  (is (= 1 (least 2 1)))
  (is (= "aa" (least "aa" "b"))))

(deftest test-least-by
  (is (nil? (least-by identity)) "least-by with no arguments is nil")
  (is (= "" (least-by count "")))
  (is (= "" (least-by count "a" "")))
  (is (= "" (least-by count "" "a")))
  (is (= "b" (least-by count "aa" "b"))))

(deftest test-all-greatest
  (is (nil? (all-greatest)) "all-greatest with no arguments is nil")
  (is (= (list 1) (all-greatest 1)))
  (is (= (list 1 1) (all-greatest 1 1)))
  (is (= (list 2) (all-greatest 2 1 1)))
  (is (= (list 2) (all-greatest 1 2 1)))
  (is (= (list 2) (all-greatest 1 1 2)))
  (is (= (list :c) (all-greatest :b :c :a))))

(deftest test-all-greatest-by
  (is (nil? (all-greatest-by identity)) "all-greatest-by with no arguments is nil")
  (is (= (list "a")) (all-greatest-by count "a"))
  (is (= (list "a" "a")) (all-greatest-by count "a" "a"))
  (is (= (list "aa")) (all-greatest-by count "aa" "b"))
  (is (= (list "aa")) (all-greatest-by count "b" "aa" "c"))
  (is (= (list "cc" "aa")) (all-greatest-by count "aa" "b" "cc")))

(deftest test-all-least
  (is (nil? (all-least)) "all-least with no arguments is nil")
  (is (= (list 1) (all-least 1)))
  (is (= (list 1 1) (all-least 1 1)))
  (is (= (list 1 1) (all-least 2 1 1)))
  (is (= (list 1 1) (all-least 1 2 1)))
  (is (= (list 1 1) (all-least 1 1 2)))
  (is (= (list :a) (all-least :b :c :a))))

(deftest test-all-least-by
  (is (nil? (all-least-by identity)) "all-least-by with no arguments is nil")
  (is (= (list "a")) (all-least-by count "a"))
  (is (= (list "a" "a")) (all-least-by count "a" "a"))
  (is (= (list "b")) (all-least-by count "aa" "b"))
  (is (= (list "c" "b")) (all-least-by count "b" "aa" "c"))
  (is (= (list "b")) (all-least-by count "aa" "b" "cc")))
