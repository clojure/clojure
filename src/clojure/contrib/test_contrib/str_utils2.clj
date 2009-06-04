(ns clojure.contrib.test-contrib.str-utils2
  (:require [clojure.contrib.str-utils2 :as s])
  (:use clojure.contrib.test-is))

(deftest t-blank
  (is (s/blank? nil))
  (is (s/blank? ""))
  (is (s/blank? " "))
  (is (s/blank? " \t \n  \r "))
  (is (not (s/blank? "  foo  "))))

(deftest t-take
  (is (= "foo" (s/take "foobar" 3)))
  (is (= "foobar" (s/take "foobar" 7)))
  (is (= "" (s/take "foo" 0))))

(deftest t-drop
  (is (= "bar" (s/drop "foobar" 3)))
  (is (= "" (s/drop "foobar" 9)))
  (is (= "foobar" (s/drop "foobar" 0))))

(deftest t-butlast
  (is (= "foob" (s/butlast "foobar" 2)))
  (is (= "" (s/butlast "foobar" 9)))
  (is (= "foobar" (s/butlast "foobar" 0))))

(deftest t-tail
  (is (= "ar" (s/tail "foobar" 2)))
  (is (= "foobar" (s/tail "foobar" 9)))
  (is (= "" (s/tail "foobar" 0))))

(deftest t-replace
  (is (= "faabar" (s/replace "foobar" \o \a)))
  (is (= "barbarbar" (s/replace "foobarfoo" "foo" "bar")))
  (is (= "FOObarFOO" (s/replace "foobarfoo" #"foo" s/upper-case))))

(deftest t-replace-first
  (is (= "barbarfoo" (s/replace-first "foobarfoo" #"foo" "bar")))
  (is (= "FOObarfoo" (s/replace-first "foobarfoo" #"foo" s/upper-case))))
