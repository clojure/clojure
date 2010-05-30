(ns clojure.test-clojure.string
  (:require [clojure.string :as s])
  (:use clojure.test))

(deftest t-split
  (is (= ["a" "b"] (s/split "a-b" #"-")))
  (is (= ["a" "b-c"] (s/split "a-b-c" #"-" 2)))
  (is (vector? (s/split "abc" #"-"))))

(deftest t-reverse
  (is (= "tab" (s/reverse "bat"))))

(deftest t-replace
  (is (= "faabar" (s/replace "foobar" \o \a)))
  (is (= "barbarbar" (s/replace "foobarfoo" "foo" "bar")))
  (is (= "FOObarFOO" (s/replace "foobarfoo" #"foo" s/upper-case))))

(deftest t-replace-first
  (is (= "barbarfoo" (s/replace-first "foobarfoo" "foo" "bar")))
  (is (= "barbarfoo" (s/replace-first "foobarfoo" #"foo" "bar")))
  (is (= "FOObarfoo" (s/replace-first "foobarfoo" #"foo" s/upper-case))))

(deftest t-join
  (are [x coll] (= x (s/join coll))
       "" nil
       "" []
       "1" [1]
       "12" [1 2])
  (are [x sep coll] (= x (s/join sep coll))
       "1,2,3" \, [1 2 3]
       "" \, []
       "1" \, [1]
       "1 and-a 2 and-a 3" " and-a " [1 2 3]))

(deftest t-trim-newline
  (is (= "foo" (s/trim-newline "foo\n")))
  (is (= "foo" (s/trim-newline "foo\r\n")))
  (is (= "foo" (s/trim-newline "foo")))
  (is (= "" (s/trim-newline ""))))

(deftest t-capitalize
  (is (= "Foobar" (s/capitalize "foobar")))
  (is (= "Foobar" (s/capitalize "FOOBAR"))))

(deftest t-triml
  (is (= "foo " (s/triml " foo ")))
  (is (= "" (s/triml "   "))))

(deftest t-trimr
  (is (= " foo" (s/trimr " foo ")))
  (is (= "" (s/trimr "   "))))

(deftest t-trim
  (is (= "foo" (s/trim "  foo  \r\n"))))

(deftest t-upper-case
  (is (= "FOOBAR" (s/upper-case "Foobar"))))

(deftest t-lower-case
  (is (= "foobar" (s/lower-case "FooBar"))))

