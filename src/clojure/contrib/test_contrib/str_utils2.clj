(ns clojure.contrib.test-contrib.str-utils2
  (:require [clojure.contrib.str-utils2 :as s])
  (:use clojure.test))

(deftest t-codepoints
  (is (= (list 102 111 111 65536 98 97 114)
         (s/codepoints "foo\uD800\uDC00bar"))
      "Handles Unicode supplementary characters"))

(deftest t-escape
  (is (= "&lt;foo&amp;bar&gt;"
         (s/escape "<foo&bar>" {\& "&amp;" \< "&lt;" \> "&gt;"})))
  (is (= " \\\"foo\\\" "
         (s/escape " \"foo\" " {\" "\\\""})))
  (is (= "faabor" (s/escape "foobar" {\a \o, \o \a}))))

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

(deftest t-repeat
  (is (= "foofoofoo" (s/repeat "foo" 3))))

(deftest t-reverse
  (is (= "tab" (s/reverse "bat"))))

(deftest t-replace
  (is (= "faabar" (s/replace "foobar" \o \a)))
  (is (= "barbarbar" (s/replace "foobarfoo" "foo" "bar")))
  (is (= "FOObarFOO" (s/replace "foobarfoo" #"foo" s/upper-case))))

(deftest t-replace-first
  (is (= "barbarfoo" (s/replace-first "foobarfoo" #"foo" "bar")))
  (is (= "FOObarfoo" (s/replace-first "foobarfoo" #"foo" s/upper-case))))

(deftest t-partition
  (is (= (list "" "abc" "123" "def")
         (s/partition  "abc123def" #"[a-z]+"))))

(deftest t-join
  (is (= "1,2,3" (s/join \, [1 2 3])))
  (is (= "" (s/join \, [])))
  (is (= "1 and-a 2 and-a 3" (s/join " and-a " [1 2 3]))))

(deftest t-chop
  (is (= "fo" (s/chop "foo")))
  (is (= "") (s/chop "f"))
  (is (= "") (s/chop "")))

(deftest t-chomp
  (is (= "foo" (s/chomp "foo\n")))
  (is (= "foo" (s/chomp "foo\r\n")))
  (is (= "foo" (s/chomp "foo")))
  (is (= "" (s/chomp ""))))

(deftest t-swap-case
  (is (= "fOO!bAR" (s/swap-case "Foo!Bar")))
  (is (= "" (s/swap-case ""))))

(deftest t-capitalize
  (is (= "Foobar" (s/capitalize "foobar")))
  (is (= "Foobar" (s/capitalize "FOOBAR"))))

(deftest t-ltrim
  (is (= "foo " (s/ltrim " foo ")))
  (is (= "" (s/ltrim "   "))))

(deftest t-rtrim
  (is (= " foo" (s/rtrim " foo ")))
  (is (= "" (s/rtrim "   "))))

(deftest t-split-lines
  (is (= (list "one" "two" "three")
         (s/split-lines "one\ntwo\r\nthree")))
  (is (= (list "foo") (s/split-lines "foo"))))

(deftest t-upper-case
  (is (= "FOOBAR" (s/upper-case "Foobar"))))

(deftest t-lower-case
  (is (= "foobar" (s/lower-case "FooBar"))))

(deftest t-trim
  (is (= "foo" (s/trim "  foo  \r\n"))))

(deftest t-contains
  (is (s/contains? "foobar" "foo"))
  (is (not (s/contains? "foobar" "baz"))))

(deftest t-get
  (is (= \o (s/get "foo" 1))))

(deftest t-partial
  (is (= "bar" ((s/partial s/drop 3) "foobar")))
  (is (= "ooba" ((comp (s/partial s/take 4)
                       (s/partial s/drop 1)) "foobar"))))
