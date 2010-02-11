(ns clojure.contrib.test-string
  (:require [clojure.contrib.string :as s])
  (:use clojure.test))

(deftest t-codepoints
  (is (= (list 102 111 111 65536 98 97 114)
         (s/codepoints "foo\uD800\uDC00bar"))
      "Handles Unicode supplementary characters"))

(deftest t-escape
  (is (= "&lt;foo&amp;bar&gt;"
         (s/escape {\& "&amp;" \< "&lt;" \> "&gt;"} "<foo&bar>")))
  (is (= " \\\"foo\\\" "
         (s/escape {\" "\\\""} " \"foo\" " )))
  (is (= "faabor" (s/escape {\a \o, \o \a} "foobar"))))

(deftest t-blank
  (is (s/blank? nil))
  (is (s/blank? ""))
  (is (s/blank? " "))
  (is (s/blank? " \t \n  \r "))
  (is (not (s/blank? "  foo  "))))

(deftest t-take
  (is (= "foo" (s/take 3 "foobar")))
  (is (= "foobar" (s/take 7 "foobar")))
  (is (= "" (s/take 0 "foo"))))

(deftest t-drop
  (is (= "bar" (s/drop 3 "foobar")))
  (is (= "" (s/drop 9 "foobar")))
  (is (= "foobar" (s/drop 0 "foobar"))))

(deftest t-butlast
  (is (= "foob" (s/butlast 2 "foobar")))
  (is (= "" (s/butlast 9 "foobar")))
  (is (= "foobar" (s/butlast 0 "foobar"))))

(deftest t-tail
  (is (= "ar" (s/tail 2 "foobar")))
  (is (= "foobar" (s/tail 9 "foobar")))
  (is (= "" (s/tail 0 "foobar"))))

(deftest t-repeat
  (is (= "foofoofoo" (s/repeat 3 "foo"))))

(deftest t-reverse
  (is (= "tab" (s/reverse "bat"))))

(deftest t-replace
  (is (= "faabar" (s/replace-char \o \a "foobar")))
  (is (= "barbarbar" (s/replace-str "foo" "bar" "foobarfoo")))
  (is (= "FOObarFOO" (s/replace-by #"foo" s/upper-case  "foobarfoo"))))

(deftest t-replace-first
  (is (= "barbarfoo" (s/replace-first-re #"foo" "bar" "foobarfoo")))
  (is (= "FOObarfoo" (s/replace-first-by #"foo" s/upper-case "foobarfoo"))))

(deftest t-partition
  (is (= (list "" "abc" "123" "def")
         (s/partition #"[a-z]+" "abc123def"))))

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

(deftest t-substring
  (is (s/substring? "foo" "foobar"))
  (is (not (s/substring? "baz" "foobar"))))

(deftest t-get
  (is (= \o (s/get "foo" 1))))

(deftest t-as-str
  (testing "keyword to string"
    (is (= "foo") (s/as-str :foo)))
  (testing "symbol to string"
    (is (= "foo") (s/as-str 'foo)))
  (testing "string to string"
    (is (= "foo") (s/as-str "foo")))
  (testing "stringifying non-namish things"
    (is (= "42") (s/as-str 42))))
