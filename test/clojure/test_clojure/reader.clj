;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Stephen C. Gilardi

;;
;;  Tests for the Clojure functions documented at the URL:
;;
;;    http://clojure.org/Reader
;;
;;  scgilardi (gmail)
;;  Created 22 October 2008

(ns clojure.test-clojure.reader
  (:use clojure.test)
  (:use [clojure.instant :only [read-instant-date
                                read-instant-calendar
                                read-instant-timestamp]])
  (:import clojure.lang.BigInt
           java.io.File
           java.util.TimeZone))

;; Symbols

(deftest Symbols
  (is (= 'abc (symbol "abc")))
  (is (= '*+!-_? (symbol "*+!-_?")))
  (is (= 'abc:def:ghi (symbol "abc:def:ghi")))
  (is (= 'abc/def (symbol "abc" "def")))
  (is (= 'abc.def/ghi (symbol "abc.def" "ghi")))
  (is (= 'abc/def.ghi (symbol "abc" "def.ghi")))
  (is (= 'abc:def/ghi:jkl.mno (symbol "abc:def" "ghi:jkl.mno")))
  (is (instance? clojure.lang.Symbol 'alphabet))
  )

;; Literals

(deftest Literals
  ; 'nil 'false 'true are reserved by Clojure and are not symbols
  (is (= 'nil nil))
  (is (= 'false false))
  (is (= 'true true)) )

;; Strings

(defn temp-file
  [prefix suffix]
  (doto (File/createTempFile prefix suffix)
    (.deleteOnExit)))

(defn read-from
  [source file form]
  (if (= :string source)
    (read-string form)
    (do
      (spit file form)
      (load-file (str file)))))

(defn code-units
  [s]
  (and (instance? String s) (map int s)))

(deftest Strings
  (is (= "abcde" (str \a \b \c \d \e)))
  (is (= "abc
  def" (str \a \b \c \newline \space \space \d \e \f)))
  (let [f (temp-file "clojure.core-reader" "test")]
    (doseq [source [:string :file]]
      (testing (str "Valid string literals read from " (name source))
        (are [x form] (= x (code-units
                            (read-from source f (str "\"" form "\""))))
             [] ""
             [34] "\\\""
             [10] "\\n"

             [0] "\\0"
             [0] "\\000"
             [3] "\\3"
             [3] "\\03"
             [3] "\\003"
             [0 51] "\\0003"
             [3 48] "\\0030"
             [0377] "\\377"
             [0 56] "\\0008"

             [0] "\\u0000"
             [0xd7ff] "\\ud7ff"
             [0xd800] "\\ud800"
             [0xdfff] "\\udfff"
             [0xe000] "\\ue000"
             [0xffff] "\\uffff"
             [4 49] "\\u00041"))
      (testing (str "Errors reading string literals from " (name source))
        (are [err msg form] (thrown-with-msg? err msg
                              (read-from source f (str "\"" form "\"")))
             Exception #"EOF while reading string" "\\"
             Exception #"Unsupported escape character: \\o" "\\o"

             Exception #"Octal escape sequence must be in range \[0, 377\]" "\\400"
             Exception #"Invalid digit: 8" "\\8"
             Exception #"Invalid digit: 8" "\\8000"
             Exception #"Invalid digit: 8" "\\0800"
             Exception #"Invalid digit: 8" "\\0080"
             Exception #"Invalid digit: a" "\\2and"

             Exception #"Invalid unicode escape: \\u" "\\u"
             Exception #"Invalid unicode escape: \\ug" "\\ug"
             Exception #"Invalid unicode escape: \\ug" "\\ug000"
             Exception #"Invalid character length: 1, should be: 4" "\\u0"
             Exception #"Invalid character length: 3, should be: 4" "\\u004"
             Exception #"Invalid digit: g" "\\u004g")))))

;; Numbers

(deftest Numbers

  ; Read Integer
  (is (instance? Long 2147483647))
  (is (instance? Long +1))
  (is (instance? Long 1))
  (is (instance? Long +0))
  (is (instance? Long 0))
  (is (instance? Long -0))
  (is (instance? Long -1))
  (is (instance? Long -2147483648))

  ; Read Long
  (is (instance? Long 2147483648))
  (is (instance? Long -2147483649))
  (is (instance? Long 9223372036854775807))
  (is (instance? Long -9223372036854775808))

  ;; Numeric constants of different types don't wash out. Regression fixed in
  ;; r1157. Previously the compiler saw 0 and 0.0 as the same constant and
  ;; caused the sequence to be built of Doubles.
  (let [x 0.0]
    (let [sequence (loop [i 0 l '()]
                     (if (< i 5)
                       (recur (inc i) (conj l i))
                       l))]
      (is (= [4 3 2 1 0] sequence))
      (is (every? #(instance? Long %)
                  sequence))))

  ; Read BigInteger
  (is (instance? BigInt 9223372036854775808))
  (is (instance? BigInt -9223372036854775809))
  (is (instance? BigInt 10000000000000000000000000000000000000000000000000))
  (is (instance? BigInt -10000000000000000000000000000000000000000000000000))

  ; Read Double
  (is (instance? Double +1.0e+1))
  (is (instance? Double +1.e+1))
  (is (instance? Double +1e+1))

  (is (instance? Double +1.0e1))
  (is (instance? Double +1.e1))
  (is (instance? Double +1e1))

  (is (instance? Double +1.0e-1))
  (is (instance? Double +1.e-1))
  (is (instance? Double +1e-1))

  (is (instance? Double 1.0e+1))
  (is (instance? Double 1.e+1))
  (is (instance? Double 1e+1))

  (is (instance? Double 1.0e1))
  (is (instance? Double 1.e1))
  (is (instance? Double 1e1))

  (is (instance? Double 1.0e-1))
  (is (instance? Double 1.e-1))
  (is (instance? Double 1e-1))

  (is (instance? Double -1.0e+1))
  (is (instance? Double -1.e+1))
  (is (instance? Double -1e+1))

  (is (instance? Double -1.0e1))
  (is (instance? Double -1.e1))
  (is (instance? Double -1e1))

  (is (instance? Double -1.0e-1))
  (is (instance? Double -1.e-1))
  (is (instance? Double -1e-1))

  (is (instance? Double +1.0))
  (is (instance? Double +1.))

  (is (instance? Double 1.0))
  (is (instance? Double 1.))

  (is (instance? Double +0.0))
  (is (instance? Double +0.))

  (is (instance? Double 0.0))
  (is (instance? Double 0.))

  (is (instance? Double -0.0))
  (is (instance? Double -0.))

  (is (instance? Double -1.0))
  (is (instance? Double -1.))

  ; Read BigDecimal
  (is (instance? BigDecimal 9223372036854775808M))
  (is (instance? BigDecimal -9223372036854775809M))
  (is (instance? BigDecimal 2147483647M))
  (is (instance? BigDecimal +1M))
  (is (instance? BigDecimal 1M))
  (is (instance? BigDecimal +0M))
  (is (instance? BigDecimal 0M))
  (is (instance? BigDecimal -0M))
  (is (instance? BigDecimal -1M))
  (is (instance? BigDecimal -2147483648M))

  (is (instance? BigDecimal +1.0e+1M))
  (is (instance? BigDecimal +1.e+1M))
  (is (instance? BigDecimal +1e+1M))

  (is (instance? BigDecimal +1.0e1M))
  (is (instance? BigDecimal +1.e1M))
  (is (instance? BigDecimal +1e1M))

  (is (instance? BigDecimal +1.0e-1M))
  (is (instance? BigDecimal +1.e-1M))
  (is (instance? BigDecimal +1e-1M))

  (is (instance? BigDecimal 1.0e+1M))
  (is (instance? BigDecimal 1.e+1M))
  (is (instance? BigDecimal 1e+1M))

  (is (instance? BigDecimal 1.0e1M))
  (is (instance? BigDecimal 1.e1M))
  (is (instance? BigDecimal 1e1M))

  (is (instance? BigDecimal 1.0e-1M))
  (is (instance? BigDecimal 1.e-1M))
  (is (instance? BigDecimal 1e-1M))

  (is (instance? BigDecimal -1.0e+1M))
  (is (instance? BigDecimal -1.e+1M))
  (is (instance? BigDecimal -1e+1M))

  (is (instance? BigDecimal -1.0e1M))
  (is (instance? BigDecimal -1.e1M))
  (is (instance? BigDecimal -1e1M))

  (is (instance? BigDecimal -1.0e-1M))
  (is (instance? BigDecimal -1.e-1M))
  (is (instance? BigDecimal -1e-1M))

  (is (instance? BigDecimal +1.0M))
  (is (instance? BigDecimal +1.M))

  (is (instance? BigDecimal 1.0M))
  (is (instance? BigDecimal 1.M))

  (is (instance? BigDecimal +0.0M))
  (is (instance? BigDecimal +0.M))

  (is (instance? BigDecimal 0.0M))
  (is (instance? BigDecimal 0.M))

  (is (instance? BigDecimal -0.0M))
  (is (instance? BigDecimal -0.M))

  (is (instance? BigDecimal -1.0M))
  (is (instance? BigDecimal -1.M))
)

;; Characters

(deftest t-Characters
  (let [f (temp-file "clojure.core-reader" "test")]
    (doseq [source [:string :file]]
      (testing (str "Valid char literals read from " (name source))
        (are [x form] (= x (read-from source f form))
             (first "o") "\\o"
             (char 0) "\\o0"
             (char 0) "\\o000"
             (char 047) "\\o47"
             (char 0377) "\\o377"

             (first "u") "\\u"
             (first "A") "\\u0041"
             (char 0) "\\u0000"
             (char 0xd7ff) "\\ud7ff"
             (char 0xe000) "\\ue000"
             (char 0xffff) "\\uffff"))
      (testing (str "Errors reading char literals from " (name source))
        (are [err msg form] (thrown-with-msg? err msg (read-from source f form))
             Exception #"EOF while reading character" "\\"
             Exception #"Unsupported character: \\00" "\\00"
             Exception #"Unsupported character: \\0009" "\\0009"

             Exception #"Invalid digit: 8" "\\o378"
             Exception #"Octal escape sequence must be in range \[0, 377\]" "\\o400"
             Exception #"Invalid digit: 8" "\\o800"
             Exception #"Invalid digit: a" "\\oand"
             Exception #"Invalid octal escape sequence length: 4" "\\o0470"

             Exception #"Invalid unicode character: \\u0" "\\u0"
             Exception #"Invalid unicode character: \\ug" "\\ug"
             Exception #"Invalid unicode character: \\u000" "\\u000"
             Exception #"Invalid character constant: \\ud800" "\\ud800"
             Exception #"Invalid character constant: \\udfff" "\\udfff"
             Exception #"Invalid unicode character: \\u004" "\\u004"
             Exception #"Invalid unicode character: \\u00041" "\\u00041"
             Exception #"Invalid digit: g" "\\u004g")))))

;; nil

(deftest t-nil)

;; Booleans

(deftest t-Booleans)

;; Keywords

(deftest t-Keywords
  (is (= :abc (keyword "abc")))
  (is (= :abc (keyword 'abc)))
  (is (= :*+!-_? (keyword "*+!-_?")))
  (is (= :abc:def:ghi (keyword "abc:def:ghi")))
  (is (= :abc/def (keyword "abc" "def")))
  (is (= :abc/def (keyword 'abc/def)))
  (is (= :abc.def/ghi (keyword "abc.def" "ghi")))
  (is (= :abc/def.ghi (keyword "abc" "def.ghi")))
  (is (= :abc:def/ghi:jkl.mno (keyword "abc:def" "ghi:jkl.mno")))
  (is (instance? clojure.lang.Keyword :alphabet))
  )

(deftest reading-keywords
  (are [x y] (= x (binding [*ns* (the-ns 'user)] (read-string y)))
       :foo ":foo"
       :foo/bar ":foo/bar"
       :user/foo "::foo")
  (are [err msg form] (thrown-with-msg? err msg (read-string form))
       Exception #"Invalid token: foo:" "foo:"
       Exception #"Invalid token: :bar/" ":bar/"
       Exception #"Invalid token: ::does.not/exist" "::does.not/exist"))
;; Lists

(deftest t-Lists)

;; Vectors

(deftest t-Vectors)

;; Maps

(deftest t-Maps)

;; Sets

(deftest t-Sets)

;; Macro characters

;; Quote (')

(deftest t-Quote)

;; Character (\)

(deftest t-Character)

;; Comment (;)

(deftest t-Comment)

;; Deref (@)

(deftest t-Deref)

;; Dispatch (#)

;; #{} - see Sets above

;; Regex patterns (#"pattern")

(deftest t-Regex)

;; Metadata (^ or #^ (deprecated))

(deftest t-Metadata
  (is (= (meta '^:static ^:awesome ^{:static false :bar :baz} sym) {:awesome true, :bar :baz, :static true})))

;; Var-quote (#')

(deftest t-Var-quote)

;; Anonymous function literal (#())

(deftest t-Anonymouns-function-literal)

;; Syntax-quote (`, note, the "backquote" character), Unquote (~) and
;; Unquote-splicing (~@)

(deftest t-Syntax-quote
  (are [x y] (= x y)
      `() ()    ; was NPE before SVN r1337
  ))

;; (read)
;; (read stream)
;; (read stream eof-is-error)
;; (read stream eof-is-error eof-value)
;; (read stream eof-is-error eof-value is-recursive)

(deftest t-read)

(deftest Instants
  (testing "Instants are read as java.util.Date by default"
    (is (= java.util.Date (class #inst "2010-11-12T13:14:15.666"))))
  (let [s "#inst \"2010-11-12T13:14:15.666-06:00\""]
    (binding [*data-readers* {'inst read-instant-date}]
      (testing "read-instant-date produces java.util.Date"
        (is (= java.util.Date (class (read-string s)))))
      (testing "java.util.Date instants round-trips"
        (is (= (-> s read-string)
               (-> s read-string pr-str read-string))))
      (testing "java.util.Date instants round-trip throughout the year"
        (doseq [month (range 1 13) day (range 1 29) hour (range 1 23)]
          (let [s (format "#inst \"2010-%02d-%02dT%02d:14:15.666-06:00\"" month day hour)]
            (is (= (-> s read-string)
                   (-> s read-string pr-str read-string))))))
      (testing "java.util.Date handling DST in time zones"
        (let [dtz (TimeZone/getDefault)]
          (try
            ;; A timezone with DST in effect during 2010-11-12
            (TimeZone/setDefault (TimeZone/getTimeZone "Australia/Sydney"))
            (is (= (-> s read-string)
                   (-> s read-string pr-str read-string)))
            (finally (TimeZone/setDefault dtz)))))
      (testing "java.util.Date should always print in UTC"
        (let [d (read-string s)
              pstr (print-str d)
              len (.length pstr)]
          (is (= (subs pstr (- len 7)) "-00:00\"")))))
    (binding [*data-readers* {'inst read-instant-calendar}]
      (testing "read-instant-calendar produces java.util.Calendar"
        (is (instance? java.util.Calendar (read-string s))))
      (testing "java.util.Calendar round-trips"
        (is (= (-> s read-string)
               (-> s read-string pr-str read-string))))
      (testing "java.util.Calendar remembers timezone in literal"
        (is (= "#inst \"2010-11-12T13:14:15.666-06:00\""
               (-> s read-string pr-str)))
        (is (= (-> s read-string)
               (-> s read-string pr-str read-string))))
      (testing "java.util.Calendar preserves milliseconds"
        (is (= 666 (-> s read-string
                       (.get java.util.Calendar/MILLISECOND)))))))
  (let [s "#inst \"2010-11-12T13:14:15.123456789\""
        s2 "#inst \"2010-11-12T13:14:15.123\""
        s3 "#inst \"2010-11-12T13:14:15.123456789123\""]
    (binding [*data-readers* {'inst read-instant-timestamp}]
      (testing "read-instant-timestamp produces java.sql.Timestamp"
        (is (= java.sql.Timestamp (class (read-string s)))))
      (testing "java.sql.Timestamp preserves nanoseconds"
        (is (= 123456789 (-> s read-string .getNanos)))
        (is (= 123456789 (-> s read-string pr-str read-string .getNanos)))
        ;; truncate at nanos for s3
        (is (= 123456789 (-> s3 read-string pr-str read-string .getNanos))))
      (testing "java.sql.Timestamp should compare nanos"
        (is (= (read-string s) (read-string s3)))
        (is (not= (read-string s) (read-string s2)))))
    (binding [*data-readers* {'inst read-instant-date}]
      (testing "read-instant-date should truncate at milliseconds"
        (is (= (read-string s) (read-string s2)) (read-string s3)))))
  (let [s "#inst \"2010-11-12T03:14:15.123+05:00\""
        s2 "#inst \"2010-11-11T22:14:15.123Z\""]
    (binding [*data-readers* {'inst read-instant-date}]
      (testing "read-instant-date should convert to UTC"
        (is (= (read-string s) (read-string s2)))))
    (binding [*data-readers* {'inst read-instant-timestamp}]
      (testing "read-instant-timestamp should convert to UTC"
        (is (= (read-string s) (read-string s2)))))
    (binding [*data-readers* {'inst read-instant-calendar}]
      (testing "read-instant-calendar should preserve timezone"
        (is (not= (read-string s) (read-string s2)))))))
      
;; UUID Literals
;; #uuid "550e8400-e29b-41d4-a716-446655440000"

(deftest UUID
  (is (= java.util.UUID (class #uuid "550e8400-e29b-41d4-a716-446655440000")))
  (is (.equals #uuid "550e8400-e29b-41d4-a716-446655440000"
               #uuid "550e8400-e29b-41d4-a716-446655440000"))
  (is (not (identical? #uuid "550e8400-e29b-41d4-a716-446655440000"
                       #uuid "550e8400-e29b-41d4-a716-446655440000")))
  (is (= 4 (.version #uuid "550e8400-e29b-41d4-a716-446655440000")))
  (is (= (print-str #uuid "550e8400-e29b-41d4-a716-446655440000")
         "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")))
