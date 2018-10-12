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
  (:require clojure.walk
            [clojure.edn :as edn]
            [clojure.test.generative :refer (defspec)]
            [clojure.test-clojure.generators :as cgen]
            [clojure.edn :as edn])
  (:import [clojure.lang BigInt Ratio]
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
        (are [err msg form] (thrown-with-cause-msg? err msg
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

  (is (= Double/POSITIVE_INFINITY ##Inf))
  (is (= Double/NEGATIVE_INFINITY ##-Inf))
  (is (and (instance? Double ##NaN) (.isNaN ##NaN)))

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

  (is (instance? Ratio 1/2))
  (is (instance? Ratio -1/2))
  (is (instance? Ratio +1/2))
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
        (are [err msg form] (thrown-with-cause-msg? err msg (read-from source f form))
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

(deftest t-line-column-numbers
  (let [code "(ns reader-metadata-test
  (:require [clojure.java.io
             :refer (resource reader)]))

(let [a 5]
  ^:added-metadata
  (defn add-5
    [x]
    (reduce + x (range a))))"
        stream (clojure.lang.LineNumberingPushbackReader.
                 (java.io.StringReader. code))
        top-levels (take-while identity (repeatedly #(read stream false nil)))
        expected-metadata '{ns {:line 1, :column 1}
                            :require {:line 2, :column 3}
                            resource {:line 3, :column 21}
                            let {:line 5, :column 1}
                            defn {:line 6, :column 3 :added-metadata true}
                            reduce {:line 9, :column 5}
                            range {:line 9, :column 17}}
        verified-forms (atom 0)]
    (doseq [form top-levels]
      (clojure.walk/postwalk
        #(when (list? %)
           (is (= (expected-metadata (first %))
                  (meta %)))
           (is (->> (meta %)
                 vals
                 (filter number?)
                 (every? (partial instance? Integer))))
           (swap! verified-forms inc))
        form))
    ;; sanity check against e.g. reading returning ()
    (is (= (count expected-metadata) @verified-forms))))

(deftest set-line-number
  (let [r (clojure.lang.LineNumberingPushbackReader. *in*)]
    (.setLineNumber r 100)
    (is (= 100 (.getLineNumber r)))))

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

(deftest division
  (is (= clojure.core// /))
  (binding [*ns* *ns*]
    (eval '(do (ns foo
                 (:require [clojure.core :as bar])
                 (:use [clojure.test]))
               (is (= clojure.core// bar//))))))

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
        (is (= (read-string s) (read-string s2) (read-string s3))))))
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

(deftest unknown-tag
  (let [my-unknown (fn [tag val] {:unknown-tag tag :value val})
        throw-on-unknown (fn [tag val] (throw (RuntimeException. (str "No data reader function for tag " tag))))
        my-uuid (partial my-unknown 'uuid)
        u "#uuid \"550e8400-e29b-41d4-a716-446655440000\""
        s "#never.heard.of/some-tag [1 2]" ]
    (binding [*data-readers* {'uuid my-uuid}
              *default-data-reader-fn* my-unknown]
      (testing "Unknown tag"
        (is (= (read-string s)
               {:unknown-tag 'never.heard.of/some-tag
                :value [1 2]})))
      (testing "Override uuid tag"
        (is (= (read-string u)
               {:unknown-tag 'uuid
                :value "550e8400-e29b-41d4-a716-446655440000"}))))

    (binding [*default-data-reader-fn* throw-on-unknown]
      (testing "Unknown tag with custom throw-on-unknown"
        (are [err msg form] (thrown-with-msg? err msg (read-string form))
             Exception #"No data reader function for tag foo" "#foo [1 2]"
             Exception #"No data reader function for tag bar/foo" "#bar/foo [1 2]"
             Exception #"No data reader function for tag bar.baz/foo" "#bar.baz/foo [1 2]")))

    (testing "Unknown tag out-of-the-box behavior (like Clojure 1.4)"
      (are [err msg form] (thrown-with-msg? err msg (read-string form))
           Exception #"No reader function for tag foo" "#foo [1 2]"
           Exception #"No reader function for tag bar/foo" "#bar/foo [1 2]"
           Exception #"No reader function for tag bar.baz/foo" "#bar.baz/foo [1 2]"))))


(defn roundtrip
  "Print an object and read it back. Returns rather than throws
   any exceptions."
  [o]
  (binding [*print-length* nil
            *print-dup* nil
            *print-level* nil]
    (try
     (-> o pr-str read-string)
     (catch Throwable t t))))

(defn roundtrip-dup
  "Print an object with print-dup and read it back.
   Returns rather than throws any exceptions."
  [o]
  (binding [*print-length* nil
            *print-dup* true
            *print-level* nil]
    (try
     (-> o pr-str read-string)
     (catch Throwable t t))))

(defspec types-that-should-roundtrip
  roundtrip
  [^{:tag cgen/ednable} o]
  (when-not (= o %)
    (throw (ex-info "Value cannot roundtrip, see ex-data" {:printed o :read %}))))

(defspec types-that-need-dup-to-roundtrip
  roundtrip-dup
  [^{:tag cgen/dup-readable} o]
  (when-not (= o %)
    (throw (ex-info "Value cannot roundtrip, see ex-data" {:printed o :read %}))))

(defrecord TestRecord [x y])

(deftest preserve-read-cond-test
  (let [x (read-string {:read-cond :preserve} "#?(:clj foo :cljs bar)" )]
       (is (reader-conditional? x))
       (is (not (:splicing? x)))
       (is (= :foo (get x :no-such-key :foo)))
       (is (= (:form x) '(:clj foo :cljs bar)))
       (is (= x (reader-conditional '(:clj foo :cljs bar) false))))
  (let [x (read-string {:read-cond :preserve} "#?@(:clj [foo])" )]
       (is (reader-conditional? x))
       (is (:splicing? x))
       (is (= :foo (get x :no-such-key :foo)))
       (is (= (:form x) '(:clj [foo])))
       (is (= x (reader-conditional '(:clj [foo]) true))))
  (is (thrown-with-msg? RuntimeException #"No reader function for tag"
                        (read-string {:read-cond :preserve} "#js {:x 1 :y 2}" )))
  (let [x (read-string {:read-cond :preserve} "#?(:cljs #js {:x 1 :y 2})")
        [platform tl] (:form x)]
       (is (reader-conditional? x))
       (is (tagged-literal? tl))
       (is (= 'js (:tag tl)))
       (is (= {:x 1 :y 2} (:form tl)))
       (is (= :foo (get tl :no-such-key :foo)))
       (is (= tl (tagged-literal 'js {:x 1 :y 2}))))
  (testing "print form roundtrips"
           (doseq [s ["#?(:clj foo :cljs bar)"
                      "#?(:cljs #js {:x 1, :y 2})"
                      "#?(:clj #clojure.test_clojure.reader.TestRecord [42 85])"]]
                  (is (= s (pr-str (read-string {:read-cond :preserve} s)))))))

(deftest reader-conditionals
  (testing "basic read-cond"
    (is (= '[foo-form]
           (read-string {:read-cond :allow :features #{:foo}} "[#?(:foo foo-form :bar bar-form)]")))
    (is (= '[bar-form]
           (read-string {:read-cond :allow :features #{:bar}} "[#?(:foo foo-form :bar bar-form)]")))
    (is (= '[foo-form]
           (read-string {:read-cond :allow :features #{:foo :bar}} "[#?(:foo foo-form :bar bar-form)]")))
    (is (= '[]
           (read-string {:read-cond :allow :features #{:baz}} "[#?( :foo foo-form :bar bar-form)]"))))
  (testing "environmental features"
    (is (= "clojure" #?(:clj "clojure" :cljs "clojurescript" :default "default"))))
  (testing "default features"
    (is (= "default" #?(:clj-clr "clr" :cljs "cljs" :default "default"))))
  (testing "splicing"
    (is (= [] [#?@(:clj [])]))
    (is (= [:a] [#?@(:clj [:a])]))
    (is (= [:a :b] [#?@(:clj [:a :b])]))
    (is (= [:a :b :c] [#?@(:clj [:a :b :c])]))
    (is (= [:a :b :c] [#?@(:clj [:a :b :c])])))
  (testing "nested splicing"
    (is (= [:a :b :c :d :e]
           [#?@(:clj [:a #?@(:clj [:b #?@(:clj [:c]) :d]):e])]))
    (is (= '(+ 1 (+ 2 3))
           '(+ #?@(:clj [1 (+ #?@(:clj [2 3]))]))))
    (is (= '(+ (+ 2 3) 1)
           '(+ #?@(:clj [(+ #?@(:clj [2 3])) 1]))))
    (is (= [:a [:b [:c] :d] :e]
           [#?@(:clj [:a [#?@(:clj [:b #?@(:clj [[:c]]) :d])] :e])])))
  (testing "bypass unknown tagged literals"
    (is (= [1 2 3] #?(:cljs #js [1 2 3] :clj [1 2 3])))
    (is (= :clojure #?(:foo #some.nonexistent.Record {:x 1} :clj :clojure))))
  (testing "error cases"
    (is (thrown-with-msg? RuntimeException #"Feature should be a keyword" (read-string {:read-cond :allow} "#?((+ 1 2) :a)")))
    (is (thrown-with-msg? RuntimeException #"even number of forms" (read-string {:read-cond :allow} "#?(:cljs :a :clj)")))
    (is (thrown-with-msg? RuntimeException #"read-cond-splicing must implement" (read-string {:read-cond :allow} "#?@(:clj :a)")))
    (is (thrown-with-msg? RuntimeException #"is reserved" (read-string {:read-cond :allow} "#?@(:foo :a :else :b)")))
    (is (thrown-with-msg? RuntimeException #"must be a list" (read-string {:read-cond :allow} "#?[:foo :a :else :b]")))
    (is (thrown-with-msg? RuntimeException #"Conditional read not allowed" (read-string {:read-cond :BOGUS} "#?[:clj :a :default nil]")))
    (is (thrown-with-msg? RuntimeException #"Conditional read not allowed" (read-string "#?[:clj :a :default nil]")))
    (is (thrown-with-msg? RuntimeException #"Reader conditional splicing not allowed at the top level" (read-string {:read-cond :allow} "#?@(:clj [1 2])")))
    (is (thrown-with-msg? RuntimeException #"Reader conditional splicing not allowed at the top level" (read-string {:read-cond :allow} "#?@(:clj [1])")))
    (is (thrown-with-msg? RuntimeException #"Reader conditional splicing not allowed at the top level" (read-string {:read-cond :allow} "#?@(:clj []) 1"))))
  (testing "clj-1698-regression"
    (let [opts {:features #{:clj} :read-cond :allow}]
      (is (= 1 (read-string opts "#?(:cljs {'a 1 'b 2} :clj 1)")))
      (is (= 1 (read-string opts "#?(:cljs (let [{{b :b} :a {d :d} :c} {}]) :clj 1)")))
      (is (= '(def m {}) (read-string opts "(def m #?(:cljs ^{:a :b} {} :clj  ^{:a :b} {}))")))
      (is (= '(def m {}) (read-string opts "(def m #?(:cljs ^{:a :b} {} :clj ^{:a :b} {}))")))
      (is (= 1 (read-string opts "#?(:cljs {:a #_:b :c} :clj 1)")))))
  (testing "nil expressions"
    (is (nil? #?(:default nil)))
    (is (nil? #?(:foo :bar :clj nil)))
    (is (nil? #?(:clj nil :foo :bar)))
    (is (nil? #?(:foo :bar :default nil)))))

(deftest eof-option
  (is (= 23 (read-string {:eof 23} "")))
  (is (= 23 (read {:eof 23} (clojure.lang.LineNumberingPushbackReader.
                             (java.io.StringReader. ""))))))

(require '[clojure.string :as s])
(deftest namespaced-maps
  (is (= #:a{1 nil, :b nil, :b/c nil, :_/d nil}
         #:a {1 nil, :b nil, :b/c nil, :_/d nil}
         {1 nil, :a/b nil, :b/c nil, :d nil}))
  (is (= #::{1 nil, :a nil, :a/b nil, :_/d nil}
         #::  {1 nil, :a nil, :a/b nil, :_/d nil}
         {1 nil, :clojure.test-clojure.reader/a nil, :a/b nil, :d nil} ))
  (is (= #::s{1 nil, :a nil, :a/b nil, :_/d nil}
         #::s  {1 nil, :a nil, :a/b nil, :_/d nil}
         {1 nil, :clojure.string/a nil, :a/b nil, :d nil}))
  (is (= (read-string "#:a{b 1 b/c 2}") {'a/b 1, 'b/c 2}))
  (is (= (binding [*ns* (the-ns 'clojure.test-clojure.reader)] (read-string "#::{b 1, b/c 2, _/d 3}")) {'clojure.test-clojure.reader/b 1, 'b/c 2, 'd 3}))
  (is (= (binding [*ns* (the-ns 'clojure.test-clojure.reader)] (read-string "#::s{b 1, b/c 2, _/d 3}")) {'clojure.string/b 1, 'b/c 2, 'd 3})))

(deftest namespaced-map-errors
  (are [err msg form] (thrown-with-msg? err msg (read-string form))
                      Exception #"Invalid token" "#:::"
                      Exception #"Namespaced map literal must contain an even number of forms" "#:s{1}"
                      Exception #"Namespaced map must specify a valid namespace" "#:s/t{1 2}"
                      Exception #"Unknown auto-resolved namespace alias" "#::BOGUS{1 2}"
                      Exception #"Namespaced map must specify a namespace" "#: s{:a 1}"
                      Exception #"Duplicate key: :user/a" "#::{:a 1 :a 2}"
                      Exception #"Duplicate key: user/a" "#::{a 1 a 2}"))

(deftest namespaced-map-edn
  (is (= {1 1, :a/b 2, :b/c 3, :d 4}
         (edn/read-string "#:a{1 1, :b 2, :b/c 3, :_/d 4}")
         (edn/read-string "#:a {1 1, :b 2, :b/c 3, :_/d 4}"))))

(deftest invalid-symbol-value
  (is (thrown-with-msg? Exception #"Invalid token" (read-string "##5")))
  (is (thrown-with-msg? Exception #"Invalid token" (edn/read-string "##5")))
  (is (thrown-with-msg? Exception #"Unknown symbolic value" (read-string "##Foo")))
  (is (thrown-with-msg? Exception #"Unknown symbolic value" (edn/read-string "##Foo"))))

(defn str->lnpr
  [s]
  (-> s (java.io.StringReader.) (clojure.lang.LineNumberingPushbackReader.)))

(deftest test-read+string
  (let [[r s] (read+string (str->lnpr "[:foo  100]"))]
    (is (= [:foo 100] r))
    (is (= "[:foo  100]" s)))

  (let [[r s] (read+string {:read-cond :allow :features #{:y}} (str->lnpr "#?(:x :foo :y :bar)"))]
    (is (= :bar r))
    (is (= "#?(:x :foo :y :bar)" s))))

(deftest t-Explicit-line-column-numbers
  (is (= {:line 42 :column 99}
         (-> "^{:line 42 :column 99} (1 2)" read-string meta (select-keys [:line :column]))))

  (are [l c s] (= {:line l :column c} (-> s str->lnpr read meta (select-keys [:line :column])))
    42 99 "^{:line 42 :column 99} (1 2)"
    1 99 "^{:column 99} (1 2)")

  (eval (-> "^{:line 42 :column 99} (defn explicit-line-numbering [])" str->lnpr read))
  (is (= {:line 42 :column 99}
         (-> 'explicit-line-numbering resolve meta (select-keys [:line :column])))))
