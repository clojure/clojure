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
  (:import clojure.lang.BigInt))

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

(deftest Strings
  (is (= "abcde" (str \a \b \c \d \e)))
  (is (= "abc
  def" (str \a \b \c \newline \space \space \d \e \f)))
  )

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

(deftest t-Characters)

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
    (is (= java.util.Date (class #@2010-11-12T13:14:15.666))))
  (let [s "#@2010-11-12T13:14:15.666-06:00"]
    (binding [*instant-reader* read-instant-date]
      (testing "read-instant-date produces java.util.Date"
        (is (= java.util.Date (class (read-string s)))))
      (testing "java.util.Date instants round-trips"
        (is (= (-> s read-string)
               (-> s read-string pr-str read-string)))))
    (binding [*instant-reader* read-instant-calendar]
      (testing "read-instant-calendar produces java.util.Calendar"
        (is (instance? java.util.Calendar (read-string s))))
      (testing "java.util.Calendar round-trips"
        (is (= (-> s read-string)
               (-> s read-string pr-str read-string))))
      (testing "java.util.Calendar remembers timezone in literal"
        (is (= "#@2010-11-12T13:14:15.666-06:00"
               (-> s read-string pr-str)))
        (is (= (-> s read-string)
               (-> s read-string pr-str read-string))))
      (testing "java.util.Calendar preserves milliseconds"
        (is (= 666 (-> s read-string
                       (.get java.util.Calendar/MILLISECOND)))))))
  (let [s "#@2010-11-12T13:14:15.123456789"]
    (binding [*instant-reader* read-instant-timestamp]
      (testing "read-instant-timestamp produces java.sql.Timestamp"
        (is (= java.sql.Timestamp (class (read-string s)))))
      (testing "java.sql.Timestamp preserves nanoseconds"
        (is (= 123456789 (-> s read-string .getNanos)))
        (is (= 123456789 (-> s read-string pr-str read-string .getNanos)))))))
