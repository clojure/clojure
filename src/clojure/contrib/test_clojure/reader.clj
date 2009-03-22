;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  Tests for the Clojure functions documented at the URL:
;;
;;    http://clojure.org/Reader
;;
;;  scgilardi (gmail)
;;  Created 22 October 2008

(ns clojure.contrib.test-clojure.reader
  (:use clojure.contrib.test-is))

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
  (is (instance? Integer 2147483647))
  (is (instance? Integer +1))
  (is (instance? Integer 1))
  (is (instance? Integer +0))
  (is (instance? Integer 0))
  (is (instance? Integer -0))
  (is (instance? Integer -1))
  (is (instance? Integer -2147483648))

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
      (is (every? #(instance? Integer %)
                  sequence))))

  ; Read BigInteger
  (is (instance? BigInteger 9223372036854775808))
  (is (instance? BigInteger -9223372036854775809))
  (is (instance? BigInteger 10000000000000000000000000000000000000000000000000))
  (is (instance? BigInteger -10000000000000000000000000000000000000000000000000))

  ; Read Double
  (is (instance? Double +1.0e1))
  (is (instance? Double +1.0))
  (is (instance? Double 1.0))
  (is (instance? Double +0.0))
  (is (instance? Double 0.0))
  (is (instance? Double -0.0))
  (is (instance? Double -1.0))
  (is (instance? Double -1.0e1))

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
  (is (instance? BigDecimal +1.0M))
  (is (instance? BigDecimal 1.0M))
  (is (instance? BigDecimal +0.0M))
  (is (instance? BigDecimal 0.0M))
  (is (instance? BigDecimal -0.0M))
  (is (instance? BigDecimal -1.0M))
)

;; Characters

(deftest t-Characters)

;; nil

(deftest t-nil)

;; Booleans

(deftest t-Booleans)

;; Keywords

(deftest t-Keywords)

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

;; Meta (^)

(deftest t-Meta)

;; Deref (@)

(deftest t-Deref)

;; Dispatch (#)

;; #{} - see Sets above

;; Regex patterns (#"pattern")

(deftest t-Regex)

;; Metadata (#^)

(deftest t-Metadata)

;; Var-quote (#')

(deftest t-Var-quote)

;; Anonymous function literal (#())

(deftest t-Anonymouns-function-literal)

;; Syntax-quote (`, note, the "backquote" character), Unquote (~) and
;; Unquote-splicing (~@)

(deftest t-Syntax-quote
  (are (= _1 _2)
      `() ()    ; was NPE before SVN r1337
  ))

;; (read)
;; (read stream)
;; (read stream eof-is-error)
;; (read stream eof-is-error eof-value)
;; (read stream eof-is-error eof-value is-recursive)

(deftest t-read)
