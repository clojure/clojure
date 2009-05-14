;;; json/read.clj: JavaScript Object Notation (JSON) parser

;; by Stuart Sierra, http://stuartsierra.com/
;; February 13, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; Change Log
;;
;; February 13, 2009: added custom handler for quoted strings, to
;; allow escaped forward backslash characters ("\/") in strings.
;;
;; January 26, 2009: initial version


;; For more information on JSON, see http://www.json.org/
;;
;; This library parses data in JSON format.  This is a fairly strict
;; implementation of JSON as described at json.org, not a full-fledged
;; JavaScript parser.  JavaScript functions and object constructors
;; are not supported.  Object field names must be quoted strings; they
;; may not be bare symbols.



(ns 
  #^{:author "Stuart Sierra",
     :doc "JavaScript Object Notation (JSON) parser

           For more information on JSON, see http://www.json.org/

           This library parses data in JSON format.  This is a fairly strict
           implementation of JSON as described at json.org, not a full-fledged
           JavaScript parser.  JavaScript functions and object constructors
           are not supported.  Object field names must be quoted strings; they
           may not be bare symbols.

           If you want to convert map keys from strings to keywords, use
           clojure.contrib.walk/keywordize-keys
",
     :see-also [["http://www.json.org", "JSON Home Page"]]}
  clojure.contrib.json.read
  (:import (java.io PushbackReader StringReader EOFException))
  (:use [clojure.contrib.test-is :only (deftest- is)]))

(declare read-json)

(def #^{:doc "If true, JSON object keys will be converted to keywords
  instead of strings.  Defaults to false.  There are no checks that the strings form valid
  keywords."} *json-keyword-keys* false)

(defn- read-json-array [#^PushbackReader stream]
  ;; Expects to be called with the head of the stream AFTER the
  ;; opening bracket.
  (loop [i (.read stream), result []]
    (let [c (char i)]
      (cond
       (= i -1) (throw (EOFException. "JSON error (end-of-file inside array)"))
       (Character/isWhitespace c) (recur (.read stream) result)
       (= c \,) (recur (.read stream) result)
       (= c \]) result
       :else (do (.unread stream (int c))
                 (let [element (read-json stream)]
                   (recur (.read stream) (conj result element))))))))

(defn- read-json-object [#^PushbackReader stream]
  ;; Expects to be called with the head of the stream AFTER the
  ;; opening bracket.
  (loop [i (.read stream), key nil, result {}]
    (let [c (char i)]
      (cond
       (= i -1) (throw (EOFException. "JSON error (end-of-file inside object)"))

       (Character/isWhitespace c) (recur (.read stream) key result)

       (= c \,) (recur (.read stream) nil result)

       (= c \:) (recur (.read stream) key result)

       (= c \}) (if (nil? key)
                  result
                  (throw (Exception. "JSON error (key missing value in object)")))

       :else (do (.unread stream i)
                 (let [element (read-json stream)]
                   (if (nil? key)
                     (if (string? element)
                       (recur (.read stream) element result)
                       (throw (Exception. "JSON error (non-string key in object)")))
                     (recur (.read stream) nil
                            (assoc result (if *json-keyword-keys* (keyword key) key)
                                   element)))))))))

(defn- read-json-hex-character [#^PushbackReader stream]
  ;; Expects to be called with the head of the stream AFTER the
  ;; initial "\u".  Reads the next four characters from the stream.
  (let [digits [(.read stream)
                (.read stream)
                (.read stream)
                (.read stream)]]
    (when (some neg? digits)
      (throw (EOFException. "JSON error (end-of-file inside Unicode character escape)")))
    (let [chars (map char digits)]
      (when-not (every? #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f \A \B \C \D \E \F}
                        chars)
        (throw (Exception. "JSON error (invalid hex character in Unicode character escape)")))
      (char (Integer/parseInt (apply str chars) 16)))))

(defn- read-json-escaped-character [#^PushbackReader stream]
  ;; Expects to be called with the head of the stream AFTER the
  ;; initial backslash.
  (let [c (char (.read stream))]
    (cond
     (#{\" \\ \/} c) c
     (= c \b) \backspace
     (= c \f) \formfeed
     (= c \n) \newline
     (= c \r) \return
     (= c \t) \tab
     (= c \u) (read-json-hex-character stream))))

(defn- read-json-quoted-string [#^PushbackReader stream]
  ;; Expects to be called with the head of the stream AFTER the
  ;; opening quotation mark.
  (let [buffer (StringBuilder.)]
    (loop [i (.read stream)]
      (let [c (char i)]
        (cond
         (= i -1) (throw (EOFException. "JSON error (end-of-file inside string)"))
         (= c \") (str buffer)
         (= c \\) (do (.append buffer (read-json-escaped-character stream))
                      (recur (.read stream)))
         :else (do (.append buffer c)
                   (recur (.read stream))))))))

(defn read-json
  "Read the next JSON record from stream, which must be an instance of
  java.io.PushbackReader."
  ([] (read-json *in* true nil))
  ([stream] (read-json stream true nil))
  ([#^PushbackReader stream eof-error? eof-value]
     (loop [i (.read stream)]
       (let [c (char i)]
         (cond
          ;; Handle end-of-stream
          (= i -1) (if eof-error?
                     (throw (EOFException. "JSON error (end-of-file)"))
                     eof-value)

          ;; Ignore whitespace
          (Character/isWhitespace c) (recur (.read stream))

          ;; Read numbers, true, and false with Clojure reader
          (#{\- \0 \1 \2 \3 \4 \5 \6 \7 \8 \9} c)
          (do (.unread stream i)
              (read stream true nil))

          ;; Read strings
          (= c \") (read-json-quoted-string stream)

          ;; Read null as nil
          (= c \n) (let [ull [(char (.read stream))
                              (char (.read stream))
                              (char (.read stream))]]
                     (if (= ull [\u \l \l])
                       nil
                       (throw (Exception. (str "JSON error (expected null): " c ull)))))

          ;; Read true
          (= c \t) (let [rue [(char (.read stream))
                              (char (.read stream))
                              (char (.read stream))]]
                     (if (= rue [\r \u \e])
                       true
                       (throw (Exception. (str "JSON error (expected true): " c rue)))))

          ;; Read false
          (= c \f) (let [alse [(char (.read stream))
                               (char (.read stream))
                               (char (.read stream))
                               (char (.read stream))]]
                     (if (= alse [\a \l \s \e])
                       false
                       (throw (Exception. (str "JSON error (expected false): " c alse)))))

          

          ;; Read JSON objects
          (= c \{) (read-json-object stream)

          ;; Read JSON arrays
          (= c \[) (read-json-array stream)

          :else (throw (Exception. (str "JSON error (unexpected character): " c))))))))


(defn read-json-string [string]
  (read-json (PushbackReader. (StringReader. string))))


;;; TESTS

(deftest- can-read-numbers
  (is (= 42 (read-json-string "42")))
  (is (= -3 (read-json-string "-3")))
  (is (= 3.14159 (read-json-string "3.14159")))
  (is (= 6.022e23 (read-json-string "6.022e23"))))

(deftest- can-read-null
  (is (= nil (read-json-string "null"))))

(deftest- can-read-strings
  (is (= "Hello, World!" (read-json-string "\"Hello, World!\""))))

(deftest- handles-escaped-slashes-in-strings
  (is (= "/foo/bar" (read-json-string "\"\\/foo\\/bar\""))))

(deftest- handles-unicode-escapes
  (is (= " \u0beb " (read-json-string "\" \\u0bEb \""))))

(deftest- handles-escaped-whitespace
  (is (= "foo\nbar" (read-json-string "\"foo\\nbar\"")))
  (is (= "foo\rbar" (read-json-string "\"foo\\rbar\"")))
  (is (= "foo\tbar" (read-json-string "\"foo\\tbar\""))))

(deftest- can-read-booleans
  (is (= true (read-json-string "true")))
  (is (= false (read-json-string "false"))))

(deftest- can-ignore-whitespace
  (is (= nil (read-json-string "\r\n   null"))))

(deftest- can-read-arrays
  (is (= [1 2 3] (read-json-string "[1,2,3]")))
  (is (= ["Ole" "Lena"] (read-json-string "[\"Ole\", \r\n \"Lena\"]"))))

(deftest- can-read-objects
  (is (= {"a" 1, "b" 2} (read-json-string "{\"a\": 1, \"b\": 2}"))))

(deftest- can-read-nested-structures
  (is (= {"a" [1 2 {"b" [3 "four"]} 5.5]}
         (read-json-string "{\"a\":[1,2,{\"b\":[3,\"four\"]},5.5]}"))))

(deftest- disallows-non-string-keys
  (is (thrown? Exception (read-json-string "{26:\"z\""))))

(deftest- disallows-barewords
  (is (thrown? Exception (read-json-string "  foo  "))))

(deftest- disallows-unclosed-arrays
  (is (thrown? Exception (read-json-string "[1, 2,  "))))

(deftest- disallows-unclosed-objects
  (is (thrown? Exception (read-json-string "{\"a\":1,  "))))

(deftest- can-get-keyword-keys
  (is (= {:a [1 2 {:b [3 "four"]} 5.5]}
         (binding [*json-keyword-keys* true]
           (read-json-string "{\"a\":[1,2,{\"b\":[3,\"four\"]},5.5]}")))))

(declare *pass1-string*)

(deftest- pass1-test
  (let [input (read-json-string *pass1-string*)]
    (is (= "JSON Test Pattern pass1" (first input)))
    (is (= "array with 1 element" (get-in input [1 "object with 1 member" 0])))
    (is (= 1234567890 (get-in input [8 "integer"])))
    (is (= "rosebud" (last input)))))

; from http://www.json.org/JSON_checker/test/pass1.json
(def *pass1-string*
     "[
    \"JSON Test Pattern pass1\",
    {\"object with 1 member\":[\"array with 1 element\"]},
    {},
    [],
    -42,
    true,
    false,
    null,
    {
        \"integer\": 1234567890,
        \"real\": -9876.543210,
        \"e\": 0.123456789e-12,
        \"E\": 1.234567890E+34,
        \"\":  23456789012E66,
        \"zero\": 0,
        \"one\": 1,
        \"space\": \" \",
        \"quote\": \"\\\"\",
        \"backslash\": \"\\\\\",
        \"controls\": \"\\b\\f\\n\\r\\t\",
        \"slash\": \"/ & \\/\",
        \"alpha\": \"abcdefghijklmnopqrstuvwyz\",
        \"ALPHA\": \"ABCDEFGHIJKLMNOPQRSTUVWYZ\",
        \"digit\": \"0123456789\",
        \"0123456789\": \"digit\",
        \"special\": \"`1~!@#$%^&*()_+-={':[,]}|;.</>?\",
        \"hex\": \"\\u0123\\u4567\\u89AB\\uCDEF\\uabcd\\uef4A\",
        \"true\": true,
        \"false\": false,
        \"null\": null,
        \"array\":[  ],
        \"object\":{  },
        \"address\": \"50 St. James Street\",
        \"url\": \"http://www.JSON.org/\",
        \"comment\": \"// /* <!-- --\",
        \"# -- --> */\": \" \",
        \" s p a c e d \" :[1,2 , 3

,

4 , 5        ,          6           ,7        ],\"compact\":[1,2,3,4,5,6,7],
        \"jsontext\": \"{\\\"object with 1 member\\\":[\\\"array with 1 element\\\"]}\",
        \"quotes\": \"&#34; \\u0022 %22 0x22 034 &#x22;\",
        \"\\/\\\\\\\"\\uCAFE\\uBABE\\uAB98\\uFCDE\\ubcda\\uef4A\\b\\f\\n\\r\\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?\"
: \"A key can be any string\"
    },
    0.5 ,98.6
,
99.44
,

1066,
1e1,
0.1e1,
1e-1,
1e00,2e+00,2e-00
,\"rosebud\"]")