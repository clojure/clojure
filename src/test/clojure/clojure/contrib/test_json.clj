(ns clojure.contrib.test-json
  (:use clojure.test clojure.contrib.json))

(deftest can-read-from-pushback-reader
  (let [s (java.io.PushbackReader. (java.io.StringReader. "42"))]
    (is (= 42 (read-json s)))))

(deftest can-read-from-reader
  (let [s (java.io.StringReader. "42")]
    (is (= 42 (read-json s)))))

(deftest can-read-numbers
  (is (= 42 (read-json "42")))
  (is (= -3 (read-json "-3")))
  (is (= 3.14159 (read-json "3.14159")))
  (is (= 6.022e23 (read-json "6.022e23"))))

(deftest can-read-null
  (is (= nil (read-json "null"))))

(deftest can-read-strings
  (is (= "Hello, World!" (read-json "\"Hello, World!\""))))

(deftest handles-escaped-slashes-in-strings
  (is (= "/foo/bar" (read-json "\"\\/foo\\/bar\""))))

(deftest handles-unicode-escapes
  (is (= " \u0beb " (read-json "\" \\u0bEb \""))))

(deftest handles-escaped-whitespace
  (is (= "foo\nbar" (read-json "\"foo\\nbar\"")))
  (is (= "foo\rbar" (read-json "\"foo\\rbar\"")))
  (is (= "foo\tbar" (read-json "\"foo\\tbar\""))))

(deftest can-read-booleans
  (is (= true (read-json "true")))
  (is (= false (read-json "false"))))

(deftest can-ignore-whitespace
  (is (= nil (read-json "\r\n   null"))))

(deftest can-read-arrays
  (is (= [1 2 3] (read-json "[1,2,3]")))
  (is (= ["Ole" "Lena"] (read-json "[\"Ole\", \r\n \"Lena\"]"))))

(deftest can-read-objects
  (is (= {:a 1, :b 2} (read-json "{\"a\": 1, \"b\": 2}"))))

(deftest can-read-nested-structures
  (is (= {:a [1 2 {:b [3 "four"]} 5.5]}
         (read-json "{\"a\":[1,2,{\"b\":[3,\"four\"]},5.5]}"))))

(deftest disallows-non-string-keys
  (is (thrown? Exception (read-json "{26:\"z\""))))

(deftest disallows-barewords
  (is (thrown? Exception (read-json "  foo  "))))

(deftest disallows-unclosed-arrays
  (is (thrown? Exception (read-json "[1, 2,  "))))

(deftest disallows-unclosed-objects
  (is (thrown? Exception (read-json "{\"a\":1,  "))))

(deftest can-get-string-keys
  (is (= {"a" [1 2 {"b" [3 "four"]} 5.5]}
         (read-json "{\"a\":[1,2,{\"b\":[3,\"four\"]},5.5]}" false true nil))))

(declare *pass1-string*)

(deftest pass1-test
  (let [input (read-json *pass1-string* false true nil)]
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


(deftest can-print-json-strings
  (is (= "\"Hello, World!\"" (json-str "Hello, World!")))
  (is (= "\"\\\"Embedded\\\" Quotes\"" (json-str "\"Embedded\" Quotes"))))

(deftest can-print-unicode
  (is (= "\"\\u1234\\u4567\"" (json-str "\u1234\u4567"))))

(deftest can-print-json-null
  (is (= "null" (json-str nil))))

(deftest can-print-json-arrays
  (is (= "[1,2,3]" (json-str [1 2 3])))
  (is (= "[1,2,3]" (json-str (list 1 2 3))))
  (is (= "[1,2,3]" (json-str (sorted-set 1 2 3))))
  (is (= "[1,2,3]" (json-str (seq [1 2 3])))))

(deftest can-print-java-arrays
 (is (= "[1,2,3]" (json-str (into-array [1 2 3])))))

(deftest can-print-empty-arrays
  (is (= "[]" (json-str [])))
  (is (= "[]" (json-str (list))))
  (is (= "[]" (json-str #{}))))

(deftest can-print-json-objects
  (is (= "{\"a\":1,\"b\":2}" (json-str (sorted-map :a 1 :b 2)))))

(deftest object-keys-must-be-strings
  (is (= "{\"1\":1,\"2\":2") (json-str (sorted-map 1 1 2 2))))

(deftest can-print-empty-objects
  (is (= "{}" (json-str {}))))

(deftest accept-sequence-of-nils
  (is (= "[null,null,null]" (json-str [nil nil nil]))))

(deftest error-on-nil-keys
  (is (thrown? Exception (json-str {nil 1}))))

(deftest characters-in-symbols-are-escaped
  (is (= "\"foo\\u1b1b\"" (json-str (symbol "foo\u1b1b")))))

;;; Pretty-printer

(deftest pretty-printing
  (let [x (read-json *pass1-string* false)]
    (is (= x (read-json (with-out-str (pprint-json x)) false)))))
