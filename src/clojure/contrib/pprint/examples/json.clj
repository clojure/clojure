;;; json.clj: A pretty printing version of the JavaScript Object Notation (JSON) generator

;; by Tom Faulhaber, based on the version by Stuart Sierra (clojure.contrib.json.write)
;; May 9, 2009

;; Copyright (c) Tom Faulhaber/Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns 
  #^{:author "Tom Faulhaber (based on the version by Stuart Sierra)",
     :doc "Pretty printing JavaScript Object Notation (JSON) generator.

This is an example of using a pretty printer dispatch function to generate JSON output",
     :see-also [["http://json.org/", "JSON Home Page"]]}
  clojure.contrib.pprint.examples.json
  (:require [clojure.contrib.java-utils :as j])
  (:use [clojure.contrib.test-is :only (deftest- is)]
        [clojure.contrib.pprint :only (write formatter-out)]))



(defmulti dispatch-json
  "The dispatch function for printing objects as JSON"
  {:arglists '[[x]]} 
  (fn [x] (cond
            (nil? x) nil ;; prevent NullPointerException on next line
            (.isArray (class x)) ::array
            :else (type x))))

;; Primitive types can be printed with Clojure's pr function.
(derive java.lang.Boolean ::pr)
(derive java.lang.Byte ::pr)
(derive java.lang.Short ::pr)
(derive java.lang.Integer ::pr)
(derive java.lang.Long ::pr)
(derive java.lang.Float ::pr)
(derive java.lang.Double ::pr)

;; Collection types can be printed as JSON objects or arrays.
(derive java.util.Map ::object)
(derive java.util.Collection ::array)

;; Symbols and keywords are converted to strings.
(derive clojure.lang.Symbol ::symbol)
(derive clojure.lang.Keyword ::symbol)


(defmethod dispatch-json ::pr [x] (pr x))

(defmethod dispatch-json nil [x] (print "null"))

(defmethod dispatch-json ::symbol [x] (pr (name x)))

(defmethod dispatch-json ::array [s] 
  ((formatter-out "~<[~;~@{~w~^, ~:_~}~;]~:>") s))

(defmethod dispatch-json ::object [m]
  ((formatter-out "~<{~;~@{~<~w:~_~w~:>~^, ~_~}~;}~:>") 
   (for [[k v] m] [(j/as-str k) v])))

(defmethod dispatch-json java.lang.CharSequence [s]
  (print \")
  (dotimes [i (count s)]
    (let [cp (Character/codePointAt s i)]
      (cond
        ;; Handle printable JSON escapes before ASCII
        (= cp 34) (print "\\\"")
        (= cp 92) (print "\\\\")
        (= cp 47) (print "\\/")
        ;; Print simple ASCII characters
        (< 31 cp 127) (print (.charAt s i))
        ;; Handle non-printable JSON escapes
        (= cp 8) (print "\\b")
        (= cp 12) (print "\\f")
        (= cp 10) (print "\\n")
        (= cp 13) (print "\\r")
        (= cp 9) (print "\\t")
        ;; Any other character is printed as Hexadecimal escape
        :else (printf "\\u%04x" cp))))
  (print \"))

(defn print-json 
  "Prints x as JSON.  Nil becomes JSON null.  Keywords become
  strings, without the leading colon.  Maps become JSON objects, all
  other collection types become JSON arrays.  Java arrays become JSON
  arrays.  Unicode characters in strings are escaped as \\uXXXX.
  Numbers print as with pr."
  [x] 
  (write x :dispatch dispatch-json))

(defn json-str
  "Converts x to a JSON-formatted string."
  [x]
  (with-out-str (print-json x)))



;;; TESTS

;; Run these tests with
;; (clojure.contrib.test-is/run-tests 'clojure.contrib.print-json)

;; Bind clojure.contrib.test-is/*load-tests* to false to omit these
;; tests from production code.

(deftest- can-print-json-strings
  (is (= "\"Hello, World!\"" (json-str "Hello, World!")))
  (is (= "\"\\\"Embedded\\\" Quotes\"" (json-str "\"Embedded\" Quotes"))))

(deftest- can-print-unicode
  (is (= "\"\\u1234\\u4567\"" (json-str "\u1234\u4567"))))

(deftest- can-print-json-null
  (is (= "null" (json-str nil))))

(deftest- can-print-json-arrays
  (is (= "[1, 2, 3]" (json-str [1 2 3])))
  (is (= "[1, 2, 3]" (json-str (list 1 2 3))))
  (is (= "[1, 2, 3]" (json-str (sorted-set 1 2 3))))
  (is (= "[1, 2, 3]" (json-str (seq [1 2 3])))))

(deftest- can-print-java-arrays
  (is (= "[1, 2, 3]" (json-str (into-array [1 2 3])))))

(deftest- can-print-empty-arrays
  (is (= "[]" (json-str [])))
  (is (= "[]" (json-str (list))))
  (is (= "[]" (json-str #{}))))

(deftest- can-print-json-objects
  (is (= "{\"a\":1, \"b\":2}" (json-str (sorted-map :a 1 :b 2)))))

(deftest- object-keys-must-be-strings
  (is (= "{\"1\":1, \"2\":2}" (json-str (sorted-map 1 1 2 2)))))

(deftest- can-print-empty-objects
  (is (= "{}" (json-str {}))))
