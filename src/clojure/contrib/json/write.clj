;;; json/write.clj: JavaScript Object Notation (JSON) generator

;; by Stuart Sierra, http://stuartsierra.com/
;; January 26, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.



;; For more information on JSON, see http://www.json.org/



(ns clojure.contrib.json.write
  (:use [clojure.contrib.test-is :only (deftest is)]))

(defmulti
  #^{:doc "Prints Clojure data types as JSON.  Nil becomes JSON null.
  Keywords become strings, without the leading colon.  Maps become
  JSON objects, all other collection types become JSON arrays.
  Strings and numbers print as with pr."}
  print-json (fn [x]
               (cond (nil? x) nil
                     (map? x) :object
                     (coll? x) :array
                     (keyword? x) :keyword
                     :else :default)))

(defmethod print-json :default [x] (pr x))

(defmethod print-json nil [x] (print "null"))

(defmethod print-json :keyword [x] (pr (name x)))

(defmethod print-json :array [s]
  (print "[")
  (loop [x s]
    (when (first x)
      (print-json (first x))
      (when (rest x)
        (print ",")
        (recur (rest x)))))
  (print "]"))

(defmethod print-json :object [m]
  (print "{")
  (loop [x m]
    (when (first x)
      (let [[k v] (first x)]
        (print-json k)
        (print ":")
        (print-json v))
      (when (rest x)
        (print ",")
        (recur (rest x)))))
  (print "}"))

(defn json-str
  "Converts Clojure data structures to a JSON-formatted string."
  [x]
  (with-out-str (print-json x)))



;;; TESTS

;; Run these tests with
;; (clojure.contrib.test-is/run-tests 'clojure.contrib.print-json)

;; Bind clojure.contrib.test-is/*load-tests* to false to omit these
;; tests from production code.

(deftest can-print-json-strings
  (is (= "\"Hello, World!\"" (json-str "Hello, World!")))
  (is (= "\"\\\"Embedded\\\" Quotes\"" (json-str "\"Embedded\" Quotes"))))

(deftest can-print-json-null
  (is (= "null" (json-str nil))))

(deftest can-print-json-arrays
  (is (= "[1,2,3]" (json-str [1 2 3])))
  (is (= "[1,2,3]" (json-str (list 1 2 3))))
  (is (= "[1,2,3]" (json-str (sorted-set 1 2 3))))
  (is (= "[1,2,3]" (json-str (seq [1 2 3])))))

(deftest can-print-empty-arrays
  (is (= "[]" (json-str [])))
  (is (= "[]" (json-str (list))))
  (is (= "[]" (json-str #{}))))

(deftest can-print-json-objects
  (is (= "{\"a\":1,\"b\":2}" (json-str (sorted-map :a 1 :b 2)))))

(deftest can-print-empty-objects
  (is (= "{}" (json-str {}))))
