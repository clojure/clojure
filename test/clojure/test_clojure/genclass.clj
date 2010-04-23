;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Stuart Halloway

(ns clojure.test-clojure.genclass
  (:use clojure.test)
  (:import clojure.test_clojure.genclass.examples.ExampleClass))

;; pull this up to a suite-wide helper if you find other tests need it!
(defn get-field
  "Access to private or protected field.  field-name is a symbol or
  keyword."
  ([klass field-name]
     (get-field klass field-name nil))
  ([klass field-name inst]
     (-> klass (.getDeclaredField (name field-name))
         (doto (.setAccessible true))
         (.get inst))))

(deftest arg-support
  (let [example (ExampleClass.)
        o (Object.)]
    (is (= "foo with o, o" (.foo example o o)))
    (is (= "foo with o, i" (.foo example o (int 1))))
    (is (thrown? java.lang.UnsupportedOperationException (.foo example o)))))

(deftest name-munging
  (testing "mapping from Java fields to Clojure vars"
    (is (= #'clojure.test-clojure.genclass.examples/-foo-Object-int
           (get-field ExampleClass 'foo_Object_int__var)))
    (is (= #'clojure.test-clojure.genclass.examples/-toString
           (get-field ExampleClass 'toString__var)))))
