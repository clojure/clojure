;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Authors: Stuart Halloway, Frantisek Sodomka

(ns clojure.test-clojure.metadata
  (:use clojure.test
        [clojure.test-helper :only (eval-in-temp-ns)]))

(def public-namespaces
  '[clojure.core
    clojure.pprint
    clojure.inspector
    clojure.set
    clojure.stacktrace
    clojure.test
    clojure.walk
    clojure.xml
    clojure.zip
    clojure.java.io
    clojure.java.browse
    clojure.java.javadoc
    clojure.java.shell
    clojure.string
    clojure.data])

(doseq [ns public-namespaces]
  (require ns))

(def public-vars
  (mapcat #(vals (ns-publics %)) public-namespaces))

(def public-vars-with-docstrings
  (filter (comp :doc meta) public-vars))

(def public-vars-with-docstrings-not-generated
  (remove #(re-find #"^->[A-Z]" (name (.sym %))) public-vars-with-docstrings))

(deftest public-vars-with-docstrings-have-added
  (is (= [] (remove (comp :added meta) public-vars-with-docstrings-not-generated))))

(deftest interaction-of-def-with-metadata
  (testing "initial def sets metadata"
    (let [v (eval-in-temp-ns
             (def ^{:a 1} foo 0)
             #'foo)]
      (is (= 1 (-> v meta :a)))))
  #_(testing "subsequent declare doesn't overwrite metadata"
    (let [v (eval-in-temp-ns
             (def ^{:b 2} bar 0)
             (declare bar)
             #'bar)]
      (is (= 2 (-> v meta :b))))
    (testing "when compiled"
      (let [v (eval-in-temp-ns
               (def ^{:c 3} bar 0)
               (defn declare-bar []
                 (declare bar))
               (declare-bar)
               #'bar)]
        (is (= 3 (-> v meta :c))))))
  (testing "subsequent def with init-expr *does* overwrite metadata"
    (let [v (eval-in-temp-ns
             (def ^{:d 4} quux 0)
             (def quux 1)
             #'quux)]
      (is (nil? (-> v meta :d))))
    (testing "when compiled"
      (let [v (eval-in-temp-ns
               (def ^{:e 5} quux 0)
               (defn def-quux []
                 (def quux 1))
               (def-quux)
               #'quux)]
        (is (nil? (-> v meta :e))))))
  (testing "IllegalArgumentException should not be thrown"
    (testing "when defining var whose value is calculated with a primitive fn."
      (testing "This case fails without a fix for CLJ-852"
        (is (eval-in-temp-ns
             (defn foo ^long [^long x] x)
             (def x (inc (foo 10))))))
      (testing "This case should pass even without a fix for CLJ-852"
        (is (eval-in-temp-ns
             (defn foo ^long [^long x] x)
             (def x (foo (inc 10)))))))))
