;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  test-util.clj
;;
;;  A Clojure implementation of Datalog -- Utilities Tests
;;
;;  straszheimjeffrey (gmail)
;;  Created 11 Feburary 2009

(ns clojure.contrib.datalog.tests.test-util
  (:use clojure.contrib.test-is
	clojure.contrib.datalog.util)
  (:use [clojure.contrib.except :only (throwf)]))

(deftest test-is-var?
  (is (is-var? '?x))
  (is (is-var? '?))
  (is (not (is-var? '??x)))
  (is (not (is-var? '??)))
  (is (not (is-var? 'x)))
  (is (not (is-var? "fred")))
  (is (not (is-var? :q))))

(deftest test-map-values
  (let [map {:fred 1 :sally 2}]
    (is (= (map-values #(* 2 %) map) {:fred 2 :sally 4}))
    (is (= (map-values identity {}) {}))))

(deftest test-keys-to-vals
  (let [map {:fred 1 :sally 2 :joey 3}]
    (is (= (set (keys-to-vals map [:fred :sally])) #{1 2}))
    (is (= (set (keys-to-vals map [:fred :sally :becky])) #{1 2}))
    (is (empty? (keys-to-vals map [])))
    (is (empty? (keys-to-vals {} [:fred])))))

(deftest test-reverse-map
  (let [map {:fred 1 :sally 2 :joey 3}
        map-1 (assoc map :mary 3)]
    (is (= (reverse-map map) {1 :fred 2 :sally 3 :joey}))
    (is (or (= (reverse-map map-1) {1 :fred 2 :sally 3 :joey})
            (= (reverse-map map-1) {1 :fred 2 :sally 3 :mary})))))

(def some-maps
     [
      { :a 1 :b 2 }
      { :c 3 :b 3 }
      { :d 4 :a 1 }
      { :g 4 :b 4 }
      { :a 2 :b 1 }
      { :e 1 :f 1 }
      ])

(def reduced (preduce + some-maps))
(def merged (apply merge-with + some-maps))

(deftest test-preduce
  (is (= reduced merged)))

(comment
 (run-tests)
)

; End of file
