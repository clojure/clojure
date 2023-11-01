;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
; Authors: Fogus

(ns clojure.test-clojure.arg-tags
  (:use clojure.test)
  (:import (clojure.lang Tuple)
           (java.util Arrays UUID Locale)))

(set! *warn-on-reflection* true)

(deftest arg-tags-in-invocation-positions
  (is (= 3 (^[long] Math/abs -3)))
  (is (= "A" (^[] .toUpperCase "a")))
  (is (= "A" (^[java.util.Locale] .toUpperCase "a" java.util.Locale/ENGLISH)))
  (is (= [1 2] (^[_ _] Tuple/create 1 2)))
  (is (= (^[long long] UUID. 1 2) #uuid "00000000-0000-0001-0000-000000000002"))
  (testing "qualified instance method invocation"
    (is (= "A" (.String/toUpperCase "a")))
    (is (= "A" (^[java.util.Locale] .String/toUpperCase "a" java.util.Locale/ENGLISH)))
    (is (= "A" (^[Locale] .String/toUpperCase "a" java.util.Locale/ENGLISH))))
  (testing "array resolutions"
     (let [lary (long-array [1 2 3 4 99 100])
           oary (into-array [1 2 3 4 99 100])
           sary (into-array String ["a" "b" "c"])]
       (is (= 4 (^[longs long] Arrays/binarySearch lary (long 99))))
       (is (= 4 (^[objects _] Arrays/binarySearch oary 99)))
       (is (= 4 (^["[Ljava.lang.Object;" _] Arrays/binarySearch oary 99)))
       (is (= 1 (^["[Ljava.lang.Object;" _] Arrays/binarySearch sary "b")))))
  (testing "array type symbolic arg-tags"
     (= 4 (^[long-* long] Arrays/binarySearch (long-array [1 2 3 4 99 100]) 99))))
