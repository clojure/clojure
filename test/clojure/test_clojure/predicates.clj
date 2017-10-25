;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka

;;
;;  Created 1/28/2009

(ns clojure.test-clojure.predicates
  (:use clojure.test))


;; *** Type predicates ***

(def myvar 42)

(def sample-data {
  :nil nil

  :bool-true true
  :bool-false false

  :byte   (byte 7)
  :short  (short 7)
  :int    (int 7)
  :long   (long 7)
  :bigint (bigint 7)
  :float  (float 7)
  :double (double 7)
  :bigdec (bigdec 7)

  :ratio 2/3

  :character \a
  :symbol 'abc
  :keyword :kw

  :empty-string ""
  :empty-regex #""
  :empty-list ()
  :empty-lazy-seq (lazy-seq nil)
  :empty-vector []
  :empty-map {}
  :empty-set #{}
  :empty-array (into-array [])

  :string "abc"
  :regex #"a*b"
  :list '(1 2 3)
  :lazy-seq (lazy-seq [1 2 3])
  :vector [1 2 3]
  :map {:a 1 :b 2 :c 3}
  :set #{1 2 3}
  :array (into-array [1 2 3])

  :fn (fn [x] (* 2 x))

  :class java.util.Date
  :object (new java.util.Date)

  :var (var myvar)
  :delay (delay (+ 1 2))
})


(def type-preds {
  nil? [:nil]

  true?  [:bool-true]
  false? [:bool-false]
  ; boolean?

  integer?  [:byte :short :int :long :bigint]
  float?    [:float :double]
  decimal?  [:bigdec]
  ratio?    [:ratio]
  rational? [:byte :short :int :long :bigint :ratio :bigdec]
  number?   [:byte :short :int :long :bigint :ratio :bigdec :float :double]

  ; character?
  symbol?  [:symbol]
  keyword? [:keyword]

  string? [:empty-string :string]
  ; regex?

  list?   [:empty-list   :list]
  vector? [:empty-vector :vector]
  map?    [:empty-map    :map]
  set?    [:empty-set    :set]

  coll? [:empty-list     :list
         :empty-lazy-seq :lazy-seq
         :empty-vector   :vector
         :empty-map      :map
         :empty-set      :set]

  seq?  [:empty-list     :list
         :empty-lazy-seq :lazy-seq]
  ; array?

  fn?  [:fn]
  ifn? [:fn
        :empty-vector :vector :empty-map :map :empty-set :set
        :keyword :symbol :var]

  class? [:class]
  var?   [:var]
  delay? [:delay]
})


;; Test all type predicates against all data types
;;
(defn- get-fn-name [f]
  (str
    (apply str (nthnext (first (.split (str f) "_"))
                        (count "clojure.core$")))
    "?"))

(deftest test-type-preds
  (doseq [tp type-preds]
    (doseq [dt sample-data]
      (if (some #(= % (first dt)) (second tp))
        (is ((first tp) (second dt))
          (pr-str (list (get-fn-name (first tp)) (second dt))))
        (is (not ((first tp) (second dt)))
          (pr-str (list 'not (list (get-fn-name (first tp)) (second dt)))))))))


;; Additional tests:
;; http://groups.google.com/group/clojure/browse_thread/thread/537761a06edb4b06/bfd4f0705b746a38
;;
(deftest test-string?-more
  (are [x] (not (string? x))
    (new java.lang.StringBuilder "abc")
    (new java.lang.StringBuffer "xyz")))

(def pred-val-table
  (let [now (java.util.Date.)
        uuid (java.util.UUID/randomUUID)
        barray (byte-array 0)
        uri (java.net.URI. "http://clojure.org")]
    ['
     [identity   int?  pos-int?  neg-int?  nat-int?  double? boolean? indexed? seqable? ident? uuid? bigdec? inst? uri?  bytes?]
     [0          true  false     false     true      false   false    false    false    false  false false   false false false]
     [1          true  true      false     true      false   false    false    false    false  false false   false false false]
     [-1         true  false     true      false     false   false    false    false    false  false false   false false false]
     [1.0        false false     false     false     true    false    false    false    false  false false   false false false]
     [true       false false     false     false     false   true     false    false    false  false false   false false false]
     [[]         false false     false     false     false   false    true     true     false  false false   false false false]
     [nil        false false     false     false     false   false    false    true     false  false false   false false false]
     [{}         false false     false     false     false   false    false    true     false  false false   false false false]
     [:foo       false false     false     false     false   false    false    false    true   false false   false false false]
     ['foo       false false     false     false     false   false    false    false    true   false false   false false false]
     [0.0M       false false     false     false     false   false    false    false    false  false true    false false false]
     [0N         false false     false     false     false   false    false    false    false  false false   false false false]
     [uuid       false false     false     false     false   false    false    false    false  true  false   false false false]
     [uri        false false     false     false     false   false    false    false    false  false false   false true  false]
     [now        false false     false     false     false   false    false    false    false  false false   true  false false]
     [barray     false false     false     false     false   false    false    true     false  false false   false false true]]))

(deftest test-preds
  (let [[preds & rows] pred-val-table]
    (doseq [row rows]
      (let [v (first row)]
        (dotimes [i (count row)]
          (is (= ((resolve (nth preds i)) v) (nth row i))
              (pr-str (list (nth preds i) v))))))))
