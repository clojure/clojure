;;  Copyright (c) Frantisek Sodomka. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  Created 1/28/2009

(ns clojure.contrib.test-clojure.predicates
  (:use clojure.contrib.test-is))


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
  :empty-vector []
  :empty-map {}
  :empty-set #{}
  :empty-array (into-array [])

  :string "abc"
  :regex #"a*b"
  :list '(1 2 3)
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

  list?   [:empty-list :list]
  vector? [:empty-vector :vector]
  map?    [:empty-map :map]
  set?    [:empty-set :set]

  coll? [:empty-list   :list
         :empty-vector :vector
         :empty-map    :map
         :empty-set    :set]
  seq? [:list]
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
(deftest test-type-preds
  (doseq [tp type-preds]
    (doseq [dt sample-data]
      (if (some #(= % (first dt)) (second tp))
        (is ((first tp) (second dt))
          (pr-str (list (first tp) (second dt))))
        (is (not ((first tp) (second dt)))
          (pr-str (list 'not (list (first tp) (second dt)))))))))


;; Additional tests:
;; http://groups.google.com/group/clojure/browse_thread/thread/537761a06edb4b06/bfd4f0705b746a38
;;
(deftest test-string?-more
  (are (not (string? _))
    (new java.lang.StringBuilder "abc")
    (new java.lang.StringBuffer "xyz")))
