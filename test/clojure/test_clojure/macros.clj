;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka

(ns clojure.test-clojure.macros
  (:use clojure.test))

; http://clojure.org/macros

; ->
; defmacro definline macroexpand-1 macroexpand


;; -> and ->> should not be dependent on the meaning of their arguments

(defmacro c
  [arg]
  (if (= 'b (first arg))
    :foo
    :bar))

(deftest ->test
  (let [a 2, b identity]
    (is (= (-> a b c)
           (c (b a))))))

(deftest ->>test
  (let [a 2, b identity]
    (is (= (->> a b c)
           (c (b a))))))

(deftest ->metadata-test
  (testing "a trivial form"
    (is (= {:hardy :har :har :-D}
           (meta (macroexpand-1 (list `-> (with-meta
                                            'quoted-symbol
                                            {:hardy :har :har :-D})))))))
  (testing "a nontrivial form"
    (let [a (with-meta 'a {:foo :bar})
          b (with-meta '(b c d) {:bar :baz})
          e (with-meta 'e {:baz :quux})
          expanded (macroexpand-1 (list `-> a b e))]
      (is (= expanded '(e (b a c d))))
      (is (= {:baz :quux} (meta (first expanded))))
      (is (= {:bar :baz} (meta (second expanded))))
      (is (= {:foo :bar} (meta (second (second expanded))))))))


(deftest ->>metadata-test
  (testing "a trivial form"
    (is (= {:hardy :har :har :-D}
           (meta (macroexpand-1 (list `->> (with-meta
                                             'quoted-symbol
                                             {:hardy :har :har :-D})))))))
  (testing "a non-trivial form"
    (let [a (with-meta 'a {:foo :bar})
          b (with-meta '(b c d) {:bar :baz})
          e (with-meta 'e {:baz :quux})
          expanded (macroexpand-1 (list `->> a b e))]
      (is (= expanded '(e (b c d a))))
      (is (= {:baz :quux} (meta (first expanded))))
      (is (= {:bar :baz} (meta (second expanded))))
      (is (= {:foo :bar} (meta (last (second expanded))))))))
