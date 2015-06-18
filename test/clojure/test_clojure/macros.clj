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

(def constantly-nil (constantly nil))

(deftest some->test
  (is (nil? (some-> nil)))
  (is (= 0 (some-> 0)))
  (is (= -1 (some-> 1 (- 2))))
  (is (nil? (some-> 1 constantly-nil (- 2)))))

(deftest some->>test
  (is (nil? (some->> nil)))
  (is (= 0 (some->> 0)))
  (is (= 1 (some->> 1 (- 2))))
  (is (nil? (some->> 1 constantly-nil (- 2)))))

(deftest cond->test
  (is (= 0 (cond-> 0)))
  (is (= -1 (cond-> 0 true inc true (- 2))))
  (is (= 0 (cond-> 0 false inc)))
  (is (= -1 (cond-> 1 true (- 2) false inc))))

(deftest cond->>test
  (is (= 0 (cond->> 0)))
  (is (= 1 (cond->> 0 true inc true (- 2))))
  (is (= 0 (cond->> 0 false inc)))
  (is (= 1 (cond->> 1 true (- 2) false inc))))

(deftest as->test
  (is (= 0 (as-> 0 x)))
  (is (= 1 (as-> 0 x (inc x))))
  (is (= 2 (as-> [0 1] x
             (map inc x)
             (reverse x)
             (first x)))))

(deftest threading-loop-recur
  (is (nil? (loop []
              (as-> 0 x
                (when-not (zero? x)
                  (recur))))))
  (is (nil? (loop [x nil] (some-> x recur))))
  (is (nil? (loop [x nil] (some->> x recur))))
  (is (= 0 (loop [x 0] (cond-> x false recur))))
  (is (= 0 (loop [x 0] (cond->> x false recur)))))
