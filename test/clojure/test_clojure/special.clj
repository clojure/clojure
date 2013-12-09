;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka

;;
;;  Test special forms, macros and metadata
;;

(ns clojure.test-clojure.special
  (:use clojure.test))

; http://clojure.org/special_forms

; let, letfn
; quote
; var
; fn

(deftest multiple-keys-in-destructuring
  (let [foo (fn [& {:keys [x]}] x)
        bar (fn [& options] (apply foo :x :b options))]
    (is (= (bar) :b))
    (is (= (bar :x :a) :a))))

(deftest empty-list-with-:as-destructuring
  (let [{:as x} '()]
    (is (= {} x))))

(deftest keywords-in-destructuring
  (let [{:keys [:a :b]} {:a 1 :b 2}]
    (is (= 1 a))
    (is (= 2 b))))

(deftest namespaced-keywords-in-destructuring
  (let [{:keys [:a/b :c/d]} {:a/b 1 :c/d 2}]
    (is (= 1 b))
    (is (= 2 d))))

(deftest namespaced-keys-in-destructuring
  (let [{:keys [a/b c/d]} {:a/b 1 :c/d 2}]
    (is (= 1 b))
    (is (= 2 d))))

(deftest namespaced-syms-in-destructuring
  (let [{:syms [a/b c/d]} {'a/b 1 'c/d 2}]
    (is (= 1 b))
    (is (= 2 d))))

(deftest keywords-not-allowed-in-let-bindings
  (is (thrown-with-msg? Exception #"Unsupported binding form: :a"
                        (eval '(let [:a 1] a))))
  (is (thrown-with-msg? Exception #"Unsupported binding form: :a/b"
                        (eval '(let [:a/b 1] b))))
  (is (thrown-with-msg? Exception #"Unsupported binding form: :a"
                        (eval '(let [[:a] [1]] a))))
  (is (thrown-with-msg? Exception #"Unsupported binding form: :a/b"
                        (eval '(let [[:a/b] [1]] b)))))

(deftest namespaced-syms-only-allowed-in-map-destructuring
  (is (thrown-with-msg? Exception #"Can't let qualified name: a/x"
                        (eval '(let [a/x 1, [y] [1]] x))))
  (is (thrown-with-msg? Exception #"Can't let qualified name: a/x"
                        (eval '(let [[a/x] [1]] x)))))

(require '[clojure.string :as s])
(deftest resolve-keyword-ns-alias-in-destructuring
  (let [{:keys [::s/x ::s/y]} {:clojure.string/x 1 :clojure.string/y 2}]
    (is (= x 1))
    (is (= y 2))))

(deftest quote-with-multiple-args
  (let [ex (is (thrown? clojure.lang.Compiler$CompilerException
                        (eval '(quote 1 2 3))))]
    (is (= '(quote 1 2 3)
           (-> ex
               (.getCause)
               (ex-data)
               (:form))))))
