;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.


;;  Tests for the Clojure functions documented at the URL:
;;
;;    http://clojure.org/Evaluation
;;
;;  by J. McConnell
;;  Created 22 October 2008

(ns clojure.test-clojure.evaluation
  (:use clojure.test))

(import '(java.lang Boolean)
        '(clojure.lang Compiler Compiler$CompilerException))

(defmacro test-that
  "Provides a useful way for specifying the purpose of tests. If the first-level
  forms are lists that make a call to a clojure.test function, it supplies the
  purpose as the msg argument to those functions. Otherwise, the purpose just
  acts like a comment and the forms are run unchanged."
  [purpose & test-forms]
  (let [tests (map
                #(if (= (:ns (meta (resolve (first %))))
                        (the-ns 'clojure.test))
                   (concat % (list purpose))
                   %)
                test-forms)]
    `(do ~@tests)))

(deftest Eval
  (is (= (eval '(+ 1 2 3)) (Compiler/eval '(+ 1 2 3))))
  (is (= (eval '(list 1 2 3)) '(1 2 3)))
  (is (= (eval '(list + 1 2 3)) (list clojure.core/+ 1 2 3)))
  (test-that "Non-closure fns are supported as code"
             (is (= (eval (eval '(list + 1 2 3))) 6)))
  (is (= (eval (list '+ 1 2 3)) 6)))

; not using Clojure's RT/classForName since a bug in it could hide a bug in
; eval's resolution
(defn class-for-name [name]
  (java.lang.Class/forName name))

(defmacro in-test-ns [& body]
  `(binding [*ns* *ns*]
     (in-ns 'clojure.test-clojure.evaluation)
     ~@body))

;;; Literals tests ;;;

(defmacro #^{:private true} evaluates-to-itself? [expr]
  `(let [v# ~expr
         q# (quote ~expr)]
     (is (= (eval q#) q#) (str q# " does not evaluate to itself"))))

(deftest Literals
  ; Strings, numbers, characters, nil and keywords should evaluate to themselves
  (evaluates-to-itself? "test")
  (evaluates-to-itself? "test
                        multi-line
                        string")
  (evaluates-to-itself? 1)
  (evaluates-to-itself? 1.0)
  (evaluates-to-itself? 1.123456789)
  (evaluates-to-itself? 1/2)
  (evaluates-to-itself? 1M)
  (evaluates-to-itself? 999999999999999999)
  (evaluates-to-itself? \a)
  (evaluates-to-itself? \newline)
  (evaluates-to-itself? nil)
  (evaluates-to-itself? :test)
  ; Boolean literals should evaluate to Boolean.{TRUE|FALSE}
  (is (identical? (eval true) Boolean/TRUE))
  (is (identical? (eval false) Boolean/FALSE)))

;;; Symbol resolution tests ;;;

(def foo "abc")
(in-ns 'resolution-test)
(def bar 123)
(def #^{:private true} baz 456)
(in-ns 'clojure.test-clojure.evaluation)

(defn a-match? [re s] (not (nil? (re-matches re s))))

(defmacro throws-with-msg
  ([re form] `(throws-with-msg ~re ~form Exception))
  ([re form x] `(throws-with-msg
                  ~re
                  ~form
                  ~(if (instance? Exception x) x Exception)
                  ~(if (instance? String x) x nil)))
  ([re form class msg]
       `(let [ex# (try
                    ~form
                    (catch ~class e# e#)
                    (catch Exception e#
                      (let [cause# (.getCause e#)]
                        (if (= ~class (class cause#)) cause# (throw e#)))))]
          (is (a-match? ~re (.toString ex#))
              (or ~msg
                  (str "Expected exception that matched " (pr-str ~re)
                       ", but got exception with message: \"" ex#))))))

(deftest SymbolResolution
  (test-that
    "If a symbol is namespace-qualified, the evaluated value is the value
     of the binding of the global var named by the symbol"
    (is (= (eval 'resolution-test/bar) 123)))

  (test-that
    "It is an error if there is no global var named by the symbol"
    (throws-with-msg
      #".*Unable to resolve symbol: bar.*" (eval 'bar)))

  (test-that
    "It is an error if the symbol reference is to a non-public var in a
    different namespace"
    (throws-with-msg
      #".*resolution-test/baz is not public.*"
      (eval 'resolution-test/baz)
      Compiler$CompilerException))

  (test-that
    "If a symbol is package-qualified, its value is the Java class named by the
    symbol"
    (is (= (eval 'java.lang.Math) (class-for-name "java.lang.Math"))))

  (test-that
    "If a symbol is package-qualified, it is an error if there is no Class named
    by the symbol"
    (is (thrown? Compiler$CompilerException (eval 'java.lang.FooBar))))

  (test-that
    "If a symbol is not qualified, the following applies, in this order:

      1. If it names a special form it is considered a special form, and must
         be utilized accordingly.

      2. A lookup is done in the current namespace to see if there is a mapping
         from the symbol to a class. If so, the symbol is considered to name a
         Java class object.

      3. If in a local scope (i.e. in a function definition), a lookup is done
         to see if it names a local binding (e.g. a function argument or
         let-bound name). If so, the value is the value of the local binding.

      4. A lookup is done in the current namespace to see if there is a mapping
         from the symbol to a var. If so, the value is the value of the binding
         of the var referred-to by the symbol.

      5. It is an error."

    ; First
    (doall (for [form '(def if do let quote var fn loop recur throw try
                         monitor-enter monitor-exit)]
             (is (thrown? Compiler$CompilerException (eval form)))))
    (let [if "foo"]
      (is (thrown? Compiler$CompilerException (eval 'if)))

    ; Second
      (is (= (eval 'Boolean) (class-for-name "java.lang.Boolean"))))
    (let [Boolean "foo"]
      (is (= (eval 'Boolean) (class-for-name "java.lang.Boolean"))))

    ; Third
    (is (= (eval '(let [foo "bar"] foo)) "bar"))

    ; Fourth
    (in-test-ns (is (= (eval 'foo) "abc")))
    (is (thrown? Compiler$CompilerException (eval 'bar))) ; not in this namespace

    ; Fifth
    (is (thrown? Compiler$CompilerException (eval 'foobar)))))

;;; Metadata tests ;;;

(defstruct struct-with-symbols (with-meta 'k {:a "A"}))

(deftest Metadata

  (test-that
    "find returns key symbols and their metadata"
    (let [s (struct struct-with-symbols 1)]
      (is (= {:a "A"} (meta (first (find s 'k))))))))

;;; Collections tests ;;;
(def x 1)
(def y 2)

(deftest Collections
  (in-test-ns
    (test-that
      "Vectors and Maps yield vectors and (hash) maps whose contents are the
      evaluated values of the objects they contain."
      (is (= (eval '[x y 3]) [1 2 3]))
      (is (= (eval '{:x x :y y :z 3}) {:x 1 :y 2 :z 3}))
      (is (instance? clojure.lang.IPersistentMap (eval '{:x x :y y})))))

  (in-test-ns
    (test-that
      "Metadata maps yield maps whose contents are the evaluated values of
      the objects they contain. If a vector or map has metadata, the evaluated
      metadata map will become the metadata of the resulting value."
      (is (= (eval #^{:x x} '[x y]) #^{:x 1} [1 2]))))

  (test-that
    "An empty list () evaluates to an empty list."
    (is (= (eval '()) ()))
    (is (empty? (eval ())))
    (is (= (eval (list)) ())))

  ;aargh, fragile tests, please fix
  #_(test-that
    "Non-empty lists are considered calls"
    (is (thrown? Compiler$CompilerException (eval '(1 2 3))))))

(deftest Macros)

(deftest Loading)
