;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka


(ns clojure.test-clojure.compilation
  (:use clojure.test
        [clojure.test-helper :only (should-not-reflect should-print-err-message)]))

; http://clojure.org/compilation

; compile
; gen-class, gen-interface


(deftest test-compiler-metadata
  (let [m (meta #'when)]
    (are [x y]  (= x y)
        (list? (:arglists m)) true
        (> (count (:arglists m)) 0) true

        (string? (:doc m)) true
        (> (.length (:doc m)) 0) true
        
        (string? (:file m)) true
        (> (.length (:file m)) 0) true

        (integer? (:line m)) true
        (> (:line m) 0) true

        (:macro m) true
        (:name m) 'when )))

(deftest test-embedded-constants
  (testing "Embedded constants"
    (is (eval `(= Boolean/TYPE ~Boolean/TYPE)))
    (is (eval `(= Byte/TYPE ~Byte/TYPE)))
    (is (eval `(= Character/TYPE ~Character/TYPE)))
    (is (eval `(= Double/TYPE ~Double/TYPE)))
    (is (eval `(= Float/TYPE ~Float/TYPE)))
    (is (eval `(= Integer/TYPE ~Integer/TYPE)))
    (is (eval `(= Long/TYPE ~Long/TYPE)))
    (is (eval `(= Short/TYPE ~Short/TYPE)))))
 
(deftest test-compiler-resolution
  (testing "resolve nonexistent class create should return nil (assembla #262)"
    (is (nil? (resolve 'NonExistentClass.)))))

(deftest test-no-recur-across-try
  (testing "don't recur to function from inside try"
    (is (thrown? Exception (eval '(fn [x] (try (recur 1)))))))
  (testing "don't recur to loop from inside try"
    (is (thrown? Exception (eval '(loop [x] (try (recur 1)))))))
  (testing "don't get confused about what the recur is targeting"
    (is (thrown? Exception (eval '(loop [x] (try (fn [x]) (recur 1)))))))
  (testing "don't allow recur accross binding"
    (is (thrown? Exception (eval '(fn [x] (binding [+ *] (recur 1)))))))
  (testing "allow loop/recur inside try"
    (is (try
          (eval '(try (loop [x 3] (if (zero? x) x (recur (dec x))))))
          (catch Exception _))))
  (testing "allow fn/recur inside try"
    (is (try
          (eval '(try
                   ((fn [x]
                      (if (zero? x)
                        x
                        (recur (dec x))))
                    3)))
          (catch Exception _)))))

;; disabled until build box can call java from mvn
#_(deftest test-numeric-dispatch
  (is (= "(int, int)" (TestDispatch/someMethod (int 1) (int 1))))
  (is (= "(int, long)" (TestDispatch/someMethod (int 1) (long 1))))
  (is (= "(long, long)" (TestDispatch/someMethod (long 1) (long 1)))))

(deftest test-CLJ-671-regression
  (testing "that the presence of hints does not cause the compiler to infinitely loop"
    (letfn [(gcd [x y]
              (loop [x (long x) y (long y)]
                (if (== y 0)
                  x
                  (recur y ^Long(rem x y)))))]
      (is (= 4 (gcd 8 100))))))

;; ensure proper use of hints / type decls

(defn hinted
  (^String [])
  (^Integer [a])
  (^java.util.List [a & args]))

;; fn names need to be fully-qualified because should-not-reflect evals its arg in a throwaway namespace

(deftest recognize-hinted-arg-vector
  (should-not-reflect #(.substring (clojure.test-clojure.compilation/hinted) 0))
  (should-not-reflect #(.floatValue (clojure.test-clojure.compilation/hinted "arg")))
  (should-not-reflect #(.size (clojure.test-clojure.compilation/hinted :many :rest :args :here))))

(defn ^String hinting-conflict ^Integer [])

(deftest calls-use-arg-vector-hint
  (should-not-reflect #(.floatValue (clojure.test-clojure.compilation/hinting-conflict)))
  (should-print-err-message #"(?s)Reflection warning.*"
    #(.substring (clojure.test-clojure.compilation/hinting-conflict) 0)))

(deftest deref-uses-var-tag
  (should-not-reflect #(.substring clojure.test-clojure.compilation/hinting-conflict 0))
  (should-print-err-message #"(?s)Reflection warning.*"
    #(.floatValue clojure.test-clojure.compilation/hinting-conflict)))

(defn ^String legacy-hinting [])

(deftest legacy-call-hint
  (should-not-reflect #(.substring (clojure.test-clojure.compilation/legacy-hinting) 0)))

(defprotocol HintedProtocol
  (hintedp ^String [a]
           ^Integer [a b]))

(deftest hinted-protocol-arg-vector
  (should-not-reflect #(.substring (clojure.test-clojure.compilation/hintedp "") 0))
  (should-not-reflect #(.floatValue (clojure.test-clojure.compilation/hintedp :a :b))))

(defn primfn
  (^long [])
  (^double [a]))

(deftest primitive-return-decl
  (should-not-reflect #(loop [k 5] (recur (clojure.test-clojure.compilation/primfn))))
  (should-not-reflect #(loop [k 5.0] (recur (clojure.test-clojure.compilation/primfn 0))))
  
  (should-print-err-message #"(?s).*k is not matching primitive.*"
    #(loop [k (clojure.test-clojure.compilation/primfn)] (recur :foo))))
