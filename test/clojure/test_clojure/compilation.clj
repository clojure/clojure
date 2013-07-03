;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka


(ns clojure.test-clojure.compilation
  (:import (clojure.lang Compiler Compiler$CompilerException))
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

        (integer? (:column m)) true
        (> (:column m) 0) true

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
    (is (thrown? Compiler$CompilerException
                 (eval '(fn [x] (try (recur 1)))))))
  (testing "don't recur to loop from inside try"
    (is (thrown? Compiler$CompilerException
                 (eval '(loop [x 5]
                          (try (recur 1)))))))
  (testing "don't recur to loop from inside of catch inside of try"
    (is (thrown? Compiler$CompilerException
                 (eval '(loop [x 5]
                          (try
                            (catch Exception e
                              (recur 1))))))))
  (testing "don't recur to loop from inside of finally inside of try"
    (is (thrown? Compiler$CompilerException
                 (eval '(loop [x 5]
                          (try
                            (finally
                              (recur 1))))))))
  (testing "don't get confused about what the recur is targeting"
    (is (thrown? Compiler$CompilerException
                 (eval '(loop [x 5]
                          (try (fn [x]) (recur 1)))))))
  (testing "don't allow recur across binding"
    (is (thrown? Compiler$CompilerException
                 (eval '(fn [x] (binding [+ *] (recur 1)))))))
  (testing "allow loop/recur inside try"
    (is (= 0 (eval '(try (loop [x 3]
                           (if (zero? x) x (recur (dec x)))))))))
  (testing "allow loop/recur fully inside catch"
    (is (= 3 (eval '(try
                      (throw (Exception.))
                      (catch Exception e
                        (loop [x 0]
                          (if (< x 3) (recur (inc x)) x))))))))
  (testing "allow loop/recur fully inside finally"
    (is (= "012" (eval '(with-out-str
                          (try
                            :return-val-discarded-because-of-with-out-str
                            (finally (loop [x 0]
                                       (when (< x 3)
                                         (print x)
                                         (recur (inc x)))))))))))
  (testing "allow fn/recur inside try"
    (is (= 0 (eval '(try
                      ((fn [x]
                         (if (zero? x)
                           x
                           (recur (dec x))))
                       3)))))))

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

#_(deftest CLJ-1154-use-out-after-compile
  ;; This test creates a dummy file to compile, sets up a dummy
  ;; compiled output directory, and a dummy output stream, and
  ;; verifies the stream is still usable after compiling.
  (spit "test/dummy.clj" "(ns dummy)")
  (try
    (let [compile-path (System/getProperty "clojure.compile.path")
          tmp (java.io.File. "tmp")
          new-out (java.io.OutputStreamWriter. (java.io.ByteArrayOutputStream.))]
      (binding [clojure.core/*out* new-out]
        (try
          (.mkdir tmp)
          (System/setProperty "clojure.compile.path" "tmp")
          (clojure.lang.Compile/main (into-array ["dummy"]))
          (println "this should still work without throwing an exception" )
          (finally
            (if compile-path
              (System/setProperty "clojure.compile.path" compile-path)
              (System/clearProperty "clojure.compile.path"))
            (doseq [f (.listFiles tmp)]
              (.delete f))
            (.delete tmp)))))
    (finally
      (doseq [f (.listFiles (java.io.File. "test"))
              :when (re-find #"dummy.clj" (str f))]
        (.delete f)))))

(deftest CLJ-1184-do-in-non-list-test
  (testing "do in a vector throws an exception"
    (is (thrown? Compiler$CompilerException
                 (eval '[do 1 2 3]))))
  (testing "do in a set throws an exception"
    (is (thrown? Compiler$CompilerException
                 (eval '#{do}))))

  ;; compile uses a separate code path so we have to call it directly
  ;; to test it
  (letfn [(compile [s]
            (spit "test/clojure/bad_def_test.clj" (str "(ns clojure.bad-def-test)\n" s))
            (try
             (binding [*compile-path* "test"]
               (clojure.core/compile 'clojure.bad-def-test))
             (finally
               (doseq [f (.listFiles (java.io.File. "test/clojure"))
                       :when (re-find #"bad_def_test" (str f))]
                 (.delete f)))))]
    (testing "do in a vector throws an exception in compilation"
      (is (thrown? Compiler$CompilerException (compile "[do 1 2 3]"))))
    (testing "do in a set throws an exception in compilation"
      (is (thrown? Compiler$CompilerException (compile "#{do}"))))))
