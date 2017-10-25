;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Authors: Frantisek Sodomka, Stuart Halloway

(ns clojure.test-clojure.ns-libs
  (:use clojure.test))

; http://clojure.org/namespaces

; in-ns ns create-ns
; alias import intern refer
; all-ns find-ns
; ns-name ns-aliases ns-imports ns-interns ns-map ns-publics ns-refers
; resolve ns-resolve namespace
; ns-unalias ns-unmap remove-ns


; http://clojure.org/libs

; require use
; loaded-libs

(deftest test-alias
	(is (thrown-with-msg? Exception #"No namespace: epicfail found" (alias 'bogus 'epicfail))))
	
(deftest test-require
         (is (thrown? Exception (require :foo)))
         (is (thrown? Exception (require))))

(deftest test-use
         (is (thrown? Exception (use :foo)))
         (is (thrown? Exception (use))))

(deftest reimporting-deftypes
  (let [inst1 (binding [*ns* *ns*]
                (eval '(do (ns exporter)
                           (defrecord ReimportMe [a])
                           (ns importer)
                           (import exporter.ReimportMe)
                           (ReimportMe. 1))))
        inst2 (binding [*ns* *ns*]
                (eval '(do (ns exporter)
                           (defrecord ReimportMe [a b])
                           (ns importer)
                           (import exporter.ReimportMe)
                           (ReimportMe. 1 2))))]
    (testing "you can reimport a changed class and see the changes"
      (is (= [:a] (keys inst1)))
      (is (= [:a :b] (keys inst2))))
    ;fragile tests, please fix
    #_(testing "you cannot import same local name from a different namespace"
      (is (thrown? clojure.lang.Compiler$CompilerException
                  #"ReimportMe already refers to: class exporter.ReimportMe in namespace: importer"
                  (binding [*ns* *ns*]
                    (eval '(do (ns exporter-2)
                               (defrecord ReimportMe [a b])
                               (ns importer)
                               (import exporter-2.ReimportMe)
                               (ReimportMe. 1 2)))))))))

(deftest naming-types
  (testing "you cannot use a name already referred from another namespace"
    (is (thrown-with-msg? IllegalStateException
                          #"String already refers to: class java.lang.String"
                          (definterface String)))
    (is (thrown-with-msg? IllegalStateException
                          #"StringBuffer already refers to: class java.lang.StringBuffer"
                          (deftype StringBuffer [])))
    (is (thrown-with-msg? IllegalStateException
                          #"Integer already refers to: class java.lang.Integer"
                          (defrecord Integer [])))))

(deftest resolution
  (let [s (gensym)]
    (are [result expr] (= result expr)
         #'clojure.core/first (ns-resolve 'clojure.core 'first)
         nil (ns-resolve 'clojure.core s)
         nil (ns-resolve 'clojure.core {'first :local-first} 'first)
         nil (ns-resolve 'clojure.core {'first :local-first} s))))
  
(deftest refer-error-messages
  (let [temp-ns (gensym)]
    (binding [*ns* *ns*]
      (in-ns temp-ns)
      (eval '(def ^{:private true} hidden-var)))
    (testing "referring to something that does not exist"
      (is (thrown-with-msg? IllegalAccessError #"nonexistent-var does not exist"
            (refer temp-ns :only '(nonexistent-var)))))
    (testing "referring to something non-public"
      (is (thrown-with-msg? IllegalAccessError #"hidden-var is not public"
            (refer temp-ns :only '(hidden-var)))))))

(deftest test-defrecord-deftype-err-msg
  (is (thrown-with-msg? clojure.lang.Compiler$CompilerException
                        #"defrecord and deftype fields must be symbols, user\.MyRecord had: :shutdown-fn, compiling:"
                        (eval '(defrecord MyRecord [:shutdown-fn]))))
  (is (thrown-with-msg? clojure.lang.Compiler$CompilerException
                        #"defrecord and deftype fields must be symbols, user\.MyType had: :key1, compiling:"
                        (eval '(deftype MyType [:key1])))))
