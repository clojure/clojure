(ns clojure.test-clojure.repl
  (:use clojure.test
        clojure.repl
        [clojure.test-helper :only [platform-newlines]]
        clojure.test-clojure.repl.example)
  (:require [clojure.string :as str]))

(deftest test-doc
  (testing "with namespaces"
    (is (= "clojure.pprint"
           (second (str/split-lines (with-out-str (doc clojure.pprint))))))))

(deftest test-source
  (is (= "(defn foo [])" (source-fn 'clojure.test-clojure.repl.example/foo)))
  (is (= (platform-newlines "(defn foo [])\n") (with-out-str (source clojure.test-clojure.repl.example/foo))))
  (is (nil? (source-fn 'non-existent-fn))))

(deftest test-source-read-eval-unknown
  (is (thrown? IllegalStateException (binding [*read-eval* :unknown] (source reduce)))))

(deftest test-source-read-eval-false
  (is (binding [*read-eval* false] (with-out-str (source reduce)))))

(deftest test-dir
  (is (thrown? Exception (dir-fn 'non-existent-ns)))
  (is (= '[bar foo] (dir-fn 'clojure.test-clojure.repl.example)))
  (is (= (platform-newlines "bar\nfoo\n") (with-out-str (dir clojure.test-clojure.repl.example)))))

(deftest test-apropos
  (testing "with a regular expression"
    (is (= '[clojure.core/defmacro] (apropos #"^defmacro$")))
    (is (some #{'clojure.core/defmacro} (apropos #"def.acr.")))
    (is (= [] (apropos #"nothing-has-this-name"))))

  (testing "with a string"
    (is (some #{'clojure.core/defmacro} (apropos "defmacro")))
    (is (some #{'clojure.core/defmacro} (apropos "efmac")))
    (is (= [] (apropos "nothing-has-this-name"))))

  (testing "with a symbol"
    (is (some #{'clojure.core/defmacro} (apropos 'defmacro)))
    (is (some #{'clojure.core/defmacro} (apropos 'efmac)))
    (is (= [] (apropos 'nothing-has-this-name)))))


(defmacro call-ns 
  "Call ns with a unique namespace name. Return the result of calling ns"
  []  `(ns a#))
(defmacro call-ns-sym 
  "Call ns wih a unique namespace name. Return the namespace symbol."
  [] `(do (ns a#) 'a#))

(deftest test-dynamic-ns
  (testing "a call to ns returns nil"
   (is (= nil (call-ns))))
  (testing "requiring a dynamically created ns should not throw an exception"
    (is (= nil (let [a (call-ns-sym)] (require a))))))
