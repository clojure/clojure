(ns clojure.test-clojure.repl
  (:use clojure.test
        clojure.repl
        [clojure.test-helper :only [platform-newlines]]
        clojure.test-clojure.repl.example))

(deftest test-source
  (is (= "(defn foo [])" (source-fn 'clojure.test-clojure.repl.example/foo)))
  (is (= (platform-newlines "(defn foo [])\n") (with-out-str (source clojure.test-clojure.repl.example/foo))))
  (is (nil? (source-fn 'non-existent-fn))))

(deftest test-dir
  (is (thrown? Exception (dir-fn 'non-existent-ns)))
  (is (= '[bar foo] (dir-fn 'clojure.test-clojure.repl.example)))
  (is (= (platform-newlines "bar\nfoo\n") (with-out-str (dir clojure.test-clojure.repl.example)))))

(deftest test-apropos
  (testing "with a regular expression"
    (is (= '[defmacro] (apropos #"^defmacro$")))
    (is (some #{'defmacro} (apropos #"def.acr.")))
    (is (= [] (apropos #"nothing-has-this-name"))))

  (testing "with a string"
    (is (some #{'defmacro} (apropos "defmacro")))
    (is (some #{'defmacro} (apropos "efmac")))
    (is (= [] (apropos "nothing-has-this-name"))))

  (testing "with a symbol"
    (is (some #{'defmacro} (apropos 'defmacro)))
    (is (some #{'defmacro} (apropos 'efmac)))
    (is (= [] (apropos 'nothing-has-this-name)))))
