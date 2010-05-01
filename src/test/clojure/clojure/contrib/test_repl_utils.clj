(ns clojure.contrib.test-repl-utils
  (:use clojure.test
	clojure.contrib.repl-utils))

(deftest test-apropos
  (testing "with a regular expression"
    (is (= '[defmacro] (apropos #"^defmacro$")))
    (is (some '#{defmacro} (apropos #"def.acr.")))
    (is (= [] (apropos #"nothing-has-this-name"))))
  

  (testing "with a string"
    (is (some '#{defmacro} (apropos "defmacro")))
    (is (some '#{defmacro} (apropos "efmac")))
    (is (= [] (apropos "nothing-has-this-name"))))

  (testing "with a symbol"
    (is (some '#{defmacro} (apropos 'defmacro)))
    (is (some '#{defmacro} (apropos 'efmac)))
    (is (= [] (apropos 'nothing-has-this-name)))))
