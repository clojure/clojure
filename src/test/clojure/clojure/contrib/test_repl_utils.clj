(ns clojure.contrib.test-repl-utils
  (:use clojure.test
	clojure.contrib.repl-utils
        [clojure.contrib.seq :only (includes?)]))

(deftest test-apropos
  (testing "with a regular expression"
    (is (= '[defmacro] (apropos #"^defmacro$")))
    (is (includes? (apropos #"def.acr.") 'defmacro))
    (is (= [] (apropos #"nothing-has-this-name"))))
  

  (testing "with a string"
    (is (includes? (apropos "defmacro") 'defmacro))
    (is (includes? (apropos "efmac") 'defmacro))
    (is (= [] (apropos "nothing-has-this-name"))))

  (testing "with a symbol"
    (is (includes? (apropos 'defmacro) 'defmacro))
    (is (includes? (apropos 'efmac) 'defmacro))
    (is (= [] (apropos 'nothing-has-this-name)))))
