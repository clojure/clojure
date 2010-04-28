(ns clojure.contrib.test-repl-utils
  (:use clojure.test
	clojure.contrib.repl-utils))

(deftest test-apropos
  (testing "with a regular expression"
    (is (= '[defmacro] (apropos #"^defmacro$")))
    (is (seq-contains? (apropos #"def.acr.") 'defmacro))
    (is (= [] (apropos #"nothing-has-this-name"))))
  

  (testing "with a string"
    (is (seq-contains? (apropos "defmacro") 'defmacro))
    (is (seq-contains? (apropos "efmac") 'defmacro))
    (is (= [] (apropos "nothing-has-this-name"))))

  (testing "with a symbol"
    (is (seq-contains? (apropos 'defmacro) 'defmacro))
    (is (seq-contains? (apropos 'efmac) 'defmacro))
    (is (= [] (apropos 'nothing-has-this-name)))))
