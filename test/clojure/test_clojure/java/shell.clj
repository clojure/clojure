(ns clojure.test-clojure.java.shell
  (:use clojure.test
	[clojure.java.shell :as sh])
  (:import (java.io File)))

(deftest test-parse-args
  (are [x y] (= x y)
    {:cmd [nil] :out "UTF-8" :dir nil :env nil} (#'sh/parse-args [])
    {:cmd ["ls"] :out "UTF-8" :dir nil :env nil} (#'sh/parse-args ["ls"])
    {:cmd ["ls" "-l"] :out "UTF-8" :dir nil :env nil} (#'sh/parse-args ["ls" "-l"])
    {:cmd ["ls"] :out "ISO-8859-1" :dir nil :env nil} (#'sh/parse-args ["ls" :out "ISO-8859-1"])))
  
(deftest test-with-sh-dir
  (are [x y] (= x y)
    nil *sh-dir*
    "foo" (with-sh-dir "foo" *sh-dir*)))

(deftest test-with-sh-env
  (are [x y] (= x y)
    nil *sh-env*
    {:KEY "VAL"} (with-sh-env {:KEY "VAL"} *sh-env*)))

(deftest test-as-env-string
  (are [x y] (= x y)
    nil (#'sh/as-env-string nil)
    ["FOO=BAR"] (seq (#'sh/as-env-string {"FOO" "BAR"}))
    ["FOO_SYMBOL=BAR"] (seq (#'sh/as-env-string {'FOO_SYMBOL "BAR"}))
    ["FOO_KEYWORD=BAR"] (seq (#'sh/as-env-string {:FOO_KEYWORD "BAR"}))))

