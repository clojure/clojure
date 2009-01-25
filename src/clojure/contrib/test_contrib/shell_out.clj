(ns clojure.contrib.test-contrib.shell-out
  (:use clojure.contrib.test-is
	clojure.contrib.shell-out)
  (:import (java.io File)))

; workaroung to access private parse-args. Better way?
(def parse-args ((ns-interns 'clojure.contrib.shell-out) 'parse-args))
(def as-file ((ns-interns 'clojure.contrib.shell-out) 'as-file))
(def as-env-string ((ns-interns 'clojure.contrib.shell-out) 'as-env-string))

(deftest test-parse-args
  (are (= _1 _2)
    {:cmd [nil] :out "UTF-8" :dir nil :env nil} (parse-args [])
    {:cmd ["ls"] :out "UTF-8" :dir nil :env nil} (parse-args ["ls"])
    {:cmd ["ls" "-l"] :out "UTF-8" :dir nil :env nil} (parse-args ["ls" "-l"])
    {:cmd ["ls"] :out "ISO-8859-1" :dir nil :env nil} (parse-args ["ls" :out "ISO-8859-1"])
))
  
(deftest test-with-sh-dir
  (are (= _1 _2)
    nil *sh-dir*
    "foo" (with-sh-dir "foo" *sh-dir*)))

(deftest test-with-sh-env
  (are (= _1 _2)
    nil *sh-env*
    {:KEY "VAL"} (with-sh-env {:KEY "VAL"} *sh-env*)))

(deftest test-as-env-string
  (are (= _1 _2)
    nil (as-env-string nil)
    ["FOO=BAR"] (seq (as-env-string {"FOO" "BAR"}))
    ["FOO_SYMBOL=BAR"] (seq (as-env-string {'FOO_SYMBOL "BAR"}))
    ["FOO_KEYWORD=BAR"] (seq (as-env-string {:FOO_KEYWORD "BAR"}))))


(deftest test-as-file
  (are (= _1 _2)
    (File. "foo") (as-file "foo")
    nil (as-file nil)
    (File. "bar") (as-file (File. "bar"))))