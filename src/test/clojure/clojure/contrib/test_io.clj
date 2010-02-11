(ns clojure.contrib.test-io
  (:use clojure.test clojure.contrib.io))

(deftest file-str-backslash
  (is (= (java.io.File.
          (str "C:" java.io.File/separator
               "Documents" java.io.File/separator
               "file.txt"))
         (file-str "C:\\Documents\\file.txt"))))
