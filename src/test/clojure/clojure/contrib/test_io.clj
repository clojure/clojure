(ns clojure.contrib.test-io
  (:use clojure.test clojure.contrib.io)
  (:import (java.io File)
           (java.net URL URI)))

(deftest file-str-backslash
  (is (= (java.io.File.
          (str "C:" java.io.File/separator
               "Documents" java.io.File/separator
               "file.txt"))
         (file-str "C:\\Documents\\file.txt"))))

(deftest test-as-file
  (testing "strings"
    (is (= (File. "foo") (as-file "foo"))))
  (testing "Files"
    (is (= (File. "bar") (as-file (File. "bar"))))))

(deftest test-as-url
  (are [result expr] (= result expr)
       (URL. "http://foo") (as-url (URL. "http://foo"))
       (URL. "http://foo") (as-url "http://foo")
       (URL. "http://foo") (as-url (URI. "http://foo"))
       (URL. "file:/foo") (as-url (File. "/foo"))))

(deftest test-delete-file
  (let [file (File/createTempFile "test" "deletion")
        not-file (File. (str (java.util.UUID/randomUUID)))]
    (delete-file (.getAbsolutePath file))
    (is (not (.exists file)))
    (is (thrown? ArithmeticException (/ 1 0)))
    (is (thrown? java.io.IOException (delete-file not-file)))
    (is (delete-file not-file :silently))))
