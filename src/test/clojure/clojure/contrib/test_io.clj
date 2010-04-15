(ns clojure.contrib.test-io
  (:use clojure.test clojure.contrib.io)
  (:import (java.io File FileInputStream BufferedInputStream)
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

(deftest test-relative-path-string
  (testing "strings"
    (is (= "foo" (relative-path-string "foo"))))
  (testing "absolute path strings are forbidden"
    (is (thrown? IllegalArgumentException (relative-path-string (str File/separator "baz")))))
  (testing "relative File paths"
    (is (= "bar" (relative-path-string (File. "bar")))))
  (testing "absolute File paths are forbidden"
    (is (thrown? IllegalArgumentException (relative-path-string (File. (str File/separator "quux")))))))

(defn stream-should-have [stream expected-bytes msg]
  (let [actual-bytes (byte-array (alength expected-bytes))]
    (.read stream actual-bytes)
    (is (= -1 (.read stream)) (str msg " : should be end of stream"))
    (is (= (seq expected-bytes) (seq actual-bytes)) (str msg " : byte arrays should match"))))

(deftest test-input-stream
  (let [file (File/createTempFile "test-input-stream" "txt")
        bytes (.getBytes "foobar")]
    (spit file "foobar")
    (doseq [[expr msg]
            [[file File]
             [(FileInputStream. file) FileInputStream]
             [(BufferedInputStream. (FileInputStream. file)) BufferedInputStream]
             [(.. file toURI) URI]
             [(.. file toURI toURL) URL]
             [(.. file toURI toURL toString) "URL as String"]
             [(.. file toString) "File as String"]]]
      (with-open [s (input-stream expr)]
        (stream-should-have s bytes msg)))))
