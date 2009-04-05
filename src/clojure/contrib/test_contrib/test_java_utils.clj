(ns clojure.contrib.test-contrib.test-java-utils
  (:use clojure.contrib.test-is
	clojure.contrib.java-utils)
  (:import [java.io File]))

(deftest test-relative-path-string
  (testing "strings"
    (is (= "foo" (relative-path-string "foo"))))
  (testing "absolute path strings are forbidden"
    (is (thrown? IllegalArgumentException (relative-path-string "/baz"))))
  (testing "relative File paths"
    (is (= "bar" (relative-path-string (File. "bar")))))
  (testing "absolute File paths are forbidden"
    (is (thrown? IllegalArgumentException (relative-path-string (File. "/quux")))))
)

(deftest test-as-file
  (testing "strings"
    (is (= (File. "foo") (as-file "foo"))))
  (testing "Files"
    (is (= (File. "bar") (as-file (File. "bar")))))
)

(deftest test-file
  (testing "single argument"
    (is (= (File. "foo") (file "foo"))))
  (testing "two arguments"
    (is (= (File. "foo/bar") (file "foo" "bar"))))
  (testing "N arguments"
    (is (= (File. "foo/bar/baz/quux") (file "foo" "bar" "baz" "quux"))))
  (testing "no sneaking in absolute paths!"
    (is (thrown? IllegalArgumentException (file "foo" "bar" "/boom" "baz" "quux"))))
)
	   
	   


