(ns clojure.contrib.test-contrib.test-java-utils
  (:use clojure.contrib.test-is
	[clojure.contrib.duck-streams :only (spit)]
	clojure.contrib.java-utils)
  (:import [java.io File]
	   [java.util Properties]))

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

(deftest test-as-str
  (testing "keyword to string"
    (is (= "foo") (as-str :foo)))
  (testing "symbol to string"
    (is (= "foo") (as-str 'foo)))
  (testing "string to string"
    (is (= "foo") (as-str "foo")))
  (testing "stringifying non-namish things"
    (is (= "42") (as-str 42)))
)

(deftest test-get-system-property
  (testing "works the same with keywords, symbols, and strings"
    (is (= (get-system-property "java.home") (get-system-property 'java.home)))
    (is (= (get-system-property "java.home") (get-system-property :java.home))))
)
    
(deftest test-set-system-properties 
  (testing "set and then unset a property"
    (let [propname :clojure.contrib.java-utils.test-set-system-properties]
      (is (nil? (get-system-property propname)))
      (set-system-properties {propname "foo"})
      (is (= "foo") (get-system-property propname))
      (set-system-properties {propname nil})
      (is (nil? (get-system-property propname)))))
)

(deftest test-with-system-properties
  (let [propname :clojure.contrib.java-utils.test-with-system-properties]
    (testing "sets a property only for the duration of a block"
      (is (= "foo" 
	     (with-system-properties {propname "foo"}
	       (get-system-property propname))))
      (is (nil? (get-system-property propname)))))
  (testing "leaves other properties alone"
    ; TODO: write this test better, using a properties -> map function
    (let [propname :clojure.contrib.java-utils.test-with-system-properties
          propcount (count (System/getProperties))]
      (with-system-properties {propname "foo"}
        (is (= (inc propcount) (count (System/getProperties)))))
      (is (= propcount (count (System/getProperties))))))
)
	   
(deftest test-as-properties
  (let [expected (doto (Properties.)
		   (.setProperty "a" "b")
		   (.setProperty "c" "d"))]
    (testing "with a map"
      (is (= expected
	     (as-properties {:a "b" :c "d"}))))
    (testing "with a sequence of pairs"
      (is (= expected
	     (as-properties [[:a :b] [:c :d]]))))))

(deftest test-read-properties
  (let [f (File/createTempFile "test" "properties")]
    (spit f "a=b\nc=d")
    (is (= {"a" "b" "c" "d"}
	   (read-properties f)))))
	   
(deftest test-write-properties
  (let [f (File/createTempFile "test" "properties")]
    (write-properties [['a 'b] ['c 'd]] f)
    (is (= {"a" "b" "c" "d"}
	   (read-properties f)))))
	   


