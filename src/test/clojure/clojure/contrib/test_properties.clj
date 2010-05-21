(ns clojure.contrib.test-properties
  (:refer-clojure :exclude (spit))
  (:use clojure.test clojure.contrib.properties
        [clojure.contrib.io :only (spit)])
  (:import (java.util Properties)
           (java.io File)))

(deftest test-get-system-property
  (testing "works the same with keywords, symbols, and strings"
    (is (= (get-system-property "java.home") (get-system-property 'java.home)))
    (is (= (get-system-property "java.home") (get-system-property :java.home))))
  (testing "treats second arg as default"
    (is (= "default" (get-system-property "testing.test-system-property" "default"))))
  (testing "returns nil for missing properties"
    (is (nil? (get-system-property "testing.test-system-property")))))

(deftest test-set-system-properties 
  (testing "set and then unset a property using keywords"
           (let [propname :clojure.contrib.java.test-set-system-properties]
             (is (nil? (get-system-property propname)))
             (set-system-properties {propname :foo})
             (is (= "foo") (get-system-property propname))
             (set-system-properties {propname nil})
             (is (nil? (get-system-property propname))))))

(deftest test-with-system-properties
  (let [propname :clojure.contrib.java.test-with-system-properties]
    (testing "sets a property only for the duration of a block"
      (is (= "foo" 
	     (with-system-properties {propname "foo"}
	       (get-system-property propname))))
      (is (nil? (get-system-property propname)))))
  (testing "leaves other properties alone"
    ; TODO: write this test better, using a properties -> map function
    (let [propname :clojure.contrib.java.test-with-system-properties
          propcount (count (System/getProperties))]
      (with-system-properties {propname "foo"}
        (is (= (inc propcount) (count (System/getProperties)))))
      (is (= propcount (count (System/getProperties)))))))

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
	   
