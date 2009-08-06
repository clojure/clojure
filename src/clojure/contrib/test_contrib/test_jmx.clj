;; Tests for JMX support for Clojure (see also clojure/contrib/jmx.clj)

;; by Stuart Halloway

;; Copyright (c) Stuart Halloway, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.test-contrib.test-jmx
  (:import javax.management.openmbean.CompositeDataSupport
           [javax.management MBeanAttributeInfo AttributeList]
           [java.util.logging LogManager Logger]
           clojure.contrib.jmx.Bean)
  (:use clojure.test)
  (:require [clojure.contrib [jmx :as jmx]]))


(defn =set [a b]
  (= (set a) (set b)))

(deftest finding-mbeans
  (testing "as-object-name"
           (are [cname object-name]
                (= cname (.getCanonicalName object-name))
                "java.lang:type=Memory" (jmx/as-object-name "java.lang:type=Memory")))
  (testing "mbean-names"
           (are [cnames object-name]
                (= cnames (map #(.getCanonicalName %) object-name))
                ["java.lang:type=Memory"] (jmx/mbean-names "java.lang:type=Memory"))))

; don't know which attributes are common on all JVM platforms. May
; need to change expectations.
(deftest reflecting-on-capabilities
  (are [attr-list mbean-name]
       (= (set attr-list) (set (jmx/attribute-names mbean-name)))
       [:Verbose :ObjectPendingFinalizationCount :HeapMemoryUsage :NonHeapMemoryUsage] "java.lang:type=Memory")
  (are [a b]
       (= (set a) (set b))
       [:gc] (jmx/operation-names "java.lang:type=Memory")))

(deftest raw-reading-attributes
  (let [mem "java.lang:type=Memory"
        log "java.util.logging:type=Logging"]
    (testing "simple scalar attributes"
             (are [a b] (= a b)
                  false (jmx/raw-read mem :Verbose))
             (are [type attr] (instance? type attr)
                  Integer (jmx/raw-read mem :ObjectPendingFinalizationCount)))))

(deftest reading-attributes
  (testing "simple scalar attributes"
           (are [type attr] (instance? type attr)
                Integer (jmx/read "java.lang:type=Memory" :ObjectPendingFinalizationCount)))
  (testing "composite attributes"
           (are [ks attr] (=set ks (keys attr))
                [:used :max :init :committed] (jmx/read "java.lang:type=Memory" :HeapMemoryUsage)))
  (testing "tabular attributes"
           (is (map? (jmx/read "java.lang:type=Runtime" :SystemProperties)))))

(deftest mbean-from-oname
  (are [oname key-names]
       (= (set key-names) (set (keys (jmx/mbean oname))))
       "java.lang:type=Memory" [:Verbose :ObjectPendingFinalizationCount :HeapMemoryUsage :NonHeapMemoryUsage]))

(deftest writing-attributes
  (let [mem "java.lang:type=Memory"]
    (jmx/write! mem :Verbose true)
    (is (true? (jmx/raw-read mem :Verbose)))
    (jmx/write! mem :Verbose false)))

(deftest test-invoke-operations
  (testing "without arguments"
           (jmx/invoke "java.lang:type=Memory" :gc))
  (testing "with arguments"
           (.addLogger (LogManager/getLogManager) (Logger/getLogger "clojure.contrib.test_contrib.test_jmx"))
           (jmx/invoke "java.util.logging:type=Logging" :setLoggerLevel "clojure.contrib.test_contrib.test_jmx" "WARNING")))

(deftest test-jmx->clj
  (testing "it works recursively on maps"
           (let [some-map {:foo (jmx/raw-read "java.lang:type=Memory" :HeapMemoryUsage)}]
             (is (map? (:foo (jmx/jmx->clj some-map))))))
  (testing "it leaves everything else untouched"
           (is (= "foo" (jmx/jmx->clj "foo")))))
  
  
(deftest test-composite-data->map
  (let [data (jmx/raw-read "java.lang:type=Memory" :HeapMemoryUsage)
        prox (jmx/composite-data->map data)]
    (testing "returns a map with keyword keys"
             (is (= (set [:committed :init :max :used]) (set (keys prox)))))))

(deftest test-tabular-data->map
  (let [raw-props (jmx/raw-read "java.lang:type=Runtime" :SystemProperties)
        props (jmx/tabular-data->map raw-props)]
    (are [k] (contains? props k)
         :java.class.path
         :path.separator)))

(deftest test-creating-attribute-infos
  (let [infos (jmx/map->attribute-infos [[:a 1] [:b 2]])
        info (first infos)]
    (testing "generates the right class"
             (is (= (class (into-array MBeanAttributeInfo [])) (class infos))))
    (testing "generates the right instance data"
             (are [result expr] (= result expr)
                  "a" (.getName info)
                  "a" (.getDescription info)))))

(deftest various-beans-are-readable
  (testing "that all java.lang beans can be read without error"
           (doseq [mb (jmx/mbean-names "*:*")]
             (jmx/mbean mb))))

(deftest test-jmx-url
  (testing "creates default url"
           (is (= "service:jmx:rmi:///jndi/rmi://localhost:3000/jmxrmi" (jmx/jmx-url))))
  (testing "creates custom url"
           (is (= "service:jmx:rmi:///jndi/rmi://example.com:4000/jmxrmi" (jmx/jmx-url {:host "example.com" :port 4000})))))

;; ----------------------------------------------------------------------
;; tests for clojure.contrib.jmx.Bean.

(deftest dynamic-mbean-from-compiled-class
  (let [mbean-name "clojure.contrib.test_contrib.test_jmx:name=Foo"]
    (jmx/register-mbean
     (Bean.
      (ref {:string-attribute "a-string"}))
     mbean-name)
    (are [result expr] (= result expr)
         "a-string" (jmx/read mbean-name :string-attribute)
         {:string-attribute "a-string"} (jmx/mbean mbean-name)
         )))

(deftest test-getAttribute
  (let [state (ref {:a 1 :b 2})
        bean (Bean. state)]
    (testing "accessing values"
             (are [result expr] (= result expr)
                  1 (.getAttribute bean "a")))))

(deftest test-bean-info
  (let [state (ref {:a 1 :b 2})
        bean (Bean. state)
        info (.getMBeanInfo bean)]
    (testing "accessing info"
             (are [result expr] (= result expr)
                  "clojure.contrib.jmx.Bean" (.getClassName info)))))

(deftest test-getAttributes
  (let [bean (Bean. (ref {:r 5 :d 4}))
        atts (.getAttributes bean (into-array ["r" "d"]))]
    (are [x y] (= x y)
         AttributeList (class atts)
         [5 4] (seq atts))))

(deftest test-guess-attribute-typename
  (are [x y] (= x (jmx/guess-attribute-typename y))
       "int" 10
       "boolean" false
       "java.lang.String" "foo"
       "long" (long 10)))