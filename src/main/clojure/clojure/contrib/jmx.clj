;; Copyright (c) Stuart Halloway, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns #^{:author "Stuart Halloway"
       :doc "JMX support for Clojure

  Requires post-Clojure 1.0 git edge for clojure.test, clojure.backtrace.
  This is prerelease.
  This API will change.
  Send reports to stu@thinkrelevance.com.

  Usage
    (require '[clojure.contrib.jmx :as jmx])

  What beans do I have?

    (jmx/mbean-names \"*:*\")
    -> #<HashSet [java.lang:type=MemoryPool,name=CMS Old Gen, 
                  java.lang:type=Memory, ...]

  What attributes does a bean have?

    (jmx/attribute-names \"java.lang:type=Memory\")
    -> (:Verbose :ObjectPendingFinalizationCount 
        :HeapMemoryUsage :NonHeapMemoryUsage)

  What is the value of an attribute? 

    (jmx/read \"java.lang:type=Memory\" :ObjectPendingFinalizationCount)
    -> 0

  Can't I just have *all* the attributes in a Clojure map?

    (jmx/mbean \"java.lang:type=Memory\")
    -> {:NonHeapMemoryUsage
         {:used 16674024, :max 138412032, :init 24317952, :committed 24317952},
        :HeapMemoryUsage
         {:used 18619064, :max 85393408, :init 0, :committed 83230720},
        :ObjectPendingFinalizationCount 0,
        :Verbose false}

  Can I find and invoke an operation?

    (jmx/operation-names \"java.lang:type=Memory\")
    -> (:gc)  
    (jmx/invoke \"java.lang:type=Memory\" :gc)
    -> nil
  
  What about some other process? Just run *any* of the above code
  inside a with-connection:

    (jmx/with-connection {:host \"localhost\", :port 3000} 
      (jmx/mbean \"java.lang:type=Memory\"))
    -> {:ObjectPendingFinalizationCount 0, 
        :HeapMemoryUsage ... etc.}

  Can I serve my own beans?  Sure, just drop a Clojure ref
  into an instance of clojure.contrib.jmx.Bean, and the bean
  will expose read-only attributes for every key/value pair
  in the ref:

    (jmx/register-mbean
       (Bean.
       (ref {:string-attribute \"a-string\"}))
       \"my.namespace:name=Value\")"}
  clojure.contrib.jmx
  (:refer-clojure :exclude [read])
  (:use clojure.contrib.def
        [clojure.contrib.java-utils :only [as-str]]
        [clojure.stacktrace :only (root-cause)]
        [clojure.walk :only [postwalk]])
  (:import [clojure.lang Associative]
           java.lang.management.ManagementFactory
           [javax.management Attribute DynamicMBean MBeanInfo ObjectName RuntimeMBeanException MBeanAttributeInfo]
           [javax.management.remote JMXConnectorFactory JMXServiceURL]))

(defvar *connection* (ManagementFactory/getPlatformMBeanServer)
  "The connection to be used for JMX ops. Defaults to the local process.")

(load "jmx/data")
(load "jmx/client")
(load "jmx/server")

(defn mbean-names
  "Finds all MBeans matching a name on the current *connection*."
   [n]
  (.queryNames *connection* (as-object-name n) nil))

(defn attribute-names 
  "All attribute names available on an MBean."
  [n]
  (doall (map #(-> % .getName keyword)
              (.getAttributes (mbean-info n)))))

(defn operation-names
  "All operation names available on an MBean."
  [n]
  (doall (map #(-> % .getName keyword) (operations n))))

(defn invoke [n op & args]
  (if ( seq args)
    (.invoke *connection* (as-object-name n) (as-str op)
             (into-array args)
             (into-array String  (op-param-types n op)))
    (.invoke *connection* (as-object-name n) (as-str op)
             nil nil)))

(defn mbean
  "Like clojure.core/bean, but for JMX beans. Returns a read-only map of
   a JMX bean's attributes. If an attribute it not supported, value is
   set to the exception thrown."
  [n]
  (into {} (map (fn [attr-name] [(keyword attr-name) (read-supported n attr-name)])
                (attribute-names n))))

