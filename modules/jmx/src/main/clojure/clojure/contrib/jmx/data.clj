;; Conversions between JMX data structures and idiomatic Clojure
;; docs in clojure/contrib/jmx.clj!!

;; by Stuart Halloway

;; Copyright (c) Stuart Halloway, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(in-ns 'clojure.contrib.jmx)

(declare jmx->clj)

(defn jmx-url
  "Build a JMX URL from options."
  ([] (jmx-url {}))
  ([overrides]
     (let [opts (merge {:host "localhost", :port "3000", :jndi-path "jmxrmi"} overrides)]
       (format "service:jmx:rmi:///jndi/rmi://%s:%s/%s" (opts :host) (opts :port) (opts :jndi-path)))))

(defmulti as-object-name
  "Interpret an object as a JMX ObjectName."
  { :arglists '([string-or-name]) }
  class)
(defmethod as-object-name String [n] (ObjectName. n))
(defmethod as-object-name ObjectName [n] n)

(defn composite-data->map [cd]
  (into {}
        (map (fn [attr] [(keyword attr) (jmx->clj (.get cd attr))])
             (.. cd getCompositeType keySet))))

(defn maybe-keywordize
  "Convert a string key to a keyword, leaving other types alone. Used to
   simplify keys in the tabular data API."
  [s]
  (if (string? s) (keyword s) s))

(defn maybe-atomize
  "Convert a list of length 1 into its contents, leaving other things alone.
  Used to simplify keys in the tabular data API."
  [k]
  (if (and (instance? java.util.List k)
           (= 1 (count k)))
    (first k)
    k))

(defvar simplify-tabular-data-key
  (comp maybe-keywordize maybe-atomize))

(defn tabular-data->map [td]
  (into {}
        ; the need for into-array here was a surprise, and may not
        ; work for all examples. Are keys always arrays?
        (map (fn [k]
               [(simplify-tabular-data-key k) (jmx->clj (.get td (into-array k)))])
             (.keySet td))))

(defmulti jmx->clj
  "Coerce JMX data structures into Clojure data.
  Handles CompositeData, TabularData, maps, and atoms."
  { :argslists '([jmx-data-structure]) }
  (fn [x]
    (cond
     (instance? javax.management.openmbean.CompositeData x) :composite
     (instance? javax.management.openmbean.TabularData x) :tabular
     (instance? clojure.lang.Associative x) :map
     :default :default)))
(defmethod jmx->clj :composite [c] (composite-data->map c))
(defmethod jmx->clj :tabular [t] (tabular-data->map t))
(defmethod jmx->clj :map [m]  (into {} (zipmap (keys m) (map jmx->clj (vals m)))))
(defmethod jmx->clj :default [obj] obj)

(def guess-attribute-map
     {"java.lang.Integer" "int"
      "java.lang.Boolean" "boolean"
      "java.lang.Long" "long"
      })

(defn guess-attribute-typename
  "Guess the attribute typename for MBeanAttributeInfo based on the attribute value."
  [value]
  (let [classname (.getName (class value))]
    (get guess-attribute-map classname classname)))

(defn build-attribute-info
  "Construct an MBeanAttributeInfo. Normally called with a key/value pair from a Clojure map."
  ([attr-name attr-value]
     (build-attribute-info
      (as-str attr-name)
      (guess-attribute-typename attr-value)
      (as-str attr-name) true false false))
  ([name type desc readable? writable? is?] (MBeanAttributeInfo. name type desc readable? writable? is? )))

(defn map->attribute-infos
  "Construct an MBeanAttributeInfo[] from a Clojure associative."
  [attr-map]
  (into-array (map (fn [[attr-name value]] (build-attribute-info attr-name value))
                   attr-map)))
