;; JMX client APIs for Clojure
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

(defmacro with-connection
  "Execute body with JMX connection specified by opts. opts can also
   include an optional :environment key which is passed as the
   environment arg to JMXConnectorFactory/connect."
  [opts & body]
  `(let [opts# ~opts
         env# (get opts# :environment {})
         opts# (dissoc opts# :environment)]
     (with-open [connector# (javax.management.remote.JMXConnectorFactory/connect
                             (JMXServiceURL. (jmx-url opts#)) env#)]
       (binding [*connection* (.getMBeanServerConnection connector#)]
         ~@body))))

(defn mbean-info [n]
  (.getMBeanInfo *connection* (as-object-name n)))

(defn raw-read
  "Read an mbean property. Returns low-level Java object model for
   composites, tabulars, etc. Most callers should use read."
  [n attr]
  (.getAttribute *connection* (as-object-name n) (as-str attr)))

(defvar read
  (comp jmx->clj raw-read)
  "Read an mbean property.")

(defn read-supported
  "Calls read to read an mbean property, *returning* unsupported
   operation exceptions instead of throwing them. Used to keep mbean
   from blowing up. Note: There is no good exception that aggregates
   unsupported operations, hence the overly-general catch block."
  [n attr]
  (try
   (read n attr)
   (catch Exception e
     e)))

(defn write! [n attr value]
  (.setAttribute
   *connection*
   (as-object-name n)
   (Attribute. (as-str attr) value)))

(defn attribute-info
  "Get the MBeanAttributeInfo for an attribute."
  [object-name attr-name]
  (filter #(= (as-str attr-name) (.getName %))
          (.getAttributes (mbean-info object-name))))

(defn readable?
  "Is attribute readable?"
  [n attr]
  (.isReadable () (mbean-info n)))

(defn operations
  "All oeprations available on an MBean."
  [n]
  (.getOperations (mbean-info n)))

(defn operation
  "The MBeanOperationInfo for operation op on mbean n. Used by invoke."
  [n op]
  (first  (filter #(= (-> % .getName keyword) op) (operations n))))

(defn op-param-types 
  "The parameter types (as class name strings) for operation op on n.
   Used for invoke."
  [n op]
  (map #(-> % .getType) (.getSignature (operation n op))))


