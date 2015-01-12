(ns clojure.test-clojure.compilation.line-number-examples
  "Example code taken from Paul Stadig and updated by Daniel Solano Gómez.

  Original source at:
    https://github.com/pjstadig/clojure-line-numbers/blob/master/src/clojure_line_numbers/core.clj"
  (:import (clojure.lang PersistentHashMap)))

(defrecord Thing [field ^long primitive])

(defn instance-field
  "I throw an exception in an instance field form."
  []
  (.field
   ^Thing (identity nil)))

(defn instance-field-reflected
  "I throw an exception in an instance field form."
  []
  (.field
   (identity nil)))

(defn instance-field-unboxed
  "I throw an exception in an instance field form."
  ^long []
  (.primitive
   ^Thing (identity nil)))

(defn instance-field-assign
  "I throw an exception in an instance field assignment form."
  []
  (set!
   (.field
    ^Thing (identity nil))
   (identity nil)))

(defn instance-field-assign-reflected
  "I throw an exception in an instance field assignment form."
  []
  (set!
   (.field
    (identity nil))
   (identity nil)))

(defn static-field-assign
  "I throw an exception in a static field assignment form."
  []
  (set!
   PersistentHashMap/EMPTY
   (identity nil)))

(defn instance-method
  "I throw an exception in an instance method form."
  []
  (.without
   ^PersistentHashMap (identity nil)
   :key))

(defn instance-method-reflected
  "I throw an exception in an instance method form."
  []
  (.without
   (identity nil)
   :key))

(defn instance-method-unboxed
  "I throw an exception in an instance method form."
  ^long []
  (.count
   ^PersistentHashMap (identity nil)))

(defn static-method
  "I throw an exception in a static method form."
  []
  (PersistentHashMap/create
   ^java.util.Map (identity nil)))

(defn static-method-reflected
  "I throw an exception in a static method form."
  []
  (String/copyValueOf
   (identity nil)))

(defn static-method-unboxed
  "I throw an exception in a static method form."
  ^long []
  (Long/parseLong
   ^String (identity nil)))

(defn invoke
  "I throw an exception in an invoke form."
  []
  ((identity nil)
   (identity nil)))

(defn threading
  "I throw an exception in a threading form."
  []
  (-> :foo
      (identity)
      (identity)
      ((identity nil))
      (identity)
      (identity)))

(defn keyword-invoke
  "I throw an exception in a keyword invoke."
  []
  (letfn [(get-map []
            (let [t (transient {})]
              (persistent! t)
              t))]
    (:foo
     (get-map))))

(defn invoke-cast
  "I throw an exception casting to IFn in an invoke form."
  []
  ;; This code formatting is intentional.
  (
   (identity 1)
   (identity nil)))
