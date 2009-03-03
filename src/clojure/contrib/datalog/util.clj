;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  util.clj
;;
;;  A Clojure implementation of Datalog -- Utilities
;;
;;  straszheimjeffrey (gmail)
;;  Created 3 Feburary 2009


(ns clojure.contrib.datalog.util
  (:use [clojure.contrib.seq-utils :only (separate)]))



;;; Bindings and logic vars.  A binding in a hash of logic vars to
;;; bound values.  Logic vars are any symbol prefixed with a \?.

(defn is-var?
  "Is this a logic variable: e.g. a symbol prefixed with a ?"
  [sym]
  (when (symbol? sym)
    (let [name (name sym)]
      (and (= \? (first name))
           (not= \? (fnext name))))))

(defn is-query-var?
  "Is this a query variable: e.g. a symbol prefixed with ??"
  [sym]
  (when (symbol? sym)
    (let [name (name sym)]
      (and (= \? (first name))
           (= \? (fnext name))))))

(defn map-values
  "Like map, but works over the values of a hash map"
  [f hash]
  (let [key-vals (map (fn [[key val]] [key (f val)]) hash)]
    (if (seq key-vals)
      (apply conj (empty hash) key-vals)
      hash)))

(defn keys-to-vals
  "Given a map and a collection of keys, return the collection of vals"
  [m ks]
  (vals (select-keys m ks)))

(defn reverse-map
  "Reverse the keys/values of a map"
  [m]
  (into {} (map (fn [[k v]] [v k]) m)))


;;; Preduce -- A parallel reduce over hashes
  
(defn preduce
  "Similar to merge-with, but the contents of each key are merged in
   parallel using f.

   f - a function of 2 arguments.
   data - a collection of hashes."
  [f data]
  (let [data-1 (map (fn [h] (map-values #(list %) h)) data)
        merged (doall (apply merge-with concat data-1))
        ; Groups w/ multiple elements are identified for parallel processing
        [complex simple] (separate (fn [[key vals]] (> (count vals) 1)) merged)
        fold-group (fn [[key vals]] {key (reduce f vals)})
        fix-single (fn [[key [val]]] [key val])]
    (apply merge (concat (pmap fold-group merged) (map fix-single simple)))))
  

;;; Debuging and Tracing

(def *trace-datalog* nil)

(defmacro trace-datalog
  "If *test-datalog* is set to true, run the enclosed commands"
  [& body]
  `(when *trace-datalog*
     ~@body))

 	
;; End of file
