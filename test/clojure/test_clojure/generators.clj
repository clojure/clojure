(ns clojure.test-clojure.generators
  (:require [clojure.data.generators :as gen])
  (:refer-clojure :exclude [namespace]))

(defn var-value-source
  "Generates a scalar suitable for an initial var value."
  []
  (let [v (gen/scalar)]
    (if (symbol? v)
      `(quote ~v)
      v)))

(defn var-source
  [n]
  `(def ~(symbol (str "var" n))
     ~(var-value-source)))

(defn record-source
  [n]
  (let [rname (str "ExampleRecord" "-" n)
        fldct (gen/geometric 0.1)]
    `(defrecord ~(symbol rname) ~(vec (map #(symbol (str "f" %)) (range fldct))))))

(defn generate-namespaces
  "Returns a map with :nses, :vars, :records"
  [{:keys [nses vars-per-ns records-per-ns]}]
  (let [nses (mapv #(create-ns (symbol (str "clojure.generated.ns" %)))
                   (range nses))
        _ (doseq [ns nses] (binding [*ns* ns] (refer 'clojure.core)))
        make-in-ns (fn [ns src] (binding [*ns* ns] (eval src)))
        vars (->> (mapcat
                   (fn [ns]
                     (map
                      #(make-in-ns ns (var-source %))
                      (range vars-per-ns)))
                   nses)
                  (into []))
        records (->> (mapcat
                      (fn [ns]
                        (map
                         #(make-in-ns ns (record-source %))
                         (range records-per-ns)))
                      nses)
                     (into []))]
    {:nses nses
     :vars vars
     :records records}))

(def shared-generation
  (delay (generate-namespaces {:nses 5 :vars-per-ns 5 :records-per-ns 5})))

(defn namespace
  []
  (gen/rand-nth (:nses @shared-generation)))

(defn var
  []
  (gen/rand-nth (:vars @shared-generation)))

(defn record
  []
  (gen/rand-nth (:records @shared-generation)))

(def keyword-pool
  (delay
   (binding [gen/*rnd* (java.util.Random. 42)]
     (into [] (repeatedly 1000 gen/keyword)))))

(defn keyword-from-pool
  []
  (gen/rand-nth @keyword-pool))

(def symbol-pool
  (delay
   (binding [gen/*rnd* (java.util.Random. 42)]
     (into [] (repeatedly 1000 gen/symbol)))))

(defn symbol-from-pool
  []
  (gen/rand-nth @keyword-pool))

(def ednable-scalars
  [(constantly nil)
   gen/byte
   gen/long
   gen/boolean
   gen/printable-ascii-char
   gen/string
   symbol-from-pool
   keyword-from-pool
   gen/uuid
   gen/date
   gen/ratio
   gen/bigint
   gen/bigdec])

(defn- call-through
  "Recursively call x until it doesn't return a function."
  [x]
  (if (fn? x)
    (recur (x))
    x))

(defn ednable-scalar
  []
  (call-through (rand-nth ednable-scalars)))

(def ednable-collections
  [[gen/vec [ednable-scalars]]
   [gen/set [ednable-scalars]]
   [gen/hash-map [ednable-scalars ednable-scalars]]])

(defn ednable-collection
  []
  (let [[coll args] (rand-nth ednable-collections)]
    (apply coll (map rand-nth args))))

(defn ednable
  []
  (gen/one-of ednable-scalar ednable-collection))

(defn non-ednable
  "Generate something that can be printed with *print-dup*, but
   cannot be read back via edn/read."
  []
  (gen/one-of namespace var))

(defn dup-readable
  "Generate something that requires print-dup to be printed in
   a roundtrippable way."
  []
  (gen/one-of namespace var))
