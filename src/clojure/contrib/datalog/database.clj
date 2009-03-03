;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  database.clj
;;
;;  A Clojure implementation of Datalog -- Support for in-memory database
;;
;;  straszheimjeffrey (gmail)
;;  Created 21 Feburary 2009


(ns clojure.contrib.datalog.database
  (:use clojure.contrib.datalog.util)
  (:use clojure.contrib.def)
  (:use [clojure.set :only (union intersection difference)])
  (:use [clojure.contrib.except :only (throwf)]))


(defstruct relation
  :schema           ; A set of key names
  :data             ; A set of tuples
  :indexes)         ; A map key names to indexes (in turn a map of value to tuples)


;;; DDL

(defmethod print-method ::datalog-database
  [db #^Writer writer]
  (binding [*out* writer]
    (do
      (println "(datalog-database")
      (println "{")
      (doseq [key (keys db)]
        (println)
        (println key)
        (print-method (db key) writer))
      (println "})"))))

(defn datalog-database
  [rels]
  (with-meta rels {:type ::datalog-database}))

(def empty-database (datalog-database {}))

(defmethod print-method ::datalog-relation
  [rel #^Writer writer]
  (binding [*out* writer]
    (do
      (println "(datalog-relation")
      (println " ;; Schema")
      (println " " (:schema rel))
      (println)
      (println " ;; Data")
      (println " #{")
      (doseq [tuple (:data rel)]
        (println "  " tuple))
      (println " }")
      (println)
      (println " ;; Indexes")
      (println "  {")
      (doseq [key (-> rel :indexes keys)]
        (println "  " key)
        (println "    {")
        (doseq [val (keys ((:indexes rel) key))]
          (println "      " val)
          (println "        " (get-in rel [:indexes key val])))
        (println "    }"))
      (println "  })"))))

(defn datalog-relation
  "Creates a relation"
  [schema data indexes]
  (with-meta (struct relation schema data indexes) {:type ::datalog-relation}))

(defn add-relation
  "Adds a relation to the database"
  [db name keys]
  (assoc db name (datalog-relation (set keys) #{} {})))

(defn add-index
  "Adds an index to an empty relation named name"
  [db name key]
  (assert (empty? (:data (db name))))
  (let [rel (db name)
        inx (assoc (:indexes rel) key {})]
    (assoc db name (datalog-relation (:schema rel)
                                     (:data rel)
                                     inx))))

(defn ensure-relation
  "If the database lacks the named relation, add it"
  [db name keys indexes]
  (if-let [rel (db name)]
    (do
      (assert (= (:schema rel) (set keys)))
      db)
    (let [db1 (add-relation db name keys)]
      (reduce (fn [db key] (add-index db name key))
              db1
              indexes))))
    

(defmacro make-database
  "Makes a database, like this
   (make-database
     (relation :fred [:mary :sue])
     (index :fred :mary)
     (relation :sally [:jen :becky])
     (index :sally :jen)
     (index :sally :becky))"
  [& commands]
  (let [wrapper (fn [cur new]
                  (let [cmd (first new)
                        body (next new)]
                    (assert (= 2 (count body)))
                    (cond
                     (= cmd 'relation)
                       `(add-relation ~cur ~(first body) ~(fnext body))
                     (= cmd 'index)
                       `(add-index ~cur ~(first body) ~(fnext body))
                     :otherwise (throwf "%s not recognized" new))))]
    (reduce wrapper `empty-database commands)))

(defn get-relation
  "Get a relation object by name"
  [db rel-name]
  (db rel-name))

(defn replace-relation
  "Add or replace a fully constructed relation object to the database."
  [db rel-name rel]
  (assoc db rel-name rel))


;;; DML


(defn database-counts
  "Returns a map with the count of elements in each relation."
  [db]
  (map-values #(-> % :data count) db))

(defn- modify-indexes
  "Perform f on the indexed tuple-set.  f should take a set and tuple,
   and return the new set."
  [idxs tuple f]
  (into {} (for [ik (keys idxs)]
             (let [im (idxs ik)
                   iv (tuple ik)
                   os (get im iv #{})]
               [ik (assoc im iv (f os tuple))]))))

(defn- add-to-indexes
  "Adds the tuple to the appropriate keys in the index map"
  [idxs tuple]
  (modify-indexes idxs tuple conj))

(defn- remove-from-indexes
  "Removes the tuple from the appropriate keys in the index map"
  [idxs tuple]
  (modify-indexes idxs tuple disj))

(defn add-tuple
  "Two forms:

   [db relation-name tuple] adds tuple to the named relation.  Returns
   the new database.

   [rel tuple] adds to the relation object.  Returns the new relation."
  ([db rel-name tuple]
     (assert (= (-> tuple keys set) (-> rel-name db :schema)))
     (assoc db rel-name (add-tuple (db rel-name) tuple)))
  ([rel tuple]
     (let [data (:data rel)
           new-data (conj data tuple)]
       (if (identical? data new-data) ; optimization hack!
         rel
         (let [idxs (add-to-indexes (:indexes rel) tuple)]
           (assoc rel :data new-data :indexes idxs))))))

(defn remove-tuple
  "Two forms:

   [db relation-name tuple] removes the tuple from the named relation,
   returns a new database.

   [rel tuple] removes the tuple from the relation.  Returns the new
   relation."
  ([db rel-name tuple] (assoc db rel-name (remove-tuple (db rel-name) tuple)))
  ([rel tuple]
     (let [data (:data rel)
           new-data (disj data tuple)]
       (if (identical? data new-data)
         rel
         (let [idxs (remove-from-indexes (:indexes rel) tuple)]
           (assoc rel :data new-data :indexes idxs))))))
                      
(defn add-tuples
  "Adds a collection of tuples to the db, as
   (add-tuples db
      [:rel-name :key-1 1 :key-2 2]
      [:rel-name :key-1 2 :key-2 3])"
  [db & tupls]
  (reduce #(add-tuple %1 (first %2) (apply hash-map (next %2))) db tupls))

(defn- find-indexes
  "Given a map of indexes and a partial tuple, return the sets of full tuples"
  [idxs pt]
  (if (empty? idxs)
    nil
    (filter identity (for [key (keys pt)]
                       (if-let [idx-map (idxs key)]
                         (idx-map (pt key))
                         nil)))))

(defn- match?
  "Is m2 contained in m1?"
  [m1 m2]
  (let [compare (fn [key]
                  (and (contains? m1 key)
                       (= (m1 key) (m2 key))))]
  (every? compare (keys m2))))

(defn- scan-space
  "Computes a stream of tuples from relation rn matching partial tuple (pt)
   and applies fun to each"
  [fun db rn pt]
  (let [rel (db rn)
        idxs (find-indexes (:indexes rel) pt)
        space (if (empty? idxs)
                (:data rel) ; table scan :(
                (reduce intersection idxs))]
    (fun #(match? % pt) space)))
    
(defn select
  "finds all matching tuples to the partial tuple (pt) in the relation named (rn)"
  [db rn pt]
;  (println "  DB Lookup: " rn pt)
  (scan-space filter db rn pt))

(defn any-match?
  "Finds if there are any matching records for the partial tuple"
  [db rn pt]
  (if (= (-> pt keys set) (:schema (db rn)))
    (contains? (:data (db rn)) pt)
    (scan-space some db rn pt)))


;;; Merge

(defn merge-indexes
  [idx1 idx2]
  (merge-with (fn [h1 h2] (merge-with union h1 h2)) idx1 idx2))
  
(defn merge-relations
  "Merges two relations"
  [r1 r2]
  (assert (= (:schema r1) (:schema r2)))
  (let [merged-indexes (merge-indexes (:indexes r1)
                                      (:indexes r2))
        merged-data (union (:data r1)
                           (:data r2))]
    (assoc r1 :data merged-data :indexes merged-indexes)))
    
(defn database-merge
  "Merges databases together"
  [dbs]
  (apply merge-with merge-relations dbs))

(defn database-merge-parallel
  "Merges databases together in parallel"
  [dbs]
  (preduce merge-relations dbs))


;; End of file
