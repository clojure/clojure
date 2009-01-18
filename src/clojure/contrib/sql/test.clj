;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  test.clj
;;
;;  test/example for clojure.contrib.sql
;;
;;  scgilardi (gmail)
;;  Created 13 September 2008

(ns clojure.contrib.sql.test
  (:require [clojure.contrib.sql :as sql]))

(def db {:classname "org.apache.derby.jdbc.EmbeddedDriver"
         :subprotocol "derby"
         :subname "/tmp/clojure.contrib.sql.test.db"
         :create true})

(defn create-fruit
  "Create a table"
  []
  (sql/create-table
   :fruit
   [:name "varchar(32)" "PRIMARY KEY"]
   [:appearance "varchar(32)"]
   [:cost :int]
   [:grade :real]))

(defn drop-fruit
  "Drop a table"
  []
  (try
   (sql/drop-table :fruit)
   (catch Exception _)))

(defn insert-rows-fruit
  "Insert complete rows"
  []
  (sql/insert-rows
   :fruit
   ["Apple" "red" 59 87]
   ["Banana" "yellow" 29 92.2]
   ["Peach" "fuzzy" 139 90.0]
   ["Orange" "juicy" 89 88.6]))

(defn insert-values-fruit
  "Insert rows with values for only specific columns"
  []
  (sql/insert-values
   :fruit
   [:name :cost]
   ["Mango" 722]
   ["Feijoa" 441]))

(defn db-write
  "Write initial values to the database as a transaction"
  []
  (sql/with-connection
   db
   (sql/transaction
    (drop-fruit)
    (create-fruit)
    (insert-rows-fruit)
    (insert-values-fruit)))
  nil)

(defn db-read
  "Read the entire fruit table"
  []
  (sql/with-connection
   db
   (sql/with-query-results
    res
    ["SELECT * FROM fruit"]
    (doseq [rec res]
      (println rec)))))

(defn db-update-appearance-cost
  "Update the appearance and cost of the named fruit"
  [name appearance cost]
  (sql/update-values
   :fruit
   ["name=?" name]
   {:appearance appearance :cost cost}))

(defn db-update
  "Update two fruits as a transaction"
  []
  (sql/with-connection
   db
   (sql/transaction
    (db-update-appearance-cost "Banana" "bruised" 14)
    (db-update-appearance-cost "Feijoa" "green" 400)))
  nil)

(defn db-update-or-insert
  "Updates or inserts a fruit"
  [record]
  (sql/with-connection
   db
   (sql/update-or-insert-values
    :fruit
    ["name=?" (:name record)]
    record)))

(defn db-read-all
  "Return all the rows of the fruit table as a vector"
  []
  (sql/with-connection
   db
   (sql/with-query-results
    res
    ["SELECT * FROM fruit"]
    (into [] res))))

(defn db-grade-range
  "Print rows describing fruit that are within a grade range"
  [min max]
  (sql/with-connection
   db
   (sql/with-query-results
    res
    [(str "SELECT name, cost, grade "
          "FROM fruit "
          "WHERE grade >= ? AND grade <= ?")
     min max]
    (doseq [rec res]
      (println rec)))))

(defn db-grade-a 
  "Print rows describing all grade a fruit (grade between 90 and 100)"
  []
  (db-grade-range 90 100))

(defn db-get-tables
  "Demonstrate getting table info"
  []
  (sql/with-connection
   db
   (into []
         (resultset-seq
          (-> (sql/connection)
              (.getMetaData)
              (.getTables nil nil nil (into-array ["TABLE" "VIEW"])))))))

(defn db-exception
  "Demonstrate rolling back a partially completed transaction on exception"
  []
  (sql/with-connection
   db
   (sql/transaction
    (sql/insert-values
     :fruit
     [:name :appearance]
     ["Grape" "yummy"]
     ["Pear" "bruised"])
    ;; at this point the insert-values call is complete, but the transaction
    ;; is not. the exception will cause it to roll back leaving the database
    ;; untouched.
    (throw (Exception. "sql/test exception")))))

(defn db-rollback
  "Demonstrate a rollback-only trasaction"
  []
  (sql/with-connection
   db
   (sql/transaction
    (prn "is-rollback-only" (sql/is-rollback-only))
    (sql/set-rollback-only)
    (sql/insert-values
     :fruit
     [:name :appearance]
     ["Grape" "yummy"]
     ["Pear" "bruised"])
    (prn "is-rollback-only" (sql/is-rollback-only))
    (sql/with-query-results
     res
     ["SELECT * FROM fruit"]
     (doseq [rec res]
      (println rec))))
   (prn)
   (sql/with-query-results
    res
    ["SELECT * FROM fruit"]
    (doseq [rec res]
      (println rec)))))
