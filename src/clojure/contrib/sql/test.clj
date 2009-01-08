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

(defn drop-fruit []
  (try
   (sql/drop-table :fruit)
   (catch Exception e)))

(defn create-fruit []
  (sql/transaction
   (sql/create-table :fruit
    [:name "varchar(32)" "NOT NULL"]
    [:appearance "varchar(32)"]
    [:cost :int]
    [:grade :real])))

(defn insert-rows-fruit []
  (sql/transaction
   (sql/insert-rows :fruit
    ["Apple" "red" 59 87]
    ["Banana" "yellow" 29 92.2]
    ["Peach" "fuzzy" 139 90.0]
    ["Orange" "juicy" 89 88.6])))

(defn insert-values-fruit []
  (sql/transaction
   (sql/insert-values :fruit
    [:name :cost]
    ["Mango" 722]
    ["Feijoa" 441])))

(defn db-write []
  (sql/with-connection db
    (sql/transaction
     (drop-fruit)
     (create-fruit)
     (insert-rows-fruit)
     (insert-values-fruit)))
  nil)

(defn db-read []
  (sql/with-connection db
    (sql/with-results res
     "select * from fruit"
      (doseq [rec res]
        (println rec)))))

(defn db-read-all []
  (sql/with-connection db
    (sql/transaction                       
     (sql/with-results res
      "select * from fruit"
      (into [] res)))))

(defn db-grade-a []
  (sql/with-connection db
    (sql/transaction
     (sql/with-results res
       "select name, cost from fruit where grade >= 90"
       (doseq [rec res]
         (println rec))))))

(defn db-get-tables []
  (sql/with-connection db
    (into []
      (resultset-seq
       (-> (sql/connection)
           (.getMetaData)
           (.getTables nil nil nil (into-array ["TABLE" "VIEW"])))))))

(defn db-exception []
  (sql/with-connection db
    (sql/transaction
      (sql/insert-values :fruit
        [:name :appearance]
        ["Grape" "yummy"]
        ["Pear" "bruised"])
      (throw (Exception. "an exception")))))
