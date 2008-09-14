;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  sql.clj
;;
;;  A Clojure interface to sql databases via jdbc
;;
;;  See clojure.contrib.sql.test for an example
;;
;;  scgilardi (gmail)
;;  Created 2 April 2008

(ns clojure.contrib.sql
  (:import
   (java.sql DriverManager Connection PreparedStatement ResultSet)))

(defn connection
  "Attempts to get a connection to a database via a jdbc URL"
  [subprotocol db-name]
  (DriverManager/getConnection (format "jdbc:%s:%s" subprotocol db-name)))

(defmacro with-connection
  "Evaluates body in the context of a connection to a database. Any updates
  are committed as one transaction after evaluating body or rolled back on
  any uncaught exception."
  [con init & body]
  `(with-open ~con ~init
     (try
      (.setAutoCommit ~con false)
      ~@body
      (.commit ~con)
      (catch Exception e#
        (.rollback ~con)
        (throw (Exception. "transaction rolled back" e#))))))

(defn do-commands
  "Executes SQL commands that do not return results"
  [con & commands]
  (with-open stmt (.createStatement con)
    (doseq cmd commands
      (.addBatch stmt cmd))
    (.executeBatch stmt)))

(defn do-prepared
  "Executes a prepared statement with parameter sets"
  [con sql & sets]
  (with-open stmt (.prepareStatement con sql)
    (doseq set sets
      (doseq [index value] (map vector (iterate inc 1) set)
        (.setObject stmt index value))
      (.addBatch stmt ))
    (.executeBatch stmt)))

(defn create-table
  "Creates a table given a name and column specifications"
  [con name & cols]
  (do-commands con
    (format "create table %s (%s)"
            name
            (apply str (interpose "," cols)))))

(defn drop-table
  "Drops a table give its name"
  [con name]
  (do-commands con
    (format "drop table %s" name)))

(defn insert-values
  "Inserts values into columns of a table. Columns is a seq of column
  names (as strings) and each value is a seq of values for those
  columns. To insert complete rows (all columns), use insert-rows"
  [con table columns & values]
  (let [count (count (first values))
        template (apply str (interpose "," (replicate count "?")))
        cols (if (seq columns)
               (format "(%s)" (apply str (interpose "," columns)))
               "")]
    (apply do-prepared
           con
           (format "insert into %s %s values (%s)" table cols template)
           values)))

(defn insert-rows
  "Inserts complete rows into a table. Each row is a seq of values for
  each of the table's columns in order."
  [con table & rows]
  (apply insert-values con table nil rows))

(defmacro with-results
  "Executes a query and then evaluates body repeatedly with rec bound to
  each of the generated results in turn"
  [rec con sql & body]
  `(with-open stmt# (.prepareStatement ~con ~sql)
     (with-open rset# (.executeQuery stmt#)
       (doseq ~rec (resultset-seq rset#)
         ~@body))))
