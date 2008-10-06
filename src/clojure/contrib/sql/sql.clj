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
  (:use clojure.contrib.except
		clojure.contrib.sql.internal))

(defmacro with-connection
  "Evaluates body in the context of a new connection to a database then
  closes it. db-spec is a map containing string values for these required
  keys:
    :classname     the jdbc driver class name
    :subprotocol   the jdbc subprotocol
    :subname       the jdbc subname
  db-spec may contain additional key-value pairs that are passed along to
  the driver as properties such as :user, :password, etc."
  [db-spec & body]
  `(do
     (clojure.lang.RT/classForName (:classname ~db-spec))
     (with-open con#
         (java.sql.DriverManager/getConnection
          (format "jdbc:%s:%s" (:subprotocol ~db-spec) (:subname ~db-spec))
          (properties (dissoc ~db-spec :classname :subprotocol :subname)))
       (binding [*db* (assoc *db* :connection con# :level 0)]
         ~@body))))

(defmacro transaction
  "Evaluates body as a transaction on the open database connection. Updates
  are committed together as a group after evaluating body or rolled back on
  any uncaught exception. Any nested transactions will be absorbed into the
  outermost transaction."
  [& body]
  `(let [con# (connection)
         level# (:level *db*)]
     (binding [*db* (assoc *db* :level (inc level#))]
       (let [auto-commit# (.getAutoCommit con#)]
         (when (zero? level#)
           (.setAutoCommit con# false))
         (try
          (let [value# (do ~@body)]
            (when (zero? level#)
              (.commit con#))
            value#)
          (catch Exception e#
            (.rollback con#)
            (throw (Exception.
                    (format "transaction rolled back: %s"
                            (.getMessage e#)) e#)))
          (finally
           (when (zero? level#)
             (.setAutoCommit con# auto-commit#))))))))

(defn do-commands
  "Executes SQL commands that don't return results on the open database
  connection"
  [& commands]
  (with-open stmt (.createStatement (connection))
    (doseq cmd commands
      (.addBatch stmt cmd))
    (.executeBatch stmt)))

(defn do-prepared
  "Executes a prepared statement on the open database connection with
  parameter sets"
  [sql & sets]
  (with-open stmt (.prepareStatement (connection) sql)
    (doseq set sets
      (doseq [index value] (map vector (iterate inc 1) set)
        (.setObject stmt index value))
      (.addBatch stmt))
    (.executeBatch stmt)))

(defn create-table
  "Creates a table on the open database connection given a name (a string
  or keyword) and column specs. A column spec is a vector containing a name
  and optionally a type and other items such as constraints, each a string
  or keyword."
  [name & cols]
  (do-commands
   (format "create table %s (%s)"
           (the-str name)
           (apply str
             (map the-str
              (apply concat
               (interpose [", "]
                (map (partial interpose " ") cols))))))))

(defn drop-table
  "Drops a table on the open database connection given its name (a string
  or keyword)"
  [name]
  (do-commands
   (format "drop table %s" (the-str name))))

(defn insert-values
  "Inserts values into columns of a table. columns is a vector of column
  names (strings or keywords) and each value is a vector of values for
  those columns. To insert complete rows (all columns), use insert-rows."
  [table columns & values]
  (let [count (count (first values))
        template (apply str (interpose "," (replicate count "?")))
        cols (if (seq columns)
               (format "(%s)" (apply str (interpose "," (map the-str columns))))
               "")]
    (apply do-prepared
           (format "insert into %s %s values (%s)" (the-str table) cols template)
           values)))

(defn insert-rows
  "Inserts complete rows into a table. Each row is a vector of values for
  each of the table's columns in order."
  [table & rows]
  (apply insert-values table nil rows))

(defmacro with-results
  "Executes a query and then evaluates body with res bound to a seq of the
  results"
  [res sql & body]
  `(with-open stmt# (.prepareStatement (connection) ~sql)
     (with-open rset# (.executeQuery stmt#)
       (let [~res (resultset-seq rset#)]
         ~@body))))
