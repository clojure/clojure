;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
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
  (:use clojure.contrib.sql.internal))

(defmacro with-connection
  "Evaluates body in the context of a new connection to a database then
  closes the connection. db-spec is a map containing string values for
  these required keys:
    :classname     the jdbc driver class name
    :subprotocol   the jdbc subprotocol
    :subname       the jdbc subname
  db-spec may contain additional key-value pairs that are passed along to
  the driver as properties such as :user, :password, etc."
  [db-spec & body]
  `(with-connection* ~db-spec (fn [] ~@body)))

(defmacro transaction
  "Evaluates body as a transaction on the open database connection. Any
  database updates are committed together as a group after evaluating, or
  rolled back on any uncaught exception. Any nested transactions are
  absorbed into the outermost transaction."
  [& body]
  `(transaction* (fn [] ~@body)))

(defn do-commands
  "Executes SQL commands that don't return results on the open database
  connection"
  [& commands]
  (with-open [stmt (create-statement)]
    (doseq [cmd commands]
      (.addBatch stmt cmd))
    (.executeBatch stmt)))

(defn do-prepared
  "Executes a prepared statement on the open database connection with
  parameter sets"
  [sql & sets]
  (with-open [stmt (prepare-statement sql)]
    (doseq [set sets]
      (doseq [[index value] (map vector (iterate inc 1) set)]
        (.setObject stmt index value))
      (.addBatch stmt))
    (.executeBatch stmt)))

(defn create-table
  "Creates a table on the open database connection given a name (a string
  or keyword) and column specs. A column spec is a vector containing a name
  and optionally a type and other items such as constraints, each a string
  or keyword."
  [name & column-specs]
  (do-commands
   (format "create table %s (%s)"
           (the-str name)
           (apply str
             (map the-str
              (apply concat
               (interpose [", "]
                (map (partial interpose " ") column-specs))))))))

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
  [table column-names & values]
  (let [count (count (first values))
        template (apply str (interpose "," (replicate count "?")))
        columns
        (if (seq column-names)
          (format "(%s)"
                  (apply str (interpose "," (map the-str column-names))))
          "")]
    (apply do-prepared
           (format "insert into %s %s values (%s)"
                   (the-str table) columns template)
           values)))

(defn insert-rows
  "Inserts complete rows into a table. Each row is a vector of values for
  each of the table's columns in order."
  [table & rows]
  (apply insert-values table nil rows))

(defmacro with-results
  "Executes a query and then evaluates body with results bound to a seq of
  the results"
  [results sql & body]
  `(with-open [stmt# (prepare-statement ~sql)
               rset# (.executeQuery stmt#)]
     (let [~results (resultset-seq rset#)]
       ~@body)))
