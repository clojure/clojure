;;  Copyright (c) Stephen C. Gilardi. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.
;;
;;  sql.clj
;;
;;  A Clojure interface to sql databases via jdbc
;;
;;  scgilardi (gmail)
;;  23 April 2008

(ns clojure.contrib.sql
  (:import
   (java.sql DriverManager Connection PreparedStatement ResultSet)))

(defn get-connection
  "Attempts to get a connection to a database via a jdbc URL"
  [subprotocol db-name]
  (let [url (str "jdbc:" subprotocol ":" db-name)]
    (.getConnection DriverManager url)))

(defmacro with-connection
  "Evaluates body in the context of a connection to a database. Any updates
  are committed as one transaction after evaluating body or rolled back on
  any uncaught exception."
  [con init & body]
  `(with-open ~con ~init
     (try
      (.setAutoCommit ~con false))
      ~@body
      (.commit ~con)
      (catch Exception e#
             (.rollback ~con)
             (throw (Exception. "transaction rolled back" e#)))))

(defn execute-commands
  "Executes a sequence of SQL commands that do not return results"
  [con commands]
  (with-open stmt (.createStatement con)
    (doseq cmd commands
      (.addBatch stmt cmd))
    (.executeBatch stmt)))

(defn execute-prepared-statement
  "Executes a prepared statement with a sequence of parameter sets"
  [con sql sets]
  (with-open stmt (.prepareStatement con sql)
    (doseq set sets
      (doseq [index value] (map vector (iterate inc 1) set)
        (.setObject stmt index value))
      (.addBatch stmt ))
    (.executeBatch stmt)))

(defmacro with-query-results
  "Executes a query and then evaluates body repeatedly with rec bound to
  each of the generated results in turn"
  [rec con sql & body]
  `(with-open stmt# (.prepareStatement ~con ~sql)
     (with-open rset# (.executeQuery stmt#)
       (doseq ~rec (resultset-seq rset#)
         ~@body))))

(comment

 ;; Examples

 ;; Simple tests of sql.clj using derby as a JDBC provider.
 ;;
 ;; Substituting a different database should only affect the definition
 ;; of 'db' below (and perhaps suggest the need for more variations of
 ;; get-connection).

(clojure/in-ns 'sql-test)
(clojure/refer 'clojure)

(lib/use sql)

(.forName Class "org.apache.derby.jdbc.EmbeddedDriver")

(defn db []
  (get-connection "derby" "/tmp/test-derby.db;create=true"))

(defn db-drop []
  (with-connection con (db)
    (try                   
     (execute-commands con
       ["drop table fruit"])
     (catch Exception e))))

(defn db-write []
  (db-drop)
  (with-connection con (db)
    (execute-commands con
      ["create table fruit (name varchar(32), appearance varchar(32), cost int, grade real)"])
    (execute-prepared-statement con
      "insert into fruit values (?, ?, ?, ?)"
      [["Apple" "red" 59 87]
       ["Banana" "yellow" 29 92.2]
       ["Peach" "fuzzy" 139 90.0]
       ["Orange" "juicy" 89 88.6]])))

(defn db-read []
  (with-connection con (db)
    (with-query-results rec con
      "select * from fruit"
      (println rec))))

(defn db-grade-a []
  (with-connection con (db)
    (with-query-results rec con
      "select name, cost from fruit where grade >= 90"
      (println rec))))

(defn db-exception []
  (with-connection con (db)
    (execute-prepared-statement con
      "insert into fruit (name, appearance) values (?, ?)"
      [["Grape" "yummy"]
       ["Pear" "bruised"]])
    (throw (Exception. "an exception"))))
)
