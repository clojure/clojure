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
;;  2 April 2008

(clojure/in-ns 'sql)
(clojure/refer 'clojure)

(import '(java.sql DriverManager Connection PreparedStatement ResultSet))

(defn get-connection
  "Attempts to get a connection to a database via a jdbc URL"
  [subprotocol db-name]
  (let [url (str "jdbc:" subprotocol ":" db-name)]
    (. DriverManager (getConnection url))))

(defmacro with-connection
  "Evaluates body in the context of a connection to a database. Any updates
  are committed as one transaction after evaluating body or rolled back on
  any uncaught exception."
  [con init & body]
  `(with-open ~con ~init
     (try
      (. ~con (setAutoCommit false))
      ~@body
      (. ~con (commit))
      (catch Exception e#
             (. ~con (rollback))
             (throw (new Exception "transaction rolled back" e#))))))

(defn execute-commands
  "Executes a sequence of SQL commands that do not return results"
  [con commands]
  (with-open stmt (. con (createStatement))
    (doseq cmd commands
      (. stmt (addBatch cmd)))
    (. stmt (executeBatch))))

(defn execute-prepared-statement
  "Executes a prepared statement with a sequence of parameter sets"
  [con sql sets]
  (with-open stmt (. con (prepareStatement sql))
    (doseq set sets
      (doseq arg (map vector (iterate inc 1) set)
        (. stmt (setObject (arg 0) (arg 1))))
      (. stmt (addBatch)))
    (. stmt (executeBatch))))

(defmacro with-query-results
  "Executes a query and then evaluates body repeatedly with rec bound to
  each of the generated results in turn"
  [rec con sql & body]
  `(with-open stmt# (. ~con (prepareStatement ~sql))
     (with-open rset# (. stmt# (executeQuery))
       (doseq ~rec (resultset-seq rset#)
         ~@body))))

(when (find-var 'pkg/provide)
  (pkg/provide 'sql))

(comment

 ;; Examples

 ;; Simple tests of sql.clj using sqlite as a JDBC provider.  Note that
 ;; unlike sql.clj itself, these tests require that java be able to
 ;; access sqlite via something like: http://zentus.com/sqlitejdbc .
 ;;
 ;; Substituting a different database should only affect the definition
 ;; of 'db' below (and perhaps suggest the need for more variations of
 ;; get-connection).

 (pkg/require-ns 'sql)

 (class org.sqlite.JDBC)

 (defn db []
   (get-connection "sqlite" "test.db"))

 (defn db-write []
   (with-connection con (db)
     (execute-commands con
        ["drop table if exists fruit"
         "create table fruit (name, appearance, cost int, grade real)"])
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
     (throw (new Exception "an exception"))))
)
