;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  internal definitions for clojure.contrib.sql
;;
;;  scgilardi (gmail)
;;  Created 3 October 2008

(ns clojure.contrib.sql.internal
  (:use
   (clojure.contrib
    [except :only (throwf throw-arg)]
    [java-utils :only (as-properties)]
    [seq-utils :only (indexed)]))
  (:import
   (clojure.lang RT)
   (java.sql BatchUpdateException DriverManager SQLException Statement)
   (java.util Hashtable Map)
   (javax.naming InitialContext Name)
   (javax.sql DataSource)))

(def *db* {:connection nil :level 0})

(def special-counts
     {Statement/EXECUTE_FAILED "EXECUTE_FAILED"
      Statement/SUCCESS_NO_INFO "SUCCESS_NO_INFO"})

(defn find-connection*
  "Returns the current database connection (or nil if there is none)"
  []
  (:connection *db*))

(defn connection*
  "Returns the current database connection (or throws if there is none)"
  []
  (or (find-connection*)
      (throwf "no current database connection")))

(defn rollback
  "Accessor for the rollback flag on the current connection"
  ([]
     (deref (:rollback *db*)))
  ([val]
     (swap! (:rollback *db*) (fn [_] val))))

(defn get-connection
  "Creates a connection to a database. db-spec is a map containing values
  for one of the following parameter sets:

  DriverManager:
    :classname   (required) a String, the jdbc driver class name
    :subprotocol (required) a String, the jdbc subprotocol
    :subname     (required) a String, the jdbc subname
    (others)     (optional) passed to the driver as properties.

  DataSource:
    :datasource  (required) a javax.sql.DataSource
    :username    (optional) a String
    :password    (optional) a String, required if :username is supplied

  JNDI:
    :name        (required) a String or javax.naming.Name
    :environment (optional) a java.util.Map"
  [{:keys [classname subprotocol subname
           datasource username password
           name environment]
    :as db-spec}]
  (cond
    (and classname subprotocol subname)
    (let [url (format "jdbc:%s:%s" subprotocol subname)
          etc (dissoc db-spec :classname :subprotocol :subname)]
      (RT/loadClassForName classname)
      (DriverManager/getConnection url (as-properties etc)))
    (and datasource username password)
    (.getConnection datasource username password)
    datasource
    (.getConnection datasource)
    name
    (let [env (and environment (Hashtable. environment))
          context (InitialContext. env)
          datasource (.lookup context name)]
      (.getConnection datasource))
    :else
    (throw-arg "db-spec %s is missing a required parameter" db-spec)))
     
(defn with-connection*
  "Evaluates func in the context of a new connection to a database then
  closes the connection."
  [db-spec func]
  (with-open [con (get-connection db-spec)]
    (binding [*db* (assoc *db*
                     :connection con :level 0 :rollback (atom false))]
      (func))))

(defn print-sql-exception
  "Prints the contents of an SQLException to stream"
  [stream exception]
  (.println
   stream
   (format (str "%s:" \newline
                " Message: %s" \newline
                " SQLState: %s" \newline
                " Error Code: %d")
           (.getSimpleName (class exception))
           (.getMessage exception)
           (.getSQLState exception)
           (.getErrorCode exception))))

(defn print-sql-exception-chain
  "Prints a chain of SQLExceptions to stream"
  [stream exception]
  (loop [e exception]
    (when e
      (print-sql-exception stream e)
      (recur (.getNextException e)))))

(defn print-update-counts
  "Prints the update counts from a BatchUpdateException to stream"
  [stream exception]
  (.println stream "Update counts:")
  (doseq [[index count] (indexed (.getUpdateCounts exception))]
    (.println stream (format " Statement %d: %s"
                             index
                             (get special-counts count count)))))

(defn throw-rollback
  "Sets rollback and throws a wrapped exception"
  [e]
  (rollback true)
  (throwf e "transaction rolled back: %s" (.getMessage e)))

(defn transaction*
  "Evaluates func as a transaction on the open database connection. Any
  nested transactions are absorbed into the outermost transaction. By
  default, all database updates are committed together as a group after
  evaluating the outermost body, or rolled back on any uncaught
  exception. If rollback is set within scope of the outermost transaction,
  the entire transaction will be rolled back rather than committed when
  complete."
  [func]
  (binding [*db* (update-in *db* [:level] inc)]
    (if (= (:level *db*) 1)
      (let [con (connection*)
            auto-commit (.getAutoCommit con)]
        (io!
         (.setAutoCommit con false)
         (try
          (func)
          (catch BatchUpdateException e
            (print-update-counts *err* e)
            (print-sql-exception-chain *err* e)
            (throw-rollback e))
          (catch SQLException e
            (print-sql-exception-chain *err* e)
            (throw-rollback e))
          (catch Exception e
            (throw-rollback e))
          (finally
           (if (rollback)
             (.rollback con)
             (.commit con))
           (rollback false)
           (.setAutoCommit con auto-commit)))))
      (func))))

(defn with-query-results*
  "Executes a query, then evaluates func passing in a seq of the results as
  an argument. The first argument is a vector containing the (optionally
  parameterized) sql query string followed by values for any parameters."
  [[sql & params :as sql-params] func]
  (when-not (vector? sql-params)
    (throw-arg "\"%s\" expected %s %s, found %s %s"
               "sql-params"
               "vector"
               "[sql param*]"
               (.getName (class sql-params))
               (pr-str sql-params)))
  (with-open [stmt (.prepareStatement (connection*) sql)]
    (doseq [[index value] (map vector (iterate inc 1) params)]
      (.setObject stmt index value))
    (with-open [rset (.executeQuery stmt)]
      (func (resultset-seq rset)))))
