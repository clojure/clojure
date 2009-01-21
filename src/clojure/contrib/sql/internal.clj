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

(ns clojure.contrib.sql.internal)

(def *db* {:connection nil :level 0})

(defn the-str
  "Returns the name or string representation of x"
  [x]
  (if (instance? clojure.lang.Named x)
    (name x)
    (str x)))

(defn properties
  "Converts a map from keywords, symbols, or strings to values into a
  java.util.Properties object that maps the key names to the values with
  all represented as strings."
  [m]
  (let [p (java.util.Properties.)]
    (doseq [[key val] (seq m)]
      (.setProperty p (the-str key) (the-str val)))
    p))

(defn find-connection*
  "Returns the current database connection (or nil if there is none)"
  []
  (:connection *db*))

(defn connection*
  "Returns the current database connection (or throws if there is none)"
  []
  (or (find-connection*)
      (throw (Exception. "no current database connection"))))

(defn rollback
  "Accessor for the rollback flag on the current connection"
  ([]
     (deref (:rollback *db*)))
  ([val]
     (swap! (:rollback *db*) (fn [_] val))))

(defn get-connection
  "Creates a connection to a database. db-spec is a map containing values
  for one of the following parameter sets:

  DataSource:
    :datasource  (required) a javax.sql.DataSource
    :username    (optional) a String
    :password    (optional) a String

  DriverManager:
    :classname   (required) a String, the jdbc driver class name
    :subprotocol (required) a String, the jdbc subprotocol
    :subname     (required) a String, the jdbc subname
    (others)     (optional) passed to the driver as properties."
  [{:keys [datasource username password classname subprotocol subname]
    :as db-spec}]
  (when classname
    (clojure.lang.RT/loadClassForName classname))
  (if datasource
    (if username
      (.getConnection datasource username password)
      (.getConnection datasource))
    (java.sql.DriverManager/getConnection
     (format "jdbc:%s:%s" subprotocol subname)
     (properties (dissoc db-spec :classname :subprotocol :subname)))))

(defn with-connection*
  "Evaluates func in the context of a new connection to a database then
  closes the connection."
  [db-spec func]
  (with-open [con (get-connection db-spec)]
    (binding [*db* (assoc *db*
                     :connection con :level 0 :rollback (atom false))]
      (func))))

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
          (catch Exception e
            (rollback true)
            (throw
             (Exception. (format "transaction rolled back: %s"
                                 (.getMessage e)) e)))
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
  [[sql & params] func]
  (with-open [stmt (.prepareStatement (connection*) sql)]
    (doseq [[index value] (map vector (iterate inc 1) params)]
      (.setObject stmt index value))
    (with-open [rset (.executeQuery stmt)]
      (func (resultset-seq rset)))))
