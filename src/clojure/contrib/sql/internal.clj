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

(defn connection*
  "Returns the current database connection or throws"
  []
  (or (:connection *db*)
      (throw (Exception. "no current database connection"))))

(defn with-connection*
  "Evaluates func in the context of a new connection to a database then
  closes the connection. db-spec is a map containing string values for
  these required keys:
    :classname     the jdbc driver class name
    :subprotocol   the jdbc subprotocol
    :subname       the jdbc subname
  If db-spec contains additional keys (such as :user, :password, etc.) and
  associated values, they will be passed along to the driver as properties."
  [{:keys [classname subprotocol subname] :as db-spec} func]
  (clojure.lang.RT/loadClassForName classname)
  (with-open
      [con
       (java.sql.DriverManager/getConnection
        (format "jdbc:%s:%s" subprotocol subname)
        (properties (dissoc db-spec :classname :subprotocol :subname)))]
    (binding [*db* (assoc *db* :connection con :level 0)]
      (func))))

(defn transaction*
  "Evaluates func as a transaction on the open database connection. Any
  nested transactions are absorbed into the outermost transaction. All
  database updates are committed together as a group after evaluating the
  outermost func, or rolled back on any uncaught exception."
  [func]
  (io!
   (let [con (connection*)
         outermost (zero? (:level *db*))
         auto-commit (when outermost (.getAutoCommit con))]
     (binding [*db* (update-in *db* [:level] inc)]
       (when outermost
         (.setAutoCommit con false))
       (try
        (let [value (func)]
          (when outermost
            (.commit con))
          value)
        (catch Exception e
          (.rollback con)
          (throw (Exception.
                  (format "transaction rolled back: %s"
                          (.getMessage e)) e)))
        (finally
         (when outermost
           (.setAutoCommit con auto-commit))))))))

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
