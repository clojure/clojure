(ns clojure.contrib.sql.test
  (:use clojure.contrib.sql))

(Class/forName "org.apache.derby.jdbc.EmbeddedDriver")

(defn db []
  (get-connection "derby" "/tmp/clojure.contrib.sql.test.db;create=true"))

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
	(seq
	 (execute-prepared-statement con
	   "insert into fruit values (?, ?, ?, ?)"
	   [["Apple" "red" 59 87]
		["Banana" "yellow" 29 92.2]
		["Peach" "fuzzy" 139 90.0]
		["Orange" "juicy" 89 88.6]]))))
  
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
