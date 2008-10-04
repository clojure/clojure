;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  test.clj
;;
;;  test/example for clojure.contrib.sql.test
;;
;;  scgilardi (gmail)
;;  Created 13 September 2008

(ns clojure.contrib.sql.test
  (:use clojure.contrib.sql))

(clojure.lang.RT/classForName "org.apache.derby.jdbc.EmbeddedDriver")

(defn db []
  (connection "derby" "/tmp/clojure.contrib.sql.test.db" :create true))

(defn db-write []
  (with-connection con (db)
    (try
     (drop-table con :fruit)
     (catch Exception e))
    (create-table con :fruit
      :name "varchar(32)"
      :appearance "varchar(32)"
      :cost :int
      :grade :real)
    (insert-rows con :fruit
      ["Apple" "red" 59 87]
      ["Banana" "yellow" 29 92.2]
      ["Peach" "fuzzy" 139 90.0]
      ["Orange" "juicy" 89 88.6])
    (insert-values con :fruit [:name :cost]
      ["Mango" 722]
      ["Feijoa" 441])))

(defn db-read []
  (with-connection con (db)
    (with-results rec con
      "select * from fruit"
      (println rec))))

(defn db-grade-a []
  (with-connection con (db)
    (with-results rec con
      "select name, cost from fruit where grade >= 90"
      (println rec))))

(defn db-exception []
  (with-connection con (db)
    (insert-values con :fruit [:name :appearance]
      ["Grape" "yummy"]
      ["Pear" "bruised"])
    (throw (Exception. "an exception"))))
