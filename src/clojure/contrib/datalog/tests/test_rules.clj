;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  test-rules.clj
;;
;;  A Clojure implementation of Datalog -- Rule Tests
;;
;;  straszheimjeffrey (gmail)
;;  Created 12 Feburary 2009


(ns clojure.contrib.datalog.tests.test-rules
  (:use clojure.contrib.test-is
        clojure.contrib.datalog.rules
        clojure.contrib.datalog.literals
        clojure.contrib.datalog.database))


(def tr-1 (<- (:fred :x ?x :y ?y) (:mary :x ?x :z ?z) (:sally :z ?z :y ?y)))
(def tr-2 (<- (:fred) (not! :mary :x 3)))
(def tr-3 (<- (:fred :x ?x :y ?y) (if > ?x ?y) (:mary :x ?x) (:sally :y ?y)))



(deftest test-rule-safety
  (is (thrown-with-msg? Exception #".*Head vars.*not bound.*"
         (<- (:fred :x ?x) (:sally :y ?y))))
  (is (thrown-with-msg? Exception #".*Body vars.*not bound.*negative position.*"
         (<- (:fred :x ?x) (:becky :x ?x) (not! :sally :y ?y))))
  (is (thrown-with-msg? Exception #".*Body vars.*not bound.*negative position.*"
         (<- (:fred :x ?x) (:becky :x ?x) (if > ?x ?y)))))


(deftest test-sip
  (is (= (compute-sip #{:x} #{:mary :sally} tr-1)
         (<- ({:pred :fred :bound #{:x}} :x ?x :y ?y)
                      ({:pred :mary :bound #{:x}} :z ?z :x ?x)
                      ({:pred :sally :bound #{:z}} :y ?y :z ?z))))

  (is (= (compute-sip #{} #{:mary :sally} tr-1)
         (<- (:fred :y ?y :x ?x) (:mary :z ?z :x ?x) ({:pred :sally :bound #{:z}} :y ?y :z ?z))))

  (is (= (compute-sip #{} #{:mary} tr-2)
         (<- (:fred) (not! {:pred :mary :bound #{:x}} :x 3))))
  
  (is (= (compute-sip #{} #{} tr-2)
         tr-2))

  (is (= (display-rule (compute-sip #{:x} #{:mary :sally} tr-3))
         (display-rule (<- ({:pred :fred :bound #{:x}} :x ?x :y ?y)
                               ({:pred :mary :bound #{:x}} :x ?x)
                               (:sally :y ?y)
                               (if > ?x ?y))))))
                   ; Display rule is used because = does not work on
                   ; (if > ?x ?y) because it contains a closure


(def rs
     (rules-set
        (<- (:path :a ?x :b ?y) (:edge :a ?x :b ?y))
        (<- (:path :a ?x :b ?y) (:edge :a ?x :b ?z) (:path :a ?z :b ?y))
        (<- (:edge :a ?x :b ?y) (:route :a ?x :b ?y) (if not= ?x ?y))))

(deftest test-rules-set
  (is (= (count rs) 3))
  (is (contains? rs (<- (:path :a ?x :b ?y) (:edge :a ?x :b ?z) (:path :a ?z :b ?y)))))
  
(deftest test-predicate-map
  (let [pm (predicate-map rs)]
    (is (= (pm :path)
           #{(<- (:path :a ?x :b ?y) (:edge :a ?x :b ?y))
             (<- (:path :a ?x :b ?y) (:edge :a ?x :b ?z) (:path :a ?z :b ?y))}))
    (is (= (-> :edge pm count) 1))))


(def db1 (make-database
           (relation :fred [:x :y])
           (index :fred :x)
           (relation :sally [:x])
           (relation :ben [:y])))

(def db2 (add-tuples db1
             [:fred :x 1 :y :mary]
             [:fred :x 1 :y :becky]
             [:fred :x 3 :y :sally]
             [:fred :x 4 :y :joe]
             [:fred :x 4 :y :bob]
             [:sally :x 1]
             [:sally :x 2]
             [:sally :x 3]
             [:sally :x 4]
             [:ben :y :bob]))


(deftest test-apply-rule
  (is (= (apply-rule db2 empty-database (<- (:becky :y ?y) (:sally :x ?x)
                                                           (:fred :x ?x :y ?y)
                                                           (not! :ben :y ?y)
                                                           (if not= ?x 3)))
         (datalog-database
          {
           :becky
           (datalog-relation
            ;; Schema
            #{:y}
            ;; Data
            #{
              {:y :joe}
              {:y :mary}
              {:y :becky}
              }
            ;; Indexes
            {
             })
           }))))




(comment
  (run-tests)
)

;; End of file

