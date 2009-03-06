;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  test-softstrat.clj
;;
;;  A Clojure implementation of Datalog -- Soft Stratification Tests
;;
;;  straszheimjeffrey (gmail)
;;  Created 28 Feburary 2009

(ns clojure.contrib.datalog.tests.test-softstrat
  (:use clojure.contrib.test-is)
  (:use clojure.contrib.datalog.softstrat
        clojure.contrib.datalog.magic
        clojure.contrib.datalog.rules
        clojure.contrib.datalog.database)
  (:use [clojure.contrib.set :only (subset?)]))



(def rs1 (rules-set
            (<- (:p :x ?x) (:b :x ?x :y ?y :z ?z) (not! :q :x ?x) (not! :q :x ?y) (not! :q :x ?z))
            (<- (:q :x ?x) (:d :x ?x))))

(def q1 (?- :p :x 1))

(def ws (build-soft-strat-work-plan rs1 q1))

(deftest test-soft-stratification
  (let [soft (:stratification ws)
        q (:query ws)]
    (is (= q (?- {:pred :p :bound #{:x}} :x 1)))
    (is (= (count soft) 4))
    (is (subset? (rules-set
                  (<- ({:pred :q :bound #{:x}} :x ?x) ({:pred :q :magic true :bound #{:x}} :x ?x)
                                                      (:d :x ?x))

                  (<- ({:pred :q :magic true :bound #{:x}} :x ?x) ({:pred :p :magic true :bound #{:x}} :x ?x)
                                                                  (:b :z ?z :y ?y :x ?x)))
                 (nth soft 0)))
    (is (= (nth soft 1)
           (rules-set
            (<- ({:pred :q :magic true :bound #{:x}} :x ?y) ({:pred :p :magic true :bound #{:x}} :x ?x)
                                                            (:b :z ?z :y ?y :x ?x)
                                                            (not! {:pred :q :bound #{:x}} :x ?x)))))
    (is (= (nth soft 2)
           (rules-set
            (<- ({:pred :q :magic true :bound #{:x}} :x ?z) ({:pred :p :magic true :bound #{:x}} :x ?x)
                                                            (:b :z ?z :y ?y :x ?x)
                                                            (not! {:pred :q :bound #{:x}} :x ?x)
                                                            (not! {:pred :q :bound #{:x}} :x ?y)))))
    (is (= (nth soft 3)
           (rules-set
            (<- ({:pred :p :bound #{:x}} :x ?x) ({:pred :p :magic true :bound #{:x}} :x ?x)
                                                (:b :z ?z :y ?y :x ?x)
                                                (not! {:pred :q :bound #{:x}} :x ?x)
                                                (not! {:pred :q :bound #{:x}} :x ?y)
                                                (not! {:pred :q :bound #{:x}} :x ?z)))))))


(def tdb-1
     (make-database
       (relation :b [:x :y :z])
       (relation :d [:x])))

(def tdb-2
     (add-tuples tdb-1
                 [:b :x 1 :y 2 :z 3]))

(deftest test-tdb-2
  (is (= (evaluate-soft-work-set ws tdb-2 {})
         [{:x 1}])))



(def tdb-3
     (add-tuples tdb-2
                 [:d :x 2]
                 [:d :x 3]))

(deftest test-tdb-3
  (is (empty? (evaluate-soft-work-set ws tdb-3 {}))))
         


;;;;;;;;;;;



(def db-base
     (make-database
      (relation :employee [:id :name :position])
      (index :employee :name)

      (relation :boss [:employee-id :boss-id])
      (index :boss :employee-id)

      (relation :can-do-job [:position :job])
      (index :can-do-job :position)

      (relation :job-replacement [:job :can-be-done-by])

      (relation :job-exceptions [:id :job])))

(def db
     (add-tuples db-base
           [:employee :id 1  :name "Bob"    :position :boss]
           [:employee :id 2  :name "Mary"   :position :chief-accountant]
           [:employee :id 3  :name "John"   :position :accountant]
           [:employee :id 4  :name "Sameer" :position :chief-programmer]
           [:employee :id 5  :name "Lilian" :position :programmer]
           [:employee :id 6  :name "Li"     :position :technician]
           [:employee :id 7  :name "Fred"   :position :sales]
           [:employee :id 8  :name "Brenda" :position :sales]
           [:employee :id 9  :name "Miki"   :position :project-management]
           [:employee :id 10 :name "Albert" :position :technician]
           
           [:boss :employee-id 2  :boss-id 1]
           [:boss :employee-id 3  :boss-id 2]
           [:boss :employee-id 4  :boss-id 1]
           [:boss :employee-id 5  :boss-id 4]
           [:boss :employee-id 6  :boss-id 4]
           [:boss :employee-id 7  :boss-id 1]
           [:boss :employee-id 8  :boss-id 7]
           [:boss :employee-id 9  :boss-id 1]
           [:boss :employee-id 10 :boss-id 6]

           [:can-do-job :position :boss               :job :management]
           [:can-do-job :position :accountant         :job :accounting]
           [:can-do-job :position :chief-accountant   :job :accounting]
           [:can-do-job :position :programmer         :job :programming]
           [:can-do-job :position :chief-programmer   :job :programming]           
           [:can-do-job :position :technician         :job :server-support]
           [:can-do-job :position :sales              :job :sales]
           [:can-do-job :position :project-management :job :project-management]

           [:job-replacement :job :pc-support :can-be-done-by :server-support]
           [:job-replacement :job :pc-support :can-be-done-by :programming]
           [:job-replacement :job :payroll    :can-be-done-by :accounting]

           [:job-exceptions :id 4 :job :pc-support]))

(def rules
     (rules-set
        (<- (:works-for :employee ?x :boss ?y) (:boss :employee-id ?e-id :boss-id ?b-id)
                                               (:employee :id ?e-id :name ?x)
                                               (:employee :id ?b-id :name ?y))
        (<- (:works-for :employee ?x :boss ?y) (:works-for :employee ?x :boss ?z)
                                               (:works-for :employee ?z :boss ?y))
        (<- (:employee-job* :employee ?x :job ?y) (:employee :name ?x :position ?pos)
                                                  (:can-do-job :position ?pos :job ?y))
        (<- (:employee-job* :employee ?x :job ?y) (:job-replacement :job ?y :can-be-done-by ?z)
                                                  (:employee-job* :employee ?x  :job ?z))
        (<- (:employee-job* :employee ?x :job ?y) (:can-do-job :job ?y)
                                                  (:employee :name ?x :position ?z)
                                                  (if = ?z :boss))
        (<- (:employee-job :employee ?x :job ?y) (:employee-job* :employee ?x :job ?y)
                                                 (:employee :id ?id :name ?x)
                                                 (not! :job-exceptions :id ?id :job ?y))
        (<- (:bj :name ?x :boss ?y) (:works-for :employee ?x :boss ?y)
                                    (not! :employee-job :employee ?y :job :pc-support))))


(def ws-1 (build-soft-strat-work-plan rules (?- :works-for :employee '??name :boss ?x)))
(defn evaluate-1 [name] (set (evaluate-soft-work-set ws-1 db {'??name name})))

(deftest test-ws-1
  (is (= (evaluate-1 "Albert")
         #{{:employee "Albert", :boss "Li"}
           {:employee "Albert", :boss "Sameer"}
           {:employee "Albert", :boss "Bob"}}))
  (is (empty? (evaluate-1 "Bob")))
  (is (= (evaluate-1 "John")
         #{{:employee "John", :boss "Bob"}
           {:employee "John", :boss "Mary"}})))
         

(def ws-2 (build-soft-strat-work-plan rules (?- :employee-job :employee '??name :job ?x)))
(defn evaluate-2 [name] (set (evaluate-soft-work-set ws-2 db {'??name name})))

(deftest test-ws-2
  (is (= (evaluate-2 "Albert")
         #{{:employee "Albert", :job :pc-support}
           {:employee "Albert", :job :server-support}}))
  (is (= (evaluate-2 "Sameer")
         #{{:employee "Sameer", :job :programming}}))
  (is (= (evaluate-2 "Bob")
         #{{:employee "Bob", :job :accounting}
           {:employee "Bob", :job :management}
           {:employee "Bob", :job :payroll}
           {:employee "Bob", :job :pc-support}
           {:employee "Bob", :job :project-management}
           {:employee "Bob", :job :programming}
           {:employee "Bob", :job :server-support}
           {:employee "Bob", :job :sales}})))

(def ws-3 (build-soft-strat-work-plan rules (?- :bj :name '??name :boss ?x)))
(defn evaluate-3 [name] (set (evaluate-soft-work-set ws-3 db {'??name name})))

(deftest test-ws-3
  (is (= (evaluate-3 "Albert")
         #{{:name "Albert", :boss "Sameer"}})))


(comment
  (run-tests)
)

;; End of file
