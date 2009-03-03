;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  example.clj
;;
;;  A Clojure implementation of Datalog - Example
;;
;;  straszheimjeffrey (gmail)
;;  Created 2 March 2009


(ns clojure.contrib.datalog.example
  (:use [clojure.contrib.datalog :only (build-work-plan run-work-plan)]
        [clojure.contrib.datalog.rules :only (<- ?- rules-set)]
        [clojure.contrib.datalog.database :only (make-database add-tuples)]
        [clojure.contrib.datalog.util :only (*trace-datalog*)]))




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



(def wp-1 (build-work-plan rules (?- :works-for :employee '??name :boss ?x)))
(run-work-plan wp-1 db {'??name "Albert"})

(def wp-2 (build-work-plan rules (?- :employee-job :employee '??name :job ?x)))
(binding [*trace-datalog* true]
  (run-work-plan wp-2 db {'??name "Li"}))

(def wp-3 (build-work-plan rules (?- :bj :name '??name :boss ?x)))
(run-work-plan wp-3 db {'??name "Albert"})


;; End of file
