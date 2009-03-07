;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  seminaive.clj
;;
;;  A Clojure implementation of Datalog -- Semi Naive Evaluation
;;
;;  straszheimjeffrey (gmail)
;;  Created 5 March 2009


(ns clojure.contrib.datalog.seminaive
  (:use clojure.contrib.datalog.util
        clojure.contrib.datalog.database
        clojure.contrib.datalog.literals
        clojure.contrib.datalog.rules
        clojure.contrib.datalog.magic)
  (:use [clojure.set :only (union intersection difference)])
  (:use [clojure.contrib.seq-utils :only (indexed)])
  (:require [clojure.contrib.graph :as graph]))


;;;;  WORK IN PROGRESS !!!


;;; Delta Rules

(defn- compute-delta-rules*
  "Perform the delta computation of a rule, where i-rules is the set
   of intensional predicates."
  [r i-preds]
  (let [head (:head r)
        head-negated (negated-literal head)
        delta-head (delta-literal head)
        body (:body r)
        build-body (fn [left lit right]
                     (assoc r :head delta-head
                              :body (concat left
                                            [(delta-literal lit)]
                                            right
                                            [head-negated])))
        new-rules (loop [lit (first body)
                        left []
                        right (next body)
                        results []]
                   (if (nil? lit)
                     results
                     (let [new-results (if (-> lit literal-predicate i-preds)
                                         (conj results (build-body left lit right))
                                         results)]
                       (recur (first right)
                              (conj left lit)
                              (next right)
                              new-results))))]
    (if (empty? new-rules)
      [(assoc r :head delta-head :body (conj (vec body) head-negated))]
      new-rules)))

(defn- compute-delta-rules
  "Compute the delta rules of a rules set, where i-rules is the set of
   intensional predicates."
  [rs i-preds]
  (-> (mapcat #(compute-delta-rules* % i-preds) rs) set make-rules-set))

(defn- compute-basic-delta-rule
  "Given a rule, return its basic delta rule form"
  [r]
  (let [head (:head r)
        delta-head (delta-literal head)
        cols (literal-columns head)
        new-bindings (into {} (for [col cols]
                                [col (gensym "?var_")]))]
    (assoc r :head (assoc head :term-bindings new-bindings)
             :body [(assoc delta-head :term-bindings new-bindings)])))

(defn- compute-basic-delta-rules
  "Given a rules set, return the basic delta rules set"
  [rs]
  (-> (map compute-basic-delta-rule rs) set make-rules-set))


;;; Work Plan

(defstruct semi-naive-work-plan
  :delta-rules
  :basic-rules
  :query)

(defn make-semi-naive-work-plan
 [rs q]
 (let [adorned-q (adorn-query q)
       adorned (adorn-rules-set rs adorned-q)
       magic (conj (magic-transform adorned)
                   (seed-rule adorned-q))
       i-preds (all-predicates magic)
       delta (compute-delta-rules magic i-preds)
       basic (compute-basic-delta-rules magic)]
   (struct-map semi-naive-work-plan
     :delta-rules delta
     :basic-rules basic
     :query adorned-q)))
      

;;; Eval

(defn- semi-naive-operator
  [deltas basics [delta-db db]]
  (trace-datalog (println)
                 (println)
                 (println "=============== Begin iteration ==============="))
  (let [new-deltas (apply-rules-set (database-merge [db delta-db]) deltas)
        new-db (apply-rules-set (database-merge [db new-deltas]) basics)]
    [new-deltas new-db]))
  
(defn run-semi-naive-work-plan
  ([wp db] (run-semi-naive-work-plan wp db {}))
  ([wp db binds]
     (let [query (:query wp)
           deltas (:delta-rules wp)
           basics (:basic-rules wp)
           seed (seed-predicate-for-insertion query)
           seeded-db (project-literal db seed [binds] is-query-var?)
           state [empty-database seeded-db]
           fun (partial semi-naive-operator deltas basics)
           equal (fn [[delta-1 db-1] [delta-2 db-2]]
                   (and (= (database-counts db-1) (database-counts db-2))
                        (= (database-counts delta-1) (database-counts delta-2))))
           [_ new-db] (graph/fixed-point state fun nil equal)
           pt (build-partial-tuple query binds)]
       (select new-db (literal-predicate query) pt))))



(comment

(def db-base
     (make-database
      (relation :employee [:id :name :position])
      (index :employee :name)

      (relation :boss [:employee-id :boss-id])
      (index :boss :employee-id)

      (relation :can-do-job [:position :job])
      (index :can-do-job :position)

      (relation :job-replacement [:job :can-be-done-by])
      ;(index :job-replacement :can-be-done-by)

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
                                                 (not! :job-exceptions :id ?id :job ?y))))



(def wp-1 (make-semi-naive-work-plan rules (?- :works-for :employee '??name :boss ?x)))

(binding [*trace-datalog* true]
  (run-semi-naive-work-plan wp-1 db {'??name "Albert"}))

(time (dotimes [_ 100]
        (run-semi-naive-work-plan wp-1 db {'??name "Albert"})))

(def wp-2 (build-work-plan rules (?- :employee-job :employee '??name :job ?x)))
(binding [*trace-datalog* true]
  (run-work-plan wp-2 db {'??name "Li"}))

(def wp-3 (build-work-plan rules (?- :bj :name '??name :boss ?x)))
(run-work-plan wp-3 db {'??name "Albert"})


  (use 'clojure.contrib.stacktrace) (e)
  (use :reload 'clojure.contrib.datalog.literals 'clojure.contrib.datalog.seminaive)
)


;; End of file
