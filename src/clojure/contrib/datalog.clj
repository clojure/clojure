;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  datalog.clj
;;
;;  A Clojure implementation of Datalog
;;
;;  straszheimjeffrey (gmail)
;;  Created 2 March 2009


;;; Please see the example.clj file in the datalog folder


(ns 
  #^{:author "Jeffrey Straszheim",
     :doc "A Clojure implementation of Datalog"
     :see-also ["DatalogOverview"]} 
  clojure.contrib.datalog
  (:use clojure.contrib.datalog.rules
        clojure.contrib.datalog.softstrat
        clojure.contrib.datalog.database)
  (:use [clojure.set :only (intersection)]
        [clojure.contrib.except :only (throwf)]))

(defstruct work-plan
  :work-plan        ; The underlying structure
  :rules            ; The original rules
  :query            ; The original query
  :work-plan-type)  ; The type of plan

(defn- validate-work-plan
  "Ensure any top level semantics are not violated"
  [work-plan database]
  (let [common-relations (-> work-plan :rules (intersection (-> database keys set)))]
    (when (-> common-relations
              empty?
              not)
      (throwf "The rules and database define the same relation(s): %s" common-relations))))
  ; More will follow

(defn build-work-plan
  "Given a list of rules and a query, build a work plan that can be
   used to execute the query."
  [rules query]
  (struct-map work-plan
    :work-plan (build-soft-strat-work-plan rules query)
    :rules rules
    :query query
    :work-plan-type ::soft-stratified))

(defn run-work-plan
  "Given a work plan, a database, and some query bindings, run the
   work plan and return the results."
  [work-plan database query-bindings]
  (validate-work-plan work-plan database)
  (evaluate-soft-work-set (:work-plan work-plan) database query-bindings))


;; End of file
