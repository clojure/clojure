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
                     (let [new-results (if (i-preds lit)
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
  (mapcat #(compute-delta-rules* % i-preds) rs))



;; End of file
