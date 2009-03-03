;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  rules.clj
;;
;;  A Clojure implementation of Datalog -- Rules Engine
;;
;;  straszheimjeffrey (gmail)
;;  Created 2 Feburary 2009


(ns clojure.contrib.datalog.rules
  (use clojure.contrib.datalog.util)
  (use clojure.contrib.datalog.literals
       clojure.contrib.datalog.database)
  (use [clojure.set :only (union intersection difference)])
  (use [clojure.contrib.set :only (subset?)])
  (use [clojure.contrib.except :only (throwf)]))


(defstruct datalog-rule
  :head
  :body)

(defn display-rule
  "Return the rule in a readable format."
  [rule]
  (list* '<-
         (-> rule :head display-literal)
         (map display-literal (:body rule))))

(defn display-query
  "Return a query in a readable format."
  [query]
  (list* '?- (display-literal query)))


;;; Check rule safety

(defn is-safe?
  "Is the rule safe according to the datalog protocol?"
  [rule]
  (let [hv (literal-vars (:head rule))
        bpv (apply union (map positive-vars (:body rule)))
        bnv (apply union (map negative-vars (:body rule)))
        ehv (difference hv bpv)
        env (difference bnv bpv)]
    (when-not (empty? ehv)
      (throwf "Head vars %s not bound in body in rule %s" ehv rule))
    (when-not (empty? env)
      (throwf "Body vars %s not bound in negative positions in rule %s" env rule))
    rule))


;;; Rule creation and printing

(defn build-rule
  [hd bd]
  (with-meta (struct datalog-rule hd bd) {:type ::datalog-rule}))

(defmacro <-
  "Build a datalog rule.  Like this:

   (<- (:head :x ?x :y ?y) (:body-1 :x ?x :y ?y) (:body-2 :z ?z) (not! :body-3 :x ?x) (if > ?y ?z))"
  [hd & body]
  (let [head (build-atom hd :clojure.contrib.datalog.literals/literal)
        body (map build-literal body)]
    `(is-safe? (build-rule ~head [~@body]))))

(defmethod print-method ::datalog-rule
  [rule #^Writer writer]
  (print-method (display-rule rule) writer))

(defn return-rule-data
  "Returns an untypted rule that will be fully printed"
  [rule]
  (with-meta rule {}))

(defmacro ?-
  "Define a datalog query"
  [& q]
  (let [qq (build-atom q :clojure.contrib.datalog.literals/literal)]
  `(with-meta ~qq {:type ::datalog-query})))

(defmethod print-method ::datalog-query
  [query #^Writer writer]
  (print-method (display-query query) writer))



;;; SIP

(defn compute-sip
  "Given a set of bound column names, return an adorned sip for this
   rule.  A set of intensional predicates should be provided to
   determine what should be adorned."
  [bindings i-preds rule]
  (let [next-lit (fn [bv body]
                   (or (first (drop-while
                               #(not (literal-appropriate? bv %))
                               body))
                       (first (drop-while (complement positive?) body))))
        adorn (fn [lit bvs]
                (if (i-preds (literal-predicate lit))
                  (let [bnds (union (get-cs-from-vs lit bvs)
                                    (get-self-bound-cs lit))]
                    (adorned-literal lit bnds))
                  lit))
        new-h (adorned-literal (:head rule) bindings)]
    (loop [bound-vars (get-vs-from-cs (:head rule) bindings)
           body (:body rule)
           sip []]
      (if-let [next (next-lit bound-vars body)]
        (recur (union bound-vars (literal-vars next))
               (remove #(= % next) body)
               (conj sip (adorn next bound-vars)))
        (build-rule new-h (concat sip body))))))


;;; Rule sets

(defn make-rules-set
  "Given an existing set of rules, make it a 'rules-set' for
   printing."
  [rs]
  (with-meta rs {:type ::datalog-rules-set}))
    
(def empty-rules-set (make-rules-set #{}))

(defn rules-set
  "Given a collection of rules return a rules set"
  [& rules]
  (reduce conj empty-rules-set rules))
  
(defmethod print-method ::datalog-rules-set
  [rules #^Writer writer]
  (binding [*out* writer]
    (do
      (print "(rules-set")
      (doseq [rule rules]
        (println)
        (print "   ")
        (print rule))
      (println ")"))))

(defn predicate-map
  "Given a rules-set, return a map of rules keyed by their predicates.
   Each value will be a set of rules."
  [rs]
  (let [add-rule (fn [m r]
                   (let [pred (-> r :head literal-predicate)
                         os (get m pred #{})]
                     (assoc m pred (conj os r))))]
    (reduce add-rule {} rs)))

(defn all-predicates
  "Given a rules-set, return all defined predicates"
  [rs]
  (set (map literal-predicate (map :head rs))))

(defn non-base-rules
  "Return a collection of rules that depend, somehow, on other rules"
  [rs]
  (let [pred (all-predicates rs)
        non-base (fn [r]
                   (if (some #(pred %)
                             (map literal-predicate (:body r)))
                     r
                     nil))]
    (remove nil? (map non-base rs))))


;;; Database operations

(def empty-bindings [{}])

(defn apply-rule
  "Apply the rule against db-1, adding the results to the appropriate
   relation in db-2.  The relation will be created if needed."
  ([db rule] (apply-rule db db rule))
  ([db-1 db-2 rule]
     (trace-datalog (println)
                    (println)
                    (println "--------------- Begin Rule ---------------")
                    (println rule))
     (let [head (:head rule)
           body (:body rule)
           step (fn [bs lit]
                  (trace-datalog (println bs)
                                 (println lit))
                  (join-literal db-1 lit bs))
           bs (reduce step empty-bindings body)]
       (do (trace-datalog (println bs))
           (project-literal db-2 head bs)))))


;; End of file