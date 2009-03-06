;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  literals.clj
;;
;;  A Clojure implementation of Datalog -- Literals
;;
;;  straszheimjeffrey (gmail)
;;  Created 25 Feburary 2009


(ns clojure.contrib.datalog.literals
  (:use clojure.contrib.datalog.util)
  (:use clojure.contrib.datalog.database)
  (:use [clojure.set :only (intersection)])
  (:use [clojure.contrib.set :only (subset?)])
  (:use [clojure.contrib.seq-utils :only (flatten)]))


;;; Type Definitions

(defstruct atomic-literal
  :predicate              ; The predicate name
  :term-bindings          ; A map of column names to bindings
  :literal-type)          ; ::literal or ::negated

(derive ::negated ::literal)

(defstruct conditional-literal
  :fun                    ; The fun to call
  :symbol                 ; The fun symbol (for display)
  :terms                  ; The formal arguments
  :literal-type)          ; ::conditional


;;; Basics


(defmulti literal-predicate
  "Return the predicate/relation this conditional operates over"
  :literal-type)

(defmulti literal-columns
  "Return the column names this applies to"
  :literal-type)

(defmulti literal-vars
  "Returns the logic vars used by this literal"
  :literal-type)

(defmulti positive-vars
  "Returns the logic vars used in a positive position"
  :literal-type)

(defmulti negative-vars
  "Returns the logic vars used in a negative position"
  :literal-type)

(defmethod literal-predicate ::literal
  [l]
  (:predicate l))

(defmethod literal-predicate ::conditional
  [l]
  nil)

(defmethod literal-columns ::literal
  [l]
  (-> l :term-bindings keys set))

(defmethod literal-columns ::conditional
  [l]
  nil)

(defmethod literal-vars ::literal
  [l]
  (set (filter is-var? (-> l :term-bindings vals))))

(defmethod literal-vars ::conditional
  [l]
  (set (filter is-var? (:terms l))))

(defmethod positive-vars ::literal
  [l]
  (literal-vars l))

(defmethod positive-vars ::negated
  [l]
  nil)

(defmethod positive-vars ::conditional
  [l]
  nil)

(defmethod negative-vars ::literal
  [l]
  nil)

(defmethod negative-vars ::negated
  [l]
  (literal-vars l))

(defmethod negative-vars ::conditional
  [l]
  (literal-vars l))

(defn negated?
  "Is this literal a negated literal?"
  [l]
  (= (:literal-type l) ::negated))

(defn positive?
  "Is this a positive literal?"
  [l]
  (= (:literal-type l) ::literal))


;;; Building Literals

(def negation-symbol 'not!)
(def conditional-symbol 'if)

(defmulti build-literal
  "(Returns an unevaluated expression (to be used in macros) of a
   literal."
  first)

(defn build-atom
  "Returns an unevaluated expression (to be used in a macro) of an
   atom."
  [f type]
  (let [p (first f)
        ts (map #(if (is-var? %) `(quote ~%) %) (next f))
        b (if (seq ts) (apply assoc {} ts) nil)]
    `(struct atomic-literal ~p ~b ~type)))

(defmethod build-literal :default
  [f]
  (build-atom f ::literal))

(defmethod build-literal negation-symbol
  [f]
  (build-atom (rest f) ::negated))

(defmethod build-literal conditional-symbol
  [f]
  (let [symbol (fnext f)
        terms (nnext f)
        fun `(fn [binds#] (apply ~symbol binds#))]
    `(struct conditional-literal
             ~fun
             '~symbol
             '~terms
             ::conditional)))


;;; Display

(defmulti display-literal
  "Converts a struct representing a literal to a normal list"
  :literal-type)

(defn- display
  [l]
  (conj (-> l :term-bindings list* flatten) (literal-predicate l)))

(defmethod display-literal ::literal
  [l]
  (display l))

(defmethod display-literal ::negated
  [l]
  (conj (display l) negation-symbol))

(defmethod display-literal ::conditional
  [l]
  (list* conditional-symbol (:symbol l) (:terms l)))


;;; Sip computation

(defmulti get-vs-from-cs
  "From a set of columns, return the vars"
  :literal-type)

(defmethod get-vs-from-cs ::literal
  [l bound]
  (set (filter is-var?
               (vals (select-keys (:term-bindings l)
                                  bound)))))

(defmethod get-vs-from-cs ::conditional
  [l bound]
  nil)


(defmulti get-cs-from-vs
  "From a set of vars, get the columns"
  :literal-type)

(defmethod get-cs-from-vs ::literal
  [l bound]
  (reduce conj
          #{}
          (remove nil? 
                  (map (fn [[k v]] (if (bound v) k nil))
                       (:term-bindings l)))))

(defmethod get-cs-from-vs ::conditional
  [l bound]
  nil)


(defmulti get-self-bound-cs
  "Get the columns that are bound withing the literal."
  :literal-type)

(defmethod get-self-bound-cs ::literal
  [l]
  (reduce conj
          #{}
          (remove nil?
                  (map (fn [[k v]] (if (not (is-var? v)) k nil))
                       (:term-bindings l)))))

(defmethod get-self-bound-cs ::conditional
  [l]
  nil)


(defmulti literal-appropriate?
  "When passed a set of bound vars, determines if this literal can be
   used during this point of a SIP computation."
  (fn [b l] (:literal-type l)))

(defmethod literal-appropriate? ::literal
  [bound l]
  (not (empty? (intersection (literal-vars l) bound))))

(defmethod literal-appropriate? ::negated
  [bound l]
  (subset? (literal-vars l) bound))

(defmethod literal-appropriate? ::conditional
  [bound l]
  (subset? (literal-vars l) bound))


(defmulti adorned-literal
  "When passed a set of bound columns, returns the adorned literal"
  (fn [l b] (:literal-type l)))

(defmethod adorned-literal ::literal
  [l bound]
  (let [pred (literal-predicate l)
        bnds (intersection (literal-columns l) bound)]
    (if (empty? bound)
      l
      (assoc l :predicate {:pred pred :bound bnds}))))

(defmethod adorned-literal ::conditional
  [l bound]
  l)


(defn get-adorned-bindings
  "Get the bindings from this adorned literal."
  [pred]
  (:bound pred))

(defn get-base-predicate
  "Get the base predicate from this predicate."
  [pred]
  (if (map? pred)
    (:pred pred)
    pred))


;;; Magic Stuff

(defn magic-literal
  "Create a magic version of this adorned predicate."
  [l]
  (assert (-> l :literal-type (isa? ::literal)))
  (let [pred (literal-predicate l)
        pred-map (if (map? pred) pred {:pred pred})
        bound (get-adorned-bindings pred)
        ntb (select-keys (:term-bindings l) bound)]
    (assoc l :predicate (assoc pred-map :magic true) :term-bindings ntb :literal-type ::literal)))

(defn literal-magic?
  "Is this literal magic?"
  [lit]
  (let [pred (literal-predicate lit)]
    (when (map? pred)
      (:magic pred))))
      
(defn build-seed-bindings
  "Given a seed literal, already adorned and in magic form, convert
   its bound constants to new variables."
  [s]
  (assert (-> s :literal-type (isa? ::literal)))
  (let [ntbs (map-values (fn [_] (gensym '?_gen_)) (:term-bindings s))]
    (assoc s :term-bindings ntbs)))


;;; Semi-naive support

(defn negated-literal
  "Given a literal l, return a negated version"
  [l]
  (assert (-> l :literal-type (= ::literal)))
  (conj l :literal-type ::negated))

;(defn delta-literal
;  "Given a literal l, return a delta version"
;  [l]
;  (let [pred (:predicate l)]
;    (if (vector? pred)
;      (assoc l :predicate (
    

        
;;; Database operations

(defn- build-partial-tuple
  [lit binds]
  (let [tbs (:term-bindings lit)
        each (fn [[key val :as pair]]
               (if (is-var? val)
                 (if-let [n (binds val)]
                   [key n]
                   nil)
                 pair))]
    (into {} (remove nil? (map each tbs)))))

(defn- project-onto-literal
  "Given a literal, and a materialized tuple, return a set of variable
   bindings."
  [lit tuple]
  (let [step (fn [binds [key val]]
               (if (and (is-var? val)
                        (contains? tuple key))
                 (assoc binds val (tuple key))
                 binds))]
    (reduce step {} (:term-bindings lit))))
  

(defn- join-literal*
  [db lit bs fun]
  (let [each (fn [binds]
               (let [pt (build-partial-tuple lit binds)]
                 (fun binds pt)))]
    (when (contains? db (literal-predicate lit))
      (apply concat (map each bs)))))

(defmulti join-literal
  "Given a database (db), a literal (lit) and a seq of bindings (bs),
   return a new seq of bindings by joining this literal."
  (fn [db lit bs] (:literal-type lit)))

(defmethod join-literal ::literal
  [db lit bs]
  (join-literal* db lit bs (fn [binds pt]
                             (map #(merge binds %)
                                  (map (partial project-onto-literal lit)
                                       (select db (literal-predicate lit) pt))))))

(defmethod join-literal ::negated
  [db lit bs]
  (join-literal* db lit bs (fn [binds pt]
                             (if (any-match? db (literal-predicate lit) pt)
                               nil
                               [binds]))))

(defmethod join-literal ::conditional
  [db lit bs]
  (let [each (fn [binds]
               (let [resolve (fn [term]
                               (if (is-var? term)
                                 (binds term)
                                 term))
                     args (map resolve (:terms lit))]
                 (if ((:fun lit) args)
                   binds
                   nil)))]
    (remove nil? (map each bs))))
                 
(defn project-literal
  "Project a stream of bindings onto a literal/relation. Returns a new
   db."
  ([db lit bs] (project-literal db lit bs is-var?))
  ([db lit bs var?]
     (assert (= (:literal-type lit) ::literal))
     (let [rel-name (literal-predicate lit)
           columns (-> lit :term-bindings keys)
           idxs (vec (get-adorned-bindings (literal-predicate lit)))
           db1 (ensure-relation db rel-name columns idxs)
           rel (get-relation db1 rel-name)
           step (fn [rel bindings]
                  (let [step (fn [t [k v]]
                               (if (var? v)
                                 (assoc t k (bindings v))
                                 (assoc t k v)))
                        tuple (reduce step {} (:term-bindings lit))]
                    (add-tuple rel tuple)))]
       (replace-relation db rel-name (reduce step rel bs)))))


;; End of file
