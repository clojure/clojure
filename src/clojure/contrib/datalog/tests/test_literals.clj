;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  test-literals.clj
;;
;;  A Clojure implementation of Datalog -- Literals tests
;;
;;  straszheimjeffrey (gmail)
;;  Created 25 Feburary 2009


(ns clojure.contrib.datalog.tests.test-literals
  (:use clojure.contrib.test-is)
  (:use clojure.contrib.datalog.literals
        clojure.contrib.datalog.database))


(def pl (eval (build-literal '(:fred :x ?x :y ?y :z 3))))
(def nl (eval (build-literal '(not! :fred :x ?x :y ?y :z 3))))
(def cl (eval (build-literal '(if > ?x 3))))

(def bl (eval (build-literal '(:fred))))

(def bns {:x '?x :y '?y :z 3})

(deftest test-build-literal
  (is (= (:predicate pl) :fred))
  (is (= (:term-bindings pl) bns))
  (is (= (:predicate nl) :fred))
  (is (= (:term-bindings nl) bns))
  (is (= (:symbol cl) '>))
  (is (= (:terms cl) '(?x 3)))
  (is ((:fun cl) [4 3]))
  (is (not ((:fun cl) [2 4])))
  (is (= (:predicate bl) :fred)))

(deftest test-literal-predicate
  (is (= (literal-predicate pl) :fred))
  (is (= (literal-predicate nl) :fred))
  (is (nil? (literal-predicate cl)))
  (is (= (literal-predicate bl) :fred)))

(deftest test-literal-columns
  (is (= (literal-columns pl) #{:x :y :z}))
  (is (= (literal-columns nl) #{:x :y :z}))
  (is (nil? (literal-columns cl)))
  (is (empty? (literal-columns bl))))

(deftest test-literal-vars
  (is (= (literal-vars pl) #{'?x '?y}))
  (is (= (literal-vars nl) #{'?x '?y}))
  (is (= (literal-vars cl) #{'?x}))
  (is (empty? (literal-vars bl))))

(deftest test-positive-vars
  (is (= (positive-vars pl) (literal-vars pl)))
  (is (nil? (positive-vars nl)))
  (is (nil? (positive-vars cl)))
  (is (empty? (positive-vars bl))))

(deftest test-negative-vars
  (is (nil? (negative-vars pl)))
  (is (= (negative-vars nl) (literal-vars nl)))
  (is (= (negative-vars cl) (literal-vars cl)))
  (is (empty? (negative-vars bl))))

(deftest test-negated?
  (is (not (negated? pl)))
  (is (negated? nl))
  (is (not (negated? cl))))

(deftest test-vs-from-cs
  (is (= (get-vs-from-cs pl #{:x}) #{'?x}))
  (is (empty? (get-vs-from-cs pl #{:z})))
  (is (= (get-vs-from-cs pl #{:x :r}) #{'?x}))
  (is (empty? (get-vs-from-cs pl #{}))))

(deftest test-cs-from-vs
  (is (= (get-cs-from-vs pl #{'?x}) #{:x}))
  (is (= (get-cs-from-vs pl #{'?x '?r}) #{:x}))
  (is (empty? (get-cs-from-vs pl #{}))))

(deftest test-literal-appropriate?
  (is (not (literal-appropriate? #{} pl)))
  (is (literal-appropriate? #{'?x} pl))
  (is (not (literal-appropriate? #{'?x} nl)))
  (is (literal-appropriate? #{'?x '?y} nl))
  (is (not (literal-appropriate? #{'?z} cl)))
  (is (literal-appropriate? #{'?x} cl)))

(deftest test-adorned-literal
  (is (= (literal-predicate (adorned-literal pl #{:x}))
         [:fred #{:x}]))
  (is (= (literal-predicate (adorned-literal nl #{:x :y :q}))
         [:fred #{:x :y}]))
  (is (= (:term-bindings (adorned-literal nl #{:x}))
         {:x '?x :y '?y :z 3}))
  (is (= (adorned-literal cl #{})
         cl)))

(deftest test-get-adorned-bindings
  (is (= (get-adorned-bindings (literal-predicate (adorned-literal pl #{:x})))
         #{:x}))
  (is (= (get-adorned-bindings (literal-predicate pl))
         nil)))

(deftest test-get-base-predicate
  (is (= (get-base-predicate (literal-predicate (adorned-literal pl #{:x})))
         :fred))
  (is (= (get-base-predicate (literal-predicate pl))
         :fred)))

(deftest test-magic-literal
  (is (= (magic-literal pl)
         {:predicate [:fred :magic nil], :term-bindings {}, :literal-type :clojure.contrib.datalog.literals/literal}))
  (is (= (magic-literal (adorned-literal pl #{:x}))
         {:predicate [:fred :magic #{:x}],
          :term-bindings {:x '?x},
          :literal-type :clojure.contrib.datalog.literals/literal})))


(def db1 (make-database
           (relation :fred [:x :y])
           (index :fred :x)
           (relation :sally [:x])))

(def db2 (add-tuples db1
             [:fred :x 1 :y :mary]
             [:fred :x 1 :y :becky]
             [:fred :x 3 :y :sally]
             [:fred :x 4 :y :joe]
             [:sally :x 1]
             [:sally :x 2]))

(def lit1 (eval (build-literal '(:fred :x ?x :y ?y))))
(def lit2 (eval (build-literal '(not! :fred :x ?x))))
(def lit3 (eval (build-literal '(if > ?x ?y))))
(def lit4 (adorned-literal (eval (build-literal '(:joan :x ?x :y ?y))) #{:x}))

(deftest test-join-literal
  (is (= (set (join-literal db2 lit1 [{'?x 1} {'?x 2} {'?x 3}]))
         #{{'?x 1, '?y :mary} {'?x 1, '?y :becky} {'?x 3, '?y :sally}}))
  (is (= (join-literal db2 lit2 [{'?x 1} {'?x 2} {'?x 3}])
         [{'?x 2}]))
  (is (= (join-literal db2 lit3 [{'?x 1 '?y 2} {'?x 3 '?y 1}])
         [{'?x 3 '?y 1}])))
         
(deftest test-project-literal
  (is (= ((project-literal db2 lit4 [{'?x 1 '?y 3}{'?x 4 '?y 2}]) [:joan #{:x}])
         (datalog-relation
          ;; Schema
          #{:y :x}

          ;; Data
          #{
            {:x 1, :y 3}
            {:x 4, :y 2}
            }
          
          ;; Indexes
          {
           :x
           {
            4
            #{{:x 4, :y 2}}
            1
            #{{:x 1, :y 3}}
            }
           }))))



(comment
  (run-tests)
)

;; End of file
