;;; auto_agent.clj: agents that update automatically from a formula

;; by Stuart Sierra, http://stuartsierra.com/
;; January 29, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.



;; This is superficially similar to Ken Tilton's "Cells" library for
;; Common Lisp.  But Cells is single-threaded and synchronous.  This
;; version is built on Clojure agents, so it is multi-threaded and
;; asynchronous.
;;
;; An auto-agent is an agent whose value is the result of an
;; expression.  That expression can include other mutable Clojure
;; types -- agents, atoms, and refs -- dereferenced with "deref" or
;; "@".  Whenever one of those derefernced things changes, the
;; auto-agent will be automatically updated to reflect the new value.



(ns 
  #^{:author "Stuart Sierra",
     :doc "This is superficially similar to Ken Tilton's \"Cells\" library for
Common Lisp.  But Cells is single-threaded and synchronous.  This
version is built on Clojure agents, so it is multi-threaded and
asynchronous.

An auto-agent is an agent whose value is the result of an
expression.  That expression can include other mutable Clojure
types -- agents, atoms, and refs -- dereferenced with \"deref\" or
\"@\".  Whenever one of those derefernced things changes, the
auto-agent will be automatically updated to reflect the new value.
"}  
  clojure.contrib.auto-agent
  (:use [clojure.contrib.walk :only (macroexpand-all)]
        [clojure.contrib.test-is :only (deftest- is)]))

(defmacro #^{:private true} find-derefs
  "Expands the expression and returns the set of all agents/atoms/refs
  dereferenced within it."
  [expr]
  (let [expr (macroexpand-all expr)]
    (if (coll? expr)
      (set (map second
                (filter #(and (list? %)
                              (symbol? (first %))
                              (= (resolve (first %)) #'clojure.core/deref))
                        (tree-seq coll? seq expr))))
      #{})))

(defmacro auto-agent 
  "Creates an agent whose value is the result of evaluating expr.
  Whenever one of the agents/atoms/refs dererenced within expr
  changes, expr is reevaluated and this agent's state is set to its
  new value."
  [expr]
  `(let [value-fn# (fn [old# a#] ~expr)
         cell# (agent (value-fn# nil nil))]
     (doseq [d# (find-derefs ~expr)]
       (add-watcher d# :send cell# value-fn#))
     cell#))



;;; TESTS

;; Run these tests with
;; (clojure.contrib.test-is/run-tests 'clojure.contrib.auto-agent)

;; Bind clojure.contrib.test-is/*load-tests* to false to omit these
;; tests from production code.


(deftest- auto-agents-can-depend-on-agents
  (let [c1 (agent 1)
        c2 (auto-agent (inc @c1))]
    (is (= 1 @c1))
    (is (= 2 @c2))
    (send c1 inc)
    (await c1 c2)
    (is (= 2 @c1))
    (is (= 3 @c2))))

(deftest- auto-agents-can-depend-on-refs
  (let [c1 (ref 1)
        c2 (auto-agent (inc @c1))]
    (is (= 1 @c1))
    (is (= 2 @c2))
    (dosync (alter c1 inc))
    (await c2)
    (is (= 2 @c1))
    (is (= 3 @c2))))

(deftest- auto-agents-can-depend-on-atoms
  (let [c1 (atom 1)
        c2 (auto-agent (inc @c1))]
    (is (= 1 @c1))
    (is (= 2 @c2))
    (swap! c1 inc)
    (await c2)
    (is (= 2 @c1))
    (is (= 3 @c2))))

(deftest- very-long-dependency-chains-work-with-agents
  (let [head (agent 1)
        chain (take 10000 (iterate (fn [p] (auto-agent (inc @p))) head))
        tail (auto-agent (inc @(last chain)))]
    (is (= 1 @head))
    (is (= 10001 @tail))
    (send head inc)
    ;; We can't just do (apply await head tail chain) because not all
    ;; the actions have been sent by the time we call await.  Instead,
    ;; we have to await each cell in order.  In general, you should
    ;; not rely on agent cell updates happening at any specific time.
    ;; This test is only to demonstrate that very long chains of agent
    ;; cells can be made without causing a stack overflow, which is
    ;; not true of ref/atom cells.
    (await head)
    (doseq [c chain] (await c))
    (await tail)
    (is (= 2 @head))
    (is (= 3 @(second chain)))
    (is (= 10001 @(last chain)))
    (is (= 10002 @tail))))

(deftest- agent-expressions-can-be-vectors
  (let [c1 (agent 1)
        c2 (agent 2)
        c3 (auto-agent [@c1 @c2])]
    (is (= [1 2] @c3))
    (send c1 inc)
    (send c2 inc)
    (await c1 c2)
    (await c3)
    (is (= [2 3] @c3))))

(deftest- agent-expressions-can-be-maps
  (let [c1 (agent 1)
        c2 (agent 2)
        c3 (auto-agent {:c1 @c1, :c2 @c2})]
    (is (= {:c1 1, :c2 2} @c3))
    (send c1 inc)
    (send c2 inc)
    (await c1 c2)
    (await c3)
    (is (= {:c1 2, :c2 3} @c3))))
