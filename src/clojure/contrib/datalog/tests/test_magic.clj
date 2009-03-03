;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  test-magic.clj
;;
;;  A Clojure implementation of Datalog -- Magic Tests
;;
;;  straszheimjeffrey (gmail)
;;  Created 18 Feburary 2009

(ns clojure.contrib.datalog.tests.test-magic
  (:use clojure.contrib.test-is)
  (:use clojure.contrib.datalog.magic
        clojure.contrib.datalog.rules))



(def rs (rules-set
             (<- (:p :x ?x :y ?y) (:e :x ?x :y ?y))
             (<- (:p :x ?x :y ?y) (:e :x ?x :y ?z) (:p :x ?z :y ?y))
             (<- (:e :x ?x :y ?y) (:b :x ?x :y ?y))
             (<- (:e :x ?y :y ?y) (:c :x ?x :y ?y))))

(def q (adorn-query (?- :p :x 1 :y ?y)))

(def ars (adorn-rules-set rs q))

(deftest test-adorn-rules-set
  (is (= ars
         (rules-set
          (<- ([:p #{:x}] :y ?y :x ?x) ([:e #{:x}] :y ?y :x ?x))
          (<- ([:p #{:x}] :y ?y :x ?x) ([:e #{:x}] :y ?z :x ?x) ([:p #{:x}] :y ?y :x ?z))
          (<- ([:e #{:x}] :y ?y :x ?y) (:c :y ?y :x ?x))
          (<- ([:e #{:x}] :y ?y :x ?x) (:b :y ?y :x ?x))))))


(def m (magic-transform ars))

(deftest test-magic-transform
  (is (= m
         (rules-set
          (<- ([:e #{:x}] :y ?y :x ?y) ([:e :magic #{:x}] :x ?y) (:c :y ?y :x ?x))
          (<- ([:e #{:x}] :y ?y :x ?x) ([:e :magic #{:x}] :x ?x) (:b :y ?y :x ?x))
          (<- ([:p :magic #{:x}] :x ?z) ([:p :magic #{:x}] :x ?x) ([:e #{:x}] :y ?z :x ?x))
          (<- ([:p #{:x}] :y ?y :x ?x) ([:p :magic #{:x}] :x ?x) ([:e #{:x}] :y ?z :x ?x) ([:p #{:x}] :y ?y :x ?z))
          (<- ([:e :magic #{:x}] :x ?x) ([:p :magic #{:x}] :x ?x))
          (<- ([:p #{:x}] :y ?y :x ?x) ([:p :magic #{:x}] :x ?x) ([:e #{:x}] :y ?y :x ?x))))))




(comment
  (run-tests)
)

;; End of file

