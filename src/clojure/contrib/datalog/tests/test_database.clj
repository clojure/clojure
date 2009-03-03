;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  test-database.clj
;;
;;  A Clojure implementation of Datalog -- Database
;;
;;  straszheimjeffrey (gmail)
;;  Created 12 Feburary 2009


(ns clojure.contrib.datalog.tests.test-database
  (:use clojure.contrib.test-is
	clojure.contrib.datalog.database))


(def test-db
     (make-database
      (relation :fred [:mary :sue])
      (index :fred :mary)
      (relation :sally [:jen :becky :joan])
      (index :sally :jen)
      (index :sally :becky)))

(deftest test-make-database
  (is (= test-db
         (datalog-database
          {:sally (datalog-relation
                   #{:jen :joan :becky}
                   #{}
                   {:becky {}
                    :jen {}})
           :fred (datalog-relation
                  #{:sue :mary}
                  #{}
                  {:mary {}})}))))


(deftest test-ensure-relation
  (is (contains? (ensure-relation test-db :bob [:sam :george] [:sam]) :bob))
  (is (contains? (ensure-relation test-db :fred [:mary :sue] [:mary]) :fred))
  (is (thrown? Exception (ensure-relation test-db :fred [:bob :joe] []))))

(deftest test-add-tuple
  (let [new-db (add-tuple test-db :fred {:mary 1 :sue 2})]
    (is (= (select new-db :fred {:mary 1}) [{:mary 1 :sue 2}])))
  (is (thrown? Exception (add-tuple test-db :fred {:mary 1}))))

(def test-db-1
     (add-tuples test-db
                 [:fred :mary 1 :sue 2]
                 [:fred :mary 2 :sue 3]
                 [:sally :jen 1 :becky 2 :joan 0]
                 [:sally :jen 1 :becky 4 :joan 3]
                 [:sally :jen 1 :becky 3 :joan 0]
                 [:sally :jen 1 :becky 2 :joan 3]
                 [:fred :mary 1 :sue 1]
                 [:fred :mary 3 :sue 1]))

(deftest test-add-tuples
  (is (= test-db-1
         (datalog-database
          {:sally (datalog-relation
                   #{:jen :joan :becky}
                   #{{:jen 1, :joan 0, :becky 3}
                     {:jen 1, :joan 0, :becky 2}
                     {:jen 1, :joan 3, :becky 2}
                     {:jen 1, :joan 3, :becky 4}}
                   {:becky {3
                            #{{:jen 1, :joan 0, :becky 3}}
                            4
                            #{{:jen 1, :joan 3, :becky 4}}
                            2
                            #{{:jen 1, :joan 0, :becky 2}
                              {:jen 1, :joan 3, :becky 2}}}
                    :jen {1
                          #{{:jen 1, :joan 0, :becky 3}
                            {:jen 1, :joan 0, :becky 2}
                            {:jen 1, :joan 3, :becky 2}
                            {:jen 1, :joan 3, :becky 4}}}})
           :fred (datalog-relation
                  #{:sue :mary}
                  #{{:sue 2, :mary 1}
                    {:sue 1, :mary 1}
                    {:sue 3, :mary 2}
                    {:sue 1, :mary 3}}
                  {:mary {3
                          #{{:sue 1, :mary 3}}
                          2
                          #{{:sue 3, :mary 2}}
                          1
                          #{{:sue 2, :mary 1}
                            {:sue 1, :mary 1}}}})}))))

(deftest test-remove-tuples
  (let [db (reduce #(apply remove-tuple %1 (first %2) (next %2))
                   test-db-1
                   [[:fred {:mary 1 :sue 1}]
                    [:fred {:mary 3 :sue 1}]
                    [:sally {:jen 1 :bekcy 2 :joan 0}]
                    [:sally {:jen 1 :bekcy 4 :joan 3}]])]
    (assert (= db
               (datalog-database
                {:sally (datalog-relation
                         #{:jen :joan :becky}
                         #{{:jen 1, :joan 0, :becky 3}
                           {:jen 1, :joan 0, :becky 2}
                           {:jen 1, :joan 3, :becky 2}
                           {:jen 1, :joan 3, :becky 4}}
                         {:becky
                          {3
                           #{{:jen 1, :joan 0, :becky 3}}
                           4
                           #{{:jen 1, :joan 3, :becky 4}}
                           2
                           #{{:jen 1, :joan 0, :becky 2}
                             {:jen 1, :joan 3, :becky 2}}}
                          :jen
                          {1
                           #{{:jen 1, :joan 0, :becky 3}
                             {:jen 1, :joan 0, :becky 2}
                             {:jen 1, :joan 3, :becky 2}
                             {:jen 1, :joan 3, :becky 4}}}})
                 :fred (datalog-relation
                        #{:sue :mary}
                        #{{:sue 2, :mary 1}
                          {:sue 3, :mary 2}}
                        {:mary
                         {3
                          #{}
                          2
                          #{{:sue 3, :mary 2}}
                          1
                          #{{:sue 2, :mary 1}}}})})))))



(deftest test-select
  (is (= (set (select test-db-1 :sally {:jen 1 :becky 2}))
         #{{:jen 1 :joan 0 :becky 2} {:jen 1 :joan 3 :becky 2}}))
  (is (= (set (select test-db-1 :fred {:sue 1})))
      #{{:mary 3 :sue 1} {:mary 1 :sue 1}})
  (is (empty? (select test-db-1 :sally {:joan 5 :jen 1}))))
         
(deftest test-any-match?
  (is (any-match? test-db-1 :fred {:mary 3}))
  (is (any-match? test-db-1 :sally {:jen 1 :becky 2 :joan 3}))
  (is (not (any-match? test-db-1 :sally {:jen 5})))
  (is (not (any-match? test-db-1 :fred {:mary 1 :sue 5}))))


(comment
  (run-tests)
)

;; End of file

