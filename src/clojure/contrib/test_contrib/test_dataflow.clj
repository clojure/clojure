;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  test-dataflow
;;
;;  A Library to Support a Dataflow Model of State - Tests
;;
;;  straszheimjeffrey (gmail)
;;  Created 11 March 2009


(ns clojure.contrib.test-contrib.test-dataflow
  (:use clojure.contrib.test-is)
  (:use clojure.contrib.dataflow))

(def df-1
     (build-dataflow
      [(cell :source base 0)
       (cell :source items ())
       (cell product (* ?base (apply + ?items)))
       (cell :validator (when (number? ?-product)
                          (assert (>= ?product ?-product))))]))

(deftest test-df-1
  (is (= (get-value df-1 'product) 0))
  (is (do (update-values df-1 {'items [4 5]})
          (= (get-value df-1 'product) 0)))
  (is (do (update-values df-1 {'base 2})
          (= (get-value df-1 'product) 18)))
  (is (thrown? Exception (update-values df-1 {'base 0})))
  (is (= (get-value df-1 'product) 18)))

(def df-2
     (build-dataflow
      [(cell :source strength 10)
       (cell :source agility 10)
       (cell :source magic 10)

       (cell total-cost (apply + ?*cost))

       (cell cost (- ?strength 10))
       (cell cost (- ?agility 10))
       (cell cost (- ?magic 10))

       (cell combat (+ ?strength ?agility ?combat-mod))
       (cell speed (+ ?agility (/ ?strength 10.0) ?speed-mod))
       (cell casting (+ ?agility ?magic ?magic-mod))

       (cell combat-mod (apply + ?*combat-mods))
       (cell speed-mod (apply + ?*speed-mods))
       (cell magic-mod (apply + ?*magic-mods))]))

(def magic-skill
     [(cell cost 5)
      (cell speed-mods 1)
      (cell magic-mods 2)])

(defn gv [n] (get-value df-2 n))

(deftest test-df-2
  (is (and (= (gv 'total-cost) 0)
           (= (gv 'strength) 10)
           (= (gv 'casting) 20)))
  (is (do (update-values df-2 {'magic 12})
          (and (= (gv 'total-cost) 2)
               (= (gv 'casting) 22))))
  (is (do (add-cells df-2 magic-skill)
          (and (= (gv 'total-cost) 7)
               (= (gv 'casting) 24))))
  (is (do (remove-cells df-2 magic-skill)
          (and (= (gv 'total-cost) 2)
               (= (gv 'casting) 22)))))
               

(comment
  (run-tests)

  (use :reload 'clojure.contrib.dataflow)
  (use 'clojure.contrib.stacktrace) (e)
  (use 'clojure.contrib.trace)

)


;; End of file
