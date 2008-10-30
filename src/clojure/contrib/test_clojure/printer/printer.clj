;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  clojure.contrib.test-clojure.printer
;;
;;  scgilardi (gmail)
;;  Created 29 October 2008

(ns clojure.contrib.test-clojure.printer
  (:use clojure.contrib.test-is))

(deftest print-length-empty-seq
  (let [coll () val "()"]
    (is (= val (binding [*print-length* 0] (print-str coll))))
    (is (= val (binding [*print-length* 1] (print-str coll))))))

(deftest print-length-seq
  (let [coll (range 5)
        length-val '((0 "(...)")
                     (1 "(0 ...)")
                     (2 "(0 1 ...)")
                     (3 "(0 1 2 ...)")
                     (4 "(0 1 2 3 ...)")
                     (5 "(0 1 2 3 4)"))]
    (doseq [length val] length-val
      (binding [*print-length* length]
        (is (= val (print-str coll)))))))

(deftest print-length-empty-vec
  (let [coll [] val "[]"]
    (is (= val (binding [*print-length* 0] (print-str coll))))
    (is (= val (binding [*print-length* 1] (print-str coll))))))

(deftest print-length-vec
  (let [coll [0 1 2 3 4]
        length-val '((0 "[...]")
                     (1 "[0 ...]")
                     (2 "[0 1 ...]")
                     (3 "[0 1 2 ...]")
                     (4 "[0 1 2 3 ...]")
                     (5 "[0 1 2 3 4]"))]
    (doseq [length val] length-val
      (binding [*print-length* length]
        (is (= val (print-str coll)))))))

(deftest print-level-seq
  (let [coll '(0 (1 (2 (3 (4)))))
        level-val '((0 "#")
                    (1 "(0 #)")
                    (2 "(0 (1 #))")
                    (3 "(0 (1 (2 #)))")
                    (4 "(0 (1 (2 (3 #))))")
                    (5 "(0 (1 (2 (3 (4)))))"))]
    (doseq [level val] level-val
      (binding [*print-level* level]
        (is (= val (print-str coll)))))))

(deftest print-level-length-coll
  (let [coll '(if (member x y) (+ (first x) 3) (foo (a b c d "Baz")))
        level-length-val
        '((0 1 "#")
          (1 1 "(if ...)")
          (1 2 "(if # ...)")
          (1 3 "(if # # ...)")
          (1 4 "(if # # #)")
          (2 1 "(if ...)")
          (2 2 "(if (member x ...) ...)")
          (2 3 "(if (member x y) (+ # 3) ...)")
          (3 2 "(if (member x ...) ...)")
          (3 3 "(if (member x y) (+ (first x) 3) ...)")
          (3 4 "(if (member x y) (+ (first x) 3) (foo (a b c d ...)))")
          (3 5 "(if (member x y) (+ (first x) 3) (foo (a b c d Baz)))"))]
    (doseq [level length val] level-length-val
      (binding [*print-level* level
                *print-length* length]
        (is (= val (print-str coll)))))))
