;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Stephen C. Gilardi

;;  clojure.test-clojure.printer
;;
;;  scgilardi (gmail)
;;  Created 29 October 2008

(ns clojure.test-clojure.printer
  (:use clojure.test
        [clojure.test-helper :only [platform-newlines]])
  (:require [clojure.pprint :refer [pprint]]))

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
    (doseq [[length val] length-val]
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
    (doseq [[length val] length-val]
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
    (doseq [[level val] level-val]
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
    (doseq [[level length val] level-length-val]
      (binding [*print-level* level
                *print-length* length]
        (is (= val (print-str coll)))))))

(deftest print-dup-expected
  (are [x s] (= s (binding [*print-dup* true] (print-str x)))
       1 "1"
       1.0 "1.0"
       1N "1N"
       (java.math.BigInteger. "1") "#=(java.math.BigInteger. \"1\")"
       1M "1M"
       "hi" "\"hi\""))

(deftest print-dup-readable
  (are [form] (let [x form]
                (= x (read-string (binding [*print-dup* true] (print-str x)))))
       1
       1.0
       1N
       1M
       "hi"))

(def ^{:foo :anything} var-with-meta 42)
(def ^{:type :anything} var-with-type 666)

(deftest print-var
  (are [x s] (= s (pr-str x))
       #'pr-str  "#'clojure.core/pr-str"
       #'var-with-meta "#'clojure.test-clojure.printer/var-with-meta"
       #'var-with-type "#'clojure.test-clojure.printer/var-with-type"))

(deftest print-meta
  (are [x s] (binding [*print-meta* true] 
               (let [pstr (pr-str x)]
                 (and (.endsWith pstr s)
                      (.startsWith pstr "^")
                      (.contains pstr (pr-str (meta x))))))
       #'pr-str  "#'clojure.core/pr-str"
       #'var-with-meta "#'clojure.test-clojure.printer/var-with-meta"
       #'var-with-type "#'clojure.test-clojure.printer/var-with-type"))

(deftest print-throwable
  (binding [*data-readers* {'error identity}]
    (are [e] (= (-> e Throwable->map)
                (-> e pr-str read-string))
         (Exception. "heyo")
         (Throwable. "I can a throwable"
                     (Exception. "chain 1"
                                 (Exception. "chan 2")))
         (ex-info "an ex-info" {:with "its" :data 29})
         (Exception. "outer"
                     (ex-info "an ex-info" {:with "data"}
                              (Error. "less outer"
                                      (ex-info "the root"
                                               {:with "even" :more 'data})))))))

(deftest print-ns-maps
  (are [m s-on pp-on s-off]
    (and (= s-on (binding [*print-namespace-maps* true] (pr-str m)))
      (= (platform-newlines pp-on) (binding [*print-namespace-maps* true] (with-out-str (pprint m))))
      (= s-off (binding [*print-namespace-maps* false] (pr-str m))))
    {} "{}" "{}\n" "{}"
    {:a 1, :b 2} "{:a 1, :b 2}" "{:a 1, :b 2}\n" "{:a 1, :b 2}"
    {:user/a 1} "#:user{:a 1}" "#:user{:a 1}\n" "{:user/a 1}"
    {:user/a 1, :user/b 2} "#:user{:a 1, :b 2}" "#:user{:a 1, :b 2}\n" "{:user/a 1, :user/b 2}"
    {:user/a 1, :b 2} "{:user/a 1, :b 2}" "{:user/a 1, :b 2}\n" "{:user/a 1, :b 2}"
    {:user/a 1, 'user/b 2} "#:user{:a 1, b 2}" "#:user{:a 1, b 2}\n" "{:user/a 1, user/b 2}"
    {:user/a 1, :foo/b 2} "{:user/a 1, :foo/b 2}" "{:user/a 1, :foo/b 2}\n" "{:user/a 1, :foo/b 2}"

    {:user/a 1, :user/b 2, 100 200}
    "{:user/a 1, :user/b 2, 100 200}"
    "{:user/a 1, :user/b 2, 100 200}\n"
    "{:user/a 1, :user/b 2, 100 200}"

    ;; CLJ-2469
    (struct (create-struct :q/a :q/b :q/c) 1 2 3)
    "#:q{:a 1, :b 2, :c 3}"
    "#:q{:a 1, :b 2, :c 3}\n"
    "{:q/a 1, :q/b 2, :q/c 3}"

    ;; CLJ-2537
    {:x.y/a {:rem 0}, :x.y/b {:rem 1}}
    "#:x.y{:a {:rem 0}, :b {:rem 1}}"
    "#:x.y{:a {:rem 0}, :b {:rem 1}}\n"
    "{:x.y/a {:rem 0}, :x.y/b {:rem 1}}"

    (into (sorted-map-by (fn [k1 k2]
                           (when-not (every? qualified-ident? [k1 k2])
                             (throw (RuntimeException. (str "Invalid keys:" [k1 k2]))))
                           (compare k1 k2))
            :x.y/a {:rem 0}, :x.y/b {:rem 1}))
    "#:x.y{:a {:rem 0}, :b {:rem 1}}"
    "#:x.y{:a {:rem 0}, :b {:rem 1}}\n"
    "{:x.y/a {:rem 0}, :x.y/b {:rem 1}}"

    (sorted-map-by #(compare %2 %1) :k/a 1 :k/b 2 :k/c 3 :k/d 4 :k/e 5 :k/f 6 :k/g 7 :k/h 8 :k/i 9)
    "#:k{:i 9, :h 8, :g 7, :f 6, :e 5, :d 4, :c 3, :b 2, :a 1}"
    "#:k{:i 9, :h 8, :g 7, :f 6, :e 5, :d 4, :c 3, :b 2, :a 1}\n"
    "{:k/i 9, :k/h 8, :k/g 7, :k/f 6, :k/e 5, :k/d 4, :k/c 3, :k/b 2, :k/a 1}")

  (let [date-map (bean (java.util.Date. 0))]
    (is (= (binding [*print-namespace-maps* true] (pr-str date-map))
           (binding [*print-namespace-maps* false] (pr-str date-map))))))

(deftest print-symbol-values
  (are [s v] (= s (pr-str v))
             "##Inf" Double/POSITIVE_INFINITY
             "##-Inf" Double/NEGATIVE_INFINITY
             "##NaN" Double/NaN
             "##Inf" Float/POSITIVE_INFINITY
             "##-Inf" Float/NEGATIVE_INFINITY
             "##NaN" Float/NaN))
