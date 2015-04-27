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
  (:use clojure.test))

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

(defn ^:private ednize-stack-trace-element
  [^StackTraceElement ste]
  [(symbol (.getClassName ste))
   (symbol (.getMethodName ste))
   (.getFileName ste)
   (.getLineNumber ste)])

(defn ^:private ednize-throwable-data
  [throwable-data]
  (-> throwable-data
      (update :via (fn [vias]
                     (map (fn [via]
                            (-> via
                                (update :type #(symbol (.getName %)))
                                (update :at ednize-stack-trace-element)))
                          vias)))
      (update :trace #(map ednize-stack-trace-element %))))

(deftest print-throwable
  (binding [*data-readers* {'error identity}]
    (are [e] (= (-> e Throwable->map ednize-throwable-data)
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
