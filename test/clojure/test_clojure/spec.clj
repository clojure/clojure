(ns clojure.test-clojure.spec
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.spec.test :as stest]
            [clojure.test :refer :all]))

(set! *warn-on-reflection* true)

(defmacro result-or-ex [x]
  `(try
     ~x
     (catch Throwable t#
       (.getName (class t#)))))

(def even-count? #(even? (count %)))

(deftest conform-explain
  (let [a (s/and #(> % 5) #(< % 10))
        o (s/or :s string? :k keyword?)
        c (s/cat :a string? :b keyword?)
        either (s/alt :a string? :b keyword?)
        star (s/* keyword?)
        plus (s/+ keyword?)
        opt (s/? keyword?)
        andre (s/& (s/* keyword?) even-count?)
        m (s/map-of keyword? string?)
        coll (s/coll-of keyword? [])]
    (are [spec x conformed ed]
      (let [co (result-or-ex (s/conform spec x))
            e (result-or-ex (::s/problems (s/explain-data spec x)))]
        (when (not= conformed co) (println "conform fail\n\texpect=" conformed "\n\tactual=" co))
        (when (not= ed e) (println "explain fail\n\texpect=" ed "\n\tactual=" e))
        (and (= conformed co) (= ed e)))

      keyword? :k :k nil
      keyword? nil ::s/invalid {[] {:pred ::s/unknown :val nil :via []}}
      keyword? "abc" ::s/invalid {[] {:pred ::s/unknown :val "abc" :via []}}

      a 6 6 nil
      a 3 ::s/invalid '{[] {:pred (> % 5), :val 3 :via []}}
      a 20 ::s/invalid '{[] {:pred (< % 10), :val 20 :via []}}
      a nil "java.lang.NullPointerException" "java.lang.NullPointerException"
      a :k "java.lang.ClassCastException" "java.lang.ClassCastException"

      o "a" [:s "a"] nil
      o :a [:k :a] nil
      o 'a ::s/invalid '{[:s] {:pred string?, :val a :via []}, [:k] {:pred keyword?, :val a :via []}}

      c nil ::s/invalid '{[:a] {:reason "Insufficient input", :pred string?, :val (), :via []}}
      c [] ::s/invalid '{[:a] {:reason "Insufficient input", :pred string?, :val (), :via []}}
      c [:a] ::s/invalid '{[:a] {:pred string?, :val :a, :via []}}
      c ["a"] ::s/invalid '{[:b] {:reason "Insufficient input", :pred keyword?, :val (), :via []}}
      c ["s" :k] '{:a "s" :b :k} nil
      c ["s" :k 5] ::s/invalid '{[] {:reason "Extra input", :pred (cat :a string? :b keyword?), :val (5), :via []}}

      either nil ::s/invalid '{[] {:reason "Insufficient input", :pred (alt :a string? :b keyword?), :val () :via []}}
      either [] ::s/invalid '{[] {:reason "Insufficient input", :pred (alt :a string? :b keyword?), :val () :via []}}
      either [:k] [:b :k] nil
      either ["s"] [:a "s"] nil
      either [:b "s"] ::s/invalid '{[] {:reason "Extra input", :pred (alt :a string? :b keyword?), :val ("s") :via []}}

      star nil [] nil
      star [] [] nil
      star [:k] [:k] nil
      star [:k1 :k2] [:k1 :k2] nil
      star [:k1 :k2 "x"] ::s/invalid '{[] {:pred keyword?, :val "x" :via []}}
      star ["a"] ::s/invalid {[] '{:pred keyword?, :val "a" :via []}}

      plus nil ::s/invalid '{[] {:reason "Insufficient input", :pred keyword?, :val () :via []}}
      plus [] ::s/invalid '{[] {:reason "Insufficient input", :pred keyword?, :val () :via []}}
      plus [:k] [:k] nil
      plus [:k1 :k2] [:k1 :k2] nil
      plus [:k1 :k2 "x"] ::s/invalid '{[] {:reason "Extra input", :pred (cat), :val ("x") :via []}}
      plus ["a"] ::s/invalid '{[] {:pred keyword?, :val "a" :via []}}

      opt nil nil nil
      opt [] nil nil
      opt :k ::s/invalid '{[] {:pred (alt), :val :k, :via []}}
      opt [:k] :k nil
      opt [:k1 :k2] ::s/invalid '{[] {:reason "Extra input", :pred (alt), :val (:k2), :via []}}
      opt [:k1 :k2 "x"] ::s/invalid '{[] {:reason "Extra input", :pred (alt), :val (:k2 "x"), :via []}}
      opt ["a"] ::s/invalid '{[] {:pred keyword?, :val "a", :via []}}

      andre nil nil nil
      andre [] nil nil
      andre :k :clojure.spec/invalid '{[] {:pred (& (* keyword?) even-count?), :val :k, :via []}}
      andre [:k] ::s/invalid '{[] {:pred even-count?, :val [:k], :via []}}
      andre [:j :k] [:j :k] nil

      m nil ::s/invalid '{[] {:pred map?, :val nil, :via []}}
      m {} {} nil
      m {:a "b"} {:a "b"} nil
      m {:a :b} ::s/invalid '{[] {:pred (coll-checker (tuple keyword? string?)), :val {:a :b}, :via []}}

      coll nil nil nil
      coll [] [] nil
      coll [:a] [:a] nil
      coll [:a :b] [:a :b] nil
      ;;coll [:a "b"] ::s/invalid '{[] {:pred (coll-checker keyword?), :val [:a b], :via []}}
      )))

(s/fdef flip-nums
        :args (s/cat :arg1 integer? :arg2 integer?)
        :ret vector?
        :fn (fn [{:keys [args ret]}]
              (= ret [(:arg2 args) (:arg1 args)])))

(def ^:dynamic *break-flip-nums* false)
(defn flip-nums
  "Set *break-flip-nums* to break this fns compatibility with
its spec for test purposes."
  [a b]
  (if *break-flip-nums*
    (when-not (= a b)
      (vec (sort [a b])))
    [b a]))

(defmacro get-ex-data
  [x]
  `(try
    ~x
    nil
    (catch Throwable t#
      (ex-data t#))))

;; Note the the complicated equality comparisons below are exactly the
;; kind of thing that spec helps you avoid, used here only because we
;; are near the bottom, testing spec itself.
(deftest test-instrument-flip-nums
  (when-not (= "true" (System/getProperty "clojure.compiler.direct-linking"))
    (binding [*break-flip-nums* true]
      (try
       (= [1 2] (flip-nums 2 1))
       (= [:a :b] (flip-nums :a :b))
       (= [1 2] (flip-nums 1 2))
       (is (nil? (flip-nums 1 1)))
       (s/instrument `flip-nums)
       (is (= [1 2] (flip-nums 2 1)))
       (is (= '{:clojure.spec/problems {[:args :arg1] {:pred integer?, :val :a, :via []}}, :clojure.spec/args (:a :b)}
              (get-ex-data (flip-nums :a :b))))
       (is (= '{:clojure.spec/problems {[:fn] {:pred (fn [{:keys [args ret]}] (= ret [(:arg2 args) (:arg1 args)])), :val {:args {:arg1 1, :arg2 2}, :ret [1 2]}, :via []}}, :clojure.spec/args (1 2)}
              (get-ex-data (flip-nums 1 2))))
       (is (= '{:clojure.spec/problems {[:ret] {:pred vector?, :val nil, :via []}}, :clojure.spec/args (1 1)}
              (get-ex-data (flip-nums 1 1))))
       (s/unstrument `flip-nums)
       (= [1 2] (flip-nums 2 1))
       (= [:a :b] (flip-nums :a :b))
       (= [1 2] (flip-nums 1 2))
       (is (nil? (flip-nums 1 1)))
       (s/unstrument `flip-nums)))))

(def core-pred-syms
  (into #{}
        (comp (map first) (filter (fn [s] (.endsWith (name s) "?"))))
        (ns-publics 'clojure.core)))

(def generatable-core-pred-syms
  (into #{}
        (filter #(gen/gen-for-pred @ (resolve %)))
        core-pred-syms))

(s/fdef generate-from-core-pred
        :args (s/cat :s generatable-core-pred-syms)
        :ret ::s/any
        :fn (fn [{:keys [args ret]}]
              (@(resolve (:s args)) ret)))

(defn generate-from-core-pred
  [s]
  (gen/generate (gen/gen-for-pred @(resolve s))))

(comment
  (require '[clojure.test :refer (run-tests)])
  (in-ns 'test-clojure.spec)
  (run-tests)

  (stest/run-all-tests)
  (stest/check-var #'generate-from-core-pred :num-tests 10000)

  )
