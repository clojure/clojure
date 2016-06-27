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

(defn submap?
  "Is m1 a subset of m2?"
  [m1 m2]
  (if (and (map? m1) (map? m2))
    (every? (fn [[k v]] (and (contains? m2 k)
                          (submap? v (get m2 k))))
      m1)
    (= m1 m2)))

(defn- ne [probs]
  (let [[path prob] (first probs)]
    [(assoc prob :path path)]))

#_(deftest conform-explain
  (let [a (s/and #(> % 5) #(< % 10))
        o (s/or :s string? :k keyword?)
        c (s/cat :a string? :b keyword?)
        either (s/alt :a string? :b keyword?)
        star (s/* keyword?)
        plus (s/+ keyword?)
        opt (s/? keyword?)
        andre (s/& (s/* keyword?) even-count?)
        m (s/map-of keyword? string?)
        coll (s/coll-of keyword? [])
        lrange (s/int-in 7 42)
        drange (s/double-in :infinite? false :NaN? false :min 3.1 :max 3.2)
        irange (s/inst-in #inst "1939" #inst "1946")
        ]
    (are [spec x conformed ed]
      (let [co (result-or-ex (s/conform spec x))
            e (result-or-ex (::s/problems (s/explain-data spec x)))]
        (when (not= conformed co) (println "conform fail\n\texpect=" conformed "\n\tactual=" co))
        (when (not (submap? ed e)) (println "explain fail\n\texpect=" ed "\n\tactual=" e))
        (and (= conformed co) (submap? ed e)))

      lrange 7 7 nil
      lrange 8 8 nil
      lrange 42 ::s/invalid [{:path [] :pred '(int-in-range? 7 42 %), :val 42, :via [], :in []}]

      irange #inst "1938" ::s/invalid [{:path [] :pred '(inst-in-range? #inst "1939-01-01T00:00:00.000-00:00" #inst "1946-01-01T00:00:00.000-00:00" %), :val #inst "1938", :via [], :in []}]
      irange #inst "1942" #inst "1942" nil
      irange #inst "1946" ::s/invalid [{:path [] :pred '(inst-in-range? #inst "1939-01-01T00:00:00.000-00:00" #inst "1946-01-01T00:00:00.000-00:00" %), :val #inst "1946", :via [], :in []}]

      drange 3.0 ::s/invalid [{:path [] :pred '(<= 3.1 %), :val 3.0, :via [], :in []}]
      drange 3.1 3.1 nil
      drange 3.2 3.2 nil
      drange Double/POSITIVE_INFINITY ::s/invalid [ {:path [] :pred '(not (isInfinite %)), :val Double/POSITIVE_INFINITY, :via [], :in []}]
      ;; can't use equality-based test for Double/NaN
      ;; drange Double/NaN ::s/invalid {[] {:pred '(not (isNaN %)), :val Double/NaN, :via [], :in []}}

      keyword? :k :k nil
      keyword? nil ::s/invalid (ne {[] {:pred ::s/unknown :val nil :via []}})
      keyword? "abc" ::s/invalid (ne {[] {:pred ::s/unknown :val "abc" :via []}})

      a 6 6 nil
      a 3 ::s/invalid (ne '{[] {:pred (> % 5), :val 3 :via []}})
      a 20 ::s/invalid (ne '{[] {:pred (< % 10), :val 20 :via []}})
      a nil "java.lang.NullPointerException" "java.lang.NullPointerException"
      a :k "java.lang.ClassCastException" "java.lang.ClassCastException"

      o "a" [:s "a"] nil
      o :a [:k :a] nil
      o 'a ::s/invalid (ne '{[:s] {:pred string?, :val a :via []}, [:k] {:pred keyword?, :val a :via []}})

      c nil ::s/invalid (ne '{[:a] {:reason "Insufficient input", :pred string?, :val (), :via []}})
      c [] ::s/invalid (ne '{[:a] {:reason "Insufficient input", :pred string?, :val (), :via []}})
      c [:a] ::s/invalid (ne '{[:a] {:pred string?, :val :a, :via []}})
      c ["a"] ::s/invalid (ne '{[:b] {:reason "Insufficient input", :pred keyword?, :val (), :via []}})
      c ["s" :k] '{:a "s" :b :k} nil
      c ["s" :k 5] ::s/invalid (ne '{[] {:reason "Extra input", :pred (cat :a string? :b keyword?), :val (5), :via []}})
      (s/cat) nil {} nil
      (s/cat) [5] ::s/invalid (ne '{[] {:reason "Extra input", :pred (cat), :val (5), :via [], :in [0]}})

      either nil ::s/invalid (ne '{[] {:reason "Insufficient input", :pred (alt :a string? :b keyword?), :val () :via []}})
      either [] ::s/invalid (ne '{[] {:reason "Insufficient input", :pred (alt :a string? :b keyword?), :val () :via []}})
      either [:k] [:b :k] nil
      either ["s"] [:a "s"] nil
      either [:b "s"] ::s/invalid (ne '{[] {:reason "Extra input", :pred (alt :a string? :b keyword?), :val ("s") :via []}})

      star nil [] nil
      star [] [] nil
      star [:k] [:k] nil
      star [:k1 :k2] [:k1 :k2] nil
      star [:k1 :k2 "x"] ::s/invalid (ne '{[] {:pred keyword?, :val "x" :via []}})
      star ["a"] ::s/invalid (ne {[] '{:pred keyword?, :val "a" :via []}})

      plus nil ::s/invalid (ne '{[] {:reason "Insufficient input", :pred keyword?, :val () :via []}})
      plus [] ::s/invalid (ne '{[] {:reason "Insufficient input", :pred keyword?, :val () :via []}})
      plus [:k] [:k] nil
      plus [:k1 :k2] [:k1 :k2] nil
      plus [:k1 :k2 "x"] ::s/invalid (ne '{[] {:pred keyword?, :val "x", :via [], :in [2]}})
      plus ["a"] ::s/invalid (ne '{[] {:pred keyword?, :val "a" :via []}})

      opt nil nil nil
      opt [] nil nil
      opt :k ::s/invalid (ne '{[] {:pred (? keyword?), :val :k, :via []}})
      opt [:k] :k nil
      opt [:k1 :k2] ::s/invalid (ne '{[] {:reason "Extra input", :pred (? keyword?), :val (:k2), :via []}})
      opt [:k1 :k2 "x"] ::s/invalid (ne '{[] {:reason "Extra input", :pred (? keyword?), :val (:k2 "x"), :via []}})
      opt ["a"] ::s/invalid (ne '{[] {:pred keyword?, :val "a", :via []}})

      andre nil nil nil
      andre [] nil nil
      andre :k :clojure.spec/invalid (ne '{[] {:pred (& (* keyword?) even-count?), :val :k, :via []}})
      andre [:k] ::s/invalid (ne '{[] {:pred even-count?, :val [:k], :via []}})
      andre [:j :k] [:j :k] nil

      m nil ::s/invalid (ne '{[] {:pred map?, :val nil, :via []}})
      m {} {} nil
      m {:a "b"} {:a "b"} nil

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
       (is (submap? '{:clojure.spec/problems {[:args :arg1] {:pred integer?, :val :a, :via []}}, :clojure.spec/args (:a :b)}
              (get-ex-data (flip-nums :a :b))))
       (is (submap? '{:clojure.spec/problems {[:fn] {:pred (fn [{:keys [args ret]}] (= ret [(:arg2 args) (:arg1 args)])), :val {:args {:arg1 1, :arg2 2}, :ret [1 2]}, :via []}}, :clojure.spec/args (1 2)}
              (get-ex-data (flip-nums 1 2))))
       (is (submap? '{:clojure.spec/problems {[:ret] {:pred vector?, :val nil, :via []}}, :clojure.spec/args (1 1)}
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
  (in-ns 'clojure.test-clojure.spec)
  (run-tests)

  (stest/run-all-tests)
  (stest/check-var #'generate-from-core-pred :num-tests 10000)

  )
