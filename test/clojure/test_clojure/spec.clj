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
        mkeys (s/map-of (s/and keyword? (s/conformer name)) any?)
        mkeys2 (s/map-of (s/and keyword? (s/conformer name)) any? :conform-keys true)
        s (s/coll-of (s/spec (s/cat :tag keyword? :val any?)) :kind list?)
        v (s/coll-of keyword? :kind vector?)
        coll (s/coll-of keyword?)
        lrange (s/int-in 7 42)
        drange (s/double-in :infinite? false :NaN? false :min 3.1 :max 3.2)
        irange (s/inst-in #inst "1939" #inst "1946")
        ]
    (are [spec x conformed ed]
      (let [co (result-or-ex (s/conform spec x))
            e (result-or-ex (::s/problems (s/explain-data spec x)))]
        (when (not= conformed co) (println "conform fail\n\texpect=" conformed "\n\tactual=" co))
        (when (not (every? true? (map submap? ed e)))
          (println "explain failures\n\texpect=" ed "\n\tactual failures=" e "\n\tsubmap?=" (map submap? ed e)))
        (and (= conformed co) (every? true? (map submap? ed e))))

      lrange 7 7 nil
      lrange 8 8 nil
      lrange 42 ::s/invalid [{:pred '(int-in-range? 7 42 %), :val 42}]

      irange #inst "1938" ::s/invalid [{:pred '(inst-in-range? #inst "1939-01-01T00:00:00.000-00:00" #inst "1946-01-01T00:00:00.000-00:00" %), :val #inst "1938"}]
      irange #inst "1942" #inst "1942" nil
      irange #inst "1946" ::s/invalid [{:pred '(inst-in-range? #inst "1939-01-01T00:00:00.000-00:00" #inst "1946-01-01T00:00:00.000-00:00" %), :val #inst "1946"}]

      drange 3.0 ::s/invalid [{:pred '(<= 3.1 %), :val 3.0}]
      drange 3.1 3.1 nil
      drange 3.2 3.2 nil
      drange Double/POSITIVE_INFINITY ::s/invalid [{:pred '(not (isInfinite %)), :val Double/POSITIVE_INFINITY}]
      ;; can't use equality-based test for Double/NaN
      ;; drange Double/NaN ::s/invalid {[] {:pred '(not (isNaN %)), :val Double/NaN}}

      keyword? :k :k nil
      keyword? nil ::s/invalid [{:pred ::s/unknown :val nil}]
      keyword? "abc" ::s/invalid [{:pred ::s/unknown :val "abc"}]

      a 6 6 nil
      a 3 ::s/invalid '[{:pred (> % 5), :val 3}]
      a 20 ::s/invalid '[{:pred (< % 10), :val 20}]
      a nil "java.lang.NullPointerException" "java.lang.NullPointerException"
      a :k "java.lang.ClassCastException" "java.lang.ClassCastException"

      o "a" [:s "a"] nil
      o :a [:k :a] nil
      o 'a ::s/invalid '[{:pred string?, :val a, :path [:s]} {:pred keyword?, :val a :path [:k]}]

      c nil ::s/invalid '[{:reason "Insufficient input", :pred string?, :val (), :path [:a]}]
      c [] ::s/invalid '[{:reason "Insufficient input", :pred string?, :val (), :path [:a]}]
      c [:a] ::s/invalid '[{:pred string?, :val :a, :path [:a], :in [0]}]
      c ["a"] ::s/invalid '[{:reason "Insufficient input", :pred keyword?, :val (), :path [:b]}]
      c ["s" :k] '{:a "s" :b :k} nil
      c ["s" :k 5] ::s/invalid '[{:reason "Extra input", :pred (cat :a string? :b keyword?), :val (5)}]
      (s/cat) nil {} nil
      (s/cat) [5] ::s/invalid '[{:reason "Extra input", :pred (cat), :val (5), :in [0]}]

      either nil ::s/invalid '[{:reason "Insufficient input", :pred (alt :a string? :b keyword?), :val () :via []}]
      either [] ::s/invalid '[{:reason "Insufficient input", :pred (alt :a string? :b keyword?), :val () :via []}]
      either [:k] [:b :k] nil
      either ["s"] [:a "s"] nil
      either [:b "s"] ::s/invalid '[{:reason "Extra input", :pred (alt :a string? :b keyword?), :val ("s") :via []}]

      star nil [] nil
      star [] [] nil
      star [:k] [:k] nil
      star [:k1 :k2] [:k1 :k2] nil
      star [:k1 :k2 "x"] ::s/invalid '[{:pred keyword?, :val "x" :via []}]
      star ["a"] ::s/invalid '[{:pred keyword?, :val "a" :via []}]

      plus nil ::s/invalid '[{:reason "Insufficient input", :pred keyword?, :val () :via []}]
      plus [] ::s/invalid '[{:reason "Insufficient input", :pred keyword?, :val () :via []}]
      plus [:k] [:k] nil
      plus [:k1 :k2] [:k1 :k2] nil
      plus [:k1 :k2 "x"] ::s/invalid '[{:pred keyword?, :val "x", :in [2]}]
      plus ["a"] ::s/invalid '[{:pred keyword?, :val "a" :via []}]

      opt nil nil nil
      opt [] nil nil
      opt :k ::s/invalid '[{:pred (? keyword?), :val :k}]
      opt [:k] :k nil
      opt [:k1 :k2] ::s/invalid '[{:reason "Extra input", :pred (? keyword?), :val (:k2)}]
      opt [:k1 :k2 "x"] ::s/invalid '[{:reason "Extra input", :pred (? keyword?), :val (:k2 "x")}]
      opt ["a"] ::s/invalid '[{:pred keyword?, :val "a"}]

      andre nil nil nil
      andre [] nil nil
      andre :k :clojure.spec/invalid '[{:pred (& (* keyword?) even-count?), :val :k}]
      andre [:k] ::s/invalid '[{:pred even-count?, :val [:k]}]
      andre [:j :k] [:j :k] nil

      m nil ::s/invalid '[{:pred map?, :val nil}]
      m {} {} nil
      m {:a "b"} {:a "b"} nil

      mkeys nil ::s/invalid '[{:pred map?, :val nil}]
      mkeys {} {} nil
      mkeys {:a 1 :b 2} {:a 1 :b 2} nil

      mkeys2 nil ::s/invalid '[{:pred map?, :val nil}]
      mkeys2 {} {} nil
      mkeys2 {:a 1 :b 2} {"a" 1 "b" 2} nil

      s '([:a 1] [:b "2"]) '({:tag :a :val 1} {:tag :b :val "2"}) nil

      v [:a :b] [:a :b] nil
      v '(:a :b) ::s/invalid '[{:pred vector? :val (:a :b)}]

      coll nil ::s/invalid '[{:path [], :pred coll?, :val nil, :via [], :in []}]
      coll [] [] nil
      coll [:a] [:a] nil
      coll [:a :b] [:a :b] nil
      coll (map identity [:a :b]) '(:a :b) nil
      ;;coll [:a "b"] ::s/invalid '[{:pred (coll-checker keyword?), :val [:a b]}]
      )))

(comment
  (require '[clojure.test :refer (run-tests)])
  (in-ns 'clojure.test-clojure.spec)
  (run-tests)

  )
