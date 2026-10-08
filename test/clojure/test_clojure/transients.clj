(ns clojure.test-clojure.transients
  (:use clojure.test)
  (:require [clojure.test.check :as chk]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest popping-off
  (testing "across a node boundary"
    (are [n] 
      (let [v (-> (range n) vec)]
        (= (subvec v 0 (- n 2)) (-> v transient pop! pop! persistent!)))
      33 (+ 32 (inc (* 32 32))) (+ 32 (inc (* 32 32 32)))))
  (testing "off the end"
    (is (thrown-with-msg? IllegalStateException #"Can't pop empty vector"
          (-> [] transient pop!))))
  (testing "copying array from a non-editable when put in tail position")
    (is (= 31 (let [pv (vec (range 34))]
                (-> pv transient pop! pop! pop! (conj! 42))
                (nth pv 31)))))

(defn- hash-obj [hash]
  (reify Object (hashCode [this] hash)))

(deftest dissocing
  (testing "dissocing colliding keys"
    (is (= [0 {}] (let [ks (concat (range 7) [(hash-obj 42) (hash-obj 42)])
                        m (zipmap ks ks)
                        dm (persistent! (reduce dissoc! (transient m) (keys m)))]
                    [(count dm) dm])))))

(deftest test-disj!
  (testing "disjoin multiple items in one call"
    (is (= #{5 20} (-> #{5 10 15 20} transient (disj! 10 15) persistent!)))))

(deftest empty-transient
  (is (= false (.contains (transient #{}) :bogus-key))))

(deftest persistent-assoc-on-collision
  (testing "Persistent assoc on a collision node which underwent a transient dissoc"
    (let [a (reify Object (hashCode [_] 42))
          b (reify Object (hashCode [_] 42))]
      (is (= (-> #{a b} transient (disj! a) persistent! (conj a))
            (-> #{a b} transient (disj! a) persistent! (conj a)))))))

(deftest transient-mod-after-persistent
  (let [v [1 2 3]
        t (transient v)
        t2 (conj! t 4)
        p (persistent! t2)]
    (is (= [1 2 3 4] p))
    (is (thrown? IllegalAccessError (conj! t2 5)))))

(deftest transient-mod-ok-across-threads
  (let [v [1 2 3]
        t (transient v)
        t2 @(future (conj! t 4))
        p (persistent! t2)]
    (is (= [1 2 3 4] p))))

(deftest transient-lookups
  (let [tv (transient [1 2 3])]
    (is (= 1 (get tv 0)))
    (is (= :foo (get tv 4 :foo)))
    (is (= true (contains? tv 0)))
    (is (= [0 1] (find tv 0)))
    (is (= nil (find tv -1))))
  (let [ts (transient #{1 2})]
    (is (= true (contains? ts 1)))
    (is (= false (contains? ts 99)))
    (is (= 1 (get ts 1)))
    (is (= nil (get ts 99))))
  (let [tam (transient (array-map :a 1 :b 2))]
    (is (= true (contains? tam :a)))
    (is (= false (contains? tam :x)))
    (is (= 1 (get tam :a)))
    (is (= nil (get tam :x)))
    (is (= [:a 1] (find tam :a)))
    (is (= nil (find tam :x))))
  (let [thm (transient (hash-map :a 1 :b 2))]
    (is (= true (contains? thm :a)))
    (is (= false (contains? thm :x)))
    (is (= 1 (get thm :a)))
    (is (= nil (get thm :x)))
    (is (= [:a 1] (find thm :a)))
    (is (= nil (find thm :x)))))

(defn assoc-transient-check-meta
  [cnt keyfn]
  (let [metam {:foo "meta"}]
    (loop [m (transient (with-meta {} metam))
           i 0]
      (if (< i cnt)
        (let [nextm (assoc! m (keyfn i) i)]
          ;; this doesn't pass right now, but maybe it should
          ;(is (= metam (meta nextm) (str "lost meta at i=" i)))
          (recur nextm (inc i)))
        (is (= metam (meta (persistent! m))))))))

(deftest metadata-propagation-across-growing-transient-map
  ;; 130 = past KW_HASHTABLE_THRESHOLD
  ;; test both keyword and non-keyword keys
  (assoc-transient-check-meta 130 #(keyword (str "kw-" %)))
  (assoc-transient-check-meta 130 #(str "kw-" %)))

(defn through-transient [r1 r2]
  (loop [acc (transient r1)
         i 0]
    (if (< i (count r2))
      (recur (conj! acc (nth r2 i)) (inc i))
      (persistent! acc))))

(deftest fill-transient
  (let [res (chk/quick-check
              5000
              (prop/for-all [cnt (gen/resize 130 gen/nat)]
                (let [v (range cnt)]
                  (= v (through-transient [] v)))))]
    (when-not (:result res)
      (is
        (:result res)
        (->
          res
          :shrunk
          :smallest
          first
          clojure.pprint/pprint
          with-out-str)))))

(deftest fill-transient-adopt
  (let [res (chk/quick-check
              5000
              (prop/for-all [cnts (gen/tuple (gen/resize 130 gen/nat) (gen/resize 130 gen/nat))]
                (let [[c1 c2] cnts
                      r1 (vec (range c1))
                      r2 (vec (range c2))]
                  (= (concat r1 r2) (through-transient r1 r2)))))]
    (when-not (:result res)
      (is
        (:result res)
        (->
          res
          :shrunk
          :smallest
          first
          clojure.pprint/pprint
          with-out-str)))))
