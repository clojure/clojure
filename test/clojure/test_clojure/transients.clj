(ns clojure.test-clojure.transients
  (:use clojure.test))

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
