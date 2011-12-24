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

(deftest contains-testing
  (testing "empty set contains? nothing"
    (is (= false (contains? (transient #{}) :k))))
  (testing "false when asked for a different element"
    (is (= false (contains? (transient #{:k}) :j))))
  (testing "true when contains"
    (is (contains? (transient #{:k}) :k))))

(deftest getting
  (testing "transient set should (get ...) existing element"
    (is (= :k (get (transient #{:k}) :k))))
  (testing "nil when no such element"
    (is (nil? (get (transient #{:k}) :j))))
  (testing "nil when set is empty"
    (is (nil? (get (transient #{}) :k)))))
