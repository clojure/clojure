(ns clojure.test-clojure.delays
  (:use clojure.test)
  (:import [java.util.concurrent CyclicBarrier]))

(deftest calls-once
  (let [a (atom 0)
        d (delay (swap! a inc))]
    (is (= 0 @a))
    (is (= 1 @d))
    (is (= 1 @d))
    (is (= 1 @a))))

(deftest calls-once-in-parallel
  (let [a (atom 0)
        d (delay (swap! a inc))
        threads 100
         ^CyclicBarrier barrier (CyclicBarrier. (+ threads 1))]
    (is (= 0 @a))
    (dotimes [_ threads]
        (->
            (Thread.
                (fn []
                    (.await barrier)
                    (dotimes [_ 10000]
                        (is (= 1 @d)))
                    (.await barrier)))
        (.start)))
    (.await barrier)
    (.await barrier)
    (is (= 1 @d))
    (is (= 1 @d))
    (is (= 1 @a))))

(deftest saves-exceptions
  (let [f #(do (throw (Exception. "broken"))
               1)
        d (delay (f))
        try-call #(try
                    @d
                    (catch Exception e e))
        first-result (try-call)]
    (is (instance? Exception first-result))
    (is (identical? first-result (try-call)))))

(deftest saves-exceptions-in-parallel
  (let [f #(do (throw (Exception. "broken"))
               1)
        d (delay (f))
        try-call #(try
                    @d
                    (catch Exception e e))
        threads 100
         ^CyclicBarrier barrier (CyclicBarrier. (+ threads 1))]
    (dotimes [_ threads]
        (->
            (Thread.
                (fn []
                    (.await barrier)
                    (let [first-result (try-call)]
                        (dotimes [_ 10000]
                            (is (instance? Exception (try-call)))
                            (is (identical? first-result (try-call)))))
                    (.await barrier)))
            (.start)))
    (.await barrier)
    (.await barrier)
    (is (instance? Exception (try-call)))
    (is (identical? (try-call) (try-call)))))
