;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Author: Shawn Hoover

(ns clojure.test-clojure.agents
  (:use clojure.test)
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

;; tests are fragile. If wait fails, could indicate that
;; build box is thrashing.
(def fragile-wait 1000)

(deftest handle-all-throwables-during-agent-actions
  ;; Bug fixed in r1198; previously hung Clojure or didn't report agent errors
  ;; after OutOfMemoryError, yet wouldn't execute new actions.
  (let [agt (agent nil)]
    (send agt (fn [state] (throw (Throwable. "just testing Throwables"))))
    (try
     ;; Let the action finish; eat the "agent has errors" error that bubbles up
     (await-for fragile-wait agt)
     (catch RuntimeException _))
    (is (instance? Throwable (first (agent-errors agt))))
    (is (= 1 (count (agent-errors agt))))

    ;; And now send an action that should work
    (clear-agent-errors agt)
    (is (= nil @agt))
    (send agt nil?)
    (is (true? (await-for fragile-wait agt)))
    (is (true? @agt))))

(deftest default-modes
  (is (= :fail (error-mode (agent nil))))
  (is (= :continue (error-mode (agent nil :error-handler println)))))

(deftest continue-handler
  (let [err (atom nil)
        agt (agent 0 :error-mode :continue :error-handler #(reset! err %&))]
    (send agt /)
    (is (true? (await-for fragile-wait agt)))
    (is (= 0 @agt))
    (is (nil? (agent-error agt)))
    (is (= agt (first @err)))
  (is (true? (instance? ArithmeticException (second @err))))))


;; TODO: make these tests deterministic (i.e. not sleep and hope)

#_(deftest fail-handler
  (let [err (atom nil)
        agt (agent 0 :error-mode :fail :error-handler #(reset! err %&))]
    (send agt /)
    (Thread/sleep 100)
    (is (true? (instance? ArithmeticException (agent-error agt))))
    (is (= 0 @agt))
    (is (= agt (first @err)))
    (is (true? (instance? ArithmeticException (second @err))))
    (is (thrown? RuntimeException (send agt inc)))))

(deftest can-send-from-error-handler-before-popping-action-that-caused-error
  (let [latch (CountDownLatch. 1)
        target-agent (agent :before-error)
        handler (fn [agt err]
                  (send target-agent
                        (fn [_] (.countDown latch))))
        failing-agent (agent nil :error-handler handler)]
    (send failing-agent (fn [_] (throw (RuntimeException.))))
    (is (.await latch 10 TimeUnit/SECONDS))))

(deftest can-send-to-self-from-error-handler-before-popping-action-that-caused-error
  (let [latch (CountDownLatch. 1)
        handler (fn [agt err]
                  (send *agent*
                        (fn [_] (.countDown latch))))
        failing-agent (agent nil :error-handler handler)]
    (send failing-agent (fn [_] (throw (RuntimeException.))))
    (is (.await latch 10 TimeUnit/SECONDS))))

#_(deftest restart-no-clear
  (let [p (promise)
        agt (agent 1 :error-mode :fail)]
    (send agt (fn [v] @p))
    (send agt /)
    (send agt inc)
    (send agt inc)
    (deliver p 0)
    (Thread/sleep 100)
    (is (= 0 @agt))
    (is (= ArithmeticException (class (agent-error agt))))
    (restart-agent agt 10)
    (is (true? (await-for fragile-wait agt)))
    (is (= 12 @agt))
    (is (nil? (agent-error agt)))))

#_(deftest restart-clear
  (let [p (promise)
        agt (agent 1 :error-mode :fail)]
    (send agt (fn [v] @p))
    (send agt /)
    (send agt inc)
    (send agt inc)
    (deliver p 0)
    (Thread/sleep 100)
    (is (= 0 @agt))
    (is (= ArithmeticException (class (agent-error agt))))
    (restart-agent agt 10 :clear-actions true)
    (is (true? (await-for fragile-wait agt)))
    (is (= 10 @agt))
    (is (nil? (agent-error agt)))
    (send agt inc)
    (is (true? (await-for fragile-wait agt)))
    (is (= 11 @agt))
    (is (nil? (agent-error agt)))))

#_(deftest invalid-restart
  (let [p (promise)
        agt (agent 2 :error-mode :fail :validator even?)]
    (is (thrown? RuntimeException (restart-agent agt 4)))
    (send agt (fn [v] @p))
    (send agt (partial + 2))
    (send agt (partial + 2))
    (deliver p 3)
    (Thread/sleep 100)
    (is (= 2 @agt))
    (is (= IllegalStateException (class (agent-error agt))))
    (is (thrown? RuntimeException (restart-agent agt 5)))
    (restart-agent agt 6)
    (is (true? (await-for fragile-wait agt)))
    (is (= 10 @agt))
    (is (nil? (agent-error agt)))))

(deftest earmuff-agent-bound
  (let [a (agent 1)]
    (send a (fn [_] *agent*))
    (await a)
    (is (= a @a))))

(def ^:dynamic *bind-me* :root-binding)

(deftest thread-conveyance-to-agents
  (let [a (agent nil)]
    (doto (Thread.
           (fn []
             (binding [*bind-me* :thread-binding]
               (send a (constantly *bind-me*)))
             (await a)))
      (.start)
      (.join))
    (is (= @a :thread-binding))))

;; check for a race condition that was causing seque to leak threads from the
;; send-off pool. Specifically, if we consume all items from the seque, and
;; the LBQ continues to grow, it means there was an agent action blocking on
;; the .put, which would block indefinitely outside of this test.
(deftest seque-threads
  (let [queue-size 5
        slow-seq (for [x (take (* 2 queue-size) (iterate inc 0))]
                   (do (Thread/sleep 25)
                       x))
        small-lbq (java.util.concurrent.LinkedBlockingQueue. queue-size)
        worker (seque small-lbq slow-seq)]
    (dorun worker)
    (is (= worker slow-seq))
    (Thread/sleep 250) ;; make sure agents have time to run or get blocked
    (let [queue-backlog (.size small-lbq)]
      (is (<= 0 queue-backlog queue-size))
      (when-not (zero? queue-backlog)
        (.take small-lbq)
        (Thread/sleep 250) ;; see if agent was blocking, indicating a thread leak
        (is (= (.size small-lbq)
               (dec queue-backlog)))))))

;; Check for a deadlock condition when one seque was fed into another
;; seque.  Note that this test does not throw an exception or
;; otherwise fail if the issue is not fixed -- it simply deadlocks and
;; hangs until killed.
(deftest seque-into-seque-deadlock
  (is (= (range 10) (seque 3 (seque 3 (range 10))))))

; http://clojure.org/agents

; agent
; deref, @-reader-macro, agent-errors
; send send-off clear-agent-errors
; await await-for
; set-validator get-validator
; add-watch remove-watch
; shutdown-agents

