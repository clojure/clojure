(ns clojure.contrib.test-logging
  [:use clojure.test]
  [:require [clojure.contrib.logging :as log]])

(def *entries* (atom []))

(defn test-factory [enabled-set]
  (reify log/LogFactory
    (impl-name [_] "test factory")
    (impl-get-log [_ log-ns]
      (reify log/Log
        (impl-enabled? [_ level] (contains? enabled-set level))
        (impl-write! [_ lvl ex msg]
          (swap! *entries* conj [(str log-ns) lvl ex msg]))))))

(use-fixtures :once
  (fn [f]
    (binding [log/*log-factory*
              (test-factory #{:trace :debug :info :warn :error :fatal})]
      (f))))

(use-fixtures :each
  (fn [f]
    (f)
    (swap! *entries* (constantly []))))


(deftest log-msg
  (log/log :debug "foo")
  (is (= ["clojure.contrib.test-logging"
          :debug
          nil
          "foo"]
        (peek @*entries*))))

(deftest log-ex
  (let [e (Exception.)]
    (log/log :debug e "foo")
    (is (= ["clojure.contrib.test-logging"
            :debug
            e
            "foo"]
          (peek @*entries*)))))

(deftest log-custom-ns
  (let [e (Exception.)]
    (log/log "other.ns" :debug e "foo")
    (is (= ["other.ns"
            :debug
            e
            "foo"]
          (peek @*entries*)))))

(comment ; Tests failing on build machine for some unknown reason.
         ; See http://groups.google.com/group/clojure-dev/browse_thread/thread/fde32342f3c006bd
(deftest log-tx-agent
  (let [flag (atom false)
        so send-off]
    (binding [send-off (fn [a f & args]
                         (compare-and-set! flag false true)
                         (apply so a f args))]
      (dosync
        (log/log :info "foo")))
    (await log/*logging-agent*)
    (is (= ["clojure.contrib.test-logging"
            :info
            nil
            "foo"]
          (peek @*entries*)))
    (is (true? @flag))))

(deftest log-tx-noagent
  (let [flag (atom false)
        so send-off]
    (binding [send-off (fn [a f & args]
                         (compare-and-set! flag false true)
                         (apply so a f args))]
      (dosync
        (log/log :error "foo")))
    (await log/*logging-agent*)
    (is (= ["clojure.contrib.test-logging"
            :error
            nil
            "foo"]
          (peek @*entries*)))
    (is (false? @flag))))

(deftest log-alter-tx-agent-levels
  (let [flag (atom false)
        so send-off]
    (binding [send-off (fn [a f & args]
                         (compare-and-set! flag false true)
                         (apply so a f args))
              log/*tx-agent-levels* #{:error}]
      (dosync
        (log/log :error "foo")))
    (await log/*logging-agent*)
    (is (= ["clojure.contrib.test-logging"
            :error
            nil
            "foo"]
          (peek @*entries*)))
    (is (true? @flag))))

(deftest log-force-agent
  (let [flag (atom false)
        so send-off]
    (binding [send-off (fn [a f & args]
                         (compare-and-set! flag false true)
                         (apply so a f args))
              log/*force* :agent]
      (log/log :debug "foo"))
    (await log/*logging-agent*)
    (is (= ["clojure.contrib.test-logging"
            :debug
            nil
            "foo"]
          (peek @*entries*)))
    (is (true? @flag))))

(deftest log-force-direct
  (let [flag (atom false)
        so send-off]
    (binding [send-off (fn [a f & args]
                         (compare-and-set! flag false true)
                         (apply so a f args))
              log/*force* :direct]
      (dosync
        (log/log :info "foo")))
    (await log/*logging-agent*)
    (is (= ["clojure.contrib.test-logging"
            :info
            nil
            "foo"]
          (peek @*entries*)))
    (is (false? @flag))))
)


(deftest logp-msg1
  (log/logp :debug "hello")
  (is (= ["clojure.contrib.test-logging"
          :debug
          nil
          "hello"]
        (peek @*entries*))))

(deftest logp-msg2
  (log/logp :debug "hello" "world")
  (is (= ["clojure.contrib.test-logging"
          :debug
          nil
          "hello world"]
        (peek @*entries*))))

(deftest logp-ex0
  (let [e (Exception.)]
    (log/logp :debug e)
    (is (= ["clojure.contrib.test-logging"
            :debug
            nil
            (print-str e)]
          (peek @*entries*)))))

(deftest logp-ex1
  (let [e (Exception.)]
    (log/logp :debug e "hello")
    (is (= ["clojure.contrib.test-logging"
            :debug
            e
            "hello"]
          (peek @*entries*)))))

(deftest logp-ex2
  (let [e (Exception.)]
    (log/logp :debug e "hello" "world")
    (is (= ["clojure.contrib.test-logging"
            :debug
            e
            "hello world"]
          (peek @*entries*)))))

(deftest logf-msg1
  (log/logf :debug "hello")
  (is (= ["clojure.contrib.test-logging"
          :debug
          nil
          "hello"]
        (peek @*entries*))))

(deftest logf-msg3
  (log/logf :debug "%s %s" "hello" "world")
  (is (= ["clojure.contrib.test-logging"
          :debug
          nil
          "hello world"]
        (peek @*entries*))))

(deftest logf-ex0
  (is (thrown? Exception (log/logf :debug (Exception.)))))

(deftest logf-ex3
  (let [e (Exception.)]
    (log/logf :debug e "%s %s" "hello" "world")
    (is (= ["clojure.contrib.test-logging"
            :debug
            e
            "hello world"]
          (peek @*entries*)))))

(deftest enabled-true
  (is (= true (log/enabled? :fatal))))

(deftest enabled-false
  (binding [log/*log-factory* (test-factory #{})]
    (is (= false (log/enabled? :fatal)))))

(deftest spy-default
  (log/spy (+ 4 5))
  (is (= ["clojure.contrib.test-logging"
          :debug
          nil
          "(+ 4 5)\n=> 9"]
        (peek @*entries*))))

(deftest spy-level
  (log/spy :fatal (+ 4 5))
  (is (= ["clojure.contrib.test-logging"
          :fatal
          nil
          "(+ 4 5)\n=> 9"]
        (peek @*entries*))))

(comment
(deftest capturing
  (log/log-capture! "foobar")
  (.println System/out "hello world")
  (is (= ["foobar"
          :info
          nil
          "hello world"]
        (peek @*entries*)))
  (.println System/err "oh noes")
  (is (= ["foobar"
          :error
          nil
          "oh noes"]
        (peek @*entries*)))
  (log/log-uncapture!))

(deftest capturing-level
  (log/log-capture! "foobar" :error :fatal)
  (.println System/out "hello world")
  (is (= ["foobar"
          :error
          nil
          "hello world"]
        (peek @*entries*)))
  (.println System/err "oh noes")
  (is (= ["foobar"
          :fatal
          nil
          "oh noes"]
        (peek @*entries*)))
  (log/log-uncapture!))

(deftest with-logs-default
  (log/with-logs "foobar" (println "hello world"))
  (is (= ["foobar"
          :info
          nil
          "hello world"]
        (peek @*entries*))))

(deftest with-logs-level
  (log/with-logs ["foobar" :fatal :fatal] (println "hello world"))
  (is (= ["foobar"
          :fatal
          nil
          "hello world"]
        (peek @*entries*))))
)

(deftest println-style
  (are [f kw]
    (= ["clojure.contrib.test-logging"
        kw
        nil
        "hello world"]
      (do
        (f "hello" "world")
        (peek @*entries*)))
    log/trace :trace
    log/debug :debug
    log/info :info
    log/warn :warn
    log/error :error
    log/fatal :fatal))

(deftest println-style-ex
  (let [e (Exception.)]
    (are [f kw]
      (= ["clojure.contrib.test-logging"
          kw
          e
          "hello world"]
        (do
          (f e "hello" "world")
          (peek @*entries*)))
      log/trace :trace
      log/debug :debug
      log/info :info
      log/warn :warn
      log/error :error
      log/fatal :fatal)))

(deftest format-style
  (are [f kw]
    (= ["clojure.contrib.test-logging"
        kw
        nil
        "hello world"]
      (do
        (f "%s %s" "hello" "world")
        (peek @*entries*)))
    log/tracef :trace
    log/debugf :debug
    log/infof :info
    log/warnf :warn
    log/errorf :error
    log/fatalf :fatal))

(deftest format-style-ex
  (let [e (Exception.)]
    (are [f kw]
      (= ["clojure.contrib.test-logging"
          kw
          e
          "hello world"]
        (do
          (f e "%s %s" "hello" "world")
          (peek @*entries*)))
      log/tracef :trace
      log/debugf :debug
      log/infof :info
      log/warnf :warn
      log/errorf :error
      log/fatalf :fatal)))
