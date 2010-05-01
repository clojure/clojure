(ns clojure.contrib.test-with-ns
  (:use clojure.test
	clojure.contrib.with-ns))

(deftest test-namespace-gets-removed
  (let [all-ns-names (fn [] (map #(.name %) (all-ns)))]
    (testing "unexceptional return"
      (let [ns-name (with-temp-ns (ns-name *ns*))]
        (is (not (some #{ns-name} (all-ns-names))))))
    (testing "when an exception is thrown"
      (let [ns-name-str
            (try
             (with-temp-ns
               (throw (RuntimeException. (str (ns-name *ns*)))))
             (catch clojure.lang.Compiler$CompilerException e
               (-> e .getCause .getMessage)))]
        (is (re-find #"^sym.*$" ns-name-str))
        (is (not (some #{(symbol ns-name-str)} (all-ns-names))))))))
