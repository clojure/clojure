(ns clojure.test-clojure.repl.deps
  (:use clojure.test)
  (:require [clojure.string :as str]
            [clojure.repl.deps :as deps]
            [clojure.main :as main]))

(defmacro with-dynamic-loader
  "Ensure or install a DynamicClassLoader as the current thread's
  context classloader and execute the body."
  [& body]
  `(let [t# (Thread/currentThread)
         cl# (.getContextClassLoader t#)]
     (if (instance? ~'clojure.lang.DynamicClassLoader cl#)
       (do ~@body)
       (try
         (.setContextClassLoader t# (clojure.lang.DynamicClassLoader. cl#))
         ~@body
         (finally
           (.setContextClassLoader t# cl#))))))

(deftest test-no-add-libs-outside-repl
  (try
    (deps/add-lib 'org.clojure/data.json {:mvn/version "2.4.0"})
    (is false "add-libs outside repl should throw")
    (catch Throwable t (str/includes? (ex-message t) "add-libs")))

  (with-dynamic-loader
    (binding [*repl* true]
      (is (some #{'org.clojure/data.json} (deps/add-lib 'org.clojure/data.json {:mvn/version "2.4.0"})))))
  )
