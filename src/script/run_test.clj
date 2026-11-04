(System/setProperty "java.awt.headless" "true")
(require
 '[clojure.test :as test]
 '[clojure.tools.namespace.find :as ns])
(def namespaces (remove (read-string (System/getProperty "clojure.test-clojure.exclude-namespaces"))
                        (ns/find-namespaces-in-dir (java.io.File. "test"))))
(def reflection-warning? (java.util.concurrent.atomic.AtomicBoolean. false))
(def detecting-err (java.io.PrintWriter.
                    (clojure.lang.Compile$ReflectionWarningCheckingWriter. *err* reflection-warning?)
                    true))

(binding [*err* detecting-err]
  (doseq [ns namespaces] (require ns)))

(when (.get reflection-warning?)
  (binding [*out* *err*]
    (println "ERROR: Reflection warnings were found while loading test namespaces; failing build.")
    (System/exit -1)))

(let [summary (apply test/run-tests namespaces)]
  (System/exit (if (test/successful? summary) 0 -1)))
