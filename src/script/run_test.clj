(System/setProperty "java.awt.headless" "true")
(require
 '[clojure.test :as test]
 '[clojure.tools.namespace.find :as ns])
(def namespaces (remove (read-string (System/getProperty "clojure.test-clojure.exclude-namespaces"))
                        (ns/find-namespaces-in-dir (java.io.File. "test"))))
(doseq [ns namespaces] (require ns))
(let [summary (apply test/run-tests namespaces)]
  (System/exit (if (test/successful? summary) 0 -1)))
