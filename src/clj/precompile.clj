;; This script is run by the Ant build task to precompile the core
;; Clojure source files.

(println "Compiling Clojure core sources...")

(binding [*compile-path* (System/getProperty "clojure.compile.path")]
  (compile 'clojure.core)
  (compile 'clojure.set)
  (compile 'clojure.xml)
  (compile 'clojure.zip)
  (compile 'clojure.inspector))
