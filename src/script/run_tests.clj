(System/setProperty "clojure.test.generative.msec" "60000")
(System/setProperty "java.awt.headless" "true")
(require '[clojure.test.generative.runner :as runner])
(runner/-main "test")
