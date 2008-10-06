(ns test)
(defn setText [])

(prn :yo)
(prn (.. clojure print-method methodTable))
(prn (JQuery "#nice"))
(prn (.ready ($ document) test/setText))
(prn (+ 1 2 3 4))
