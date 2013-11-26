(ns clojure.gal
  (:use clojure.string))

(defn aaa []
(letfn [(twice [x]
               (* x 2))
        (six-times [y]
                   (* (twice y) 3))]
  (println "Twice 15 =" (twice 15))
  (println "Six times 15 =" (six-times 15))))
