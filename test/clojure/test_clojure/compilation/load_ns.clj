(ns clojure.test-clojure.compilation.load-ns)

(defn a [] 1)
(defprotocol p (f [_]))
(deftype x []
  :load-ns true
  p (f [_] (a)))
