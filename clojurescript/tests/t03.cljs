(ns net.n01se)

(def x 5)
(def y 10)

(defn bind-test []
  (when (= x 2)
    (set! y 90))
  (binding [x (dec x) y (inc y)]
    (when (pos? x)
      (bind-test)))
  (prn x y))

(bind-test)
