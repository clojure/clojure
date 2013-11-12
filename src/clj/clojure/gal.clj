(ns clojure.gal)

(defn hello
  [a b]
  (let [sum (+ a b)]
    (if (< sum 100)
      (recur sum 10)
      sum)))

(hello 1 2)
