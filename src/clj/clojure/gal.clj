(ns clojure.gal)

(defn aa []
  (let [mystr "hello"]
    (case mystr
      "" 0
      "hello" (count mystr)))

  (let [mystr "no match"]
    (case mystr
      "" 0
      "hello" (count mystr)))

  (let [mystr "no match"]
    (case mystr
      "" 0
      "hello" (count mystr)
      "default"))

  (case 'y
    (x y z) "x, y, or z"
    "default")

  (let [myseq '(1 2)]
    (case myseq
      (()) "empty seq"
      ((1 2)) "my seq"
      "default"))

  (let [myvec [1 2]]
    (case myvec
      [] "empty vec"
      (vec '(1 2)) "my vec"
      "default")))
