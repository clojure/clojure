;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Accumulator application examples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns clojure.contrib.accumulators.examples
  (:use [clojure.contrib.accumulators
	 :only (combine add add-items
		empty-vector empty-list empty-queue empty-set empty-map
		empty-counter empty-counter-with-total
		empty-sum empty-product empty-maximum empty-minimum
		empty-min-max empty-string empty-tuple)]))

; Vector accumulator: combine is concat, add is conj
(combine [:a :b] [:c :d] [:x :y])
(add [:a :b] :c)
(add-items empty-vector [:a :b :a])

; List accumulator: combine is concat, add is conj
(combine '(:a :b) '(:c :d) '(:x :y))
(add '(:a :b) :c)
(add-items empty-list [:a :b :a])

; Queue accumulator
(let [q1 (add-items empty-queue [:a :b :a])
      q2 (add-items empty-queue [:x :y])]
  (combine q1 q2))

; Set accumulator: combine is union, add is conj
(combine #{:a :b} #{:c :d} #{:a :d})
(add #{:a :b} :c)
(add-items empty-set [:a :b :a])

; Map accumulator: combine is merge, add is conj
(combine {:a 1} {:b 2 :c 3} {})
(add {:a 1} [:b 2])
(add-items empty-map [[:a 1] [:b 2] [:a 0]])

; Counter accumulator
(let [c1 (add-items empty-counter [:a :b :a])
      c2 (add-items empty-counter [:x :y])]
  (combine c1 c2))

; Counter-with-total accumulator
(let [c1 (add-items empty-counter-with-total [:a :b :a])
      c2 (add-items empty-counter-with-total [:x :y])]
  (combine c1 c2))

; Sum accumulator: combine is addition
(let [s1 (add-items empty-sum [1 2 3])
      s2 (add-items empty-sum [-1 -2 -3])]
  (combine s1 s2))

; Product accumulator: combine is multiplication
(let [p1 (add-items empty-product [2 3])
      p2 (add-items empty-product [(/ 1 2)])]
  (combine p1 p2))

; Maximum accumulator: combine is max
(let [m1 (add-items empty-maximum [2 3])
      m2 (add-items empty-maximum [(/ 1 2)])]
  (combine m1 m2))

; Minimum accumulator: combine is min
(let [m1 (add-items empty-minimum [2 3])
      m2 (add-items empty-minimum [(/ 1 2)])]
  (combine m1 m2))

; Min-max accumulator: combination of minimum and maximum
(let [m1 (add-items empty-min-max [2 3])
      m2 (add-items empty-min-max [(/ 1 2)])]
  (combine m1 m2))

; String accumulator: combine is concatenation
(combine "a" "b" "c" "def")
(add "a" (char 44))
(add-items empty-string [(char 55) (char 56) (char 57)])

; Accumulator tuples permit to update several accumulators in parallel
(let [pair (empty-tuple [empty-vector empty-string])]
  (add-items pair [[1 "a"] [2 "b"]]))
