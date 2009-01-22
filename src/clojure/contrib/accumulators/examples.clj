;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Accumulator application examples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use 'clojure.contrib.accumulators)

; Vector accumulator: combine is concat, add is conj
(combine [:a :b] [:c :d] [:x :y])
(add [:a :b] :c)
(add-coll empty-vector [:a :b :a])

; List accumulator: combine is concat, add is conj
(combine '(:a :b) '(:c :d) '(:x :y))
(add '(:a :b) :c)
(add-coll empty-list [:a :b :a])

; Set accumulator: combine is union, add is conj
(combine #{:a :b} #{:c :d} #{:a :d})
(add #{:a :b} :c)
(add-coll empty-set [:a :b :a])

; Map accumulator: combine is merge, add is conj
(combine {:a 1} {:b 2 :c 3} {})
(add {:a 1} [:b 2])
(add-coll empty-map [[:a 1] [:b 2] [:a 0]])

; Counter accumulator
(let [c1 (add-coll empty-counter [:a :b :a])
      c2 (add-coll empty-counter [:x :y])]
  (combine c1 c2))

; Counter-with-total accumulator
(let [c1 (add-coll empty-counter-with-total [:a :b :a])
      c2 (add-coll empty-counter-with-total [:x :y])]
  (combine c1 c2))

; Sum accumulator: combine is addition
(let [s1 (add-coll empty-sum [1 2 3])
      s2 (add-coll empty-sum [-1 -2 -3])]
  (combine s1 s2))

; Product accumulator: combine is multiplication
(let [p1 (add-coll empty-product [2 3])
      p2 (add-coll empty-product [(/ 1 2)])]
  (combine p1 p2))

; String accumulator: combine is concatenation
(combine "a" "b" "c" "def")
(add "a" (char 44))
(add-coll empty-string [(char 55) (char 56) (char 57)])

; Accumulator tuples permit to update several accumulators in parallel
(let [pair (empty-tuple [empty-vector empty-string])]
  (add-coll pair [[1 "a"] [2 "b"]]))
