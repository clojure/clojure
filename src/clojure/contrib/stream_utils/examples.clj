;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stream application examples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns clojure.contrib.stream-utils.examples
  (:use [clojure.contrib.stream-utils
	 :only (defst-seq defst-gen pick pick-all stream-as-seq)])
  (:use [clojure.contrib.monads :only (domonad)]))

; Transform a stream of numbers into a stream of sums of
; two consecutive numbers.
(defst-seq sum-two [] [xs]
  (domonad
    [x1 (pick xs)
     x2 (pick xs)]
    (+ x1 x2)))

(sum-two '(1 2 3 4 5 6 7 8))

; The same example, but with a generator interface for the output stream
(defst-gen sum-two-gen [] [xs]
  (domonad
    [x1 (pick xs)
     x2 (pick xs)]
    (+ x1 x2)))

(def g (sum-two-gen '(1 2 3 4 5 6 7 8)))
(let [[v1 g] (g :eos)]
  (let [[v2 g] (g :eos)]
    (let [[v3 g] (g :eos)]
      (let [[v4 g] (g :eos)]
	(let [[v5 g] (g :eos)]
	  [v1 v2 v3 v4 v5])))))

; Map (for a single stream) written as a stream transformer
(defst-seq my-map-1 [f] [xs]
  (domonad
   [x (pick xs)]
   (f x)))

(my-map-1 inc [1 2 3])

; Map for two stream arguments
(defst-seq my-map-2 [f] [xs ys]
  (domonad
    [x (pick xs)
     y (pick ys)]
    (f x y)))

(my-map-2 + '(1 2 3 4) '(10 20 30 40))

; Map for any number of stream arguments
(defst-seq my-map [f] [& streams]
  (domonad
    [vs pick-all]
    (apply f vs)))

(my-map inc [1 2 3])
(my-map + '(1 2 3 4) '(10 20 30 40))

; Filter written as a stream transformer
(defst-seq my-filter [p] [xs]
  (domonad
   [x (pick xs) :when (p x)]
   x))

(my-filter odd? [1 2 3])

; A simple random number generator, implemented as a generator function
(defn rng [seed]
  (fn [eos]
    (let [m      259200
	  value  (/ (float seed) (float m))
	  next   (rem (+ 54773 (* 7141 seed)) m)]
      [value (rng next)])))

(take 10 (stream-as-seq (rng 1)))
