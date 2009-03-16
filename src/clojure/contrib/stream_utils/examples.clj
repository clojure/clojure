;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stream application examples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns clojure.contrib.stream-utils.examples
  (:use [clojure.contrib.stream-utils
	 :only (defst stream-next
		pick pick-all
		stream-type defstream
		stream-drop stream-map stream-filter stream-flatten)])
  (:use [clojure.contrib.monads :only (domonad)])
  (:use [clojure.contrib.types :only (deftype)])
  (:require [clojure.contrib.generic.collection :as gc]))

;
; Define a stream of Fibonacci numbers
;
(deftype ::fib-stream last-two-fib)

(defstream ::fib-stream
  [fs]
  (let [[n1 n2] fs]
    [n1 (last-two-fib [n2 (+ n1 n2)])]))

(def fib-stream (last-two-fib [0 1]))

(take 10 (gc/seq fib-stream))

;
; A simple random number generator, implemented as a stream
;
(deftype ::random-seed rng-seed vector seq)

(defstream ::random-seed
  [seed]
  (let [[seed] seed
	m      259200
	value  (/ (float seed) (float m))
	next   (rem (+ 54773 (* 7141 seed)) m)]
    [value (rng-seed next)]))

(take 10 (gc/seq (rng-seed 1)))

;
; Various stream utilities
;
(take 10 (gc/seq (stream-drop 10 (rng-seed 1))))
(gc/seq (stream-map inc (range 5)))
(gc/seq (stream-filter odd? (range 10)))
(gc/seq (stream-flatten (partition 3 (range 9))))

;
; Stream transformers
;

; Transform a stream of numbers into a stream of sums of two
; consecutive numbers.
(defst sum-two [] [xs]
  (domonad
    [x1 (pick xs)
     x2 (pick xs)]
    (+ x1 x2)))

(def s (sum-two '(1 2 3 4 5 6 7 8)))

(let [[v1 s] (stream-next s)]
  (let [[v2 s] (stream-next s)]
    (let [[v3 s] (stream-next s)]
      (let [[v4 s] (stream-next s)]
	(let [[v5 s] (stream-next s)]
	  [v1 v2 v3 v4 v5])))))

(gc/seq s)

; Map (for a single stream) written as a stream transformer
(defst my-map-1 [f] [xs]
  (domonad
   [x (pick xs)]
   (f x)))

(gc/seq (my-map-1 inc [1 2 3]))

; Map for two stream arguments
(defst my-map-2 [f] [xs ys]
  (domonad
    [x (pick xs)
     y (pick ys)]
    (f x y)))

(gc/seq (my-map-2 + '(1 2 3 4) '(10 20 30 40)))

; Map for any number of stream arguments
(defst my-map [f] [& streams]
  (domonad
    [vs pick-all]
    (apply f vs)))

(gc/seq (my-map inc [1 2 3]))
(gc/seq (my-map + '(1 2 3 4) '(10 20 30 40)))

; Filter written as a stream transformer
(defst my-filter [p] [xs]
  (domonad
   [x (pick xs) :when (p x)]
   x))

(gc/seq (my-filter odd? [1 2 3]))

