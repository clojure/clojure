;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Monad application examples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns
  #^{:author "Konrad Hinsen"
     :skip-wiki true
     :doc "Examples for using monads"}
  clojure.contrib.monads.examples
  (:use [clojure.contrib.monads
	 :only (domonad with-monad m-lift m-seq m-reduce m-when
		sequence-m
		maybe-m
		state-m fetch-state set-state 
		writer-m write
		cont-m run-cont call-cc
		maybe-t)])
  (:require (clojure.contrib [accumulators :as accu])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Sequence manipulations with the sequence monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note: in the Haskell world, this monad is called the list monad.
; The Clojure equivalent to Haskell's lists are (possibly lazy)
; sequences. This is why I call this monad "sequence". All sequences
; created by sequence monad operations are lazy.

; Monad comprehensions in the sequence monad work exactly the same
; as Clojure's 'for' construct, except that :while clauses are not
; available.
(domonad sequence-m
   [x (range 5)
    y (range 3)]
    (+ x y))

; Inside a with-monad block, domonad is used without the monad name.
(with-monad sequence-m
  (domonad
     [x (range 5)
      y (range 3)]
     (+ x y)))

; Conditions are written with :when, as in Clojure's for form:
(domonad sequence-m
   [x  (range 5)
    y  (range (+ 1 x))
    :when (= (+ x y) 2)]
   (list x y))

; :let is also supported like in for:
(domonad sequence-m
   [x  (range 5)
    y  (range (+ 1 x))
    :let [sum (+ x y)
	  diff (- x y)]
    :when  (= sum 2)]
   (list diff))

; An example of a sequence function defined in terms of a lift operation.
(with-monad sequence-m
   (defn pairs [xs]
      ((m-lift 2 #(list %1 %2)) xs xs)))

(pairs (range 5))

; Another way to define pairs is through the m-seq operation. It takes
; a sequence of monadic values and returns a monadic value containing
; the sequence of the underlying values, obtained from chaining together
; from left to right the monadic values in the sequence.
(with-monad sequence-m
   (defn pairs [xs]
      (m-seq (list xs xs))))

(pairs (range 5))

; This definition suggests a generalization:
(with-monad sequence-m
   (defn ntuples [n xs]
      (m-seq (replicate n xs))))

(ntuples 2 (range 5))
(ntuples 3 (range 5))

; Lift operations can also be used inside a monad comprehension:
(domonad sequence-m
   [x  ((m-lift 1 (partial * 2)) (range 5))
    y  (range 2)]
    [x y])

; The m-plus operation does concatenation in the sequence monad.
(domonad sequence-m
   [x  ((m-lift 2 +) (range 5) (range 3))
    y  (m-plus (range 2) '(10 11))]
   [x y])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Handling failures with the maybe monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Maybe monad versions of basic arithmetic
(with-monad maybe-m
   (def m+ (m-lift 2 +))
   (def m- (m-lift 2 -))
   (def m* (m-lift 2 *)))

; Division is special for two reasons: we can't call it m/ because that's
; not a legal Clojure symbol, and we want it to fail if a division by zero
; is attempted. It is best defined by a monad comprehension with a
; :when clause:
(defn safe-div [x y]
  (domonad maybe-m
     [a x
      b y
      :when (not (zero? b))]
     (/ a b)))

; Now do some non-trivial computation with division
; It fails for (1) x = 0, (2) y = 0 or (3) y = -x.
(with-monad maybe-m
   (defn some-function [x y]
      (let [one (m-result 1)]
 	   (safe-div one (m+ (safe-div one (m-result x))
			     (safe-div one (m-result y)))))))

; An example that doesn't fail:
(some-function 2 3)
; And two that do fail, at different places:
(some-function 2 0)
(some-function 2 -2)

; In the maybe monad, m-plus selects the first monadic value that
; holds a valid value.
(with-monad maybe-m
   (m-plus (some-function 2 0) (some-function 2 -2) (some-function 2 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Random numbers with the state monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A state monad item represents a computation that changes a state and
; returns a value. Its structure is a function that takes a state argument
; and returns a two-item list containing the value and the updated state.
; It is important to realize that everything you put into a state monad
; expression is a state monad item (thus a function), and everything you
; get out as well. A state monad does not perform a calculation, it
; constructs a function that does the computation when called.

; First, we define a simple random number generator with explicit state.
; rng is a function of its state (an integer) that returns the
; pseudo-random value derived from this state and the updated state 
; for the next iteration. This is exactly the structure of a state
;  monad item.
(defn rng [seed]
  (let [m      259200
	value  (/ (float seed) (float m))
	next   (rem (+ 54773 (* 7141 seed)) m)]
    [value next]))

; We define a convenience function that creates an infinite lazy seq
; of values obtained from iteratively applying a state monad value.
(defn value-seq [f seed]
  (lazy-seq
    (let [[value next] (f seed)]
      (cons value (value-seq f next)))))

; Next, we define basic statistics functions to check our random numbers
(defn sum [xs]  (apply + xs))
(defn mean [xs]  (/ (sum xs) (count xs)))
(defn variance [xs]
  (let [m (mean xs)
	sq #(* % %)]
    (mean (for [x xs] (sq (- x m))))))

; rng implements a uniform distribution in the interval [0., 1.), so
; ideally, the mean would be 1/2 (0.5) and the variance 1/12 (0.8333).
(mean (take 1000 (value-seq rng 1)))
(variance (take 1000 (value-seq rng 1)))

; We make use of the state monad to implement a simple (but often sufficient)
; approximation to a Gaussian distribution: the sum of 12 random numbers
; from rng's distribution, shifted by -6, has a distribution that is
; approximately Gaussian with 0 mean and variance 1, by virtue of the central
; limit theorem.
; In the first version, we call rng 12 times explicitly and calculate the
; shifted sum in a monad comprehension:
(def gaussian1
   (domonad state-m
      [x1  rng
       x2  rng
       x3  rng
       x4  rng
       x5  rng
       x6  rng
       x7  rng
       x8  rng
       x9  rng
       x10 rng
       x11 rng
       x12 rng]
      (- (+ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) 6.)))

; Let's test it:
(mean (take 1000 (value-seq gaussian1 1)))
(variance (take 1000 (value-seq gaussian1 1)))

; Of course, we'd rather have a loop construct for creating the 12
; random numbers. This would be easy if we could define a summation
; operation on random-number generators, which would then be used in
; combination with reduce. The lift operation gives us exactly that.
; More precisely, we need (m-lift 2 +), because we want both arguments
; of + to be lifted to the state monad:
(def gaussian2
   (domonad state-m
      [sum12 (reduce (m-lift 2 +) (replicate 12 rng))]
      (- sum12 6.)))

; Such a reduction is often quite useful, so there's m-reduce predefined
; to simplify it:
(def gaussian2
   (domonad state-m
      [sum12 (m-reduce + (replicate 12 rng))]
      (- sum12 6.)))

; The statistics should be strictly the same as above, as long as
; we use the same seed:
(mean (take 1000 (value-seq gaussian2 1)))
(variance (take 1000 (value-seq gaussian2 1)))

; We can also do the subtraction of 6 in a lifted function, and get rid
; of the monad comprehension altogether:
(with-monad state-m
   (def gaussian3
        ((m-lift 1 #(- % 6.))
           (m-reduce + (replicate 12 rng)))))

; Again, the statistics are the same:
(mean (take 1000 (value-seq gaussian3 1)))
(variance (take 1000 (value-seq gaussian3 1)))

; For a random point in two dimensions, we'd like a random number generator
; that yields a list of two random numbers. The m-seq operation can easily
; provide it:
(with-monad state-m
   (def rng2 (m-seq (list rng rng))))

; Let's test it:
(rng2 1)

; fetch-state and get-state can be used to save the seed of the random
; number generator and go back to that saved seed later on:
(def identical-random-seqs
  (domonad state-m
    [seed (fetch-state)
     x1   rng
     x2   rng
     _    (set-state seed)
     y1   rng
     y2   rng]
    (list [x1 x2] [y1 y2])))

(identical-random-seqs 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Logging with the writer monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A basic logging example
(domonad (writer-m accu/empty-string)
  [x (m-result 1)
   _ (write "first step\n")
   y (m-result 2)
   _ (write "second step\n")]
  (+ x y))

; For a more elaborate application, let's trace the recursive calls of
; a naive implementation of a Fibonacci function. The starting point is:
(defn fib [n]
  (if (< n 2)
    n
    (let [n1 (dec n)
	  n2 (dec n1)]
      (+ (fib n1) (fib n2)))))

; First we rewrite it to make every computational step explicit
; in a let expression:
(defn fib [n]
  (if (< n 2)
    n
    (let [n1 (dec n)
	  n2 (dec n1)
	  f1 (fib n1)
	  f2 (fib n2)]
      (+ f1 f2))))

; Next, we replace the let by a domonad in a writer monad that uses a
; vector accumulator. We can then place calls to write in between the
; steps, and obtain as a result both the return value of the function
; and the accumulated trace values.
(with-monad (writer-m accu/empty-vector)

  (defn fib-trace [n]
    (if (< n 2)
      (m-result n)
      (domonad
        [n1 (m-result (dec n))
	 n2 (m-result (dec n1))
	 f1 (fib-trace n1)
	 _  (write [n1 f1])
	 f2 (fib-trace n2)
	 _  (write [n2 f2])
	 ]
	(+ f1 f2))))

)

(fib-trace 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sequences with undefined value: the maybe-t monad transformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A monad transformer is a function that takes a monad argument and
; returns a monad as its result. The resulting monad adds some
; specific behaviour aspect to the input monad.

; The simplest monad transformer is maybe-t. It adds the functionality
; of the maybe monad (handling failures or undefined values) to any other
; monad. We illustrate this by applying maybe-t to the sequence monad.
; The result is an enhanced sequence monad in which undefined values
; (represented by nil) are not subjected to any transformation, but
; lead immediately to a nil result in the output.

; First we define the combined monad:
(def seq-maybe-m (maybe-t sequence-m))

; As a first illustration, we create a range of integers and replace
; all even values by nil, using a simple when expression. We use this
; sequence in a monad comprehension that yields (inc x). The result
; is a sequence in which inc has been applied to all non-nil values,
; whereas the nil values appear unmodified in the output:
(domonad seq-maybe-m
  [x  (for [n (range 10)] (when (odd? n) n))]
  (inc x))

; Next we repeat the definition of the function pairs (see above), but
; using the seq-maybe monad:
(with-monad seq-maybe-m
   (defn pairs-maybe [xs]
      (m-seq (list xs xs))))

; Applying this to a sequence containing nils yields the pairs of all
; non-nil values interspersed with nils that result from any combination
; in which one or both of the values is nil:
(pairs-maybe (for [n (range 5)] (when (odd? n) n)))

; It is important to realize that undefined values (nil) are not eliminated
; from the iterations. They are simply not passed on to any operations.
; The outcome of any function applied to arguments of which at least one
; is nil is supposed to be nil as well, and the function is never called.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Continuation-passing style in the cont monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A simple computation performed in continuation-passing style.
; (m-result 1) returns a function that, when called with a single
; argument f, calls (f 1). The result of the domonad-computation is
; a function that behaves in the same way, passing 3 to its function
; argument. run-cont executes a continuation by calling it on identity.
(run-cont
  (domonad cont-m
    [x (m-result 1)
     y (m-result 2)]
    (+ x y)))

; Let's capture a continuation using call-cc. We store it in a global
; variable so that we can do with it whatever we want. The computation
; is the same one as in the first example, but it has the side effect
; of storing the continuation at (m-result 2).
(def continuation nil)

(run-cont
  (domonad cont-m
    [x (m-result 1)
     y (call-cc (fn [c] (def continuation c) (c 2)))]
    (+ x y)))

; Now we can call the continuation with whatever argument we want. The
; supplied argument takes the place of 2 in the above computation:
(run-cont (continuation 5))
(run-cont (continuation 42))
(run-cont (continuation -1))

; Next, a function that illustrates how a captured continuation can be
; used as an "emergency exit" out of a computation:
(defn sqrt-as-str [x]
  (call-cc
   (fn [k]
     (domonad cont-m
       [_ (m-when (< x 0) (k (str "negative argument " x)))]
       (str (. Math sqrt x))))))

(run-cont (sqrt-as-str 2))
(run-cont (sqrt-as-str -2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
