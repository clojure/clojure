;; Monads in Clojure

;; by Konrad Hinsen
;; last updated February 15, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.monads
  (:require [clojure.contrib.accumulators]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Defining monads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro monad
   "Define a monad by defining the monad operations. The definitions
    are written like bindings to the monad operations m-bind and
    m-result (required) and m-zero and m-plus (optional)."
   [operations]
   `(let [~'m-bind   ::undefined
	  ~'m-result ::undefined
	  ~'m-zero   ::undefined
	  ~'m-plus   ::undefined
	  ~@operations]
      {:m-result ~'m-result
       :m-bind ~'m-bind 
       :m-zero ~'m-zero
       :m-plus ~'m-plus}))

(defmacro defmonad
   "Define a named monad by defining the monad operations. The definitions
    are written like bindings to the monad operations m-bind and
    m-result (required) and m-zero and m-plus (optional)."

   ([name doc-string operations]
    (let [doc-name (with-meta name {:doc doc-string})]
      `(defmonad ~doc-name ~operations)))

   ([name operations]
    `(def ~name (monad ~operations))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Using monads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- add-monad-step
  "Add a monad comprehension step before the already transformed
   monad comprehension expression mexpr."
  [mexpr step]
  (let [[bform expr] step]
    (if (identical? bform :when)
      (list 'm-bind `(if ~expr (~'m-result ::any) ~'m-zero)
	    (list 'fn ['_] mexpr))
      (list 'm-bind expr (list 'fn [bform] mexpr)))))

(defn- monad-expr
   "Transforms a monad comprehension, consisting of a list of steps
    and an expression defining the final value, into an expression
    chaining together the steps using :bind and returning the final value
    using :result. The steps are given as a vector of
    binding-variable/monadic-expression pairs."
   [steps expr]
   (when (odd? (count steps))
     (throw (Exception. "Odd number of elements in monad comprehension steps")))
   (let [rsteps (reverse (partition 2 steps))
	 [lr ls] (first rsteps)]
     (if (= lr expr)
       ; Optimization: if the result expression is equal to the result
       ; of the last computation step, we can eliminate an m-bind to
       ; m-result.
       (reduce add-monad-step
	       ls
	       (rest rsteps))
       ; The general case.
       (reduce add-monad-step
	       (list 'm-result expr)
	       rsteps))))

(defmacro with-monad
   "Evaluates an expression after replacing the keywords defining the
    monad operations by the functions associated with these keywords
    in the monad definition given by name."
   [name & exprs]
   `(let [~'m-bind   (:m-bind ~name)
	  ~'m-result (:m-result ~name)
	  ~'m-zero   (:m-zero ~name)
	  ~'m-plus   (:m-plus ~name)]
      (do ~@exprs)))

(defmacro domonad
   "Monad comprehension. Takes the name of a monad, a vector of steps
    given as binding-form/monadic-expression pairs, and a result value
    specified by expr. The monadic-expression terms can use the binding
    variables of the previous steps. If the monad contains a definition
    of :zero, the step list can also contain conditions of the form [:when p],
    where the predicate p can contain the binding variables from all previous
    steps."
   ([steps expr]
    (monad-expr steps expr))
   ([name steps expr]
    (let [mexpr (monad-expr steps expr)]
      `(with-monad ~name ~mexpr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Defining functions used with monads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defmonadfn
  "Like defn, but for functions that use monad operations and are used inside
   a with-monad block."

  ([name doc-string args expr]
    (let [doc-name (with-meta name {:doc doc-string})]
      `(defmonadfn ~doc-name ~args ~expr)))

  ([name args expr]
   (let [fn-name (symbol (format "m+%s+m" (str name)))]
   `(do
      (def ~fn-name nil)
      (defmacro ~name ~args
        (list (quote ~fn-name)
	      '~'m-bind '~'m-result '~'m-zero '~'m-plus
	      ~@args))
      (defn ~fn-name [~'m-bind ~'m-result ~'m-zero ~'m-plus ~@args] ~expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commonly used monad functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro m-lift
  "Converts a function f of n arguments into a function of n
  monadic arguments returning a monadic value."
  [n f]
  (let [expr (take n (repeatedly #(gensym "x_")))
	vars (vec (take n (repeatedly #(gensym "mv_"))))
	steps (vec (interleave expr vars))]
    (list `fn vars (monad-expr steps (cons f expr)))))

(defmonadfn m-join
  "Converts a monadic value containing a monadic value into a 'simple'
   monadic value."
  [m]
  (m-bind m identity))

(defmonadfn m-fmap
  "Bind the monadic value m to the function returning (f x) for argument x"
  [f m]
  (m-bind m (fn [x] (m-result (f x)))))

(defmonadfn m-seq
  "'Executes' the monadic values in ms and returns a sequence of the
   basic values contained in them."
  [ms]
  (reduce (fn [q p]
	    (m-bind p (fn [x]
			(m-bind q (fn [y]
				    (m-result (cons x y)))) )))
	  (m-result '())
	  (reverse ms)))

(defmonadfn m-map
  "'Executes' the sequence of monadic values resulting from mapping
   f onto the values xs. f must return a monadic value."
  [f xs]
  (m-seq (map f xs)))

(defmonadfn m-chain
  "Chains together monadic computation steps that are each functions
   of one parameter. Each step is called with the result of the previous
   step as its argument. (m-chain (step1 step2)) is equivalent to
   (fn [x] (domonad [r1 (step1 x) r2 (step2 r1)] r2))."
  [steps]
  (reduce (fn m-chain-link [chain-expr step]
	    (fn [v] (m-bind (chain-expr v) step)))
	  m-result
	  steps))

(defmacro m-when
  "If test if logical true, return monadic value m-expr, else return
   (m-result nil)."
  [test m-expr]
  `(if ~test ~m-expr (~'m-result nil)))

(defmacro m-when-not
  "If test if logical false, return monadic value m-expr, else return
   (m-result nil)."
  [test m-expr]
  `(if ~test (~'m-result nil) ~m-expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commonly used monads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Identity monad
(defmonad id
   "Monad describing plain computations. This monad does in fact nothing
    at all. It is useful for testing, for combination with monad
    transformers, and for code that is parameterized with a monad."
  [m-result identity
   m-bind   (fn m-result-id [mv f]
	      (f mv))
  ])

; Maybe monad
(defmonad maybe
   "Monad describing computations with possible failures. Failure is
    represented by nil, any other value is considered valid. As soon as
    a step returns nil, the whole computation will yield nil as well."
   [m-zero   nil
    m-result (fn m-result-maybe [v] v)
    m-bind   (fn m-bind-maybe [mv f]
               (if (nil? mv) nil (f mv)))
    m-plus   (fn m-plus-maybe [& mvs]
	       (first (drop-while nil? mvs)))
    ])

; Sequence monad (called "list monad" in Haskell)
(defmonad sequence
   "Monad describing multi-valued computations, i.e. computations
    that can yield multiple values. Any object implementing the seq
    protocol can be used as a monadic value."
   [m-result (fn m-result-sequence [v]
	       (list v))
    m-bind   (fn m-bind-sequence [mv f]
               (apply concat (map f mv)))
    m-zero   (list)
    m-plus   (fn m-plus-sequence [& mvs]
               (apply concat mvs))
    ])

; State monad
(defmonad state
   "Monad describing stateful computations. The monadic values have the
    structure (fn [old-state] (list result new-state))."
   [m-result  (fn m-result-state [v]
	        (fn [s] (list v s)))
    m-bind    (fn m-bind-state [mv f]
	        (fn [s]
		  (let [[v ss] (mv s)]
		    ((f v) ss))))
   ])

(defn update-state [f]
  (fn [s] (list s (f s))))

(defn set-state [s]
  (update-state (fn [_] s)))

(defn fetch-state []
  (update-state identity))

; Writer monad
(defn writer
  "Monad describing computations that accumulate data on the side, e.g. for
   logging. The monadic values have the structure [value log]. Any of the
   accumulators from clojure.contrib.accumulators can be used for storing the
   log data. Its empty value is passed as a parameter."
  [empty-accumulator]
  (monad
     [m-result  (fn m-result-writer [v]
	          [v empty-accumulator])
      m-bind    (fn m-bind-writer [mv f]
	          (let [[v1 a1] mv
			[v2 a2] (f v1)]
		    [v2 (clojure.contrib.accumulators/combine a1 a2)]))
     ]))

(defmonadfn write [v]
  (let [[_ a] (m-result nil)]
    [nil (clojure.contrib.accumulators/add a v)]))

(defn listen [mv]
  (let [[v a] mv] [[v a] a]))

(defn censor [f mv]
  (let [[v a] mv] [v (f a)]))

; Continuation monad

(defmonad cont
  "Monad describing computations in continuation-passing style. The monadic
   values are functions that are called with a single argument representing
   the continuation of the computation, to which they pass their result."
  [m-result   (fn m-result-cont [v]
		(fn [c] (c v)))
   m-bind     (fn m-bind-cont [mv f]
		(fn [c]
		  (mv (fn [v] ((f v) c)))))
   ])

(defn run-cont
  "Execute the computation c in the cont monad and return its result."
  [c]
  (c identity))

(defn call-cc
  "A computation in the cont monad that calls function f with a single
   argument representing the current continuation. The function f should
   return a continuation (which becomes the return value of call-cc),
   or call the passed-in current continuation to terminate."
  [f]
  (fn [c]
    (let [cc (fn cc [a] (fn [_] (c a)))
	  rc (f cc)]
      (rc c))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Monad transformers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn maybe-t
  "Monad transformer that transforms a monad m into a monad in which
   the base values can be invalid (represented by nothing, which defaults
   to nil). The third argument chooses if m-zero and m-plus are inherited
   from the base monad (use :m-plus-from-base) or adopt maybe-like
   behaviour (use :m-plus-from-maybe)."
  ([m] (maybe-t m nil :m-plus-from-base))
  ([m nothing which-m-plus]
   (let [combined-m-zero
	 (cond
	  (identical? which-m-plus :m-plus-from-base)
	  (with-monad m m-zero)
	  (identical? which-m-plus :m-plus-from-maybe)
	  (with-monad m (m-result nothing))
	  :else ::undefined)
	 combined-m-plus
	 (cond
	  (identical? which-m-plus :m-plus-from-base)
	  (with-monad m m-plus)
	  (identical? which-m-plus :m-plus-from-maybe)
	  (with-monad m
	    (fn [& mvs]
	      (m-result (loop [mv (first mvs)]
			  (if (nil? mv)
			    nothing
			    (let [v (m-bind mv identity)]
			      (if (identical? v nothing)
				(recur (rest mvs))
				v)))))))
	  :else ::undefined)]
     (monad [m-result (with-monad m
		        m-result)
	     m-bind   (with-monad m
		        (fn m-bind-maybe-t [mv f]
			  (m-bind mv
				  (fn [x]
				    (if (identical? x nothing)
				      (m-result nothing)
				      (f x))))))
	     m-zero   combined-m-zero
	     m-plus   combined-m-plus
	     ]))))

(defn sequence-t
  "Monad transformer that transforms a monad m into a monad in which
   the base values are sequences."
  [m]
  (monad [m-result (with-monad m
		     (fn m-result-sequence-t [v]
		       (m-result (list v))))
	  m-bind   (with-monad m
		     (fn m-bind-sequence-t [mv f]
		       (m-bind mv
			       (fn [xs]
				 (apply concat (map f xs))))))
	  ]))
