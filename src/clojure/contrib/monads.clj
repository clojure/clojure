;; Monads in Clojure

;; by Konrad Hinsen
;; last updated December 30, 2008

;; Copyright (c) Konrad Hinsen, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.monads)

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
   `(let [~'m-zero nil ~'m-plus nil ~@operations]
      {:m-result ~'m-result :m-bind ~'m-bind 
       :m-zero ~'m-zero :m-plus ~'m-plus}))

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
      (list 'm-bind `(if ~expr (~'m-result nil) ~'m-zero)
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
   (reduce add-monad-step
     (list 'm-result expr)
     (reverse (partition 2 steps))))

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
				    (m-result (lazy-cons x y)))) )))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commonly used monads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Maybe monad
(defmonad maybe
   "Monad describing computations with possible failures. Failure is
    represented by an empty vector, success by a vector with a single
    element, the resulting value."
   [m-zero   []
    m-result (fn m-result-maybe [v]
	       [v])
    m-bind   (fn m-bind-maybe [mv f]
               (if (= mv m-zero)
                   m-zero
		   (f (first mv))))
    m-plus   (fn m-plus-maybe [& mvs]
	       (let [first-valid (first (drop-while empty? mvs))]
		 (if (nil? first-valid) m-zero first-valid)))
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
