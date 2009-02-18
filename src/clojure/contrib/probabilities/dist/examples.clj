;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Probability distribution application examples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use 'clojure.contrib.probabilities.dist
     'clojure.contrib.monads)
(require 'clojure.contrib.accumulators)

;; Simple examples using dice

; A single die is represented by a uniform distribution over the
; six possible outcomes.
(def die (uniform #{1 2 3 4 5 6}))

; The probability that the result is odd...
(prob odd? die)
; ... or greater than four.
(prob #(> % 4) die)

; The sum of two dice
(def two-dice (join-with + die die))
(prob #(> % 6) two-dice)

; The sum of two dice using a monad comprehension
(assert (= two-dice
	   (domonad dist-m
		    [d1 die
		     d2 die]
		    (+ d1 d2))))

; The two values separately, but as an ordered pair
(domonad dist-m
  [d1 die
   d2 die]
  (if (< d1 d2) (list d1 d2) (list d2 d1)))

; The conditional probability for two dice yielding X if X is odd:
(cond-prob odd? two-dice)

; A two-step experiment: throw a die, and then add 1 with probability 1/2
(domonad dist-m
  [d die
   x (choose (/ 1 2)  d
	     :else    (inc d))]
  x)

; The sum of n dice
(defn dice [n]
   (domonad dist-m
      [ds (m-seq (replicate n die))]
      (apply + ds)))

(assert (= two-dice (dice 2)))

(dice 3)


;; Construct an empirical distribution from counters

; Using an ordinary counter:
(def dist1
  (normalize
    (clojure.contrib.accumulators/add-items
      clojure.contrib.accumulators/empty-counter
      (for [_ (range 1000)] (rand-int 5)))))

; Or, more efficiently, using a counter that already keeps track of its total:
(def dist2
  (normalize
    (clojure.contrib.accumulators/add-items
      clojure.contrib.accumulators/empty-counter-with-total
      (for [_ (range 1000)] (rand-int 5)))))


;; The Monty Hall game
;; (see http://en.wikipedia.org/wiki/Monty_Hall_problem for a description)

; The set of doors. In the classical variant, there are three doors,
; but the code can also work with more than three doors.
(def doors #{:A :B :C})

; A simulation of the game, step by step:
(domonad dist-m
  [; The prize is hidden behind one of the doors.
   prize  (uniform doors)
   ; The player make his initial choice.
   choice (uniform doors)
   ; The host opens a door which is neither the prize door nor the
   ; one chosen by the player.
   opened (uniform (disj doors prize choice))
   ; If the player stays with his initial choice, the game ends and the
   ; following line should be commented out. It describes the switch from
   ; the initial choice to a door that is neither the opened one nor
   ; his original choice.
   choice (uniform (disj doors opened choice))
   ]
  ; If the chosen door has the prize behind it, the player wins.
  (if (= choice prize) :win :loose))


;; Tree growth simulation
;; Adapted from the code in:
;; Martin Erwig and Steve Kollmansberger,
;; "Probabilistic Functional Programming in Haskell",
;; Journal of Functional Programming, Vol. 16, No. 1, 21-34, 2006
;; http://web.engr.oregonstate.edu/~erwig/papers/abstracts.html#JFP06a

; A tree is represented by two attributes: its state (alive, hit, fallen),
; and its height (an integer). A new tree starts out alive and with zero height.
(def new-tree {:state :alive, :height 0})

; An evolution step in the simulation modifies alive trees only. They can
; either grow by one (90% probability), be hit by lightning and then stop
; growing (4% probability), or fall down (6% probability).
(defn evolve-1 [tree]
  (let [{s :state h :height} tree]
    (if (= s :alive)
      (choose 0.9   (assoc tree :height (inc (:height tree)))
	      0.04  (assoc tree :state :hit) 
	      :else {:state :fallen, :height 0})
      (certainly tree))))

; Multiple evolution steps can be chained together with m-chain,
; since each step's input is the output of the previous step.
(with-monad dist-m
  (defn evolve [n tree]
    ((m-chain (replicate n evolve-1)) tree)))

; Try it for zero, one, or two steps.
(evolve 0 new-tree)
(evolve 1 new-tree)
(evolve 2 new-tree)

; We can also get a distribution of the height only:
(with-monad dist-m
  ((m-lift 1 :height) (evolve 2 new-tree)))



;; Bayesian inference
;;
;; Suppose someone has three dice, one with six faces, one with eight, and
;; one with twelve. This person throws one die and gives us the number,
;; but doesn't tell us which die it was. What are the Bayesian probabilities
;; for each of the three dice, given the observation we have?

; A function that returns the distribution of a dice with n faces.
(defn die-n [n] (uniform (range 1 (inc n))))

; The three dice in the game with their distributions. With this map, we
; can easily calculate the probability for an observation under the
; condition that a particular die was used.
(def dice {:six     (die-n 6)
	   :eight   (die-n 8)
	   :twelve  (die-n 12)})

; The only prior knowledge is that one of the three dice is used, so we
; have no better than a uniform distribution to start with.
(def prior (uniform (keys dice)))

; Add a single observation to the information contained in the
; distribution. Adding an observation consists of
; 1) Draw a die from the prior distribution.
; 2) Draw an observation from the distribution of that die.
; 3) Eliminate (replace by nil) the trials that do not match the observation.
; 4) Normalize the distribution for the non-nil values.
(defn add-observation [prior observation]
  (normalize-cond
    (domonad cond-dist-m
      [die    prior
       number (get dice die)]
      (when (= number observation) die))))

; Add one observation.
(add-observation prior 1)

; Add three consecutive observations.
(-> prior (add-observation 1)
          (add-observation 3)
	  (add-observation 7))

; We can also add multiple observations in a single trial, but this
; is slower because more combinations have to be taken into account.
; With Bayesian inference, it is most efficient to eliminate choices
; as early as possible.
(defn add-observations [prior observations]
  (with-monad cond-dist-m
    (let [n-nums #(m-seq (replicate (count observations) (get dice %)))]
      (normalize-cond
        (domonad
          [die    prior
           nums   (n-nums die)]
	  (when (= nums observations) die))))))

(add-observations prior [1 3 7])
