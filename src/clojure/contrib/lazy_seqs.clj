;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  lazy-seqs
;;
;;  == Lazy sequences ==
;;
;;  primes - based on the "naive" implemention described in [1] plus a
;;           small "wheel" which eliminates multiples of 2, 3, 5, and
;;           7 from consideration by incrementing past them. Also inspired
;;           by code from Christophe Grand in [2].
;;
;;  fibs   - attributed to 'every haskell beginner tutorial'
;;
;;  powers-of-2 - all the powers of 2
;;
;;  == Lazy sequence functions ==
;;
;;  (rotations, partition-all, shuffle, rand-elt  moved to seq_utils.clj)
;;  (permutations and combinations moved to combinatorics.clj)
;;
;;  [1] http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
;;  [2] http://clj-me.blogspot.com/2008/06/primes.html
;;
;;  scgilardi (gmail)
;;  Created 07 June 2008

(ns clojure.contrib.lazy-seqs
  (:use clojure.contrib.def))

(defvar primes
  (lazy-cat [2 3 5 7]
    (let [primes-from
          (fn primes-from [n [f & r]]
            (if (some #(zero? (rem n %))
                      (take-while #(<= (* % %) n) primes))
              (recur (+ n f) r)
              (lazy-cons n (primes-from (+ n f) r))))
          wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                        6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                        2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))
  "A lazy sequence of all the prime numbers.")

(defvar fibs
  (lazy-cat [0 1] (map + fibs (rest fibs)))
  "A lazy sequence of all the fibonacci numbers.")

(defvar powers-of-2
  (lazy-cons 1 (map #(bit-shift-left % 1) powers-of-2))
  "A lazy sequence of all the powers of 2")
