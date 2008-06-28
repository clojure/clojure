;;  Copyright (c) Stephen C. Gilardi. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.
;;
;;  lazy-seqs.clj
;;
;;  Lazy sequences
;;
;;  primes - based on the "naive" implemention described in [1] plus a
;;           small "wheel" which eliminates multiples of 2, 3, 5, and
;;           7 from consideration by incrementing past them. Also inspired
;;           by code from Christophe Grand in [2].
;;
;;  fibs   - based on code from Rich Hickey at the Clojure wiki [3]
;;
;;  powers-of-2 - all the powers of 2
;;
;;  [1] http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
;;  [2] http://clj-me.blogspot.com/2008/06/primes.html
;;  [3] http://en.wikibooks.org/wiki/Clojure_Programming#Examples
;;
;;  scgilardi (gmail)
;;  07 June 2008

(clojure/in-ns 'lazy-seqs)
(clojure/refer 'clojure)

(lib/use def)

(defvar primes
  (lazy-cat [2 3 5 7]
    (let [primes-from
          (fn primes-from [n [f & r]]
            (if (some #(zero? (rem n %)) (take-while #(<= (* % %) n) primes))
              (recur (+ n f) r)
              (lazy-cons n (primes-from (+ n f) r))))
          wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                        6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                        2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))
  "A lazy sequence of all the prime numbers.")

(defvar fibs
  (lazy-cat [0 1]
    (let [rest-fn
          (fn rest-fn [a b]
            (let [next (+ a b)]
              (lazy-cons next (rest-fn b next))))]
      (rest-fn 0 1)))
  "A lazy sequence of all the fibonacci numbers.")

(defvar powers-of-2
  (lazy-cons 1
    (let [rest-fn
          (fn rest-fn [n]
            (let [next (bit-shift-left n 1)]
              (lazy-cons next (rest-fn next))))]
      (rest-fn 1)))
  "A lazy sequence of all the powers of 2")
