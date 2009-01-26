;; Stream utilities

;; by Konrad Hinsen
;; last updated January 25, 2009

;; Note: this module is at least as experimental as Clojure's stream
;; facilities. It may change significantly, change name, or disappear
;; in the future. Don't rely on it for your applications. I put this
;; module in clojure.contrib so that it can serve as a testground and
;; a basis for discussions ont the group.

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.stream-utils
  (:use [clojure.contrib.monads]))

;; Streams

; Stream of integers
(defn int-stream
  "Return a stream of integers starting from init and incrementing by step
   until end, the first value not returned. The defaults are 0, 1, and nil,
   where nil stands for an infinite stream."
  ([] (int-stream 0 1 nil))
  ([init] (int-stream init 1 nil))
  ([init step] (int-stream init step nil))
  ([init step end]
   (let [n (into-array [init])]
     (stream
      (fn [eos]
	(let [current (aget n 0)]
	  (if (or (nil? end) (< current end))
	    (do
	      (aset n 0 (+ current step))
	      current)
	    eos)))))))

;; Stream transformers

(defn drop-first
  "Return a stream that returns the elements of s except for the first n ones."
  [n s]
  (let [eos (Object.)
	iter (stream-iter s)
	gen (fn [eos] (next! iter eos))]
    (doseq [_ (range n)] (gen eos))
    (stream gen)))

;; Monadic stream transformers

; Generator monad
;
; This monad represents computations with generators, as defined for
; Clojure streams: a function of one argument, an end-of-stream value,
; that returns at each call the "next" value of the stream, until the
; end of the stream is reached, and from then on it returns the supplied
; eos value forever. This monad permits the simple composition of such
; generators.
;
; The use of generators instead of streams as the basis of this monad
; has two reasons:
; 1) It is not possible to pass around a stream such that each
;    monadic computation step takes one item of it, because there can
;    be only one iterator per stream.
; 2) One could use iterators instead of generators as the monadic
;    values, even with minimal changes, but each computational step
;    in the monad would then create a new stream and a new iter,
;    adding overhead for no apparent benefit.

(defmonad generator
   "Monad describing stream-transforming computations. The monadic
    values are generator functions."
   [m-result (fn m-result-generator [v]
	       (fn [eos] v))
    m-bind   (fn m-bind-generator [mv f]
               (fn [eos]
		 (let [v (mv eos)]
		   (if (identical? v eos)
		     eos
		     ((f v) eos)))))
    ])

; A macro for defining stream transformers using the generator monad.
; It wraps the input streams into iters-that-look-like-generators,
; and converts the resulting generator into a stream.
(defmacro stream-transformer
  [stream-args & body]
  `(stream (let [~stream-args (map stream-iter ~stream-args)]
	     (with-monad generator ~@body))))


; Derive a variant of the generator monad that permits the
; use of :when conditions. Invalid values in the output are
; represented by ::skip, which is filtered out in the end
; by applying remove-skip. The macro stream-transformer-cond
; takes care of all these administrative details.
(def generator-cond (maybe-t generator ::skip :m-plus-from-maybe))

(defn remove-skip [g]
  (fn [eos]
    (loop [v (g eos)]
      (if (identical? v ::skip)
	(recur (g eos))
	v))))

(defmacro stream-transformer-cond
  [stream-args & body]
  `(stream (remove-skip
    (let [~stream-args (map stream-iter ~stream-args)]
      (with-monad generator-cond ~@body)))))


;; Some examples of monadic stream transformers

(defn skip-s [s]
  "Return a stream that skips every second item of s"
  (stream-transformer [s]
    (domonad
      [x1 s
       x2 s]
      x2)))

(defn partition-s [n s]
  "Return a stream in which each element is a sequence of n consecutive
   elements of the input stream s."
  (stream-transformer [s]
    (m-seq (replicate n s))))

; Map a function on streams.
; Map has to be defined by an explicit expression for every number of
; arguments because the m-lift macro requires a constant as its first
; argument.
(defn map-s
  "Return a stream of the results of applying f on one item of each
   input sequence."
  ([f s]
   (stream-transformer [s]
     ((m-lift 1 f) s)))
  ([f s1 s2]
   (stream-transformer [s1 s2]
     ((m-lift 2 f) s1 s2)))
  ([f s1 s2 s3]
   (stream-transformer [s1 s2 s3]
     ((m-lift 3 f) s1 s2 s3)))
  ([f s1 s2 s3 s4]
   (stream-transformer [s1 s2 s3 s4]
     ((m-lift 4 f) s1 s2 s3 s4)))
  ([f s1 s2 s3 s4 s5]
   (stream-transformer [s1 s2 s3 s4 s5]
     ((m-lift 5 f) s1 s2 s3 s4 s5))))

(defn filter-s [p s]
  "Return a stream consisting of the elements of s that satisfy predicate p."
  (stream-transformer-cond [s]
    (domonad
     [x s :when (p x)]
     x)))

(comment

; Some example expressions

(take 10 (drop-first 5 (int-stream)))

(take 10 (skip-s (int-stream)))

(take 10 (partition-s 3 (int-stream)))

(take 10 (map-s inc (int-stream)))
(take 10 (map-s + (int-stream) (map-s inc (int-stream))))

(take 10 (filter-s #(zero? (rem % 3)) (int-stream)))

)