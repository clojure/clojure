;; Stream utilities

;; by Konrad Hinsen
;; last updated February 23, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.stream-utils
  "Functions for setting up computational pipelines via data streams.

   NOTE: This library is experimental. It may change significantly
   with future release.

   This library defines:
   - the multimethod stream-next as consumer interface to data streams
   - implementations of stream-next for three data stream types:
     1) Clojure seqs, sequences, and vectors
     2) Stream generator functions
     3) nil representing an empty stream
   - the monad stream-m for writing stream transformers
   - macros defst-seq and defst-gen for writing monad-based stream
     transformers with a seq or a generator function interface
   - various utility functions for working with data streams

   Stream generator functions are called with a single argument representing
   the end-of-stream sentinel value. At each call, they return the next
   element of their data stream and the new stream state. When the end of
   the stream is reached, they return the passed-in end-of-stream object.
   The new stream state is typically a closure. While stream generator
   functions are less flexible than lazy seqs (because they cannot be used
   with Clojure's seq-handling functions), they have a few advantages:
   - The data stream is never cached.
   - The state of a stream can be stored in any Clojure data structure,
     and the stream can be re-generated from it any number of times.
   Nothing prevents a generator function from storing the stream state
   in a mutable data structure and just return itself as the new state,
   but such functions are neither thread-safe nor safe to be used to
   reproduce their stream more than once.
   
   Stream transformers take any number of input streams and produce one
   output stream. They are typically written using the stream-m
   monad. Input streams can be defined by any value that stream-next
   can handle. The output stream can have a (lazy) seq interface or
   a generator function interface. In the definition of a stream
   transformer, (pick s) returns the next value of stream argument s,
   whereas pick-all returns the next value of all stream arguments
   in the form of a vector."

  (:use [clojure.contrib.monads])
  (:use [clojure.contrib.macros :only (letfn-kh)]))


(let [eos (Object.)
      skip (Object.)]

  (defn stream-eos?
    "Returns true if x is the special end-of-stream value used by
     stream-next and the stream-m monad."
    [x]
    (identical? x eos))

  (defn stream-skip?
    "Returns true if x is the special skip value used by the stream-m monad."
    [x]
    (identical? x skip))

  (defmulti stream-next
    "Returns a vector of length two whose first element is the next
     item in the data stream defined by stream-state and whose second
     element is the new state of the stream. At the end of the stream,
     the returned value is a special end-of-stream object for which
     stream-eos? returns true."
    {:arglists '([stream-state])}
    class)

  (defmethod stream-next nil
    [s]
    [eos nil])

  (defmethod stream-next clojure.lang.ISeq
    [s]
    (if (seq s)
      [(first s) (rest s)]
      [eos nil]))

  (defmethod stream-next clojure.lang.IPersistentVector
    [v]
    (stream-next (seq v)))

  (defmethod stream-next clojure.lang.Fn
    [g]
    (g eos))

  (defmonad stream-m
     "Monad describing stream computations. The monadic values can be
      of any type handled by stream-next."
     [m-result  (fn m-result-stream [v]
  	        (fn [s] [v s]))
      m-bind    (fn m-bind-stream [mv f]
  		(fn [s]
  		  (let [[v ss] (mv s)]
  		    (if (or (stream-eos? v) (stream-skip? v))
  		      [v ss]
  		      ((f v) ss)))))
      m-zero     (fn [s] [skip s])
      ])

  (defn pick
    "Return the next value of stream argument n inside a stream
     transformer. When used inside of defst, the name of the stream
     argument can be used instead of its index n."
    [n]
    (fn [streams]
      (let [[v ns] (stream-next (streams n))]
	[v (assoc streams n ns)])))

  (defn pick-all
    "Return a vector containing the next value of each stream argument
     inside a stream transformer."
    [streams]
    (if (some nil? streams)
      [eos streams]
      (let [next    (map stream-next streams)
	    values  (map first next)
	    streams (vec (map second next))]
	(if (some stream-eos? values)
	  [eos streams]
	  [values streams]))))

  (defn st-as-seq
    "Takes a stream transformer expression st (typically written using the
     stream-m monad) and a vector of stream arguments and returns a lazy
     seq representing the output stream of the transformer."
    [st streams]
    (lazy-seq
     (loop [s streams]
       (let [[v ns] (st s)]
	 (cond (stream-eos? v) nil
	       (stream-skip? v) (recur ns)
	       :else (cons v (st-as-seq st ns)))))))

  (defn st-as-generator
    "Takes a stream transformer expression st (typically written using the
     stream-m monad) and a vector of stream arguments and returns a stream
     generator function representing the output stream of the transformer."
    [st streams]
    (letfn-kh [make-gen [s]
	    (fn [eos]
	      (loop [s s]
		(let [[v ns] (st s)]
		  (cond (stream-eos? v) [eos nil]
			(stream-skip? v) (recur ns)
			:else [v (make-gen ns)]))))]
      (make-gen streams)))
)

(defn- defst [wrapper name args streams body]
  (if (= (first streams) '&)
    `(defn ~name ~(vec (concat args streams))
       (let [~'st (with-monad stream-m ~@body)]
	 (~wrapper ~'st ~(second streams))))
    `(defn ~name ~(vec (concat args streams))
       (let [~'st (with-monad stream-m
	  	    (let [~streams (range ~(count streams))]
		      ~@body))]
	 (~wrapper ~'st ~streams)))))

(defmacro defst-seq
  "Define the seq-returning stream transformer name by body.
   The non-stream arguments args and the stream arguments streams
   are given separately, with args being possibly empty."
  [name args streams & body]
  (defst 'st-as-seq name args streams body))

(defmacro defst-gen
  "Define the generator-returning stream transformer name by body.
   The non-stream arguments args and the stream arguments streams
   are given separately, with args being possibly empty."
  [name args streams & body]
  (defst 'st-as-generator name args streams body))

(defn stream-drop
  "Return a stream containing all but the first n elements of stream."
  [n stream]
  (if (zero? n)
    stream
    (let [[_ s] (stream-next stream)]
      (recur (dec n) s))))

(defn stream-as-seq
  "Return a lazy seq of the stream s."
  [s]
  (lazy-seq
   (let [[v ns] (stream-next s)]
     (if (stream-eos? v)
       nil
       (cons v (stream-as-seq ns))))))

(defn stream-flatten
  "Converts a stream of sequences into a stream of the elements of the
   sequences. Flattening is not recursive, only one level of sequences
   will be removed."
  [s]
  (letfn-kh [buffer-gen [buffer stream]
	  (fn [eos]
	    (loop [buffer buffer
		   stream stream]
	      (if (nil? buffer)
		(let [[v new-stream] (stream-next stream)]
		  (cond (stream-eos? v) [eos nil]
			(empty? v) (recur nil new-stream)
			:else (recur v new-stream)))
		[(first buffer) (buffer-gen (next buffer) stream)])))]
    (buffer-gen nil s)))

