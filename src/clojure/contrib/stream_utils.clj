;; Stream utilities

;; by Konrad Hinsen
;; last updated May 3, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns
  #^{:author "Konrad Hinsen"
     :doc "Functions for setting up computational pipelines via data streams.

           NOTE: This library is experimental. It may change significantly
                 with future release.

           This library defines:
           - an abstract stream type, whose interface consists of the
             multimethod stream-next
           - a macro for implementing streams
           - implementations of stream for
             1) Clojure sequences, and vectors
             2) nil, representing an empty stream
           - tools for writing stream transformers, including the
             monad stream-m
           - various utility functions for working with streams

           Streams are building blocks in the construction of computational
           pipelines. A stream is represented by its current state plus
           a function that takes a stream state and obtains the next item
           in the stream as well as the new stream state. The state is
           implemented as a Java class or a Clojure type (as defined by the
           function clojure.core/type), and the function is provided as an
           implementation of the multimethod stream-next for this class or type.

           While setting up pipelines using this mechanism is somewhat more
           cumbersome than using Clojure's lazy seq mechanisms, there are a
           few advantages:
           - The state of a stream can be stored in any Clojure data structure,
             and the stream can be re-generated from it any number of times.
             Any number of states can be stored this way.
           - The elements of the stream are never cached, so keeping a reference
             to a stream state does not incur an uncontrollable memory penalty.

           Note that the stream mechanism is thread-safe as long as the
           concrete stream implementations do not use any mutable state.

           Stream transformers take any number of input streams and produce one
           output stream. They are typically written using the stream-m
           monad. In the definition of a stream transformer, (pick s) returns
           the next value of stream argument s, whereas pick-all returns the
           next value of all stream arguments in the form of a vector."}
  clojure.contrib.stream-utils
  (:use [clojure.contrib.types :only (deftype deftype-)])
  (:use [clojure.contrib.monads :only (defmonad with-monad)])
  (:use [clojure.contrib.def :only (defvar defvar-)])
  (:require [clojure.contrib.seq-utils])
  (:require [clojure.contrib.generic.collection]))


;
; Stream type and interface
;
(defvar stream-type ::stream
  "The root type for the stream hierarchy. For each stream type,
   add a derivation from this type.")

(defmacro defstream
  "Define object of the given type as a stream whose implementation
   of stream-next is defined by args and body. This macro adds
   a type-specific method for stream-next and derives type
   from stream-type."
  [type-tag args & body]
  `(do
     (derive ~type-tag stream-type)
     (defmethod stream-next ~type-tag ~args ~@body)))

(defvar- stream-skip ::skip
  "The skip-this-item value.")

(defn- stream-skip?
  "Returns true if x is the stream-skip."
  [x]
  (identical? x stream-skip))

(defmulti stream-next
  "Returns a vector [next-value new-state] where next-value is the next
   item in the data stream defined by stream-state and new-state
   is the new state of the stream. At the end of the stream,
   next-value and new-state are nil."
  {:arglists '([stream-state])}
  type)

(defmethod stream-next nil
  [s]
  [nil nil])

(defmethod stream-next clojure.lang.ISeq
  [s]
  (if (seq s)
    [(first s) (rest s)]
    [nil nil]))

(defmethod stream-next clojure.lang.IPersistentVector
  [v]
  (stream-next (seq v)))

(defn stream-seq
  "Return a lazy seq on the stream. Also accessible via
   clojure.contrib.seq-utils/seq-on and
   clojure.contrib.generic.collection/seq for streams."
  [s]
  (lazy-seq
   (let [[v ns] (stream-next s)]
     (if (nil? ns)
       nil
       (cons v (stream-seq ns))))))

(defmethod clojure.contrib.seq-utils/seq-on stream-type
  [s]
  (stream-seq s))

(defmethod clojure.contrib.generic.collection/seq stream-type
  [s]
  (stream-seq s))

;
; Stream transformers
;
(defmonad stream-m
  "Monad describing stream computations. The monadic values can be
   of any type handled by stream-next."
  [m-result  (fn m-result-stream [v]
	       (fn [s] [v s]))
   m-bind    (fn m-bind-stream [mv f]
	       (fn [s]
		 (let [[v ss :as r] (mv s)]
		   (if (or (nil? ss) (stream-skip? v))
		     r
		     ((f v) ss)))))
   m-zero     (fn [s] [stream-skip s])
   ])

(defn pick
  "Return the next value of stream argument n inside a stream
   transformer. When used inside of defst, the name of the stream
   argument can be used instead of its index n."
  [n]
  (fn [streams]
    (let [[v ns] (stream-next (streams n))]
      (if (nil? ns)
	[nil nil]
	[v (assoc streams n ns)]))))

(defn pick-all
  "Return a vector containing the next value of each stream argument
   inside a stream transformer."
  [streams]
  (let [next    (map stream-next streams)
	values  (map first next)
	streams (vec (map second next))]
    (if (some nil? streams)
      [nil nil]
      [values streams])))

(deftype ::stream-transformer st-as-stream
  (fn [st streams] [st streams])
  seq)

(defstream ::stream-transformer
  [[st streams]]
  (loop [s streams]
    (let [[v ns] (st s)]
      (cond (nil? ns) [nil nil]
	    (stream-skip? v) (recur ns)
	    :else [v (st-as-stream st ns)]))))

(defmacro defst
  "Define the stream transformer name by body.
   The non-stream arguments args and the stream arguments streams
   are given separately, with args being possibly empty."
  [name args streams & body]
  (if (= (first streams) '&)
    `(defn ~name ~(vec (concat args streams))
       (let [~'st (with-monad stream-m ~@body)]
	 (st-as-stream ~'st ~(second streams))))
    `(defn ~name ~(vec (concat args streams))
       (let [~'st (with-monad stream-m
		    (let [~streams (range ~(count streams))]
		      ~@body))]
	 (st-as-stream ~'st ~streams)))))

;
; Stream utilities
;
(defn stream-drop
  "Return a stream containing all but the first n elements of stream."
  [n stream]
  (if (zero? n)
    stream
    (let [[_ s] (stream-next stream)]
      (recur (dec n) s))))

; Map a function on a stream
(deftype- ::stream-map stream-map-state)

(defstream ::stream-map
  [[f stream]]
  (let [[v ns] (stream-next stream)]
    (if (nil? ns)
      [nil nil]
      [(f v) (stream-map-state [f ns])])))

(defmulti stream-map
  "Return a new stream by mapping the function f on the given stream."
  {:arglists '([f stream])}
  (fn [f stream] (type stream)))

(defmethod stream-map :default
  [f stream]
  (stream-map-state [f stream]))

(defmethod stream-map ::stream-map
  [f [g stream]]
  (stream-map-state [(comp f g) stream]))

; Filter stream elements
(deftype- ::stream-filter stream-filter-state)

(defstream ::stream-filter
  [[p stream]]
  (loop [stream stream]
    (let [[v ns] (stream-next stream)]
      (cond (nil? ns) [nil nil]
	    (p v) [v (stream-filter-state [p ns])]
	    :else (recur ns)))))

(defmulti stream-filter
  "Return a new stream that contrains the elements of stream
   that satisfy the predicate p."
  {:arglists '([p stream])}
  (fn [p stream] (type stream)))

(defmethod stream-filter :default
  [p stream]
  (stream-filter-state [p stream]))

(defmethod stream-filter ::stream-filter
  [p [q stream]]
  (stream-filter-state [(fn [v] (and (q v) (p v))) stream]))

; Flatten a stream of sequences
(deftype- ::stream-flatten stream-flatten-state)

(defstream ::stream-flatten
  [[buffer stream]]
  (loop [buffer buffer
  	 stream stream]
    (if (nil? buffer)
      (let [[v new-stream] (stream-next stream)]
  	(cond (nil? new-stream) [nil nil]
  	      (empty? v) (recur nil new-stream)
  	      :else (recur v new-stream)))
      [(first buffer) (stream-flatten-state [(next buffer) stream])])))

(defn stream-flatten
  "Converts a stream of sequences into a stream of the elements of the
   sequences. Flattening is not recursive, only one level of nesting
   will be removed."
  [s]
  (stream-flatten-state [nil s]))
