;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.core.protocols)

(set! *warn-on-reflection* true)

(defprotocol CollReduce
  "Protocol for collection types that can implement reduce faster than
  first/next recursion. Called by clojure.core/reduce. Baseline
  implementation defined in terms of Iterable."
  (coll-reduce [coll f] [coll f val]))

(defprotocol InternalReduce
  "Protocol for concrete seq types that can reduce themselves
   faster than first/next recursion. Called by clojure.core/reduce."
  (internal-reduce [seq f start]))

(defn- seq-reduce
  ([coll f]
     (if-let [s (seq coll)]
       (internal-reduce (next s) f (first s))
       (f)))
  ([coll f val]
     (let [s (seq coll)]
       (internal-reduce s f val))))

(defn- iter-reduce
  ([^java.lang.Iterable coll f]
   (let [iter (.iterator coll)]
     (if (.hasNext iter)
       (loop [ret (.next iter)]
         (if (.hasNext iter)
           (let [ret (f ret (.next iter))]
             (if (reduced? ret)
               @ret
               (recur ret)))
           ret))
       (f))))
  ([^java.lang.Iterable coll f val]
   (let [iter (.iterator coll)]
     (loop [ret val]
       (if (.hasNext iter)
         (let [ret (f ret (.next iter))]
           (if (reduced? ret)
             @ret
             (recur ret)))
         ret)))))

(defn- naive-seq-reduce
  "Reduces a seq, ignoring any opportunities to switch to a more
  specialized implementation."
  [s f val]
  (loop [s (seq s)
         val val]
    (if s
      (let [ret (f val (first s))]
        (if (reduced? ret)
          @ret
          (recur (next s) ret)))
      val)))

(defn- interface-or-naive-reduce
  "Reduces via IReduceInit if possible, else naively."
  [coll f val]
  (if (instance? clojure.lang.IReduceInit coll)
    (.reduce ^clojure.lang.IReduceInit coll f val)
    (naive-seq-reduce coll f val)))

(extend-protocol CollReduce
  nil
  (coll-reduce
   ([coll f] (f))
   ([coll f val] val))

  Object
  (coll-reduce
   ([coll f] (seq-reduce coll f))
   ([coll f val] (seq-reduce coll f val)))

  clojure.lang.IReduceInit
  (coll-reduce
    ([coll f] (.reduce ^clojure.lang.IReduce coll f))
    ([coll f val] (.reduce coll f val)))

  ;;aseqs are iterable, masking internal-reducers
  clojure.lang.ASeq
  (coll-reduce
   ([coll f] (seq-reduce coll f))
   ([coll f val] (seq-reduce coll f val)))

  ;;for range
  clojure.lang.LazySeq
  (coll-reduce
   ([coll f] (seq-reduce coll f))
   ([coll f val] (seq-reduce coll f val)))

  ;;vector's chunked seq is faster than its iter
  clojure.lang.PersistentVector
  (coll-reduce
   ([coll f] (seq-reduce coll f))
   ([coll f val] (seq-reduce coll f val)))
  
  Iterable
  (coll-reduce
   ([coll f] (iter-reduce coll f))
   ([coll f val] (iter-reduce coll f val)))

  clojure.lang.APersistentMap$KeySeq
  (coll-reduce
    ([coll f] (iter-reduce coll f))
    ([coll f val] (iter-reduce coll f val)))

  clojure.lang.APersistentMap$ValSeq
  (coll-reduce
    ([coll f] (iter-reduce coll f))
    ([coll f val] (iter-reduce coll f val))))

(extend-protocol InternalReduce
  nil
  (internal-reduce
   [s f val]
   val)
  
  ;; handles vectors and ranges
  clojure.lang.IChunkedSeq
  (internal-reduce
   [s f val]
   (if-let [s (seq s)]
     (if (chunked-seq? s)
       (let [ret (.reduce (chunk-first s) f val)]
         (if (reduced? ret)
           @ret
           (recur (chunk-next s)
                  f
                  ret)))
       (interface-or-naive-reduce s f val))
     val))
 
  clojure.lang.StringSeq
  (internal-reduce
   [str-seq f val]
   (let [s (.s str-seq)]
     (loop [i (.i str-seq)
            val val]
       (if (< i (.length s))
         (let [ret (f val (.charAt s i))]
                (if (reduced? ret)
                  @ret
                  (recur (inc i) ret)))
         val))))
  
  java.lang.Object
  (internal-reduce
   [s f val]
   (loop [cls (class s)
          s s
          f f
          val val]
     (if-let [s (seq s)]
       (if (identical? (class s) cls)
         (let [ret (f val (first s))]
                (if (reduced? ret)
                  @ret
                  (recur cls (next s) f ret)))
         (interface-or-naive-reduce s f val))
       val))))

(defprotocol IKVReduce
  "Protocol for concrete associative types that can reduce themselves
   via a function of key and val faster than first/next recursion over map
   entries. Called by clojure.core/reduce-kv, and has same
   semantics (just different arg order)."
  (kv-reduce [amap f init]))
