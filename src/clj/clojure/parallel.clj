;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "DEPRECATED Wrapper of the ForkJoin library (JSR-166)."
       :author "Rich Hickey"}
    clojure.parallel)
(alias 'parallel 'clojure.parallel)

(comment "
The parallel library wraps the ForkJoin library scheduled for inclusion in JDK 7:

http://gee.cs.oswego.edu/dl/concurrency-interest/index.html

You'll need jsr166y.jar in your classpath in order to use this
library.  The basic idea is that Clojure collections, and most
efficiently vectors, can be turned into parallel arrays for use by
this library with the function par, although most of the functions
take collections and will call par if needed, so normally you will
only need to call par explicitly in order to attach bound/filter/map
ops. Parallel arrays support the attachment of bounds, filters and
mapping functions prior to realization/calculation, which happens as
the result of any of several operations on the
array (pvec/psort/pfilter-nils/pfilter-dupes). Rather than perform
composite operations in steps, as would normally be done with
sequences, maps and filters are instead attached and thus composed by
providing ops to par. Note that there is an order sensitivity to the
attachments - bounds precede filters precede mappings.  All operations
then happen in parallel, using multiple threads and a sophisticated
work-stealing system supported by fork-join, either when the array is
realized, or to perform aggregate operations like preduce/pmin/pmax
etc. A parallel array can be realized into a Clojure vector using
pvec.
")

(import '(jsr166y.forkjoin ParallelArray ParallelArrayWithBounds ParallelArrayWithFilter 
                           ParallelArrayWithMapping 
                           Ops$Op Ops$BinaryOp Ops$Reducer Ops$Predicate Ops$BinaryPredicate 
                           Ops$IntAndObjectPredicate Ops$IntAndObjectToObject))

(defn- op [f]
  (proxy [Ops$Op] []
    (op [x] (f x))))

(defn- binary-op [f]
  (proxy [Ops$BinaryOp] []
    (op [x y] (f x y))))

(defn- int-and-object-to-object [f]
  (proxy [Ops$IntAndObjectToObject] []
    (op [i x] (f x i))))

(defn- reducer [f]
  (proxy [Ops$Reducer] []
    (op [x y] (f x y))))

(defn- predicate [f]
  (proxy [Ops$Predicate] []
    (op [x] (boolean (f x)))))

(defn- binary-predicate [f]
  (proxy [Ops$BinaryPredicate] []
    (op [x y] (boolean (f x y)))))

(defn- int-and-object-predicate [f]
  (proxy [Ops$IntAndObjectPredicate] []
    (op [i x] (boolean (f x i)))))

(defn par
  "Creates a parallel array from coll. ops, if supplied, perform
  on-the-fly filtering or transformations during parallel realization
  or calculation. ops form a chain, and bounds must precede filters,
  must precede maps. ops must be a set of keyword value pairs of the
  following forms:

     :bound [start end] 

  Only elements from start (inclusive) to end (exclusive) will be
  processed when the array is realized.

     :filter pred 

  Filter preds remove elements from processing when the array is realized. pred
  must be a function of one argument whose return will be processed
  via boolean.

     :filter-index pred2 

  pred2 must be a function of two arguments, which will be an element
  of the collection and the corresponding index, whose return will be
  processed via boolean.

     :filter-with [pred2 coll2] 

  pred2 must be a function of two arguments, which will be
  corresponding elements of the 2 collections.

     :map f 

  Map fns will be used to transform elements when the array is
  realized. f must be a function of one argument.

     :map-index f2 

  f2 must be a function of two arguments, which will be an element of
  the collection and the corresponding index.

     :map-with [f2 coll2]

  f2 must be a function of two arguments, which will be corresponding
  elements of the 2 collections."

  ([coll] 
     (if (instance? ParallelArrayWithMapping coll)
       coll
       (. ParallelArray createUsingHandoff  
        (to-array coll) 
        (. ParallelArray defaultExecutor))))
  ([coll & ops]
     (reduce (fn [pa [op args]] 
                 (cond
                  (= op :bound) (. pa withBounds (args 0) (args 1))
                  (= op :filter) (. pa withFilter (predicate args))
                  (= op :filter-with) (. pa withFilter (binary-predicate (args 0)) (par (args 1)))
                  (= op :filter-index) (. pa withIndexedFilter (int-and-object-predicate args))
                  (= op :map) (. pa withMapping (parallel/op args))
                  (= op :map-with) (. pa withMapping (binary-op (args 0)) (par (args 1)))
                  (= op :map-index) (. pa withIndexedMapping (int-and-object-to-object args))
                  :else (throw (Exception. (str "Unsupported par op: " op)))))
             (par coll) 
             (partition 2 ops))))

;;;;;;;;;;;;;;;;;;;;; aggregate operations ;;;;;;;;;;;;;;;;;;;;;;
(defn pany
  "Returns some (random) element of the coll if it satisfies the bound/filter/map"
  [coll] 
  (. (par coll) any))

(defn pmax
  "Returns the maximum element, presuming Comparable elements, unless
  a Comparator comp is supplied"
  ([coll] (. (par coll) max))
  ([coll comp] (. (par coll) max comp)))

(defn pmin
  "Returns the minimum element, presuming Comparable elements, unless
  a Comparator comp is supplied"
  ([coll] (. (par coll) min))
  ([coll comp] (. (par coll) min comp)))

(defn- summary-map [s]
  {:min (.min s) :max (.max s) :size (.size s) :min-index (.indexOfMin s) :max-index (.indexOfMax s)})

(defn psummary 
  "Returns a map of summary statistics (min. max, size, min-index, max-index, 
  presuming Comparable elements, unless a Comparator comp is supplied"
  ([coll] (summary-map (. (par coll) summary)))
  ([coll comp] (summary-map (. (par coll) summary comp))))

(defn preduce 
  "Returns the reduction of the realized elements of coll
  using function f. Note f will not necessarily be called
  consecutively, and so must be commutative. Also note that 
  (f base an-element) might be performed many times, i.e. base is not
  an initial value as with sequential reduce."
  [f base coll]
  (. (par coll) (reduce (reducer f) base)))

;;;;;;;;;;;;;;;;;;;;; collection-producing operations ;;;;;;;;;;;;;;;;;;;;;;

(defn- pa-to-vec [pa]
  (vec (. pa getArray)))

(defn- pall
  "Realizes a copy of the coll as a parallel array, with any bounds/filters/maps applied"
  [coll]
  (if (instance? ParallelArrayWithMapping coll)
    (. coll all)
    (par coll)))

(defn pvec 
  "Returns the realized contents of the parallel array pa as a Clojure vector"
  [pa] (pa-to-vec (pall pa)))

(defn pdistinct
  "Returns a parallel array of the distinct elements of coll"
  [coll]
  (pa-to-vec (. (pall coll) allUniqueElements)))

;this doesn't work, passes null to reducer?
(defn- pcumulate [coll f init]
  (.. (pall coll) (precumulate (reducer f) init)))

(defn psort 
  "Returns a new vector consisting of the realized items in coll, sorted, 
  presuming Comparable elements, unless a Comparator comp is supplied"
  ([coll] (pa-to-vec (. (pall coll) sort)))
  ([coll comp] (pa-to-vec (. (pall coll) sort comp))))

(defn pfilter-nils
  "Returns a vector containing the non-nil (realized) elements of coll"
  [coll]
  (pa-to-vec (. (pall coll) removeNulls)))

(defn pfilter-dupes 
  "Returns a vector containing the (realized) elements of coll, 
  without any consecutive duplicates"
  [coll]
  (pa-to-vec (. (pall coll) removeConsecutiveDuplicates)))


(comment
(load-file "src/parallel.clj")
(refer 'parallel)
(pdistinct [1 2 3 2 1])
;(pcumulate [1 2 3 2 1] + 0) ;broken, not exposed
(def a (make-array Object 1000000))
(dotimes i (count a)
  (aset a i (rand-int i)))
(time (reduce + 0 a))
(time (preduce + 0 a))
(time (count (distinct a)))
(time (count (pdistinct a)))

(preduce + 0 [1 2 3 2 1])
(preduce + 0 (psort a))
(pvec (par [11 2 3 2] :filter-index (fn [x i] (> i x))))
(pvec (par [11 2 3 2] :filter-with [(fn [x y] (> y x)) [110 2 33 2]]))

(psummary ;or pvec/pmax etc
 (par [11 2 3 2] 
      :filter-with [(fn [x y] (> y x)) 
                    [110 2 33 2]]
      :map #(* % 2)))

(preduce + 0
  (par [11 2 3 2] 
       :filter-with [< [110 2 33 2]]))

(time (reduce + 0 (map #(* % %) (range 1000000))))
(time (preduce + 0 (par (range 1000000) :map-index *)))
(def v (range 1000000))
(time (preduce + 0 (par v :map-index *)))
(time (preduce + 0 (par v :map  #(* % %))))
(time (reduce + 0 (map #(* % %) v)))
)