;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'parallel)
(clojure/refer 'clojure)

(comment "
The parallel library wraps the ForkJoin library scheduled for inclusion in JDK 7:

http://gee.cs.oswego.edu/dl/concurrency-interest/index.html

You'll need jsr166y.jar in your classpath in order to use this
library.  The basic idea is that Clojure collections, and most
efficiently vectors, can be turned into parallel arrays for use by
this library with the function par, although most of the functions
take collections and will call par if needed. Parallel arrays support
the attachment of bounds, filters and mapping functions prior to
realization, which happens as the result of any of several operations
on the array (pall/psort/pfilter-nils/pfilter-dupes/pvec). Rather than
perform composite operations in steps, as would normally be done with
sequences, maps and filters are instead attached and thus composed
with the with-* functions. Note that there is an order sensitivity to
the attachments - bounds precede filters precede mappings.  All
operations then happen in parallel, using multiple threads and a
sophisticated work-stealing system supported by fork-join, either when
the array is realized, or to perform aggregate operations like
preduce/pmin/pmax etc. A parallel array can be realized into a Clojure
vector using pvec.
")

(import '(jsr166y.forkjoin ParallelArray ParallelArrayWithBounds ParallelArrayWithFilter 
                           ParallelArrayWithMapping 
                           Ops$Op Ops$BinaryOp Ops$Reducer Ops$Predicate Ops$BinaryPredicate 
                           Ops$IntAndObjectPredicate Ops$IntAndObjectToObject))

(defn par
  "Creates a parallel array from coll if it is not already one"
  [coll]
  (if (instance? ParallelArrayWithMapping coll)
    coll
    (ParallelArray.createUsingHandoff  
     (to-array coll) 
     (ParallelArray.defaultExecutor))))

(defn pall
  "Realizes a copy of the coll as a parallel array, with any bounds/filters/maps applied"
  [coll]
  (if (instance? ParallelArrayWithMapping coll)
    (. coll all)
    (par coll)))

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
  "Returns the maximum element, presuming Comparable elements, unless
  a Comparator comp is supplied"
  ([coll] (. (par coll) min))
  ([coll comp] (. (par coll) min comp)))

(defn- summary-map [s]
  {:min (s.min) :max (s.max) :size (s.size) :min-index (s.indexOfMin) :max-index (s.indexOfMax)})

(defn psummary 
  "Returns a map of summary statistics (min. max, size, min-index, max-index, 
  presuming Comparable elements, unless a Comparator comp is supplied"
  ([coll] (summary-map (. (par coll) summary)))
  ([coll comp] (summary-map (. (par coll) summary comp))))

(defn pdistinct
  "Returns a parallel array of the distinct elements of coll"
  [coll]
  (. (par coll) allUniqueElements))

(defn- op [f]
  (proxy [Ops$Op] []
    (op [x] (f x))))

(defn- binary-op [f]
  (proxy [Ops$BinaryOp] []
    (op [x y] (f x y))))

(defn- int-and-object-to-object [f]
  (proxy [Ops$IntAndObjectToObject] []
    (op [i x] (f i x))))

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
    (op [i x] (boolean (f i x)))))

;this doesn't work, passes null to reducer?
(defn- pcumulate [coll f init]
  (.. (pall coll) (precumulate (reducer f) init)))

(defn preduce 
  "Returns a parallel array of the reduction of the realized elements of coll
  using function f. Note f will not necessarily be called
  consecutively, and so must be commutative. Also note that 
  (f base an-element) might be performed many times, i.e. base is not
  an initial value as with sequential reduce. Returns a new parallel
  array."
  [f base coll]
  (. (par coll) (reduce (reducer f) base)))

(defn psort 
  "Returns a new parallel array consisting of the realized items in coll, sorted, 
  presuming Comparable elements, unless a Comparator comp is supplied"
  ([coll] (. (pall coll) sort))
  ([coll comp] (. (pall coll) sort comp)))

(defn pfilter-nils
  "Returns a parallel array containing the non-nil (realized) elements of coll"
  [coll]
  (. (pall coll) removeNulls))

(defn pfilter-dupes 
  "Returns a parallel array containing the (realized) elements of coll, 
  without any consecutive duplicates"
  [coll]
  (. (pall coll) removeConsecutiveDuplicates))

(defn with-bounds
  "Returns a parallel array with the bounds attached. Note bounds must
  be attached before filters or mappings. Only elements from
  start (inclusive) to end (exclusive) will be processed when the
  array is realized."
  [coll start end]
  (. (par coll) withBounds start end))

(defn with-filter
  "Returns a parallel array with the filter attached. Note filters
  must be attached before mappings. pred must be a function of one
  argument whose return will be processed via boolean. pred2 must be a
  function of two arguments, which will be corresponding elements of
  the 2 collections. The predicate will be used to remove elements
  from processing when the array is realized."
  ([coll pred]
     (. (par coll) withFilter (predicate pred)))
  ([coll pred2 coll2]
     (. (par coll) withFilter (binary-predicate pred2) (par coll2))))

(defn with-indexed-filter
  "Returns a parallel array with the filter attached. Note filters
  must be attached before mappings. pred2 must be a function of two
  arguments, which will be an index and the corresponding element of
  the collection, whose return will be processed via boolean. The
  predicate will be used to remove elements from processing when
  the array is realized."
  ([coll pred2]
     (. (par coll) withIndexedFilter (int-and-object-predicate pred2))))

(defn with-mapping
  "Returns a parallel array with the mapping function attached.  f
  must be a function of one argument. f2 must be a function of two
  arguments, which will be corresponding elements of the 2
  collections. The mapping fn will be used to transform elements when
  the array is realized."
  ([coll f]
     (. (par coll) withMapping (op f)))
  ([coll f2 coll2]
     (. (par coll) withMapping (binary-op f2) (par coll2))))

(defn with-indexed-mapping
  "Returns a parallel array with the mapping function attached. f2
  must be a function of two arguments, which will be an index and the
  corresponding element of the collection. The mapping fn will be used
  to transform elements when the array is realized."
  ([coll f2]
     (. (par coll) withIndexedMapping (int-and-object-to-object f2))))

(defn pvec 
  "Returns the realized contents of the parallel array pa as a Clojure vector"
  [pa] (vec (. (pall pa) getArray)))


(comment
(refer 'parallel)
(pdistinct [1 2 3 2 1])
;(pcumulate [1 2 3 2 1] + 0) ;broken, not exposed
(def a (make-array Object 1000000))
(dotimes i (count a)
  (aset a i i))
(time (reduce + 0 a))
(time (preduce + 0 a))
(preduce + 0 [1 2 3 2 1])
(preduce + 0 (psort a))
(pall (with-indexed-filter (par [11 2 3 2]) (fn [i x] (> i x))))
(pall (with-filter (par [11 2 3 2]) (fn [x y] (> y x)) (par [110 2 33 2])))

(psummary ;or pvec/pmax etc
 (-> (par [11 2 3 2]) 
     (with-filter (fn [x y] (> y x)) 
                  (par [110 2 33 2]))
     (with-mapping #(* % 2))))

(preduce + 0
  (-> (par [11 2 3 2]) 
      (with-filter (fn [x y] (> y x)) 
                   (par [110 2 33 2]))))
)