;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'parallel)
(clojure/refer 'clojure)

(import '(jsr166y.forkjoin ParallelArray ParallelArrayWithBounds ParallelArrayWithFilter 
                           ParallelArrayWithMapping 
                           Ops$Op Ops$BinaryOp Ops$Reducer Ops$Predicate Ops$BinaryPredicate Ops$IntAndObjectPredicate
                           Ops$IntAndObjectToObject))

(defn par [coll]
  (if (instance? ParallelArrayWithMapping coll)
    coll
    (ParallelArray.createUsingHandoff  
     (to-array coll) 
     (ParallelArray.defaultExecutor))))

(defn pall [coll]
  (if (instance? ParallelArrayWithMapping coll)
    (. coll all)
    (par coll)))

(defn pany [coll] 
  (. (par coll) any))

(defn pmax 
  ([coll] (. (par coll) max))
  ([coll comp] (. (par coll) max comp)))

(defn pmin
  ([coll] (. (par coll) min))
  ([coll comp] (. (par coll) min comp)))

(defn- summary-map [s]
  {:min (s.min) :max (s.max) :size (s.size) :min-index (s.indexOfMin) :max-index (s.indexOfMax)})

(defn psummary 
  ([coll] (summary-map (. (par coll) summary)))
  ([coll comp] (summary-map (. (par coll) summary comp))))

(defn punique [coll]
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
(defn pcumulate [coll f init]
  (.. (pall coll) (precumulate (reducer f) init)))

(defn preduce [f base coll]
  (. (par coll) (reduce (reducer f) base)))

(defn psort 
  ([coll] (. (pall coll) sort))
  ([coll comp] (. (pall coll) sort comp)))

(defn pfilter-nils [coll]
  (. (pall coll) removeNulls))

(defn pfilter-dupes [coll]
  (. (pall coll) removeConsecutiveDuplicates))

(defn with-bounds [coll start end]
  (. (par coll) withBounds start end))

(defn with-filter
  ([coll f]
     (. (par coll) withFilter (predicate f)))
  ([coll f2 coll2]
     (. (par coll) withFilter (binary-predicate f2) (par coll2))))

(defn with-indexed-filter
  ([coll f]
     (. (par coll) withIndexedFilter (int-and-object-predicate f))))

(defn with-mapping
  ([coll f]
     (. (par coll) withMapping (op f)))
  ([coll f2 coll2]
     (. (par coll) withMapping (binary-op f2) (par coll2))))

(defn with-indexed-mapping
  ([coll f]
     (. (par coll) withIndexedMapping (int-and-object-to-object f))))

(defn pvec [pa]
  (vec (. (pall pa) getArray)))

(comment

(punique [1 2 3 2 1])
(pcumulate [1 2 3 2 1] + 0) ;broken
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