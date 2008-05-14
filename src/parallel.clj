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
                           ParallelArrayWithMapping Ops$Reducer Ops$Predicate))

(defn parray [coll]
  (if (instance? ParallelArrayWithMapping coll)
    coll
    (ParallelArray.createUsingHandoff 
     (if (identical? (class coll) (class (clojure.lang.RT.EMPTY_ARRAY)))
       coll 
       (to-array coll)) 
     (ParallelArray.defaultExecutor))))

(defn- pall [coll]
  (if (instance? ParallelArrayWithMapping coll)
    (. coll all)
    (parray coll)))

(defn punique [coll]
  (. (parray coll) allUniqueElements))

(defn- reducer [f]
  (proxy [Ops$Reducer] []
    (op [x y] (f x y))))

(defn- predicate [f]
  (proxy [Ops$Predicate] []
    (op [x] (boolean (f x)))))

;this doesn't work, passes null to reducer
(defn pcumulate [coll f init]
  (.. (pall coll) (cumulate (reducer f) init)))

(defn preduce [f init coll]
  (. (parray coll) (reduce (reducer f) init)))

(defn psort [coll]
  (. (pall coll) sort))

(defn pbound [coll start end]
  (. (parray coll) withBounds start end))

(defn pfilter
  ([coll f]
     (. (parray coll) withFilter (predicate f))))

(defn pfilter
  ([coll f]
     (. (parray coll) withFilter (predicate f))))

(comment

(punique [1 2 3 2 1])
(def v (range 10000000))
(def a (make-array Object 1000000))
(dotimes i (count a)
  (aset a i i))
(time (reduce + 0 a))
(time (preduce + 0 a))
(preduce + 0 [1 2 3 2 1])
(preduce + 0 (psort a))
)