;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.core.reduce
  (:refer-clojure :exclude [map filter remove take take-while drop flatten]))

(set! *warn-on-reflection* true)

(defn reducer
  "Given a reducible collection, and a transformation function xf,
  returns a reducible collection, where any supplied reducing
  fn will be transformed by xf. xf is a function of reducing fn to
  reducing fn."
  {:added "1.5"}
  [coll xf]
  (reify clojure.core.protocols/CollReduce
         (coll-reduce [_ f1]
                      (throw (Exception. "You must supply an init value for reducer-based reduce")))
         (coll-reduce [_ f1 init]
                      (clojure.core.protocols/coll-reduce coll (xf f1) init))))

(defn map
  "Applies f to every value in the reduction of coll"
  {:added "1.5"}
  [f coll]
  (reducer
   coll
   (fn [f1]
     (fn [ret v] (f1 ret (f v))))))

(defn filter
  "Retains values in the reduction of coll for which (pred val) returns logical true"
  {:added "1.5"}
  [pred coll]
  (reducer
   coll
   (fn [f1]
     (fn [ret v]
       (if (pred v)
         (f1 ret v)
         ret)))))

(defn remove
  "Removes values in the reduction of coll for which (pred val) returns logical true"
  {:added "1.5"}
  [pred coll]
  (filter (complement pred) coll))

(defn take-while
  "Ends the reduction of coll when (pred val) returns logical false."
  {:added "1.5"}
  [pred coll]
  (reducer
   coll
   (fn [f1]
     (fn [ret v]
       (if (pred v)
         (f1 ret v)
         (reduced ret))))))

(defn take
  "Ends the reduction of coll after consuming n values."
  {:added "1.5"}
  [n coll]
  (reducer
   coll
   (fn [f1]
     (let [cnt (atom n)]
       (fn [ret v]
         (swap! cnt dec)
         (if (neg? @cnt)
           (reduced ret)
           (f1 ret v)))))))

(defn drop
  "Elides the first n values from the reduction of coll."
  {:added "1.5"}
  [n coll]
  (reducer
   coll
   (fn [f1]
     (let [cnt (atom n)]
       (fn [ret v]
         (swap! cnt dec)
         (if (neg? @cnt)
           (f1 ret v)
           ret))))))

(defn flatten
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat reducible
  collection."
  {:added "1.5"}
  [coll]
  (reify clojure.core.protocols/CollReduce
         (coll-reduce [_ f1]
                      (throw (Exception. "You must supply an init value for reducer-based reduce")))
         (coll-reduce [_  f1 init]
                      (clojure.core.protocols/coll-reduce
                       coll
                       (fn [ret v]
                         (if (sequential? v)
                           (clojure.core.protocols/coll-reduce (flatten v) f1 ret)
                           (f1 ret v)))
                       init))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
(require '[clojure.core.reduce :as r])
(def v (take 1000000 (range)))
(reduce + 0 (r/map inc [1 2 3 4]))
(into [] (r/take 12 (range 100)))
(into [] (r/drop 12 (range 100)))
(reduce + 0 (r/filter even? [1 2 3 4]))
(into [] (r/filter even? [1 2 3 4]))
(reduce + (filter even? [1 2 3 4]))
(dotimes [_ 10] (time (reduce + 0 (r/map inc v))))
(dotimes [_ 10] (time (reduce + 0 (map inc v))))
(dotimes [_ 100] (time (reduce + 0 v)))
(dotimes [_ 100] (time (reduce + 0 v)))
(dotimes [_ 20] (time (reduce + 0 (r/map inc (r/filter even? v)))))
(dotimes [_ 20] (time (reduce + 0 (map inc (filter even? v)))))
(reduce + 0 (r/take-while even? [2 4 3]))
(into [] (r/filter even? (r/flatten (r/remove #{4} [[1 2 3] 4 [5 [6 7 8]] [9] 10]))))
(into [] (r/flatten nil))
)