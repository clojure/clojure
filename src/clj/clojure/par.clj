;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.par
    (:import (jsr166y ForkJoinTask ForkJoinPool RecursiveTask)
             (clojure.lang PersistentVector PersistentVector$Node FJTask)
             (java.util Collection)))

(def #^{:tag ForkJoinPool :private true} pool (ForkJoinPool.))

(defmacro fjtask [& body]
  `(FJTask. (fn [] ~@body)))

(defn- fjvtree [#^PersistentVector v combine-fn leaf-fn]
  (let [tfn (fn tfn [shift #^PersistentVector$Node node]
               (fjtask
                (let [nodes (remove nil? (.array node))]
                  (if (= shift PersistentVector/SHIFT)
                    (let [lts (reduce #(cons (doto (fjtask (leaf-fn %2)) .fork) %1)
                                      () nodes)]
                      (combine-fn (reduce (fn [rets #^FJTask t]
                                            (cons (if (.tryUnfork t)
                                                    (.compute t)
                                                    (do (.join t) (.get t))) rets))
                                          () lts)))      
                    (let [tasks (map #(tfn (- shift PersistentVector/SHIFT) %) nodes)]
                      (ForkJoinTask/invokeAll #^Collection tasks)
                      (combine-fn (map #(.get #^ForkJoinTask %) tasks)))))))
        task #^ForkJoinTask (tfn (.shift v) (.root v))]
    (if (ForkJoinTask/getPool) ;nested task
      (.invoke task)
      (.invoke pool task))))

(defn pvmap [f #^PersistentVector v]
  (let [new-node #(PersistentVector$Node. (.. v root edit) %)
        new-root (fjvtree v 
                   #(new-node (to-array %))
                   #(new-node 
                     (amap (.array #^PersistentVector$Node %) i a
                           (f (aget a i)))))
        new-tail (to-array (map f (.tail v)))]
    (PersistentVector. (.cnt v) (.shift v) new-root new-tail)))

(defn pvreduce [f #^PersistentVector v]
  (if (<= (count v) PersistentVector/CHUNK)
    (reduce f v)
    (let [tr (fjvtree v
               #(reduce f %)
               #(let [a (.array #^PersistentVector$Node %)]
                  (loop [ret (aget a 0) i (int 1)]
                    (if (< i PersistentVector/CHUNK)
                      (recur (f ret (aget a i)) (inc i))
                      ret))))]
      (f tr (reduce f (.tail v))))))

