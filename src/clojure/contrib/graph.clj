;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  graph
;;
;;  Basic Graph Theory Algorithms
;;
;;  straszheimjeffrey (gmail)
;;  Created 23 June 2009

(ns clojure.contrib.graph
  (use [clojure.contrib.macros :only (letfn)])
  (use [clojure.contrib.seq-utils :only (flatten indexed)]))



(defstruct directed-graph
  :count       ; The count of nodes in the graph
  :neighbors)  ; A function that, given a node (0 .. count-1) returns
               ; a collection neighbor nodes.

(defn get-neighbors
  "Get the neighbors of a node."
  [g n]
  ((:neighbors g) n))


(defn reverse-graph
  "Given a directed graph, return another directed graph with the
   order of the edges reversed."
  [g]
  (let [op (fn [rna idx]
             (let [ns (get-neighbors g idx)
                   am (fn [m val]
                        (assoc m val (conj (get m val []) idx)))]
               (reduce am rna ns)))
        rn (reduce op {} (range (:count g)))]
    (struct directed-graph (:count g) rn)))

(comment
  (def test-graph-1
       (struct directed-graph 5
               {0 [1 2]
                1 [0 2]
                2 [3 4]
                3 [0 1]
                4 [3]}))

  test-graph-1
  (reverse-graph test-graph-1)
  (reverse-graph (reverse-graph test-graph-1))
  (= test-graph-1 (reverse-graph (reverse-graph test-graph-1)))
)

(defn- post-ordered-visit
  "Starting at node n, perform a post-ordered walk."
  [g n [visited acc :as state]]
  (if (visited n)
    state
    (let [[v2 acc2] (reduce (fn [st nd] (post-ordered-visit g nd st))
                            [(conj visited n) acc]
                            (get-neighbors g n))]
      [v2 (conj acc2 n)])))
  
(defn post-ordered-nodes
  "Return a sequence of indexes of a post-ordered walk of the graph."
  [g]
  (fnext (reduce #(post-ordered-visit g %2 %1)
                 [#{} []]
                 (range (:count g)))))

(defn scc
  "Returns, as a sequence of sets, the strongly connected components
   of g."
  [g]
  (let [po (reverse (post-ordered-nodes g))
        rev (reverse-graph g)
        step (fn [stack visited acc]
               (if (empty? stack)
                 acc
                 (let [[nv comp] (post-ordered-visit rev
                                                     (first stack)
                                                     [visited #{}])
                       ns (remove nv stack)]
                   (recur ns nv (conj acc comp)))))]
    (step po #{} [])))

(defn self-recursive-sets
  "Returns, as a sequence of sets, the components of a graph that are
   self-recursive."
  [g]
  (letfn [recursive? [n]
            (or (> (count n) 1)
                (some n (get-neighbors g (first n))))]
     (filter recursive? (scc g))))
                          
(comment

  (def test-graph-2
       (struct directed-graph 10
               {0 [1 2]
                1 [0 2]
                2 [3 4]
                3 [0 1]
                4 [3]
                5 [5]
                6 [0 5]
                7 []
                8 [9]
                9 [8]}))


  (post-ordered-visit test-graph-2 0 [#{} []])
  (post-ordered-nodes test-graph-2)
  (scc test-graph-2)
  (self-recursive-sets test-graph-2)

)


(defn fixed-point
  "Repeatedly apply fun to data until (equal old-data new-data)
   returns true.  If max iterations occur, it will throw an
   exception.  Set max to nil for unlimited iterations."
  [data fun max equal]
  (let [step (fn step [data idx]
               (when (and idx (= 0 idx))
                 (throw (Exception. "Fixed point overflow")))
               (let [new-data (fun data)]
                 (if (equal data new-data)
                   new-data
                   (recur new-data (and idx (dec idx))))))]
    (step data max)))
                  
(defn- fold-into-sets
  [priorities]
  (let [step (fn [acc [idx dep]]
               (assoc acc dep (conj (acc dep) idx)))]
    (reduce step
            (vec (replicate (inc (apply max 0 priorities)) #{}))
            (indexed priorities))))
            
(defn dependency-list
  "Similar to a topological sort, this returns a vector of sets, each
   a set of nodes that depend on the earlier nodes in the vector.
   Assume the input graph (which much be acyclic) has an edge a->b
   when a depend on b."
  [g]
  (let [step (fn [d]
               (let [update (fn [n]
                              (inc (apply max -1 (map d (get-neighbors g n)))))]
                 (vec (map update (range (:count g))))))
        counts (fixed-point (vec (replicate (:count g) 0))
                            step
                            (inc (:count g))
                            =)]
    (fold-into-sets counts)))
    
(defn stratification-list
  "Similar to dependency-list (see doc), except two graphs are
   provided.  The first is as dependency-list.  The second (which may
   have cycles) provides a partial-dependency relation.  If node a
   depends on node b (meaning an edge a->b exists) in the second
   graph, node a must be equal or later in the sequence."
  [g1 g2]
  (assert (= (:count g1) (:count g2)))
  (let [step (fn [d]
               (letfn [update [n]
                       (max (inc (apply max -1
                                        (map d (get-neighbors g1 n))))
                            (apply max -1 (map d (get-neighbors g2 n))))]
                  (vec (map update (range (:count g1))))))
        counts (fixed-point (vec (replicate (:count g1) 0))
                            step
                            (inc (:count g1))
                            =)]
    (fold-into-sets counts)))
        
    

(comment

  (dependency-list test-graph-2)

  (def test-graph-3
       (struct directed-graph 6
               {0 [1]
                1 [2]
                2 [3]
                3 [4]
                4 [5]
                5 []}))

  (dependency-list test-graph-3)

  (def test-graph-4
       (struct directed-graph 8
               {0 []
                1 [0]
                2 [0]
                3 [0 1]
                4 [3 2]
                5 [4]
                6 [3]
                7 [5]}))

  (dependency-list test-graph-4)

  (def test-graph-5
       (struct directed-graph 8
               {0 []
                1 []
                2 [1]
                3 []
                4 []
                5 []
                6 [5]
                7 []}))

  (dependency-list test-graph-5)
  (stratification-list test-graph-4 test-graph-5)

)



;; End of file