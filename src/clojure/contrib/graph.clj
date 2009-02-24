;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
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

(ns clojure.contrib.graph)



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

)



;; End of file