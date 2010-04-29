;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;functional hierarchical zipper, with navigation, editing and enumeration
;see Huet

(ns ^{:doc "Functional hierarchical zipper, with navigation, editing,
  and enumeration.  See Huet"
       :author "Rich Hickey"}
  clojure.zip
  (:refer-clojure :exclude (replace remove next)))

(defn zipper
  "Creates a new zipper structure. 

  branch? is a fn that, given a node, returns true if can have
  children, even if it currently doesn't.

  children is a fn that, given a branch node, returns a seq of its
  children.

  make-node is a fn that, given an existing node and a seq of
  children, returns a new branch node with the supplied children.
  root is the root node."
  {:added "1.0"}
  [branch? children make-node root]
    ^{:zip/branch? branch? :zip/children children :zip/make-node make-node}
    [root nil])

(defn seq-zip
  "Returns a zipper for nested sequences, given a root sequence"
  {:added "1.0"}
  [root]
    (zipper seq?
            identity
            (fn [node children] (with-meta children (meta node)))
            root))

(defn vector-zip
  "Returns a zipper for nested vectors, given a root vector"
  {:added "1.0"}
  [root]
    (zipper vector?
            seq
            (fn [node children] (with-meta (vec children) (meta node)))
            root))

(defn xml-zip
  "Returns a zipper for xml elements (as from xml/parse),
  given a root element"
  {:added "1.0"}
  [root]
    (zipper (complement string?) 
            (comp seq :content)
            (fn [node children]
              (assoc node :content (and children (apply vector children))))
            root))

(defn node
  "Returns the node at loc"
  {:added "1.0"}
  [loc] (loc 0))

(defn branch?
  "Returns true if the node at loc is a branch"
  {:added "1.0"}
  [loc]
    ((:zip/branch? (meta loc)) (node loc)))

(defn children
  "Returns a seq of the children of node at loc, which must be a branch"
  {:added "1.0"}
  [loc]
    (if (branch? loc)
      ((:zip/children (meta loc)) (node loc))
      (throw (Exception. "called children on a leaf node"))))

(defn make-node
  "Returns a new branch node, given an existing node and new
  children. The loc is only used to supply the constructor."
  {:added "1.0"}
  [loc node children]
    ((:zip/make-node (meta loc)) node children))

(defn path
  "Returns a seq of nodes leading to this loc"
  {:added "1.0"}
  [loc]
    (:pnodes (loc 1)))

(defn lefts
  "Returns a seq of the left siblings of this loc"
  {:added "1.0"}
  [loc]
    (seq (:l (loc 1))))

(defn rights
  "Returns a seq of the right siblings of this loc"
  {:added "1.0"}
  [loc]
    (:r (loc 1)))


(defn down
  "Returns the loc of the leftmost child of the node at this loc, or
  nil if no children"
  {:added "1.0"}
  [loc]
    (when (branch? loc)
      (let [[node path] loc
            [c & cnext :as cs] (children loc)]
        (when cs
          (with-meta [c {:l [] 
                         :pnodes (if path (conj (:pnodes path) node) [node]) 
                         :ppath path 
                         :r cnext}] (meta loc))))))

(defn up
  "Returns the loc of the parent of the node at this loc, or nil if at
  the top"
  {:added "1.0"}
  [loc]
    (let [[node {l :l, ppath :ppath, pnodes :pnodes r :r, changed? :changed?, :as path}] loc]
      (when pnodes
        (let [pnode (peek pnodes)]
          (with-meta (if changed?
                       [(make-node loc pnode (concat l (cons node r))) 
                        (and ppath (assoc ppath :changed? true))]
                       [pnode ppath])
                     (meta loc))))))

(defn root
  "zips all the way up and returns the root node, reflecting any
 changes."
  {:added "1.0"}
  [loc]
    (if (= :end (loc 1))
      (node loc)
      (let [p (up loc)]
        (if p
          (recur p)
          (node loc)))))

(defn right
  "Returns the loc of the right sibling of the node at this loc, or nil"
  {:added "1.0"}
  [loc]
    (let [[node {l :l  [r & rnext :as rs] :r :as path}] loc]
      (when (and path rs)
        (with-meta [r (assoc path :l (conj l node) :r rnext)] (meta loc)))))

(defn rightmost
  "Returns the loc of the rightmost sibling of the node at this loc, or self"
  {:added "1.0"}
  [loc]
    (let [[node {l :l r :r :as path}] loc]
      (if (and path r)
        (with-meta [(last r) (assoc path :l (apply conj l node (butlast r)) :r nil)] (meta loc))
        loc)))

(defn left
  "Returns the loc of the left sibling of the node at this loc, or nil"
  {:added "1.0"}
  [loc]
    (let [[node {l :l r :r :as path}] loc]
      (when (and path (seq l))
        (with-meta [(peek l) (assoc path :l (pop l) :r (cons node r))] (meta loc)))))

(defn leftmost
  "Returns the loc of the leftmost sibling of the node at this loc, or self"
  {:added "1.0"}
  [loc]
    (let [[node {l :l r :r :as path}] loc]
      (if (and path (seq l))
        (with-meta [(first l) (assoc path :l [] :r (concat (rest l) [node] r))] (meta loc))
        loc)))

(defn insert-left
  "Inserts the item as the left sibling of the node at this loc,
 without moving"
  {:added "1.0"}
  [loc item]
    (let [[node {l :l :as path}] loc]
      (if (nil? path)
        (throw (new Exception "Insert at top"))
        (with-meta [node (assoc path :l (conj l item) :changed? true)] (meta loc)))))

(defn insert-right
  "Inserts the item as the right sibling of the node at this loc,
  without moving"
  {:added "1.0"}
  [loc item]
    (let [[node {r :r :as path}] loc]
      (if (nil? path)
        (throw (new Exception "Insert at top"))
        (with-meta [node (assoc path :r (cons item r) :changed? true)] (meta loc)))))

(defn replace
  "Replaces the node at this loc, without moving"
  {:added "1.0"}
  [loc node]
    (let [[_ path] loc]
      (with-meta [node (assoc path :changed? true)] (meta loc))))

(defn edit
  "Replaces the node at this loc with the value of (f node args)"
  {:added "1.0"}
  [loc f & args]
    (replace loc (apply f (node loc) args)))

(defn insert-child
  "Inserts the item as the leftmost child of the node at this loc,
  without moving"
  {:added "1.0"}
  [loc item]
    (replace loc (make-node loc (node loc) (cons item (children loc)))))

(defn append-child
  "Inserts the item as the rightmost child of the node at this loc,
  without moving"
  {:added "1.0"}
  [loc item]
    (replace loc (make-node loc (node loc) (concat (children loc) [item]))))

(defn next
  "Moves to the next loc in the hierarchy, depth-first. When reaching
  the end, returns a distinguished loc detectable via end?. If already
  at the end, stays there."
  {:added "1.0"}
  [loc]
    (if (= :end (loc 1))
      loc
      (or 
       (and (branch? loc) (down loc))
       (right loc)
       (loop [p loc]
         (if (up p)
           (or (right (up p)) (recur (up p)))
           [(node p) :end])))))

(defn prev
  "Moves to the previous loc in the hierarchy, depth-first. If already
  at the root, returns nil."
  {:added "1.0"}
  [loc]
    (if-let [lloc (left loc)]
      (loop [loc lloc]
        (if-let [child (and (branch? loc) (down loc))]
          (recur (rightmost child))
          loc))
      (up loc)))

(defn end?
  "Returns true if loc represents the end of a depth-first walk"
  {:added "1.0"}
  [loc]
    (= :end (loc 1)))

(defn remove
  "Removes the node at loc, returning the loc that would have preceded
  it in a depth-first walk."
  {:added "1.0"}
  [loc]
    (let [[node {l :l, ppath :ppath, pnodes :pnodes, rs :r, :as path}] loc]
      (if (nil? path)
        (throw (new Exception "Remove at top"))
        (if (pos? (count l))
          (loop [loc (with-meta [(peek l) (assoc path :l (pop l) :changed? true)] (meta loc))]
            (if-let [child (and (branch? loc) (down loc))]
              (recur (rightmost child))
              loc))
          (with-meta [(make-node loc (peek pnodes) rs) 
                      (and ppath (assoc ppath :changed? true))]
                     (meta loc))))))
  
(comment

(load-file "/Users/rich/dev/clojure/src/zip.clj")
(refer 'zip)
(def data '[[a * b] + [c * d]])
(def dz (vector-zip data))

(right (down (right (right (down dz)))))
(lefts (right (down (right (right (down dz))))))
(rights (right (down (right (right (down dz))))))
(up (up (right (down (right (right (down dz)))))))
(path (right (down (right (right (down dz))))))

(-> dz down right right down right)
(-> dz down right right down right (replace '/) root)
(-> dz next next (edit str) next next next (replace '/) root)
(-> dz next next next next next next next next next remove root)
(-> dz next next next next next next next next next remove (insert-right 'e) root)
(-> dz next next next next next next next next next remove up (append-child 'e) root)

(end? (-> dz next next next next next next next next next remove next))

(-> dz next remove next remove root)

(loop [loc dz]
  (if (end? loc)
    (root loc)
    (recur (next (if (= '* (node loc)) 
                   (replace loc '/)
                   loc)))))

(loop [loc dz]
  (if (end? loc)
    (root loc)
    (recur (next (if (= '* (node loc)) 
                   (remove loc)
                   loc)))))
)
