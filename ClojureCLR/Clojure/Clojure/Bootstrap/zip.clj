;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;functional hierarchical zipper, with navigation, editing and enumeration
;see Huet

(ns clojure.zip
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
  [branch? children make-node root]
    #^{:zip/branch? branch? :zip/children children :zip/make-node make-node} 
    [root nil])

(defn seq-zip
  "Returns a zipper for nested sequences, given a root sequence"
  [root]
    (zipper seq? identity (fn [node children] children) root))

(defn vector-zip
  "Returns a zipper for nested vectors, given a root vector"
  [root]
    (zipper vector? seq (fn [node children] (apply vector children)) root))

(defn xml-zip
  "Returns a zipper for xml elements (as from xml/parse),
  given a root element"
  [root]
    (zipper (complement string?) 
            (comp seq :content)
            (fn [node children]
              (assoc node :content (and children (apply vector children))))
            root))

(defn node
  "Returns the node at loc"
  [loc] (loc 0))

(defn branch?
  "Returns true if the node at loc is a branch"
  [loc]
    ((:zip/branch? ^loc) (node loc)))

(defn children
  "Returns a seq of the children of node at loc, which must be a branch"
  [loc]
    ((:zip/children ^loc) (node loc)))

(defn make-node
  "Returns a new branch node, given an existing node and new
  children. The loc is only used to supply the constructor."
  [loc node children]
    ((:zip/make-node ^loc) node children))

(defn path
  "Returns a seq of nodes leading to this loc"
  [loc]
    (:pnodes (loc 1)))

(defn lefts
  "Returns a seq of the left siblings of this loc"
  [loc]
    (seq (:l (loc 1))))

(defn rights
  "Returns a seq of the right siblings of this loc"
  [loc]
    (:r (loc 1)))


(defn down
  "Returns the loc of the leftmost child of the node at this loc, or
  nil if no children"
  [loc]
    (let [[node path] loc
          [c & cnext :as cs] (children loc)]
      (when cs
        (with-meta [c {:l [] 
                       :pnodes (if path (conj (:pnodes path) node) [node]) 
                       :ppath path 
                       :r cnext}] ^loc))))

(defn up
  "Returns the loc of the parent of the node at this loc, or nil if at
  the top"
  [loc]
    (let [[node {l :l, ppath :ppath, pnodes :pnodes r :r, changed? :changed?, :as path}] loc]
      (when pnodes
        (let [pnode (peek pnodes)]
          (with-meta (if changed?
                       [(make-node loc pnode (concat l (cons node r))) 
                        (and ppath (assoc ppath :changed? true))]
                       [pnode ppath])
                     ^loc)))))

(defn root
  "zips all the way up and returns the root node, reflecting any
 changes."
  [loc]
    (if (= :end (loc 1))
      (node loc)
      (let [p (up loc)]
        (if p
          (recur p)
          (node loc)))))

(defn right
  "Returns the loc of the right sibling of the node at this loc, or nil"
  [loc]
    (let [[node {l :l  [r & rnext :as rs] :r :as path}] loc]
      (when (and path rs)
        (with-meta [r (assoc path :l (conj l node) :r rnext)] ^loc))))

(defn rightmost
  "Returns the loc of the rightmost sibling of the node at this loc, or self"
  [loc]
    (let [[node {l :l r :r :as path}] loc]
      (if (and path r)
        (with-meta [(last r) (assoc path :l (apply conj l node (butlast r)) :r nil)] ^loc)
        loc)))

(defn left
  "Returns the loc of the left sibling of the node at this loc, or nil"
  [loc]
    (let [[node {l :l r :r :as path}] loc]
      (when (and path (seq l))
        (with-meta [(peek l) (assoc path :l (pop l) :r (cons node r))] ^loc))))

(defn leftmost
  "Returns the loc of the leftmost sibling of the node at this loc, or self"
  [loc]
    (let [[node {l :l r :r :as path}] loc]
      (if (and path (seq l))
        (with-meta [(first l) (assoc path :l [] :r (concat (rest l) [node] r))] ^loc)
        loc)))

(defn insert-left
  "Inserts the item as the left sibling of the node at this loc,
 without moving"
  [loc item]
    (let [[node {l :l :as path}] loc]
      (if (nil? path)
        (throw (new Exception "Insert at top"))
        (with-meta [node (assoc path :l (conj l item) :changed? true)] ^loc))))

(defn insert-right
  "Inserts the item as the right sibling of the node at this loc,
  without moving"
  [loc item]
    (let [[node {r :r :as path}] loc]
      (if (nil? path)
        (throw (new Exception "Insert at top"))
        (with-meta [node (assoc path :r (cons item r) :changed? true)] ^loc))))

(defn replace
  "Replaces the node at this loc, without moving"
  [loc node]
    (let [[_ path] loc]
      (with-meta [node (assoc path :changed? true)] ^loc)))

(defn edit
  "Replaces the node at this loc with the value of (f node args)"
  [loc f & args]
    (replace loc (apply f (node loc) args)))

(defn insert-child
  "Inserts the item as the leftmost child of the node at this loc,
  without moving"
  [loc item]
    (replace loc (make-node loc (node loc) (cons item (children loc)))))

(defn append-child
  "Inserts the item as the rightmost child of the node at this loc,
  without moving"
  [loc item]
    (replace loc (make-node loc (node loc) (concat (children loc) [item]))))

(defn next
  "Moves to the next loc in the hierarchy, depth-first. When reaching
  the end, returns a distinguished loc detectable via end?. If already
  at the end, stays there."
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
  [loc]
    (if-let [lloc (left loc)]
      (loop [loc lloc]
        (if-let [child (and (branch? loc) (down loc))]
          (recur (rightmost child))
          loc))
      (up loc)))

(defn end?
  "Returns true if loc represents the end of a depth-first walk"
  [loc]
    (= :end (loc 1)))

(defn remove
  "Removes the node at loc, returning the loc that would have preceded
  it in a depth-first walk."
  [loc]
    (let [[node {l :l, ppath :ppath, pnodes :pnodes, rs :r, :as path}] loc]
      (if (nil? path)
        (throw (new Exception "Remove at top"))
        (if (pos? (count l))
          (loop [loc (with-meta [(peek l) (assoc path :l (pop l) :changed? true)] ^loc)]
            (if-let [child (and (branch? loc) (down loc))]
              (recur (rightmost child))
              loc))
          (with-meta [(make-node loc (peek pnodes) rs) 
                      (and ppath (assoc ppath :changed? true))]
                     ^loc)))))
  
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
