;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;functional hierarchical zipper, with navigation, editing and enumeration
;see Huet

(in-ns 'zip)
(clojure/refer 'clojure)

(defn 
  #^{:doc "Creates a new zipper structure. 
    branch? is a fn that, given a node, returns true if can have children, even if it currently doesn't. 
    children is a fn that, given a branch node, returns a seq of its children. 
    make-node is a fn that, given an existing node and a seq of children, returns a new branch node with the supplied children. 
    root is the root node."}
zipper [branch? children make-node root]
  #^{:zip/branch? branch? :zip/children children :zip/make-node make-node} [root nil])

(defn 
  #^{:doc "Returns a zipper for nested sequences, given a root sequence"}
seq-zip [root]
  (zipper seq? identity (fn [node children] children) root))

(defn 
  #^{:doc "Returns a zipper for nested vectors, given a root vector"}
vector-zip [root]
  (zipper vector? seq (fn [node children] (apply vector children)) root))

(defn 
  #^{:doc "Returns a zipper for xml elements (as from xml/parse), given a root element"}
xml-zip [root]
  (zipper (complement string?) 
          (comp seq :content)
          (fn [node children]
            (assoc node :content (and children (apply vector children))))
          root))

(defn 
  #^{:doc "Returns the node at loc"}
node [loc]
  (loc 0))

(defn 
  #^{:doc "Returns true if the node at loc is a branch"}
branch? [loc]
  ((:zip/branch? ^loc) (node loc)))

(defn 
  #^{:doc "Returns a seq of the children of node at loc, which must be a branch"}
children [loc]
  ((:zip/children ^loc) (node loc)))

(defn 
  #^{:doc "Returns a new branch node, given an existing node and new children. The loc is only used to supply the constructor."}
make-node [loc node children]
  ((:zip/make-node ^loc) node children))

(defn 
  #^{:doc "Returns a seq of nodes leading to this loc"}
path [loc]
  (:pnodes (loc 1)))

(defn 
  #^{:doc "Returns a seq of the left siblings of this loc"}
lefts [loc]
  (seq (:l (loc 1))))

(defn 
  #^{:doc "Returns a seq of the right siblings of this loc"}
rights [loc]
  (:r (loc 1)))


(defn 
  #^{:doc "Returns the loc of the leftmost child of the node at this loc, or nil if no children"}
down [loc]
  (let [[node path] loc
        [c & crest :as cs] (children loc)]
    (when cs
      (with-meta [c {:l [] 
                     :pnodes (if path (conj (:pnodes path) node) [node]) 
                     :ppath path 
                     :r crest}] ^loc))))

(defn 
  #^{:doc "Returns the loc of the parent of the node at this loc, or nil if at the top"}
up [loc]
  (let [[node {l :l, ppath :ppath, pnodes :pnodes r :r, changed? :changed?, :as path}] loc]
    (when path
      (let [pnode (peek pnodes)]
        (with-meta (if changed?
                     [(make-node loc pnode (concat l (cons node r))) 
                      (and ppath (assoc ppath :changed? true))]
                     [pnode ppath])
                   ^loc)))))

(defn 
  #^{:doc "zips all the way up and returns the root node, reflecting any changes."}
root [loc]
  (if (= :end (loc 1))
    (node loc)
    (let [p (up loc)]
      (if p
        (recur p)
        (node loc)))))

(defn 
  #^{:doc "Returns the loc of the right sibling of the node at this loc, or nil"}
right [loc]
  (let [[node {l :l  [r & rrest :as rs] :r :as path}] loc]
    (when (and path rs)
      (with-meta [r (assoc path :l (conj l node) :r rrest)] ^loc))))

(defn 
  #^{:doc "Returns the loc of the left sibling of the node at this loc, or nil"}
left [loc]
  (let [[node {l :l r :r :as path}] loc]
    (when (and path (seq l))
      (with-meta [(peek l) (assoc path :l (pop l) :r (cons node r))] ^loc))))

(defn 
  #^{:doc "Inserts the item as the left sibling of the node at this loc, without moving"}
insert-left [loc item]
  (let [[node {l :l :as path}] loc]
    (if (nil? path)
      (throw (new Exception "Insert at top"))
      (with-meta [node (assoc path :l (conj l item) :changed? true)] ^loc))))

(defn 
  #^{:doc "Inserts the item as the right sibling of the node at this loc, without moving"}
insert-right [loc item]
  (let [[node {r :r :as path}] loc]
    (if (nil? path)
      (throw (new Exception "Insert at top"))
      (with-meta [node (assoc path :r (cons item r) :changed? true)] ^loc))))

(defn
  #^{:doc "Replaces the node at this loc, without moving"}
replace [loc node]
  (let [[_ path] loc]
    (with-meta [node (assoc path :changed? true)] ^loc)))

(defn 
  #^{:doc "Replaces the node at this loc with the value of (f node args)"}
edit [loc f & args]
  (replace loc (apply f (node loc) args)))

(defn 
  #^{:doc "Inserts the item as the leftmost child of the node at this loc, without moving"}
insert-child [loc item]
  (replace loc (make-node loc (node loc) (cons item (children loc)))))

(defn 
  #^{:doc "Inserts the item as the rightmost child of the node at this loc, without moving"}
append-child [loc item]
  (replace loc (make-node loc (node loc) (concat (children loc) [item]))))

(defn 
  #^{:doc "Moves to the next loc in the hierarchy, depth-first. When reaching the end, 
returns a distinguished loc detectable via end?. If already at the end, stays there."}
next [loc]
  (if (= :end (loc 1))
    loc
    (or 
     (and (branch? loc) (down loc))
     (right loc)
     (loop [p loc]
       (if (up p)
         (or (right (up p)) (recur (up p)))
         [(node p) :end])))))

(defn 
  #^{:doc "Returns true if loc represents the end of a depth-first walk"}
end? [loc]
  (= :end (loc 1)))

(defn 
  #^{:doc "Removes the node at loc, returning the loc that would have preceded it in a depth-first walk."}
remove [loc]
  (let [[node {l :l, ppath :ppath, pnodes :pnodes, rs :r, :as path}] loc]
    (if (nil? path)
      (throw (new Exception "Remove at top"))
      (if (pos? (count l)) 
        (with-meta [(peek l) (assoc path :l (pop l) :changed? true)] ^loc)
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

)
