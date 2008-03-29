;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'inspector)
(clojure/refer 'clojure)

(import '(javax.swing.tree TreeModel)
	'(javax.swing.table TableModel)
	'(javax.swing JTree JTable JScrollPane JFrame))

(defn atom? [x]
  (not (instance? clojure.lang.IPersistentCollection x)))

(defn collection-tag [x]
  (cond 
   (instance? java.util.Map$Entry x) :entry
   (instance? clojure.lang.IPersistentMap x) :map 
   (instance? java.util.Map x) :map 
   (instance? clojure.lang.Sequential x) :seq
   :else :atom))

(defmulti is-leaf collection-tag)
(defmulti get-child (fn [parent index] (collection-tag parent)))
(defmulti get-child-count collection-tag)

(defmethod is-leaf :default [node]
  (atom? node))
(defmethod get-child :default [parent index]
  (nth parent index))
(defmethod get-child-count :default [parent]
  (count parent))

(defmethod is-leaf :entry [e]
  (is-leaf (val e)))
(defmethod get-child :entry [e index]
  (get-child (val e) index))
(defmethod get-child-count :entry [e]
  (count (val e)))

(defmethod is-leaf :map [m]
  false)
(defmethod get-child :map [m index]
  (nth (seq m) index))

(defn tree-model [data]
  (proxy [TreeModel] []
    (getRoot [] data)
    (addTreeModelListener [treeModelListener])
    (getChild [parent index]
      (get-child parent index))
    (getChildCount [parent]
       (get-child-count parent))
    (isLeaf [node]
      (is-leaf node))
    (valueForPathChanged [path newValue])
    (getIndexOfChild [parent child]
      -1)
    (addTreeModelListener [treeModelListener])
    (removeTreeModelListener [treeModelListener])))


(defn table-model [data]
  (let [row1 (first data)
	colcnt (count row1)
	cnt (count data)
	vals (if (instance? clojure.lang.IPersistentMap row1) vals identity)]
    (proxy [TableModel] []
      (addTableModelListener [tableModelListener])
      (getColumnClass [columnIndex] Object)
      (getColumnCount [] colcnt)
      (getColumnName [columnIndex]
	(if (instance? clojure.lang.IPersistentMap row1)
	  (name (nth (keys row1) columnIndex))
	  (str columnIndex)))
      (getRowCount [] cnt)
      (getValueAt [rowIndex columnIndex]
	(nth (vals (nth data rowIndex)) columnIndex))
      (isCellEditable [rowIndex columnIndex] false)
      (removeTableModelListener [tableModelListener]))))
      
(defn inspect-tree 
  "creates a graphical (Swing) inspector on the supplied hierarchical data"
  [data]
  (doto (new JFrame "Clojure Inspector")
    (add (new JScrollPane (new JTree (tree-model data))))
    (setSize 400 600)
    (setVisible true)))

(defn inspect-table 
  "creates a graphical (Swing) inspector on the supplied regular
  data, which must be a sequential data structure of data structures
  of equal length"
    [data]
  (doto (new JFrame "Clojure Inspector")
    (add (new JScrollPane (new JTable (table-model data))))
    (setSize 400 600)
    (setVisible true)))

(comment

(load-file "src/inspector.clj")
(inspect-tree {:a 1 :b 2 :c [1 2 3 {:d 4 :e 5 :f [6 7 8]}]})
(inspect-table [[1 2 3][4 5 6][7 8 9][10 11 12]])

)