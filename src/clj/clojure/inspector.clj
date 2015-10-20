;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Graphical object inspector for Clojure data structures."
       :author "Rich Hickey"}
    clojure.inspector
    (:import
     (java.awt BorderLayout)
     (java.awt.event ActionEvent ActionListener)
     (javax.swing.tree TreeModel)
     (javax.swing.table TableModel AbstractTableModel)
     (javax.swing JPanel JTree JTable JScrollPane JFrame JToolBar JButton SwingUtilities)))

(defn atom? [x]
  (not (coll? x)))

(defn collection-tag [x]
  (cond 
   (map-entry? x) :entry
   (instance? java.util.Map x) :seqable
   (instance? java.util.Set x) :seqable
   (sequential? x) :seq
   (instance? clojure.lang.Seqable x) :seqable
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

(defmethod is-leaf :seqable [parent]
  false)
(defmethod get-child :seqable [parent index]
  (nth (seq parent) index))
(defmethod get-child-count :seqable [parent]
  (count (seq parent)))

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
    (removeTreeModelListener [treeModelListener])))


(defn old-table-model [data]
  (let [row1 (first data)
	colcnt (count row1)
	cnt (count data)
	vals (if (map? row1) vals identity)]
    (proxy [TableModel] []
      (addTableModelListener [tableModelListener])
      (getColumnClass [columnIndex] Object)
      (getColumnCount [] colcnt)
      (getColumnName [columnIndex]
	(if (map? row1)
	  (name (nth (keys row1) columnIndex))
	  (str columnIndex)))
      (getRowCount [] cnt)
      (getValueAt [rowIndex columnIndex]
	(nth (vals (nth data rowIndex)) columnIndex))
      (isCellEditable [rowIndex columnIndex] false)
      (removeTableModelListener [tableModelListener]))))
      
(defn inspect-tree 
  "creates a graphical (Swing) inspector on the supplied hierarchical data"
  {:added "1.0"}
  [data]
  (doto (JFrame. "Clojure Inspector")
    (.add (JScrollPane. (JTree. (tree-model data))))
    (.setSize 400 600)
    (.setVisible true)))

(defn inspect-table 
  "creates a graphical (Swing) inspector on the supplied regular
  data, which must be a sequential data structure of data structures
  of equal length"
  {:added "1.0"}
    [data]
  (doto (JFrame. "Clojure Inspector")
    (.add (JScrollPane. (JTable. (old-table-model data))))
    (.setSize 400 600)
    (.setVisible true)))


(defmulti list-provider class)

(defmethod list-provider :default [x]
  {:nrows 1 :get-value (fn [i] x) :get-label (fn [i] (.getName (class x)))})

(defmethod list-provider java.util.List [c]
  (let [v (if (vector? c) c (vec c))]
    {:nrows (count v) 
     :get-value (fn [i] (v i)) 
     :get-label (fn [i] i)}))

(defmethod list-provider java.util.Map [c]
  (let [v (vec (sort (map (fn [[k v]] (vector k v)) c)))]
    {:nrows (count v) 
     :get-value (fn [i] ((v i) 1)) 
     :get-label (fn [i] ((v i) 0))}))

(defn list-model [provider]
  (let [{:keys [nrows get-value get-label]} provider]
    (proxy [AbstractTableModel] []
      (getColumnCount [] 2)
      (getRowCount [] nrows)
      (getValueAt [rowIndex columnIndex]
        (cond 
         (= 0 columnIndex) (get-label rowIndex)
         (= 1 columnIndex) (print-str (get-value rowIndex)))))))

(defmulti table-model class)

(defmethod table-model :default [x]
  (proxy [AbstractTableModel] []
    (getColumnCount [] 2)
    (getRowCount [] 1)
    (getValueAt [rowIndex columnIndex]
      (if (zero? columnIndex)
        (class x)
        x))))

;(defn make-inspector [x]
;  (agent {:frame frame :data x :parent nil :index 0}))


(defn inspect
  "creates a graphical (Swing) inspector on the supplied object"
  {:added "1.0"}
  [x]
  (doto (JFrame. "Clojure Inspector")
    (.add
      (doto (JPanel. (BorderLayout.))
        (.add (doto (JToolBar.)
                (.add (JButton. "Back"))
                (.addSeparator)
                (.add (JButton. "List"))
                (.add (JButton. "Table"))
                (.add (JButton. "Bean"))
                (.add (JButton. "Line"))
                (.add (JButton. "Bar"))
                (.addSeparator)
                (.add (JButton. "Prev"))
                (.add (JButton. "Next")))
              BorderLayout/NORTH)
        (.add
          (JScrollPane. 
            (doto (JTable. (list-model (list-provider x)))
              (.setAutoResizeMode JTable/AUTO_RESIZE_LAST_COLUMN)))
          BorderLayout/CENTER)))
    (.setSize 400 400)
    (.setVisible true)))


(comment

(load-file "src/inspector.clj")
(refer 'inspector)
(inspect-tree {:a 1 :b 2 :c [1 2 3 {:d 4 :e 5 :f [6 7 8]}]})
(inspect-table [[1 2 3][4 5 6][7 8 9][10 11 12]])

)
