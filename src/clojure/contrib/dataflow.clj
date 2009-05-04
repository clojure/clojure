;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  dataflow.clj
;;
;;  A Library to Support a Dataflow Model of State
;;
;;  straszheimjeffrey (gmail)
;;  Created 10 March 2009


(ns
  #^{:author "Jeffrey Straszheim",
     :doc "A library to support a dataflow model of state"}
  clojure.contrib.dataflow
  (:use [clojure.set :only (union intersection difference)])
  (:use [clojure.contrib.graph :only (directed-graph
                                      reverse-graph
                                      dependency-list
                                      get-neighbors)])
  (:use [clojure.contrib.walk :only (postwalk)])
  (:use [clojure.contrib.except :only (throwf)]))


;;; Chief Data Structures


;; Source Cell

; The data of a source cell is directly set by a calling function.  It
; never depends on other cells.

(defstruct source-cell
  :name             ; The name, a symbol
  :value            ; Its value, a Ref
  :cell-type)       ; Should be ::source-cell

;; Cell

; A standard cell that computes its value from other cells.

(defstruct standard-cell
  :name            ; The name, a symbol
  :value           ; Its value, a Ref
  :dependents      ; The names of cells on which this depends, a collection
  :fun             ; A closure that computes the value, given an environment
  :display         ; The original expression for display
  :cell-type)      ; Should be ::cell

(derive ::cell ::dependent-cell) ; A cell that has a dependents field

;; Validator

; A cell that has no value, but can throw an exception when run

(defstruct validator-cell
  :name            ; Always ::validator
  :dependents      ; The names of cells on which this depends, a collection
  :fun             ; A clojure that can throw an exception
  :display         ; The original exprssion for display
  :cell-type)      ; Should be ::validator-cell

(derive ::validator-cell ::dependent-cell)


;; A sentinal value

(def *empty-value* (java.lang.Object.))


;; Dataflow

; A collection of cells and dependency information

(defstruct dataflow
  :cells          ; A set of all cells
  :cells-map      ; A map of cell names (symbols) to collections of cells
  :fore-graph     ; The inverse of the dependency graph, nodes are cells
  :topological)   ; A vector of sets of independent nodes -- orders the computation


;;; Environment Access

(defn get-cells
  "Get all the cells named by name"
  [df name]
  ((:cells-map @df) name))

(defn get-cell
  "Get the single cell named by name"
  [df name]
  (let [cells (get-cells df name)]
    (cond
     (= (count cells) 1) (first cells)
     (> (count cells) 1) (throwf Exception "Cell %s has multiple instances" name)
     :otherwise (throwf Exception "Cell %s is undefined" name))))

(defn source-cell?
  "Is this cell a source cell?"
  [cell]
  (isa? (:cell-type cell) ::source-cell))

(defn get-source-cells
  "Returns a collection of source cells from the dataflow"
  [df]
  (for [cell (:cells @df)
        :when (source-cell? cell)]
    cell))

(defn get-value
  "Gets a value from the df matching the passed symbol.
   Signals an error if the name is not present, or if it not a single
   value."  
  [df name]
  (let [cell (get-cell df name)
        result @(:value cell)]
    (do (when (= *empty-value* result)
          (throwf Exception "Cell named %s empty" name))
        result)))

(defn get-values
  "Gets a collection of values from the df by name"
  [df name]
  (let [cells (get-cells df name)
        results (map #(-> % :value deref) cells)]
    (do
      (when (some #(= % *empty-value*) results)
        (throwf Exception "At least one empty cell named %s found" name))
      results)))

(defn get-old-value
  "Looks up an old value"
  [df env name]
  (if (contains? env name)
    (env name)
    (get-value df name)))

(defn get-value-from-cell
  "Given a cell, get its value"
  [cell]
  (-> cell :value deref))

;;; Build Dataflow Structure

(defn- build-cells-map
  "Given a collection of cells, build a name->cells-collection map
   from it."
  [cs]
  (let [step (fn [m c]
               (let [n (:name c)
                     o (get m n #{})
                     s (conj o c)]
                 (assoc m n s)))]
    (reduce step {} cs)))

(defn- build-back-graph
  "Builds the backward dependency graph from the cells map.  Each
   node of the graph is a cell."
  [cells cells-map]
  (let [step (fn [n]
               (apply union (for [dep-name (:dependents n)]
                              (cells-map dep-name))))
        neighbors (zipmap cells (map step cells))]
    (struct-map directed-graph
        :nodes cells
        :neighbors neighbors)))

(defn- build-dataflow*
  "Builds the dataflow structure"
  [cs]
  (let [cells (set cs)
        cells-map (build-cells-map cs)
        back-graph (build-back-graph cells cells-map)
        fore-graph (reverse-graph back-graph)]
    (struct-map dataflow
      :cells cells
      :cells-map cells-map
      :fore-graph fore-graph
      :topological (dependency-list back-graph))))

(def initialize)

(defn build-dataflow
  "Given a collection of cells, build and return a dataflow object"
  [cs]
  (dosync
   (let [df (ref (build-dataflow* cs))]
     (initialize df)
     df)))


;;; Displaying a dataflow

(defn print-dataflow
  "Prints a dataflow, one cell per line"
  [df]
  (println)
  (let [f (fn [cell] (-> cell :name str))]
    (doseq [cell (sort-by f (:cells @df))]
      (prn cell))))


;;; Modifying a Dataflow

(defn add-cells
  "Given a collection of cells, add them to the dataflow."
  [df cells]
  (dosync
   (let [new-cells (union (set cells) (:cells @df))]
     (ref-set df (build-dataflow* new-cells))
     (initialize df))))

(defn remove-cells
  "Given a collection of cells, remove them from the dataflow."
  [df cells]
  (dosync
   (let [new-cells (difference (:cells @df) (set cells))]
     (ref-set df (build-dataflow* new-cells))
     (initialize df))))


;;; Cell building

(def *meta* {:type ::dataflow-cell})

(defn build-source-cell
  "Builds a source cell"
  [name init]
  (with-meta (struct source-cell name (ref init) ::source-cell)
             *meta*))

(defn- is-col-var?
  [symb]
  (let [name (name symb)]
    (and (= \? (first name))
         (= \* (second name)))))

(defn- is-old-var?
  [symb]
  (let [name (name symb)]
    (and (= \? (first name))
         (= \- (second name)))))

(defn- is-var?
  [symb]
  (let [name (name symb)]
    (and (= \? (first name))
         (-> symb is-col-var? not)
         (-> symb is-old-var? not))))

(defn- cell-name
  [symb]
  `(quote ~(cond (is-var? symb) (-> symb name (.substring 1) symbol)
                 (or (is-col-var? symb)
                     (is-old-var? symb)) (-> symb name (.substring 2) symbol))))

(defn- replace-symbol
  "Walk the from replacing the ?X forms with the needed calls"
  [dfs ov form]
  (cond
   (-> form symbol? not) form
   (is-var? form) `(get-value ~dfs ~(cell-name form))
   (is-col-var? form) `(get-values ~dfs ~(cell-name form))
   (is-old-var? form) `(get-old-value ~dfs ~ov ~(cell-name form))
   :otherwise form))

(defn- build-fun
  "Build the closure needed to compute a cell"
  [form]
  (let [dfs (gensym "df_")
        ov (gensym "old_")]
    `(fn [~dfs ~ov] ~(postwalk (partial replace-symbol dfs ov) form))))

(defn- get-deps
  "Get the names of the dependent cells"
  [form]
  (let [step (fn [f]
               (cond
                (coll? f) (apply union f)
                (-> f symbol? not) nil
                (is-var? f) #{(cell-name f)}
                (is-col-var? f) #{(cell-name f)}
                (is-old-var? f) #{(cell-name f)}
                :otherwise nil))]
    (postwalk step form)))

(defn build-standard-cell
  "Builds a standard cell"
  [name deps fun expr]
  (with-meta (struct standard-cell name (ref *empty-value*) deps fun expr ::cell)
             *meta*))

(defn build-validator-cell
  "Builds a validator cell"
  [deps fun expr]
  (with-meta (struct validator-cell ::validator deps fun expr ::validator-cell)
             *meta*))

(defmacro cell
  "Build a standard cell, like this:

    (cell fred
       (* ?mary ?joe))

   Which creates a cell named fred that is the product of a cell mary and cell joe

   Or:

    (cell joe
      (apply * ?*sally))

   Which creates a cell that applies * to the collection of all cells named sally

   Or:

    (cell :source fred 0)

   Which builds a source cell fred with initial value 0

   Or:

     (cell :validator (when (< ?fred ?sally)
                          (throwf \"%s must be greater than %s\" ?fred ?sally))

   Which will perform the validation"
  [type & data]
  (cond
   (symbol? type) (let [name type ; No type for standard cell
                        expr (first data) ; we ignore extra data!
                        deps (get-deps expr)
                        fun (build-fun expr)]
                    `(build-standard-cell '~name ~deps ~fun '~expr))
   (= type :source) (let [[name init] data]
                      `(build-source-cell '~name ~init))
   (= type :validator) (let [[expr] data
                             deps (get-deps expr)
                             fun (build-fun expr)]
                         `(build-validator-cell ~deps ~fun '~expr))))

                         
;;; Cell Display

(defmulti display-cell
  "A 'readable' form of the cell"
  :cell-type)

(defmethod display-cell ::source-cell
  [cell]
  (list 'cell :source (:name cell) (-> cell :value deref)))

(defmethod display-cell ::cell
  [cell]
  (list 'cell (:name cell) (:display cell) (-> cell :value deref)))

(defmethod display-cell ::validator-cell
  [cell]
  (list 'cell :validator (:display cell)))

(defmethod print-method ::dataflow-cell
  [f #^Writer w]
  (binding [*out* w]
    (pr (display-cell f))))


;;; Evaluation

(defmulti eval-cell
  "Evaluate a dataflow cell.  Return [changed, old val]"
  (fn [df data old cell] (:cell-type cell)))

(defmethod eval-cell ::source-cell
  [df data old cell]
  (let [name (:name cell)
        val (:value cell)
        ov @val]
    (if (contains? data name)
      (let [new-val (data name)]
        (if (not= ov new-val)
          (do (ref-set val new-val)
              [true ov])
          [false ov]))
      [false ov])))

(defmethod eval-cell ::cell
  [df data old cell]
  (let [val (:value cell)
        old-val @val
        new-val ((:fun cell) df old)]
    (if (not= old-val new-val)
      (do (ref-set val new-val)
          [true old-val])
      [false old-val])))

(defmethod eval-cell ::validator-cell
  [df data old cell]
  (do ((:fun cell) df old)
      [false nil]))

(defn- perform-flow
  "Evaluate the needed cells (a set) from the given dataflow.  Data is
   a name-value mapping of new values for the source cells"
  [df data needed]
  (loop [needed needed
         tops (:topological @df)
         old {}]
    (let [now (first tops) ; Now is a set of nodes
          new-tops (next tops)]
      (when (and (-> needed empty? not)
                 (-> now empty? not))
        (let [step (fn [[needed old] cell]
                     (let [[changed ov] (try
                                         (eval-cell df data old cell)
                                         (catch Exception e
                                           (throw (Exception. (str cell) e))))
                           nn (disj needed cell)]
                       (if changed
                         [(union nn (get-neighbors (:fore-graph @df) cell))
                          (assoc old (:name cell) ov)]
                         [nn old])))
              [new-needed new-old] (reduce step
                                           [needed old]
                                           (intersection now needed))]
          (recur new-needed new-tops new-old))))))
         
(defn- validate-update
  "Ensure that all the updated cells are source cells"
  [df names]
  (let [scns (set (map :name (get-source-cells df)))]
    (doseq [name names]
      (when (-> name scns not)
        (throwf Exception "Cell %n is not a source cell" name)))))
        
(defn update-values
  "Given a dataflow, and a map of name-value pairs, update the
   dataflow by binding the new values.  Each name must be of a source
   cell"
  [df data]
  (dosync
   (validate-update df (keys data))
   (let [needed (apply union (for [name (keys data)]
                               (set ((:cells-map @df) name))))]
     (perform-flow df data needed))))

(defn- initialize
  "Apply all the current source cell values.  Useful for a new
   dataflow, or one that has been updated with new cells"
  [df]
  (let [needed (:cells @df)
        fg (:fore-graph @df)]
    (perform-flow df {} needed)))


;;; Watchers

(defn add-cell-watcher
  "Adds a watcher to a cell to respond to changes of value.  The is a
   function of 4 values: a key, the cell, its old value, its new
   value.  This is implemented using Clojure's add-watch to the
   underlying ref, and shared its sematics"
  [cell key fun]
  (let [val (:value cell)]
    (add-watch val key (fn [key _ old-v new-v]
                         (fun key cell old-v new-v)))))


(comment

  (def df
   (build-dataflow
    [(cell :source fred 1)
     (cell :source mary 0)
     (cell greg (+ ?fred ?mary))
     (cell joan (+ ?fred ?mary))
     (cell joan (* ?fred ?mary))
     (cell sally (apply + ?*joan))
     (cell :validator (when (number? ?-greg)
                        (when (<= ?greg ?-greg)
                          (throwf Exception "Non monotonic"))))]))

  (do (println)
      (print-dataflow df))

  (add-cell-watcher (get-cell df 'sally)
                    nil
                    (fn [key cell o n]
                      (printf "sally changed from %s to %s\n" o n)))

  (update-values df {'fred 1 'mary 1})
  (update-values df {'fred 5 'mary 1})
  (update-values df {'fred 0 'mary 0})

  (get-value df 'fred)
  (get-values df 'joan)
  (get-value df 'sally)
  (get-value df 'greg)

  (use :reload 'clojure.contrib.dataflow)
  (use 'clojure.contrib.stacktrace) (e)
  (use 'clojure.contrib.trace)
)
    

;; End of file
