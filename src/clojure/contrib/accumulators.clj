;; Accumulators

;; by Konrad Hinsen
;; last updated February 23, 2009

;; This module defines various accumulators (list, vector, map,
;; sum, product, counter, and combinations thereof) with a common
;; interface defined by the multimethods add and combine.
;; For each accumulator type, its empty value is defined in this module.
;; Applications typically use this as a starting value and add data
;; using the add multimethod.

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.accumulators
  (:use [clojure.contrib.macros :only (letfn)])
  (:use [clojure.contrib.def :only (defvar defvar- defmacro-)]))

(defn- selector
  [& vs]
  (let [fv (first vs)
	tag (get ^fv ::accumulator nil)]
    (if (nil? tag) (class fv) tag)))

(defmulti add
  "Add item to the accumulator acc. The exact meaning of adding an
   an item depends on the type of the accumulator."
   {:arglists '([acc item])}
  selector)

(defn add-items
  "Add all elements of a collection coll to the accumulator acc."
  [acc items]
  (reduce add acc items))

(defmulti combine
  "Combine the values of the accumulators acc1 and acc2 into a
   single accumulator of the same type."
  {:arglists '([acc1 acc2])}
  selector)


;
; Vector accumulator
;
(defvar empty-vector []
  "An empty vector accumulator. Adding an item appends it at the end.")

(defmethod combine clojure.lang.IPersistentVector
  [& vs]
  (vec (apply concat vs)))

(defmethod add clojure.lang.IPersistentVector
  [v e]
  (conj v e))

;
; List accumulator
;
(defvar empty-list '()
  "An empty list accumulator. Adding an item appends it at the beginning.")

(defmethod combine clojure.lang.IPersistentList
  [& vs]
  (apply concat vs))

(defmethod add clojure.lang.IPersistentList
  [v e]
  (conj v e))

;
; Queue accumulator
;
(defvar empty-queue clojure.lang.PersistentQueue/EMPTY
  "An empty queue accumulator. Adding an item appends it at the end.")

(defmethod combine clojure.lang.PersistentQueue
  [& vs]
  (add-items (first vs) (apply concat (rest vs))))

(defmethod add clojure.lang.PersistentQueue
  [v e]
  (conj v e))

;
; Set accumulator
;
(defvar empty-set #{}
  "An empty set accumulator.")

; A multi-argument version of set/union
(defn- union
  [set & sets]
  (reduce clojure.set/union set sets))

(defmethod combine (class empty-set)
  [& vs]
  (apply union vs))

(defmethod add (class empty-set)
  [v e]
  (conj v e))

;
; String accumulator
;
(defvar empty-string ""
  "An empty string accumulator. Adding an item (string or character)
   appends it at the end.")

(defmethod combine java.lang.String
  [& vs]
  (apply str vs))

(defmethod add java.lang.String
  [v e]
  (str v e))

;
; Map accumulator
;
(defvar empty-map {}
  "An empty map accumulator. Items to be added must be [key value] pairs.")

(defmethod combine clojure.lang.IPersistentMap
  [& vs]
  (apply merge vs))

(defmethod add clojure.lang.IPersistentMap
  [v e]
  (conj v e))

;
; Numerical accumulators: sum, product, minimum, maximum
;
(defmacro- defacc
  [name op empty doc-string]
  (let [struct-tag (keyword (str name))
	meta-tag (keyword (str *ns*) (str name))
	empty-symbol (symbol (str "empty-" name))]
  `(let [op# ~op
	 meta-data# {::accumulator ~meta-tag}
	 struct-basis# (create-struct ~struct-tag)
	 get-value# (accessor struct-basis# ~struct-tag)
	 make-fn# (fn [n#] (with-meta (struct struct-basis# n#) meta-data#))]
     (defvar ~empty-symbol (make-fn# ~empty) ~doc-string)
     (defmethod combine ~meta-tag [& vs#]
       (make-fn# (apply op# (map get-value# vs#))))
     (defmethod add ~meta-tag [v# e#]
       (make-fn# (op# (get-value# v#) e#))))))

(defacc sum + 0
  "An empty sum accumulator. Only numbers can be added.")

(defacc product * 1
  "An empty sum accumulator. Only numbers can be added.")

; The empty maximum accumulator should have value -infinity.
; This is represented by nil and taken into account in an
; adapted max function. In the minimum accumulator, nil is
; similarly used to represent +infinity.

(defacc maximum (fn [& xs]
		  (when-let [xs (seq (filter identity xs))]
		      (apply max xs)))
                nil
  "An empty maximum accumulator. Only numbers can be added.")

(defacc minimum (fn [& xs]
		  (when-let [xs (seq (filter identity xs))]
		      (apply min xs)))
                nil
  "An empty minimum accumulator. Only numbers can be added.")

;
; Numeric range accumulator
; (combination of minimum and maximum)
;
(let [range-tag {::accumulator ::range}]
  (defn- make-range
    [min max]
    (with-meta {:minimum min :maximum max} range-tag)))

(defvar empty-range (make-range nil nil)
  "An empty range accumulator, combining minimum and maximum.
   Only numbers can be added.")

(defmethod combine ::range
  [& vs]
  (let [total-min (apply min (map :minimum vs))
	total-max (apply max (map :maximum vs))]
    (make-range total-min total-max)))

(defmethod add ::range
  [v e]
  (let [{min-v :minimum max-v :maximum} v
	new-min (if (nil? min-v) e (min min-v e))
	new-max (if (nil? max-v) e (max max-v e))]
    (make-range new-min new-max)))

;
; Counter accumulator
;
(defvar empty-counter (with-meta {} {::accumulator ::counter})
  "An empty counter accumulator. Its value is a map that stores for
   every item the number of times it was added.")

(defmethod combine ::counter
  [v & vs]
  (letfn [add-item [counter [item n]]
	    (assoc counter item (+ n (get counter item 0)))
	  add-two [c1 c2] (reduce add-item c1 c2)]
    (reduce add-two v vs)))

(defmethod add ::counter
  [v e]
  (assoc v e (inc (get v e 0))))

;
; Counter accumulator with total count
;
(derive ::counter-with-total ::counter)

(defvar empty-counter-with-total
  (with-meta {:total 0} {::accumulator ::counter-with-total})
  "An empty counter-with-total accumulator. It works like the counter
   accumulator, except that the total number of items added is stored as the
   value of the key :totall.")

(defmethod add ::counter-with-total
  [v e]
  (assoc v e (inc (get v e 0))
           :total (inc (:total v))))

;
; Accumulator n-tuple
;
(let [tuple-tag {::accumulator ::tuple}]
  (defn- make-tuple
    [seq]
    (with-meta (vec seq) tuple-tag)))

(defn empty-tuple
  "Returns an accumulator tuple with the supplied empty-accumulators
   as its value. Accumulator tuples consist of several accumulators that
   work in parallel. Added items must be sequences whose number of elements
   matches the number of sub-accumulators."
  [empty-accumulators]
  (make-tuple empty-accumulators))

(defmethod combine ::tuple
  [& vs]
  (make-tuple (map combine vs)))

(defmethod add ::tuple
  [v e]
  (make-tuple (map add v e)))
