;;; template.clj - anonymous functions that pre-evaluate sub-expressions

;; By Stuart Sierra, http://stuartsierra.com/
;; December 15, 2008

;; Copyright (c) Stuart Sierra, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.



;; This file defines macros for using template expressions.  These are
;; useful for writing macros.
;;
;; A template is an expression containing "holes" represented by the
;; symbols _1, _2, _3, and so on.  ("_" is a synonym for "_1".)
;;
;; The "template" macro is similar to #().  It returns an anonymous
;; function containing the body of the template.  Unlike #() or "fn",
;; however, any expressions that do not have any holes will be
;; evaluated only once, at the time the function is created, not every
;; time the function is called.
;;
;; Examples:
;;
(comment
  ;; Assume we have some big, slow calculation.
  (defn think-hard []
    (Thread/sleep 1000)
    1000)

  ;; With #(), think-hard gets called every time.
  (time (doall (map #(+ % (think-hard))
                    (range 5))))
  ;;=> "Elapsed time: 5001.33455 msecs"
  ;;=> (1000 1001 1002 1003 1004)

  ;; With a template, think-hard only gets called once.
  (time (doall (map (template (+ _ (think-hard)))
                    (range 5))))
  ;;=> "Elapsed time: 1000.907326 msecs"
  ;;=> (1000 1001 1002 1003 1004)
)
;;
;;
;; There is also the do-template macro, which works differently.  It
;; calls the same template multiple times, filling in values, and puts
;; it all inside a "do" block.  It will split up the values based on
;; the number of holes in the template.
(comment
  (do-template (foo _1 _2) :a :b :c :d)
  ;; expands to: (do (foo :a :b) (foo :c :d))

  (do-template (foo _1 _2 _3) 10 11 12 13 14 15)
  ;; expands to: (foo 10 11 12) (foo 13 14 15)
  )



(ns clojure.contrib.template
    (:use clojure.contrib.walk))

(defn find-symbols
  "Recursively finds all symbols in form."
  [form]
  (distinct (filter symbol? (tree-seq coll? seq form))))

(defn find-holes
  "Recursively finds all symbols starting with _ in form."
  [form]
  (sort (distinct (filter #(.startsWith (name %) "_")
                          (find-symbols form)))))

(defn find-pure-exprs
  "Recursively finds all sub-expressions in form that do not contain
  any symbols starting with _"
  [form]
  (filter #(and (list? %)
                (empty? (find-holes %)))
          (tree-seq seq? seq form)))

(defn flatten-map
  "Transforms a map into a vector like [key value key value]."
  [m]
  (reduce (fn [coll [k v]] (conj coll k v))
          [] m))

(defmacro template
  "Expands to a fn using _1, _2, _3, etc. as arguments (_ is the same
  as _1).  Any sub-expressions without any _* variables are evaluated
  when the fn is created, not when it is called."
  [& form]
  (let [form (postwalk-replace {'_ '_1} form)
        holes (find-holes form)
        pures (find-pure-exprs form)
        smap (zipmap pures (repeatedly #(gensym "HOLE_")))
        newform (prewalk-replace smap form)
        ;; Now, make sure we omit nested sub-expressions:
        used (set (filter #(.startsWith (name %) "HOLE_")
                          (find-symbols newform)))
        newmap (reduce (fn [m [k v]] (if (used v) (assoc m k v) m))
                       {} smap)]
    `(let ~(flatten-map (clojure.set/map-invert newmap))
       (fn ~(vec holes)
           ~@newform))))

(defn apply-template
  "Replaces _1, _2, _3, etc. in expr with corresponding elements of
  values.  Returns the modified expression.  For use in macros."
  [expr values]
  (let [expr (postwalk-replace {'_ '_1} expr)
        holes (find-holes expr)
        smap (zipmap holes values)]
    (prewalk-replace smap expr)))

(defmacro do-template
  "Repeatedly evaluates template expr (in a do block) using values in
  args.  args are grouped by the number of holes in the template.
  Example: (do-template (check _1 _2) :a :b :c :d)
  expands to (do (check :a :b) (check :c :d))"
  [expr & args]
  (let [expr (postwalk-replace {'_ '_1} expr)
        argcount (count (find-holes expr))]
    `(do ~@(map (fn [a] (apply-template expr a))
                (partition argcount args)))))
