;;; test_is.clj: Compatibility layer for old clojure.contrib.test-is

;; by Stuart Sierra, http://stuartsierra.com/
;; August 28, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.



(ns #^{:doc "Backwards-compatibility for clojure.contrib.test-is

  The clojure.contrib.test-is library moved from Contrib into the
  Clojure distribution as clojure.test.

  This happened on or around clojure-contrib Git commit
  82cf0409d0fcb71be477ebfc4da18ee2128a2ad1 on June 25, 2009.

  This file makes the clojure.test interface available under the old
  namespace clojure.contrib.test-is.

  This includes support for the old syntax of the 'are' macro.

  This was suggested by Howard Lewis Ship in ticket #26, 
  http://www.assembla.com/spaces/clojure-contrib/tickets/26"
       :author "Stuart Sierra"}
    clojure.contrib.test-is
    (:require clojure.test
              [clojure.walk :as walk]))


;;; COPY INTERNED VARS (EXCEPT are) FROM clojure.test

(doseq [v (disj (set (vals (ns-interns 'clojure.test)))
                #'clojure.test/are)]
  (intern *ns* (with-meta (:name (meta v)) (meta v)) (var-get v)))


;;; REDEFINE OLD clojure.contrib.template 

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

(defn template?
  "Returns true if form is a valid template expression."
  [form]
  (if (seq (find-holes form)) true false))

(defn apply-template
  "Replaces _1, _2, _3, etc. in expr with corresponding elements of
  values.  Returns the modified expression.  For use in macros."
  [expr values]
  (when-not (template? expr)
    (throw (IllegalArgumentException. (str (pr-str expr) " is not a valid template."))))
  (let [expr (walk/postwalk-replace {'_ '_1} expr)
        holes (find-holes expr)
        smap (zipmap holes values)]
    (walk/prewalk-replace smap expr)))

(defmacro do-template
  "Repeatedly evaluates template expr (in a do block) using values in
  args.  args are grouped by the number of holes in the template.
  Example: (do-template (check _1 _2) :a :b :c :d)
  expands to (do (check :a :b) (check :c :d))"
  [expr & args]
  (when-not (template? expr)
    (throw (IllegalArgumentException. (str (pr-str expr) " is not a valid template."))))
  (let [expr (walk/postwalk-replace {'_ '_1} expr)
        argcount (count (find-holes expr))]
    `(do ~@(map (fn [a] (apply-template expr a))
                (partition argcount args)))))



;;; REDEFINE are MACRO TO MATCH OLD TEMPLATE BEHAVIOR

(defmacro are
  "Checks multiple assertions with a template expression.
  See clojure.contrib.template/do-template for an explanation of
  templates.

  Example: (are (= _1 _2)  
                2 (+ 1 1)
                4 (* 2 2))
  Expands to: 
           (do (is (= 2 (+ 1 1)))
               (is (= 4 (* 2 2))))

  Note: This breaks some reporting features, such as line numbers."
  [expr & args]
  `(do-template (is ~expr) ~@args))
