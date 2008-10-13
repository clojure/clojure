;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  internal.clj
;;
;;  Internal functions for 'clojure.contrib.miglayout

(ns clojure.contrib.miglayout.internal
  (:import (java.awt Container Component)))

(defn format-constraints
  "Formats constraints expressed as a series of strings, keywords, vectors
  and/or maps into strings for miglayout."
  [& constraints]
  (loop [[c & cs] constraints
         cv []]
    (if c
      (recur cs (concat cv [", "]
        (cond (or (string? c) (keyword? c))
              [c]
              (vector? c)
              (interpose " " c)
              (map? c)
              (apply concat (interpose [", "] (map #(interpose " " %) c)))
              :else
              (throw
               (IllegalArgumentException.
                (format "unrecognized constraint: %s (%s)" c (class c)))))))
      (apply str (map #((if (keyword? %) name str) %) (rest cv))))))

(defn component?
  [x]
  (instance? Component x))

(defn keyword-item?
  [x]
  (#{:layout :column :row} x))

(defn item?
  [x]
  (or (component? x) (keyword-item? x)))

(defn constraint?
  [x]
  (not (item? x)))
