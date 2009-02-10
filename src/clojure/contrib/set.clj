;;  Copyright (c) Jason Wolfe. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  set.clj
;;
;;  Clojure functions for operating on sets (supplemental to clojure.set)
;;
;;  jason at w01fe dot com
;;  Created 2 Feb 2009

(ns clojure.contrib.set)

(defn subset? 
  "Is set1 a subset of set2?"
  [set1 set2]
  (and (<= (count set1) (count set2))
       (every? set2 set1)))

(defn superset? 
  "Is set1 a superset of set2?"
  [set1 set2]
  (and (>= (count set1) (count set2))
       (every? set1 set2)))

(defn proper-subset? 
  "Is s1 a proper subset of s2?"
  [set1 set2]
  (and (< (count set1) (count set2))
       (every? set2 set1)))

(defn proper-superset? 
  "Is s1 a proper superset of s2?"
  [set1 set2]
  (and (> (count set1) (count set2))
       (every? set1 set2)))
