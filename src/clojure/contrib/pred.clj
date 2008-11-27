;;  Copyright (c) Stephen C. Gilardi. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.
;;
;;  pred.clj
;;
;;  Some clojure predicates
;;
;;  Items commented out below are defined in the clojure namespace
;;
;;  scgilardi (gmail)
;;  28 June 2008

(ns clojure.contrib.pred)

;; coll?
;; list?
;; map?
;; set?

(defn stack?
  "Returns true if x implements IPersistentStack"
  [x]
  (instance? clojure.lang.IPersistentStack x))

;; vector?

(defn ref?
  "Returns true if x implements IRef"
  [x]
  (instance? clojure.lang.IRef x))

;; seq?
;; var?

(defn map-entry?
  "Returns true if x is a MapEntry"
  [x]
  (instance? clojure.lang.MapEntry x))

(defn atom?
  "Returns true if x is not a collection"
  [x]
  (not (coll? x)))

;; number?
;; ratio?

(defn range?
  "Returns true if x is a Range"
  [x]
  (instance? clojure.lang.Range x))

;; function? -> fn?

(defn macro?
  "Returns true if x names a macro"
  [x]
  (and (symbol? x)
       (boolean
        (:macro (meta (resolve x))))))

;; integer?
;; even?
;; odd?
;; empty?
