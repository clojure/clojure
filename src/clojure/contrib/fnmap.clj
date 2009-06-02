;;; fnmap.clj: maps that dispatch get/assoc to functions

;; Copyright (c) Stuart Sierra, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns #^{:author "Stuart Sierra"
       :doc "Maps that dispatch get/assoc to user-defined functions.

  Note: requires AOT-compilation"}
  clojure.contrib.fnmap
  (:require clojure.contrib.fnmap.PersistentFnMap))

(defn fnmap
  "Creates a fnmap, or functional map.  A fnmap behaves like an
  ordinary Clojure map, except that calls to get and assoc are
  filtered through user-defined getter and setter functions, which
  operate on an internal map.

  (getter m key) should return a value for key.

  (setter m key value) should assoc key with value and return a new
  map for m.

  All other map operations are passed through to the internal map."
  ([getter setter] (clojure.contrib.fnmap.PersistentFnMap/create getter setter))
  ([getter setter & keyvals]
      (apply assoc
             (clojure.contrib.fnmap.PersistentFnMap/create getter setter)
             keyvals)))

