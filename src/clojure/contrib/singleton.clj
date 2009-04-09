;;; singleton.clj: singleton functions

;; by Stuart Sierra, http://stuartsierra.com/
;; April 9, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns clojure.contrib.singleton)

(defn singleton
  "Returns a memoized version of a function with no arguments.  The
  memoized version caches the function's return value.

  This is useful for lazily creating global objects that are expensive
  to initialize.  Warning: Make sure you really want a single global
  instance, and not one instance per thread."
  [f]
  (let [instance (atom nil)
        make-instance (fn [_] (f))]
    (fn [] (or @instance (swap! instance make-instance)))))
