;;; singleton.clj: singleton functions

;; by Stuart Sierra, http://stuartsierra.com/
;; April 14, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; Change Log:
;;
;; April 14, 2009: added per-thread-singleton, renamed singleton to
;; global-singleton
;;
;; April 9, 2009: initial version


(ns clojure.contrib.singleton)

(defn global-singleton
  "Returns a global singleton function.  f is a function of no
  arguments that creates and returns some object.  The singleton
  function will call f just once, the first time it is needed, and
  cache the value for all subsequent calls.

  Warning: global singletons are often unsafe in multi-threaded code.
  Consider per-thread-singleton instead."
  [f]
  (let [instance (atom nil)
        make-instance (fn [_] (f))]
    (fn [] (or @instance (swap! instance make-instance)))))

(defn per-thread-singleton
  "Returns a per-thread singleton function.  f is a function of no
  arguments that creates and returns some object.  The singleton
  function will call f only once for each thread, and cache its value
  for subsequent calls from the same thread.  This allows you to
  safely and lazily initialize shared objects on a per-thread basis.

  Warning: due to a bug in JDK 5, it may not be safe to use a
  per-thread-singleton in the initialization function for another
  per-thread-singleton.  See
  http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=5025230"
  [f]
  (let [thread-local (proxy [ThreadLocal] [] (initialValue [] (f)))]
    (fn [] (.get thread-local))))
