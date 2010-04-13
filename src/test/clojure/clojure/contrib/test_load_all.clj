;;; test_load_all.clj - loads all contrib libraries for testing purposes

;; by Stuart Halloway, http://blog.thinkrelevance.com

;; Copyright (c) Stuart Halloway, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;; This is only intended to check that the libraries will load without
;; errors, not that they work correctly.

;; The code includes several design choices I don't love, but find
;; tolerable in a test-only lib:
;; 
;;   * namespaces that blow up to document deprecation
;;   * using directory paths to find contrib
;;   * using a macro to reflectively write tests
;;
;; I *am* happy that code that won't even load now breaks the build.

(ns clojure.contrib.test-load-all
  (:use clojure.test clojure.contrib.find-namespaces))

(def deprecated-contrib-namespaces
  '[clojure.contrib.javadoc])

(defn loadable-contrib-namespaces
  "Contrib namespaces that can be loaded (everything except
   deprecated nses that throw on load.)"
  []
  (apply disj
         (into #{} (find-namespaces-in-dir (java.io.File. "src/main")))
         deprecated-contrib-namespaces))

(defn emit-test-load
  []
  `(do
     ~@(map
        (fn [ns]
          `(deftest ~(symbol (str "test-loading-" (.replace (str ns) "." "-")))
             (require :reload '~ns)))
        (loadable-contrib-namespaces))))

(defmacro test-load
  []
  (emit-test-load))

(test-load)

