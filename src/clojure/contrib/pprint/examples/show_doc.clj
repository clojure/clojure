;;; show_doc.clj -- part of the pretty printer for Clojure

;; by Tom Faulhaber
;; April 3, 2009

;   Copyright (c) Tom Faulhaber, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; This example uses cl-format as part of a routine to display all the doc
;; strings and function arguments from one or more namespaces.

(ns clojure.contrib.pprint.examples.show-doc
  (:use clojure.contrib.pprint))

(defn ns-list
  ([] (ns-list nil))
  ([pattern] 
     (filter 
      (if pattern
        (comp (partial re-find pattern) name ns-name)
        (constantly true))
      (sort-by ns-name (all-ns)))))

(defn show-doc 
  ([] (show-doc nil)) 
  ([pattern] 
     (cl-format 
      true 
      "~:{~A: ===============================================~
       ~%~{~{~a: ~{~a~^, ~}~%~a~%~}~^~%~}~2%~}" 
      (map 
       #(vector (ns-name %) 
                (map
                 (fn [f] 
                   (let [f-meta ^(find-var (symbol (str (ns-name %)) (str f)))] 
                     [f (:arglists f-meta) (:doc f-meta)]))
                 (filter 
                  (fn [a] (instance? clojure.lang.IFn a)) 
                  (sort (map key (ns-publics %))))))
       (ns-list pattern)))))

(defn create-api-file [pattern out-file]
  (with-open [f (java.io.FileWriter. out-file)]
    (binding [*out* f]
      (show-doc pattern))))
