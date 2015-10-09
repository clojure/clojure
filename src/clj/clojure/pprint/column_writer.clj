;;; column_writer.clj -- part of the pretty printer for Clojure


;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Author: Tom Faulhaber
;; April 3, 2009
;; Revised to use proxy instead of gen-class April 2010

;; This module implements a column-aware wrapper around an instance of java.io.Writer

(in-ns 'clojure.pprint)

(import [clojure.lang IDeref]
        [java.io Writer])

(def ^:dynamic ^{:private true} *default-page-width* 72)

(defn- get-field [^Writer this sym]
  (sym @@this))

(defn- set-field [^Writer this sym new-val] 
  (alter @this assoc sym new-val))

(defn- get-column [this]
  (get-field this :cur))

(defn- get-line [this]
  (get-field this :line))

(defn- get-max-column [this]
  (get-field this :max))

(defn- set-max-column [this new-max]
  (dosync (set-field this :max new-max))
  nil)

(defn- get-writer [this]
  (get-field this :base))

(defn- c-write-char [^Writer this ^Integer c]
  (dosync (if (= c (int \newline))
	    (do
              (set-field this :cur 0)
              (set-field this :line (inc (get-field this :line))))
	    (set-field this :cur (inc (get-field this :cur)))))
  (.write ^Writer (get-field this :base) c))

(defn- column-writer   
  ([writer] (column-writer writer *default-page-width*))
  ([^Writer writer max-columns]
     (let [fields (ref {:max max-columns, :cur 0, :line 0 :base writer})]
       (proxy [Writer IDeref] []
         (deref [] fields)
         (flush []
           (.flush writer))
         (write
          ([^chars cbuf ^Integer off ^Integer len] 
             (let [^Writer writer (get-field this :base)] 
               (.write writer cbuf off len)))
          ([x]
             (condp = (class x)
               String 
               (let [^String s x
                     nl (.lastIndexOf s (int \newline))]
                 (dosync (if (neg? nl)
                           (set-field this :cur (+ (get-field this :cur) (count s)))
                           (do
                             (set-field this :cur (- (count s) nl 1))
                             (set-field this :line (+ (get-field this :line)
                                                      (count (filter #(= % \newline) s)))))))
                 (.write ^Writer (get-field this :base) s))

               Integer
               (c-write-char this x)
               Long
               (c-write-char this x))))))))
