;;; ColumnWriter.clj -- part of the pretty printer for Clojure

;; by Tom Faulhaber
;; April 3, 2009

;   Copyright (c) Tom Faulhaber, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; This module implements a column-aware wrapper around an instance of java.io.Writer

(ns clojure.contrib.pprint.ColumnWriter
  (:gen-class
   :extends java.io.Writer
   :init init
   :constructors {[java.io.Writer Integer] [], 
                  [java.io.Writer] []}
   :methods [[getColumn [] Integer]
             [getMaxColumn [] Integer]
             [setMaxColumn [Integer] Void]
             [getWriter [] java.io.Writer]]
   :state state))

(def *default-page-width* 72)

(defn- -init 
  ([writer] (-init writer *default-page-width*))
  ([writer max-columns] [[] (ref {:max max-columns, :cur 0, :base writer})]))

(defn- get-field [#^clojure.contrib.pprint.ColumnWriter this sym]
  (sym @(.state this)))

(defn- set-field [#^clojure.contrib.pprint.ColumnWriter this sym new-val] 
  (alter (.state this) assoc sym new-val))

(defn- -getColumn [this]
  (get-field this :cur))

(defn- -getMaxColumn [this]
  (get-field this :max))

(defn- -setMaxColumn [this new-max]
  (dosync (set-field this :max new-max)))

(defn- -getWriter [this]
  (get-field this :base))

(declare write-char)

(defn- -write 
  ([#^clojure.contrib.pprint.ColumnWriter this #^chars cbuf #^Integer off #^Integer len] 
     (let [#^java.io.Writer writer (get-field this :base)] 
       (.write writer cbuf off len)))
  ([#^clojure.contrib.pprint.ColumnWriter this x]
     (condp = (class x)
       String 
       (let [#^String s x
	     nl (.lastIndexOf s (int \newline))]
	 (dosync (if (neg? nl)
		   (set-field this :cur (+ (get-field this :cur) (count s)))
		   (set-field this :cur (- (count s) nl 1))))
	 (.write #^java.io.Writer (get-field this :base) s))

       Integer
       (write-char this x))))

(defn- write-char [#^clojure.contrib.pprint.ColumnWriter this #^Integer c]
  (dosync (if (= c (int \newline))
	    (set-field this :cur 0)
	    (set-field this :cur (inc (get-field this :cur)))))
  (.write #^java.io.Writer (get-field this :base) c))

(defn- -flush [this]) ;; Currently a no-op

(defn- -close [this]) ;; Currently a no-op
