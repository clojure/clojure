;;; str_utils.clj -- string utilities for Clojure

;; by Stuart Sierra <mail@stuartsierra.com>
;; April 8, 2008

;; Copyright (c) 2008 Stuart Sierra. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(clojure/in-ns clojure.contrib.str-utils
               (:import (java.util.regex Pattern)))

(defn re-split
  "Splits the string on instances of 'pattern'.  Returns a sequence of
  strings.  Optional 'limit' argument is the maximum number of
  splits.  Like Perl's 'split'."
  ([#^Pattern pattern string] (seq (. pattern (split string))))
  ([#^Pattern pattern string limit] (seq (. pattern (split string limit)))))

(defn re-gsub
  "Replaces all instances of 'pattern' in 'string' with
  'replacement'.  Like Ruby's 'String#gsub'."
  [#^Pattern regex replacement #^String string]
  (.. regex (matcher string) (replaceAll replacement)))

(defn re-sub
  "Replaces the first instance of 'pattern' in 'string' with
  'replacement'.  Like Ruby's 'String#sub'."
  [#^Pattern regex replacement #^String string]
  (.. regex (matcher string) (replaceFirst replacement)))

(defn str-join
  "Returns a string of all elements in 'sequence', separated by
  'separator'.  Like Perl's 'join'."
  [separator sequence]
  (apply str (interpose separator sequence)))
