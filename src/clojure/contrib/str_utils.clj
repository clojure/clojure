;;; str_utils.clj -- string utilities for Clojure

;; by Stuart Sierra <mail@stuartsierra.com>
;; April 8, 2008

;; Copyright (c) Stuart Sierra, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns clojure.contrib.str-utils
    (:import (java.util.regex Pattern)))

(defn re-split
  "Splits the string on instances of 'pattern'.  Returns a sequence of
  strings.  Optional 'limit' argument is the maximum number of
  splits.  Like Perl's 'split'."
  ([#^Pattern pattern string] (seq (. pattern (split string))))
  ([#^Pattern pattern string limit] (seq (. pattern (split string limit)))))

(defn re-partition
  "Splits the string into a lazy sequence of substrings, alternating
  between substrings that match the patthern and the substrings
  between the matches.  The sequence always starts with the substring
  before the first match, or an empty string if the beginning of the
  string matches.

  For example: (re-partition #\"[a-z]+\" \"abc123def\")

  Returns: (\"\" \"abc\" \"123\" \"def\")"
  [#^Pattern re string]
  (let [m (re-matcher re string)]
    ((fn step [prevend]
       (lazy-seq
        (if (.find m)
          (cons (.subSequence string prevend (.start m))
                (cons (re-groups m)
                      (step (+ (.start m) (count (.group m))))))
          (when (< prevend (.length string))
            (list (.subSequence string prevend (.length string)))))))
     0)))

(defn re-gsub 
  "Replaces all instances of 'pattern' in 'string' with
  'replacement'.  Like Ruby's 'String#gsub'.
  
  If (ifn? replacment) is true, the replacement is called with the
  match.
  "
  [#^java.util.regex.Pattern regex replacement #^String string]
  (if (ifn? replacement)
    (let [parts (vec (re-partition regex string))]
      (apply str
             (reduce (fn [parts match-idx]
                       (update-in parts [match-idx] replacement))
                     parts (range 1 (count parts) 2))))
    (.. regex (matcher string) (replaceAll replacement))))

(defn re-sub
  "Replaces the first instance of 'pattern' in 'string' with
  'replacement'.  Like Ruby's 'String#sub'.
  
  If (ifn? replacement) is true, the replacement is called with
  the match.
  "
  [#^Pattern regex replacement #^String string]
  (if (ifn? replacement)
    (let [m (re-matcher regex string)]
      (if (.find m)
        (str (.subSequence string 0 (.start m))
             (replacement (re-groups m))
             (.subSequence string (.end m) (.length string)))
        string))
    (.. regex (matcher string) (replaceFirst replacement))))


(defn str-join
  "Returns a string of all elements in 'sequence', separated by
  'separator'.  Like Perl's 'join'."
  [separator sequence]
  (apply str (interpose separator sequence)))
