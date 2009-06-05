;;; str_utils2.clj -- experimental new string utilities for Clojure

;; by Stuart Sierra, http://stuartsierra.com/
;; June 4, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns #^{:author "Stuart Sierra"
       :doc "This is a library of string manipulation functions.  It
    is intented as a replacement for clojure.contrib.str-utils.

    You cannot (use 'clojure.contrib.str-utils2) because it defines
    functions with the same names as functions in clojure.core.
    Instead, do (require '[clojure.contrib.str-utils2 :as s]) 
    or something similar.

    Goals:
      1. Be functional
      2. String argument first, to work with ->
      3. Performance linear in string length

    Some ideas are borrowed from
    http://github.com/francoisdevlin/devlinsf-clojure-utils/"}
 clojure.contrib.str-utils2
 (:refer-clojure :exclude (take replace drop butlast partition contains? get))
 (:require [clojure.contrib.java-utils :as j])
 (:import (java.util.regex Pattern)))

(defmacro dochars
  "bindings => [name string]
  Repeatedly executes body, with name bound to each character in
  string."
  [bindings & body]
  (assert (vector bindings))
  (assert (= 2 (count bindings)))
  `(let [#^String s# ~(second bindings)]
     (dotimes [i# (.length ~(second bindings))]
       (let [~(first bindings) (.charAt s# i#)]
         ~@body))))

(defn escape
  "Escapes characters in string according to a cmap, a function or map
  from characters to their replacements."
  [#^String s cmap]
  (let [buffer (StringBuilder. (.length s))]
    (dochars [c s]
      (if-let [r (cmap s)]
        (.append buffer r)
        (.append buffer c)))
    (.toString buffer)))

(defn escape-pattern [#^String s]
  (escape s (fn [c] (when (#{\\ \[ \] \. \^ \$ \? \* \+ \( \)} c)
                      (str \\ c)))))

(defn as-pattern [re]
  (if (instance? Pattern re)
    re
    (Pattern/compile (escape-pattern (j/as-str re)))))

(defn blank?
  "True if s is nil, empty, or contains only whitespace."
  [#^String s]
  (every? (fn [#^Character c] (Character/isWhitespace c)) s))

(defn take
  "Take first n characters from s, up to the length of s."
  [#^String s n]
  (if (< (count s) n)
    s
    (.substring s 0 n)))

(defn drop [#^String s n]
  "Drops first n characters from s.  Returns an empty string if n is
  greater than the length of s."
  (if (< (count s) n)
    ""
    (.substring s n)))

(defn butlast
  "Returns s without the last n characters.  Returns an empty string
  if n is greater than the length of s."
  [#^String s n]
  (if (< (count s) n)
    ""
    (.substring s 0 (- (count s) n))))

(defn tail
  "Returns the last n characters of s."
  [#^String s n]
  (if (< (count s) n)
    s
    (.substring s (- (count s) n))))

(defmulti
  #^{:doc "Replaces all instances of a in s with b.  a and b may be
  Characters, Strings, Pattern/String, or Pattern/Fn."
     :arglists '([s a b])}
  replace
  (fn [#^String s a b]
    [(class a) (class b)]))

(defmethod replace [String String] [#^String s #^String a #^String b]
  (.replace s a b))

(defmethod replace [Character Character] [#^String s #^Character a #^Character b]
  (.replace s a b))

(defmethod replace [Pattern String] [#^String s re replacement]
  (.replaceAll (re-matcher re s) replacement))

(defmethod replace [Pattern clojure.lang.IFn] [#^String s re replacement]
  (let [m (re-matcher re s)]
    (let [buffer (StringBuffer. (.length s))]
      (loop []
        (if (.find m)
          (do (.appendReplacement m buffer (replacement (re-groups m)))
              (recur))
          (do (.appendTail m buffer)
              (.toString buffer)))))))

(defmulti
  #^{:doc "Replaces the first instance of a in s with b.  a must be
  Pattern, b may be String or Fn."
     :arglists '([s a b])}
  replace-first
  (fn [s a b]
    [(class a) (class b)]))

(defmethod replace-first [Pattern String] [#^String s #^Pattern re replacement]
  (.replaceFirst (re-matcher re s) replacement))

(defmethod replace-first [Pattern clojure.lang.IFn] [#^String s #^Pattern re f]
  (let [m (re-matcher re s)]
    (let [buffer (StringBuffer.)]
      (if (.find m)
        (let [rep (f (re-groups m))]
          (.appendReplacement m buffer rep)
          (.appendTail m buffer)
          (str buffer))))))

(defn partition
  "Splits the string into a lazy sequence of substrings, alternating
  between substrings that match the patthern and the substrings
  between the matches.  The sequence always starts with the substring
  before the first match, or an empty string if the beginning of the
  string matches.

  For example: (partition \"abc123def\" #\"[a-z]+\")
  returns: (\"\" \"abc\" \"123\" \"def\")"
  [#^String s #^Pattern re]
  (let [m (re-matcher re s)]
    ((fn step [prevend]
       (lazy-seq
        (if (.find m)
          (cons (.subSequence s prevend (.start m))
                (cons (re-groups m)
                      (step (+ (.start m) (count (.group m))))))
          (when (< prevend (.length s))
            (list (.subSequence s prevend (.length s)))))))
     0)))

(defn join
  "Returns a string of all elements in coll, separated by
  separator.  Like Perl's join."
  [#^String separator coll]
  (apply str (interpose separator coll)))

(defn chop
  "Removes the last character of string."
  [#^String s]
  (subs s 0 (dec (count s))))

(defn chomp
  "Removes all trailing newline \\n or return \\r characters from
  string.  Note: String.trim() is similar and faster."
  [#^String s]
  (replace s #"[\r\n]+$" ""))

(defn title-case [#^String s]
  (throw (IllegalStateException. "title-case not implemented yet.")))

(defn swap-case [#^String s]
  (throw (IllegalStateException. "swap-case not implemented yet.")))

(defn ltrim [#^String s]
  (replace s #"^\s+" ""))

(defn rtrim [#^String s]
  (replace s #"\s+$" ""))

(defn split-lines [#^String s]
  (seq (.split #"\r?\n" s)))


;;; WRAPPERS

;; The following functions are simple wrappers around java.lang.String
;; functions.  They are included here for completeness, and for use
;; when mapping over a collection of strings.

(defn upper-case [#^String s]
  (.toUpperCase s))

(defn lower-case [#^String s]
  (.toLowerCase s))

(defn split
  ([#^String s #^Pattern re] (seq (.split re s)))
  ([#^String s #^Pattern re limit] (seq (.split re s limit))))

(defn trim [#^String s]
  (.trim s))

(defn contains? [#^String s substring]
  (.contains s substring))

(defn get [#^String s i]
  (.charAt s i))

