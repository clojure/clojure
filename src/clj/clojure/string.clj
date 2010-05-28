;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "String utilities"
       :author "Stuart Sierra"}
  clojure.string
  (:refer-clojure :exclude (replace reverse))
  (:import (java.util.regex Pattern)))

(defn ^String reverse
  "Returns s with its characters reversed."
  [^String s]
  (.toString (.reverse (StringBuilder. s))))

(defn replace-str
  "Replaces all instances of substring a with b in s."
  [^String a ^String b ^String s]
  (.replace s a b))

(defn replace-char
  "Replaces all instances of character a with character b in s."
  [^Character a ^Character b ^String s]
  (.replace s a b))

(defn replace-re
  "Replaces all matches of re with replacement in s."
  [re replacement ^String s]
  (.replaceAll (re-matcher re s) replacement))

(defn replace-by
  "Replaces all matches of re in s with the result of 
  (f (re-groups the-match))."
  [re f ^String s]
  (let [m (re-matcher re s)]
    (let [buffer (StringBuffer. (.length s))]
      (loop []
        (if (.find m)
          (do (.appendReplacement m buffer (f (re-groups m)))
              (recur))
          (do (.appendTail m buffer)
              (.toString buffer)))))))

(defn replace-first-str
  "Replace first occurance of substring a with b in s."
  [^String a ^String b ^String s]
  (.replaceFirst (re-matcher (Pattern/quote a) s) b))

(defn replace-first-re
  "Replace first match of re in s."
  [^Pattern re ^String replacement ^String s]
  (.replaceFirst (re-matcher re s) replacement))

(defn replace-first-by
  "Replace first match of re in s with the result of
  (f (re-groups the-match))."
  [^Pattern re f ^String s]
  (let [m (re-matcher re s)]
    (let [buffer (StringBuffer.)]
      (if (.find m)
        (let [rep (f (re-groups m))]
          (.appendReplacement m buffer rep)
          (.appendTail m buffer)
          (str buffer))))))

(defn ^String join
  "Returns a string of all elements in coll, separated by
  separator.  Like Perl's join."
  [^String separator coll]
  (apply str (interpose separator coll)))

(defn ^String chop
  "Removes the last character of string, does nothing on a zero-length
  string."
  [^String s]
  (let [size (count s)]
    (if (zero? size)
      s
      (subs s 0 (dec (count s))))))

(defn ^String chomp
  "Removes all trailing newline \\n or return \\r characters from
  string.  Note: String.trim() is similar and faster."
  [^String s]
  (replace-re #"[\r\n]+$" "" s))

(defn ^String capitalize
  "Converts first character of the string to upper-case, all other
  characters to lower-case."
  [^String s]
  (if (< (count s) 2)
    (.toUpperCase s)
    (str (.toUpperCase ^String (subs s 0 1))
         (.toLowerCase ^String (subs s 1)))))

(defn ^String ltrim
  "Removes whitespace from the left side of string."
  [^String s]
  (replace-re #"^\s+" "" s))

(defn ^String rtrim
  "Removes whitespace from the right side of string."
  [^String s]
  (replace-re #"\s+$" "" s))

(defn ^String upper-case
  "Converts string to all upper-case."
  [^String s]
  (.toUpperCase s))

(defn ^String lower-case
  "Converts string to all lower-case."
  [^String s]
  (.toLowerCase s))

(defn split
  "Splits string on a regular expression.  Optional argument limit is
  the maximum number of splits."
  ([^Pattern re ^String s] (seq (.split re s)))
  ([^Pattern re limit ^String s] (seq (.split re s limit))))

(defn ^String trim
  "Removes whitespace from both ends of string."
  [^String s]
  (.trim s))

