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

(defn- replace-by
  [^String s re f]
  (let [m (re-matcher re s)]
    (let [buffer (StringBuffer. (.length s))]
      (loop []
        (if (.find m)
          (do (.appendReplacement m buffer (f (re-groups m)))
              (recur))
          (do (.appendTail m buffer)
              (.toString buffer)))))))

(defn replace
  "Replaces all instance of match with replacement in s.

   match/replacement can be:

   string / string
   char / char
   pattern / (string or function of match)"
  [^String s match replacement]
  (cond 
   (instance? Character match) (.replace s ^Character match ^Character replacement)
   (instance? String match) (.replace s ^String match ^String replacement)
   (instance? Pattern match) (if (string? replacement)
                               (.replaceAll (re-matcher ^Pattern match s) ^String replacement)
                               (replace-by s match replacement))
   :default (throw (IllegalArgumentException. (str "Invalid match arg: " match)))))

(defn- replace-first-by
  "Replace first match of re in s with the result of
  (f (re-groups the-match))."
  [^String s ^Pattern re f]
  (let [m (re-matcher re s)]
    (let [buffer (StringBuffer.)]
      (if (.find m)
        (let [rep (f (re-groups m))]
          (.appendReplacement m buffer rep)
          (.appendTail m buffer)
          (str buffer))))))

(defn replace-first
  ""
  [^String s match replacement]
  (cond
   (instance? String match)
   (.replaceFirst s (Pattern/quote ^String match) ^String replacement)
   (instance? Pattern match)
   (if (string? replacement)
     (.replaceFirst (re-matcher ^Pattern match s) ^String replacement)
     (replace-first-by s match replacement))
   :default (throw (IllegalArgumentException. (str "Invalid match arg: " match)))))


(defn ^String join
  "Returns a string of all elements in coll, separated by
  separator.  Like Perl's join."
  ([coll]
     (apply str coll))
  ([separator [x & more]]
     (loop [sb (StringBuilder. (str x))
            more more
            sep (str separator)]
       (if more
         (recur (-> sb (.append sep) (.append (str (first more))))
                (next more)
                sep)
         (str sb)))))

(defn ^String capitalize
  "Converts first character of the string to upper-case, all other
  characters to lower-case."
  [^String s]
  (if (< (count s) 2)
    (.toUpperCase s)
    (str (.toUpperCase ^String (subs s 0 1))
         (.toLowerCase ^String (subs s 1)))))

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

(defn ^String triml
  "Removes whitespace from the left side of string."
  [^String s]
  (replace s #"^\s+" ""))

(defn ^String trimr
  "Removes whitespace from the right side of string."
  [^String s]
  (replace s #"\s+$" ""))

(defn ^String trim-nl
  "Removes all trailing newline \\n or return \\r characters from
  string.  Note: String.trim() is similar and faster."
  [^String s]
  (loop [offset (.length s)]
    (if (zero? offset)
      ""
      (let [ch (.charAt s (dec offset))]
        (if (or (= ch \newline) (= ch \return))
          (recur (dec offset))
          (.substring s 0 offset))))))


