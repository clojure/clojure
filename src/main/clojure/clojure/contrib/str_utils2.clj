;;; str_utils2.clj -- functional string utilities for Clojure

;; by Stuart Sierra, http://stuartsierra.com/
;; August 19, 2009

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
 (:refer-clojure :exclude (take replace drop butlast partition
                           contains? get repeat reverse partial))
 (:import (java.util.regex Pattern)))


(defmacro dochars 
  "bindings => [name string]

  Repeatedly executes body, with name bound to each character in
  string.  Does NOT handle Unicode supplementary characters (above
  U+FFFF)."
  [bindings & body]
  (assert (vector bindings))
  (assert (= 2 (count bindings)))
  ;; This seems to be the fastest way to iterate over characters.
  `(let [#^String s# ~(second bindings)]
     (dotimes [i# (.length s#)]
       (let [~(first bindings) (.charAt s# i#)]
         ~@body))))


(defmacro docodepoints
  "bindings => [name string]

  Repeatedly executes body, with name bound to the integer code point
  of each Unicode character in the string.  Handles Unicode
  supplementary characters (above U+FFFF) correctly."
  [bindings & body]
  (assert (vector bindings))
  (assert (= 2 (count bindings)))
  (let [character (first bindings)
        string (second bindings)]
    `(let [#^String s# ~string
           len# (.length s#)]
       (loop [i# 0]
         (when (< i# len#)
           (let [~character (.charAt s# i#)]
             (if (Character/isHighSurrogate ~character)
               (let [~character (.codePointAt s# i#)]
                 ~@body
                 (recur (+ 2 i#)))
               (let [~character (int ~character)]
                 ~@body
                 (recur (inc i#))))))))))

(defn codepoints
  "Returns a sequence of integer Unicode code points in s.  Handles
  Unicode supplementary characters (above U+FFFF) correctly."
  [#^String s]
  (let [len (.length s)
        f (fn thisfn [#^String s i]
            (when (< i len)
              (let [c (.charAt s i)]
                (if (Character/isHighSurrogate c)
                  (cons (.codePointAt s i) (thisfn s (+ 2 i)))
                  (cons (int c) (thisfn s (inc i)))))))]
    (lazy-seq (f s 0))))

(defn #^String escape
  "Returns a new String by applying cmap (a function or a map) to each
   character in s.  If cmap returns nil, the original character is
   added to the output unchanged."
  [#^String s cmap]
  (let [buffer (StringBuilder. (.length s))]
    (dochars [c s]
      (if-let [r (cmap c)]
        (.append buffer r)
        (.append buffer c)))
    (.toString buffer)))

(defn blank?
  "True if s is nil, empty, or contains only whitespace."
  [#^String s]
  (every? (fn [#^Character c] (Character/isWhitespace c)) s))

(defn #^String take
  "Take first n characters from s, up to the length of s.

  Note the argument order is the opposite of clojure.core/take; this
  is to keep the string as the first argument for use with ->"
  [#^String s n]
  (if (< (count s) n)
    s
    (.substring s 0 n)))

(defn #^String drop
  "Drops first n characters from s.  Returns an empty string if n is
  greater than the length of s.

  Note the argument order is the opposite of clojure.core/drop; this
  is to keep the string as the first argument for use with ->"
  [#^String s n]
  (if (< (count s) n)
    ""
    (.substring s n)))

(defn #^String butlast
  "Returns s without the last n characters.  Returns an empty string
  if n is greater than the length of s.

  Note the argument order is the opposite of clojure.core/butlast;
  this is to keep the string as the first argument for use with ->"
  [#^String s n]
  (if (< (count s) n)
    ""
    (.substring s 0 (- (count s) n))))

(defn #^String tail
  "Returns the last n characters of s."
  [#^String s n]
  (if (< (count s) n)
    s
    (.substring s (- (count s) n))))

(defn #^String repeat
  "Returns a new String containing s repeated n times."
  [#^String s n]
  (apply str (clojure.core/repeat n s)))

(defn #^String reverse
  "Returns s with its characters reversed."
  [#^String s]
  (.toString (.reverse (StringBuilder. s))))

(defmulti
  #^{:doc "Replaces all instances of pattern in string with replacement.  
  
  Allowed argument types for pattern and replacement are:
   1. String and String
   2. Character and Character
   3. regex Pattern and String
      (Uses java.util.regex.Matcher.replaceAll)
   4. regex Pattern and function
      (Calls function with re-groups of each match, uses return 
       value as replacement.)"
     :arglists '([string pattern replacement])
     :tag String}
  replace
  (fn [#^String string pattern replacement]
    [(class pattern) (class replacement)]))

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
  #^{:doc "Replaces the first instance of pattern in s with replacement.

  Allowed argument types for pattern and replacement are:
   1. String and String
   2. regex Pattern and String
      (Uses java.util.regex.Matcher.replaceAll)
   3. regex Pattern and function
"
     :arglists '([s pattern replacement])
     :tag String}
  replace-first
  (fn [s pattern replacement]
    [(class pattern) (class replacement)]))

(defmethod replace-first [String String] [#^String s pattern replacement]
  (.replaceFirst (re-matcher (Pattern/quote pattern) s) replacement))

(defmethod replace-first [Pattern String] [#^String s re replacement]
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

(defn #^String join
  "Returns a string of all elements in coll, separated by
  separator.  Like Perl's join."
  [#^String separator coll]
  (apply str (interpose separator coll)))

(defn #^String chop
  "Removes the last character of string, does nothing on a zero-length
  string."
  [#^String s]
  (let [size (count s)]
    (if (zero? size)
      s
      (subs s 0 (dec (count s))))))

(defn #^String chomp
  "Removes all trailing newline \\n or return \\r characters from
  string.  Note: String.trim() is similar and faster."
  [#^String s]
  (replace s #"[\r\n]+$" ""))

(defn title-case [#^String s]
  (throw (Exception. "title-case not implemeted yet")))

(defn #^String swap-case
  "Changes upper case characters to lower case and vice-versa.
  Handles Unicode supplementary characters correctly.  Uses the
  locale-sensitive String.toUpperCase() and String.toLowerCase()
  methods."
  [#^String s]
  (let [buffer (StringBuilder. (.length s))
        ;; array to make a String from one code point
        #^"[I" array (make-array Integer/TYPE 1)]
    (docodepoints [c s]
      (aset-int array 0 c)
      (if (Character/isLowerCase c)
        ;; Character.toUpperCase is not locale-sensitive, but
        ;; String.toUpperCase is; so we use a String.
        (.append buffer (.toUpperCase (String. array 0 1)))
        (.append buffer (.toLowerCase (String. array 0 1)))))
    (.toString buffer)))

(defn #^String capitalize
  "Converts first character of the string to upper-case, all other
  characters to lower-case."
  [#^String s]
  (if (< (count s) 2)
    (.toUpperCase s)
    (str (.toUpperCase #^String (subs s 0 1))
         (.toLowerCase #^String (subs s 1)))))

(defn #^String ltrim
  "Removes whitespace from the left side of string."
  [#^String s]
  (replace s #"^\s+" ""))

(defn #^String rtrim
  "Removes whitespace from the right side of string."
  [#^String s]
  (replace s #"\s+$" ""))

(defn split-lines
  "Splits s on \\n or \\r\\n."
  [#^String s]
  (seq (.split #"\r?\n" s)))

;; borrowed from compojure.str-utils, by James Reeves, EPL 1.0
(defn #^String map-str
  "Apply f to each element of coll, concatenate all results into a
  String."
  [f coll]
  (apply str (map f coll)))

;; borrowed from compojure.str-utils, by James Reeves, EPL 1.0
(defn grep
  "Filters elements of coll by a regular expression.  The String
  representation (with str) of each element is tested with re-find."
  [re coll]
  (filter (fn [x] (re-find re (str x))) coll))

(defn partial
  "Like clojure.core/partial for functions that take their primary
  argument first.

  Takes a function f and its arguments, NOT INCLUDING the first
  argument.  Returns a new function whose first argument will be the
  first argument to f.

  Example: (str-utils2/partial str-utils2/take 2)
           ;;=> (fn [s] (str-utils2/take s 2))"
  [f & args]
  (fn [s & more] (apply f s (concat args more))))


;;; WRAPPERS

;; The following functions are simple wrappers around java.lang.String
;; functions.  They are included here for completeness, and for use
;; when mapping over a collection of strings.

(defn #^String upper-case
  "Converts string to all upper-case."
  [#^String s]
  (.toUpperCase s))

(defn #^String lower-case
  "Converts string to all lower-case."
  [#^String s]
  (.toLowerCase s))

(defn split
  "Splits string on a regular expression.  Optional argument limit is
  the maximum number of splits."
  ([#^String s #^Pattern re] (seq (.split re s)))
  ([#^String s #^Pattern re limit] (seq (.split re s limit))))

(defn #^String trim
  "Removes whitespace from both ends of string."
  [#^String s]
  (.trim s))

(defn #^String contains?
  "True if s contains the substring."
  [#^String s substring]
  (.contains s substring))

(defn #^String get
  "Gets the i'th character in string."
  [#^String s i]
  (.charAt s i))

