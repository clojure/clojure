;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Clojure String utilities

clojure.string adheres to the following design principles:

1. Strings are objects (as opposed to sequences). As such, the
   string being manipulated is the first argument to a function;
   passing nil will result in a NullPointerException. If you
   want sequence-y behavior instead, use a sequence.

2. Functions are generally not lazy, and call straight to host
   methods where those are available and efficient.

3. When a function is documented to accept a string argument, it
   will take any implementation of the correct *interface* on the
   host platform. In Java, this is CharSequence, which is more
   general than String. In ordinary usage you will almost always
   pass concrete strings. If you are doing something unusual,
   e.g. passing a mutable implementation of CharSequence, then
   thead-safety is your responsibility."
      :author "Stuart Sierra, Stuart Halloway, David Liebke"}
  clojure.string
  (:refer-clojure :exclude (replace reverse))
  (:import (java.util.regex Pattern)
           clojure.lang.LazilyPersistentVector))

(defn ^CharSequence reverse
  "Returns s with its characters reversed."
  {:added "1.2"}
  [^CharSequence s]
  (.toString (.reverse (StringBuilder. s))))

(defn- replace-by
  [^CharSequence s re f]
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
   pattern / (string or function of match).

   See also replace-first."
  {:added "1.2"}
  [^CharSequence s match replacement]
  (let [s (.toString s)]
    (cond 
     (instance? Character match) (.replace s ^Character match ^Character replacement)
     (instance? CharSequence match) (.replace s ^CharSequence match ^CharSequence replacement)
     (instance? Pattern match) (if (string? replacement)
                                 (.replaceAll (re-matcher ^Pattern match s) ^CharSequence replacement)
                                 (replace-by s match replacement))
     :else (throw (IllegalArgumentException. (str "Invalid match arg: " match))))))

(defn- replace-first-by
  [^CharSequence s ^Pattern re f]
  (let [m (re-matcher re s)]
    (let [buffer (StringBuffer.)]
      (if (.find m)
        (let [rep (f (re-groups m))]
          (.appendReplacement m buffer rep)
          (.appendTail m buffer)
          (str buffer))))))

(defn- replace-first-char
  [^CharSequence s ^Character match replace]
  (let [s (.toString s)
        i (.indexOf s (int match))]
    (if (= -1 i)
      s
      (str (subs s 0 i) replace (subs s (inc i))))))

(defn replace-first
  "Replaces the first instance of match with replacement in s.

   match/replacement can be:

   char / char
   string / string
   pattern / (string or function of match).

   See also replace-all."
  {:added "1.2"}
  [^CharSequence s match replacement]
  (let [s (.toString s)]
    (cond
     (instance? Character match)
     (replace-first-char s match replacement)
     (instance? CharSequence match)
     (.replaceFirst s (Pattern/quote (.toString ^CharSequence match))
                    (.toString ^CharSequence replacement))
     (instance? Pattern match)
     (if (instance? CharSequence replacement)
       (.replaceFirst (re-matcher ^Pattern match s)
                      (.toString ^CharSequence replacement))
       (replace-first-by s match replacement))
     :else (throw (IllegalArgumentException. (str "Invalid match arg: " match))))))


(defn ^CharSequence join
  "Returns a string of all elements in coll, separated by
   an optional separator.  Like Perl's join."
  {:added "1.2"}
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

(defn ^CharSequence capitalize
  "Converts first character of the string to upper-case, all other
  characters to lower-case."
  {:added "1.2"}
  [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toUpperCase s)
      (str (.toUpperCase (subs s 0 1))
           (.toLowerCase (subs s 1))))))

(defn ^CharSequence upper-case
  "Converts string to all upper-case."
  {:added "1.2"}
  [^CharSequence s]
  (.. s toString toUpperCase))

(defn ^CharSequence lower-case
  "Converts string to all lower-case."
  {:added "1.2"}
  [^CharSequence s]
  (.. s toString toLowerCase))

(defn split
  "Splits string on a regular expression.  Optional argument limit is
  the maximum number of splits. Not lazy. Returns vector of the splits."
  {:added "1.2"}
  ([^CharSequence s ^Pattern re]
     (LazilyPersistentVector/createOwning (.split re s)))
  ([ ^CharSequence s ^Pattern re limit]
     (LazilyPersistentVector/createOwning (.split re s limit))))

(defn ^CharSequence trim
  "Removes whitespace from both ends of string."
  {:added "1.2"}
  [^CharSequence s]
  (.. s toString trim))

(defn ^CharSequence triml
  "Removes whitespace from the left side of string."
  {:added "1.2"}
  [^CharSequence s]
  (loop [index (int 0)]
    (if (= (.length s) index)
      ""
      (if (Character/isWhitespace (.charAt s index))
        (recur (inc index))
        (.. s (subSequence index (.length s)) toString)))))

(defn ^CharSequence trimr
  "Removes whitespace from the right side of string."
  {:added "1.2"}
  [^CharSequence s]
  (loop [index (.length s)]
    (if (zero? index)
      ""
      (if (Character/isWhitespace (.charAt s (dec index)))
        (recur (dec index))
        (.. s (subSequence 0 index) toString)))))

(defn ^CharSequence trim-newline
  "Removes all trailing newline \\n or return \\r characters from
  string.  Similar to Perl's chomp."
  {:added "1.2"}
  [^CharSequence s]
  (loop [index (.length s)]
    (if (zero? index)
      ""
      (let [ch (.charAt s (dec index))]
        (if (or (= ch \newline) (= ch \return))
          (recur (dec index))
          (.. s (subSequence 0 index) toString))))))


