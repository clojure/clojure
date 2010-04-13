;;; strint.clj -- String interpolation for Clojure
;; originally proposed/published at http://muckandbrass.com/web/x/AgBP

;; by Chas Emerick <cemerick@snowtide.com>
;; December 4, 2009

;; Copyright (c) Chas Emerick, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns
  #^{:author "Chas Emerick",
     :doc "String interpolation for Clojure."}
  clojure.contrib.strint
 (:use [clojure.contrib.io :only (slurp*)]))

(defn- silent-read
  "Attempts to clojure.core/read a single form from the provided String, returning
   a vector containing the read form and a String containing the unread remainder
   of the provided String.  Returns nil if no valid form can be read from the
   head of the String."
  [s]
  (try
    (let [r (-> s java.io.StringReader. java.io.PushbackReader.)]
      [(read r) (slurp* r)])
    (catch Exception e))) ; this indicates an invalid form -- the head of s is just string data

(defn- interpolate
  "Yields a seq of Strings and read forms."
  ([s atom?]
    (lazy-seq
      (if-let [[form rest] (silent-read (subs s (if atom? 2 1)))]
        (cons form (interpolate (if atom? (subs rest 1) rest)))
        (cons (subs s 0 2) (interpolate (subs s 2))))))
  ([#^String s]
    (if-let [start (->> ["~{" "~("]
                     (map #(.indexOf s %))
                     (remove #(== -1 %))
                     sort
                     first)]
      (lazy-seq (cons
                  (subs s 0 start)
                  (interpolate (subs s start) (= \{ (.charAt s (inc start))))))
      [s])))

(defmacro <<
  "Takes a single string argument and emits a str invocation that concatenates
   the string data and evaluated expressions contained within that argument.
   Evaluation is controlled using ~{} and ~() forms.  The former is used for
   simple value replacement using clojure.core/str; the latter can be used to
   embed the results of arbitrary function invocation into the produced string.

   Examples:
   user=> (def v 30.5)
   #'user/v
   user=> (<< \"This trial required ~{v}ml of solution.\")
   \"This trial required 30.5ml of solution.\"
   user=> (<< \"There are ~(int v) days in November.\")
   \"There are 30 days in November.\"
   user=> (def m {:a [1 2 3]})
   #'user/m
   user=> (<< \"The total for your order is $~(->> m :a (apply +)).\")
   \"The total for your order is $6.\"

   Note that quotes surrounding string literals within ~() forms must be
   escaped."
  [string]
  `(str ~@(interpolate string)))

