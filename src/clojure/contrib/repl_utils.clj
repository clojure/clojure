;   Copyright (c) Chris Houser, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Utilities meant to be used interactively at the REPL

(ns clojure.contrib.repl-utils
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (java.lang.reflect Modifier Method Constructor)
           (clojure.lang RT))
  (:use [clojure.contrib.seq-utils :only (indexed)]
        [clojure.contrib.str-utils :only (str-join re-sub re-partition)]))

(defn- sortable [t]
  (apply str (map (fn [[a b]] (str a (format "%04d" (Integer. b))))
                  (partition 2 (concat (re-partition #"\d+" t) [0])))))

(defn- param-str [m]
  (str " (" (str-join
              "," (map (fn [[c i]]
                         (if (> i 3)
                           (str (.getSimpleName c) "*" i)
                           (str-join "," (replicate i (.getSimpleName c)))))
                       (reduce (fn [pairs y] (let [[x i] (peek pairs)]
                                               (if (= x y)
                                                 (conj (pop pairs) [y (inc i)])
                                                 (conj pairs [y 1]))))
                               [] (.getParameterTypes m))))
  ")"))

(defn- member-details [m]
  (let [static? (Modifier/isStatic (.getModifiers m))
        method? (instance? Method m)
        ctor?   (instance? Constructor m)
        text (if ctor?
               (str "<init>" (param-str m))
               (str
                 (when static? "static ")
                 (.getName m) " : "
                 (if method?
                   (str (.getSimpleName (.getReturnType m)) (param-str m))
                   (str (.getSimpleName (.getType m))))))]
    (assoc (bean m)
           :sort-val [(not static?) method? (sortable text)]
           :text text
           :member m)))

(defn show
  "With one arg prints all static and instance members of x or (class x).
  Each member is listed with a number which can be given as 'selector'
  to return the member object -- the REPL will print more details for
  that member.

  The selector also may be a string or regex, in which case only
  members whose names match 'selector' as a case-insensitive regex
  will be printed.

  Finally, the selector also may be a predicate, in which case only
  members for which the predicate returns true will be printed.  The
  predicate will be passed a single argument, a map that includes the
  :text that will be printed and the :member object itself, as well as
  all the properies of the member object as translated by 'bean'.

  Examples: (show Integer)  (show [])  (show String 23)  (show String \"case\")"
  ([x] (show x (constantly true)))
  ([x selector]
      (let [c (if (class? x) x (class x))
            members (sort-by :sort-val
                             (map member-details
                                  (concat (.getFields c)
                                          (.getMethods c)
                                          (.getConstructors c))))]
        (if (number? selector)
          (:member (nth members selector))
          (let [pred (if (ifn? selector)
                       selector
                       #(re-seq (re-pattern (str "(?i)" selector)) (:name %)))]
            (println "=== " (Modifier/toString (.getModifiers c)) c " ===")
            (doseq [[i m] (indexed members)]
              (when (pred m)
                (printf "[%2d] %s\n" i (:text m)))))))))

(defn get-source
  "Returns a string of the source code for the given symbol, if it can
  find it.  This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the classpath.  Returns nil if
  it can't find the source.  For most REPL usage, 'source' is more
  convenient.
  
  Example: (get-source 'filter)"
  [x]
  (when-let [v (resolve x)]
    (let [ns-str (str (ns-name (:ns ^v)))
          path (first (re-seq #"^.*(?=/[^/]*$)"
                              (-> ns-str
                                (.replace "." "/")
                                (.replace "-" "_"))))
          fname (str path "/" (:file ^v))]
      (when-let [strm (.getResourceAsStream (RT/baseLoader) fname)]
        (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
          (dotimes [_ (dec (:line ^v))] (.readLine rdr))
          (let [text (StringBuilder.)
                pbr (proxy [PushbackReader] [rdr]
                      (read [] (let [i (proxy-super read)]
                                 (.append text (char i))
                                 i)))]
            (read (PushbackReader. pbr))
            (str text)))))))

(defmacro source
  "Prints the source code for the given symbol, if it can find it.
  This requires that the symbol resolve to a Var defined in a
  namespace for which the .clj is in the classpath.
  
  Example: (source filter)"
  [n]
  `(println (or (get-source '~n) (str "Source not found"))))
