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

(defn- member-vec [m]
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
    [[(not static?) method? (sortable text)] text m]))

(defn show
  ([x] (show x nil))
  ([x i]
      (let [c (if (class? x) x (class x))
            items (sort (for [m (concat (.getFields c)
                                        (.getMethods c)
                                        (.getConstructors c))]
                          (member-vec m)))]
        (if i
          (last (nth items i))
          (do
            (println "=== " (Modifier/toString (.getModifiers c)) c " ===")
            (doseq [[i e] (indexed items)]
              (printf "[%2d] %s\n" i (second e))))))))

(defn get-source [x]
  (when-let [v (resolve x)]
    (let [ns-name (str (.name (.ns v)))
          path (first (re-seq #"^.*(?=/[^/]*$)" (.replace ns-name "." "/")))
          fname (str path "/" (:file ^v))]
      (when-let [strm (.getResourceAsStream RT/ROOT_CLASSLOADER fname)]
        (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
          (dotimes [_ (dec (:line ^v))] (.readLine rdr))
          (let [text (StringBuilder.)
                pbr (proxy [PushbackReader] [rdr]
                      (read [] (let [i (proxy-super read)]
                                 (.append text (char i))
                                 i)))]
            (read (PushbackReader. pbr))
            (str text)))))))

(defmacro source [n]
  `(println (or (get-source '~n) (str "Source not found"))))
