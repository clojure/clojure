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
  (:use [clojure.contrib.str-utils :only (str-join)]))

(defn show
  ([x] (show x nil))
  ([x i]
      (let [c (if (class? x) x (class x))
            items (sort
                    (for [m (concat (.getFields c)
                                    (.getMethods c)
                                    (.getConstructors c))]
                      (let [static? (bit-and Modifier/STATIC
                                             (.getModifiers m))
                            method? (instance? Method m)
                            ctor?   (instance? Constructor m)
                            text (if ctor?
                                   (str "(" (str-join
                                              ", " (.getParameterTypes m)) ")")
                                   (str
                                     (if (pos? static?) "static ")
                                     (.getName m) " : "
                                     (if method?
                                       (str (.getReturnType m) " ("
                                            (count (.getParameterTypes m)) ")")
                                       (str (.getType m)))))]
                        [(- static?) method? text (str m) m])))]
        (if i
          (last (nth items i))
          (do (println "=== " c " ===")
            (doseq [[e i] (map list items (iterate inc 0))]
              (printf "[%2d] %s\n" i (nth e 2))))))))

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
