;   Copyright (c) Chris Houser, Nov-Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Process command-line arguments according to a given cmdspec

(ns 
    #^{:author "Chris Houser", 
       :doc "Process command-line arguments according to a given cmdspec"}
    clojure.contrib.command-line
    (:require (clojure.contrib [seq-utils :as su]))
    (:use     (clojure.contrib [str-utils :only (str-join)])))

(defn make-map [args cmdspec]
  (let [{spec true [rest-sym] false} (su/group-by vector? cmdspec)
        rest-str (str rest-sym)
        key-data (into {} (for [[syms [_ default]] (map #(split-with symbol? %)
                                                        (conj spec '[help? h?]))
                                sym syms]
                            [(re-find #"^.*[^?]" (str sym))
                             {:sym (str (first syms)) :default default}]))]
    (loop [[argkey & [argval :as r]] (if (seq args) args ["--help"])
           cmdmap {:cmdspec cmdspec rest-str []}]
      (if argkey
        (let [[_ & [keybase]] (re-find #"^--?(.*)" argkey)]
          (cond
            (= keybase nil) (recur r (update-in cmdmap [rest-str] conj argkey))
            (= keybase "")  (update-in cmdmap [rest-str] #(apply conj % r))
            :else (if-let [found (key-data keybase)]
                    (if (= \? (last (:sym found)))
                      (recur r (assoc cmdmap (:sym found) true))
                      (recur (next r) (assoc cmdmap (:sym found)
                                             (if (or (nil? r) (= \- (ffirst r)))
                                               (:default found)
                                               (first r)))))
                    (throw (Exception. (str "Unknown option " argkey))))))
        cmdmap))))

(defn- align
   "Align strings given as vectors of columns, with first vector
   specifying right or left alignment (:r or :l) for each column."
   [spec & rows]
   (let [maxes (vec (for [n (range (count (first rows)))]
                        (apply max (map (comp count #(nth % n)) rows))))
         fmt (str-join " " 
                  (for [n (range (count maxes))] 
                     (str "%" 
                        (when-not (zero? (maxes n))
                           (str (when (= (spec n) :l) "-") (maxes n))) 
                          "s")))]
      (str-join "\n"
         (for [row rows]
            (apply format fmt row)))))

(defn- rmv-q
   "Remove ?"
   [#^String s]
   (if (.endsWith s "?")
      (.substring s 0 (dec (count s)))
      s))

(defn print-help [desc cmdmap]
  (println desc)
  (println "Options")
  (println 
     (apply align [:l :l :l] 
        (for [spec (:cmdspec cmdmap) :when (vector? spec)]
            (let [[argnames [text default]] (split-with symbol? spec)
                  [_ opt q] (re-find #"^(.*[^?])(\??)$"
                                 (str (first argnames)))
                  argnames  (map (comp rmv-q str) argnames)
                  argnames
                        (str-join ", "
                          (for [arg argnames]
                            (if (= 1 (count arg))
                              (str "-" arg)
                              (str "--" arg))))]
               [(str "  " argnames (when (= "" q) " <arg>") " ")
                text 
                (if-not default
                  ""
                  (str " [default " default "]"))])))))

(defmacro with-command-line 
  "Bind locals to command-line args."
  [args desc cmdspec & body]
  (let [locals (vec (for [spec cmdspec]
                      (if (vector? spec)
                        (first spec)
                        spec)))]
    `(let [{:strs ~locals :as cmdmap#} (make-map ~args '~cmdspec)]
       (if (cmdmap# "help?")
         (print-help ~desc cmdmap#)
         (do ~@body)))))

(comment

; example of usage:

(with-command-line *command-line-args*
  "tojs -- Compile ClojureScript to JavaScript"
  [[simple? s? "Runs some simple built-in tests"]
   [serve      "Starts a repl server on the given port" 8081]
   [mkboot?    "Generates a boot.js file"]
   [verbose? v? "Includes extra fn names and comments in js"]
   filenames]
  (binding [*debug-fn-names* verbose? *debug-comments* verbose?]
    (cond
      simple? (simple-tests)
      serve   (start-server (Integer/parseInt serve))
      mkboot? (mkboot)
      :else   (doseq [filename filenames]
                 (filetojs filename)))))

)
