;   Copyright (c) Chris Houser, Nov-Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Process command-line arguments according to a given cmdspec

(ns clojure.contrib.command-line
    (:require (clojure.contrib [seq-utils :as su])))

(defn make-map [args cmdspec]
  (let [{specs true [rest-name] false} (su/group-by vector? cmdspec)
        names (assoc (into {} (for [[n txt d] specs] [(str n) d]))
                 "help?" nil)]
    (loop [[argkey & [argval :as r]] (if (seq args) args ["--help"])
           cmdmap {:cmdspec cmdspec rest-name []}]
      (if argkey
        (let [[_ & [keybase]] (re-find #"^--?(.*)" argkey)]
          (cond
            (= keybase nil) (recur r (update-in cmdmap [rest-name] conj argkey))
            (= keybase "")  (update-in cmdmap [rest-name] #(apply conj % r))
            (contains? names keybase)
                 (recur (rest r) (assoc cmdmap (symbol keybase)
                                        (if (or (nil? r) (= \- (ffirst r)))
                                          (names keybase)
                                          (first r))))
            (contains? names (str keybase "?"))
                 (recur r (assoc cmdmap (symbol (str keybase "?")) true))
            :else (throw (Exception. (str "Unknown option " argkey)))))
        cmdmap))))

(defn print-help [desc cmdmap]
  (println desc "\n")
  (doseq [spec (:cmdspec cmdmap)]
    (when (vector? spec)
      (let [[argname text default] spec]
        (println (format "--%-10s %s"
                         (let [[_ opt q] (re-find #"^(.*[^?])(\??)$"
                                                  (str argname))]
                           (str opt (when (= "" q) " x")))
                         (str text (when default
                                     (str " [default " default "]")))))))))

(defmacro with-command-line [args desc cmdspec & body]
  "Bind locals to command-line args."
  (let [locals (vec (for [spec cmdspec]
                      (if (vector? spec)
                        (first spec)
                        spec)))]
    `(let [{:syms ~locals :as cmdmap#} (make-map ~args '~cmdspec)]
       (if (cmdmap# '~'help?)
         (print-help ~desc cmdmap#)
         (do ~@body)))))

(comment

; example of usage:

(with-command-line *command-line-args*
  "tojs -- Compile ClojureScript to JavaScript"
  [[simple? "Runs some simple built-in tests"]
   [serve   "Starts a repl server on the given port" 8081]
   [mkboot? "Generates a boot.js file"]
   [v?      "Includes extra fn names and comments in js"]
   filenames]
  (binding [*debug-fn-names* v? *debug-comments* v?]
    (cond
      simple? (simple-tests)
      serve   (start-server (Integer/parseInt serve))
      mkboot? (mkboot)
      :else   (doseq filename filenames
                 (filetojs filename)))))

)
