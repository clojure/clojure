;   Copyright (c) Chris Houser, Sep 2008-Jan 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Command Line Interface for generating JavaScript from Clojure code.

(ns clojure.contrib.clojurescript.cli
  (:import (java.io PrintWriter StringReader)
           (java.net URLDecoder))
  (:use [clojure.contrib.command-line :only (with-command-line)]
        [clojure.contrib.clojurescript :only (formtojs filetojs)])
  (:require [clojure.contrib.duck-streams :as ds]))

(defn start-server [port]
  ;(println "Opening port" port)
  (loop [server (java.net.ServerSocket. port)] ; TODO bind only to 127.0.0.1
    (send-off (agent (.accept server))
      (fn [socket]
        (with-open [socket socket]
          (binding [*out* (-> socket .getOutputStream ds/writer)]
            (try
              (print "HTTP/1.0 200 OK\nContent-Type: text/javascript\n\n")
              (let [line1 (-> socket .getInputStream ds/reader .readLine)
                    [_ url] (re-find #"^GET /\?(.*?) HTTP" line1)
                    codestr (URLDecoder/decode url)
                    js (with-out-str (filetojs (StringReader. codestr)
                                               :debug-fn-names false
                                               :debug-comments false
                                               :eval-defmacro false))]
                (println "jsrepl.state('compiled');try{")
                (println "jsrepl.lastval=" js )
                (println "jsrepl.state('done');}catch(e){jsrepl.err(e)};"))
              (catch Exception e
                (if (= (.getMessage e) "EOF while reading")
                  (println "jsrepl.state('incomplete');")
                  (let [trace (with-out-str
                                (.printStackTrace e (PrintWriter. *out*)))]
                    (println "jsrepl.state('error',\""
                             (.replace trace "\n" "\\n") "\");")))))))))
    (recur server)))

(defn mkcore []
  (binding [*out* (ds/writer "core.js")]
    (doseq [file ["clojure/core.clj" "clojure/core_print.clj"]]
      (filetojs (.getResourceAsStream (clojure.lang.RT/baseLoader) file)))))

(defn simple-tests []
  (println (formtojs
    '(defn foo
      ([a b c & d] (prn 3 a b c))
      ([c]
        ;(String/asd "hello")
        ;(.foo 55)
        (let [[a b] [1 2]]
          (prn a b c)
          "hi")))))

  (println (formtojs
    '(defn foo [a]
      (prn "hi")
      (let [a 5]
        (let [a 10]
          (prn "yo")
          (prn a))
        (prn a))
      (prn a))))

  (println (formtojs
    '(defn x [] (conj [] (loop [i 5] (if (pos? i) (recur (- i 2)) i))))))

  ;(println (formtojs '(binding [*out* 5] (set! *out* 10))))
  (println (formtojs '(.replace "a/b/c" "/" ".")))
  (println (formtojs '(.getName ":foo")))
  (println (formtojs '(list '(1 "str" 'sym :key) 4 "str2" 6 #{:set 9 8})))
  (println (formtojs '(fn forever[] (forever))))
  (println (formtojs '(fn forever[] (loop [] (recur))))))

(when-not *compile-files*
  (with-command-line *command-line-args*
    "clojurescript.cli -- Compile ClojureScript to JavaScript"
    [[simple? "Runs some simple built-in tests"]
     [serve   "Starts a repl server on the given port" 8081]
     [mkcore? "Generates a core.js file"]
     [v?      "Includes extra fn names and comments in js"]
     filenames]
    (cond
      simple? (simple-tests)
      serve   (start-server (Integer/parseInt serve))
      mkcore? (mkcore)
      :else   (doseq [filename filenames]
                (filetojs filename :debug-fn-names v? :debug-comments v?)))))
