;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  clojure.contrib.miglayout.example
;;
;;  A temperature converter using miglayout. Demonstrates accessing
;;  components by their id constraint.
;;
;;  scgilardi (gmail)
;;  Created 31 May 2009

(ns clojure.contrib.miglayout.example
  (:import (javax.swing JButton JFrame JLabel JPanel JTextField))
  (:use (clojure.contrib
         [miglayout :only (miglayout components)]
         [swing-utils :only (add-key-typed-listener)])))

(defn fahrenheit
  "Converts a Celsius temperature to Fahrenheit. Input and output are
  strings. Returns \"input?\" if the input can't be parsed as a Double."
  [celsius]
  (try
   (format "%.2f" (+ 32 (* 1.8 (Double/parseDouble celsius))))
   (catch NumberFormatException _ "input?")))

(defn- handle-key
  "Clear output on most keys, show conversion on \"Enter\""
  [event in out]
  (.setText out
    (if (= (.getKeyChar event) \newline)
      (fahrenheit (.getText in))
      "")))

(defn main
  "Lays out and shows a Temperature Converter UI"
  []
  (let [panel
        (miglayout (JPanel.)
         (JTextField. 6) {:id :input}
         (JLabel. "\u00b0Celsius") :wrap
         (JLabel.) {:id :output}
         (JLabel. "\u00b0Fahrenheit"))
        {:keys [input output]} (components panel)]
    (add-key-typed-listener input handle-key input output)
    (doto (JFrame. "Temperature Converter")
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.add panel)
      (.pack)
      (.setVisible true))))
