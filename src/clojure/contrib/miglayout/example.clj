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
;;  Temperature converter using miglayout. Demonstrates accessing components
;;  by their id constraint.
;;
;;  scgilardi (gmail)
;;  Created 31 May 2009

(ns clojure.contrib.miglayout.example
  (:import (javax.swing JButton JFrame JLabel JPanel JTextField))
  (:use (clojure.contrib
         [miglayout :only (miglayout components)]
         [swing-utils :only (add-action-listener)])))

(defn fahrenheit
  "Converts a Celsius temperature to Fahrenheit. Input and output are
  strings. Returns \"input?\" if the input can't be parsed as a Double."
  [celsius]
  (try
   (format "%.2f \u00b0Fahrenheit"
           (+ 32 (* 1.8 (Double/parseDouble celsius))))
   (catch NumberFormatException _
     "input?")))

(defn main
  "Lays out and displays the Temperature Converter UI"
  []
  (let [panel
        (miglayout (JPanel.)
         (JTextField.) {:id :input :width 120}
         (JLabel. "\u00b0Celsius") :wrap
         (JButton. "Convert") {:id :convert}
         (JLabel. "\u00b0Fahrenheit") {:id :output :width 120})
        {:keys [convert input output]} (components panel)]
    (add-action-listener convert
      (fn [evt in out] (.setText out (fahrenheit (.getText in))))
      input output)
    (doto (JFrame. "Temperature Converter")
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.add panel)
      (.pack)
      (.setVisible true))))
