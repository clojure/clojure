;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  clojure.contrib.miglayout.test
;;
;;  Test/example for clojure.contrib.miglayout
;;
;;  scgilardi (gmail)
;;  Created 5 October 2008

(ns clojure.contrib.miglayout.test
  (:import (javax.swing JButton JFrame JLabel JList JPanel
                        JScrollPane JTabbedPane JTextField JSeparator))
  (:use clojure.contrib.miglayout))

(def tests)

(defn run-test
  [index]
  (doto (JFrame. (format "MigLayout Test %d" index))
    (add ((tests index) (JPanel.)))
    (pack)
    (setVisible true)))

(def tests [

  (fn test0
    [panel]
    (miglayout panel
      (JLabel. "Hello")
      (JLabel. "World") {:gap :unrelated}
      (JTextField. 10) :wrap
      (JLabel. "Bonus!")
      (JButton. "Bang it") {:wmin :button :grow :x :span 2} :center))

  ;; test1 and test2 are based on code from
  ;; http://www.devx.com/java/Article/38017/1954

  ;; constraints as strings exclusively
  (fn test1
    [panel]
    (miglayout panel
      :column               "[right]"
      (JLabel. "General")   "split, span"
      (JSeparator.)         "growx, wrap"
      (JLabel. "Company")   "gap 10"
      (JTextField. "")      "span, growx"
      (JLabel. "Contact")   "gap 10"
      (JTextField. "")      "span, growx, wrap"
      (JLabel. "Propeller") "split, span, gaptop 10"
      (JSeparator.)         "growx, wrap, gaptop 10"
      (JLabel. "PTI/kW")    "gapx 10, gapy 15"
      (JTextField. 10)
      (JLabel. "Power/kW")  "gap 10"
      (JTextField. 10)      "wrap"
      (JLabel. "R/mm")      "gap 10"
      (JTextField. 10)
      (JLabel. "D/mm")      "gap 10"
      (JTextField. 10)))

  ;; the same constraints as strings, keywords, vectors, and maps
  (fn test2
    [panel]
    (miglayout panel
      :column               "[right]"
      (JLabel. "General")   "split, span"
      (JSeparator.)         :growx :wrap
      (JLabel. "Company")   [:gap 10]
      (JTextField. "")      :span :growx
      (JLabel. "Contact")   [:gap 10]
      (JTextField. "")      :span :growx :wrap
      (JLabel. "Propeller") :split :span [:gaptop 10]
      (JSeparator.)         :growx :wrap [:gaptop 10]
      (JLabel. "PTI/kW")    {:gapx 10 :gapy 15}
      (JTextField. 10)
      (JLabel. "Power/kW")  [:gap 10]
      (JTextField. 10)      :wrap
      (JLabel. "R/mm")      [:gap 10]
      (JTextField. 10)
      (JLabel. "D/mm")      [:gap 10]
      (JTextField. 10)))])
