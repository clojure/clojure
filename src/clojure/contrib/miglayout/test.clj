;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
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
    (.add ((tests index) (JPanel.)))
    (.pack)
    (.setVisible true)))

(defn label
  "Returns a swing label"
  [text]
  (JLabel. text))

(defn text-field
  "Returns a swing text field"
  ([] (text-field 10))
  ([width]
     (JTextField. width)))

(defn sep
  "Returns a swing separator"
  []
  (JSeparator.))

(def tests [

  (fn test0
    [panel]
    (miglayout panel
      (label "Hello")
      (label "World") {:gap :unrelated}
      (text-field) :wrap
      (label "Bonus!")
      (JButton. "Bang it") {:wmin :button :grow :x :span 2} :center))

  ;; test1 and test2 are based on code from
  ;; http://www.devx.com/java/Article/38017/1954

  ;; constraints as strings exclusively
  (fn test1
    [panel]
    (miglayout panel
      :column             "[right]"
      (label "General")   "split, span"
      (sep)               "growx, wrap"
      (label "Company")   "gap 10"
      (text-field "")     "span, growx"
      (label "Contact")   "gap 10"
      (text-field "")     "span, growx, wrap"
      (label "Propeller") "split, span, gaptop 10"
      (sep)               "growx, wrap, gaptop 10"
      (label "PTI/kW")    "gapx 10, gapy 15"
      (text-field)
      (label "Power/kW")  "gap 10"
      (text-field)        "wrap"
      (label "R/mm")      "gap 10"
      (text-field)
      (label "D/mm")      "gap 10"
      (text-field)))

  ;; the same constraints as strings, keywords, vectors, and maps
  (fn test2
    [panel]
    (miglayout panel
      :column             "[right]"
      (label "General")   "split, span"
      (sep)               :growx :wrap
      (label "Company")   [:gap 10]
      (text-field "")     :span :growx
      (label "Contact")   [:gap 10]
      (text-field "")     :span :growx :wrap
      (label "Propeller") :split :span [:gaptop 10]
      (sep)               :growx :wrap [:gaptop 10]
      (label "PTI/kW")    {:gapx 10 :gapy 15}
      (text-field)
      (label "Power/kW")  [:gap 10]
      (text-field)        :wrap
      (label "R/mm")      [:gap 10]
      (text-field)
      (label "D/mm")      [:gap 10]
      (text-field)))

  ;; the same constraints using symbols to name groups of constraints
  (fn test3
    [panel]
    (let [g [:gap 10]
          gt [:gaptop 10]
          gxs #{:growx :span}
          gxw #{:growx :wrap}
          gxy {:gapx 10 :gapy 15}
          right "[right]"
          ss #{:split :span}
          w :wrap]
      (miglayout panel
        :column             right
        (label "General")   ss
        (sep)               gxw
        (label "Company")   g
        (text-field "")     gxs
        (label "Contact")   g
        (text-field "")     gxs
        (label "Propeller") ss gt
        (sep)               gxw g
        (label "PTI/kW")    gxy
        (text-field)
        (label "Power/kW")  g
        (text-field)        w
        (label "R/mm")      g
        (text-field)
        (label "D/mm")      g
        (text-field))))

  (fn test4
    [panel]
    (miglayout panel
      (label "First Name")
      (text-field)
      (label "Surname")   [:gap :unrelated]
      (text-field)        :wrap
      (label "Address")
      (text-field)        :span :grow))

])
