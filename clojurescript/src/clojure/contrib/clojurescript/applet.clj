;   Copyright (c) Chris Houser, Jan 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Applet that provides Clojure-to-JavaScript functionality to a browser

(ns clojure.contrib.clojurescript.applet
  (:import (java.io PrintWriter StringReader))
  (:gen-class
     :extends java.applet.Applet
     :methods [[tojs [String] Object]])
  (:use [clojure.contrib.clojurescript :only (formtojs filetojs)])
  (:require [clojure.contrib.duck-streams :as ds]))

(defn -tojs [this cljstr]
  (try
    ["js" (with-out-str (filetojs (StringReader. cljstr)
                                  :debug-fn-names false
                                  :debug-comments false
                                  :eval-defmacro true))]
    (catch Throwable e
      (if (= (.getMessage e) "EOF while reading")
        ["incomplete"]
        ["err" (with-out-str (.printStackTrace e (PrintWriter. *out*)))]))))

