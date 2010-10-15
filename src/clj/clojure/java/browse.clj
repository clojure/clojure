;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns 
  ^{:author "Christophe Grand",
    :doc "Start a web browser from Clojure"}
  clojure.java.browse
  (:require [clojure.java.shell :as sh]) 
  (:import (java.net URI)))

(defn- macosx? []
  (-> "os.name" System/getProperty .toLowerCase
    (.startsWith "mac os x")))

(def ^:dynamic *open-url-script* (when (macosx?) "/usr/bin/open"))

(defn- open-url-in-browser
  "Opens url (a string) in the default system web browser.  May not
  work on all platforms.  Returns url on success, nil if not
  supported."
  [url]
  (try 
    (when (clojure.lang.Reflector/invokeStaticMethod "java.awt.Desktop" 
      "isDesktopSupported" (to-array nil))
      (-> (clojure.lang.Reflector/invokeStaticMethod "java.awt.Desktop" 
            "getDesktop" (to-array nil))
        (.browse (URI. url)))
      url)
    (catch ClassNotFoundException e
      nil)))        

(defn- open-url-in-swing
 "Opens url (a string) in a Swing window."
 [url]
  ; the implementation of this function resides in another namespace to be loaded "on demand"
  ; this fixes a bug on mac os x where the process turns into a GUI app
  ; see http://code.google.com/p/clojure-contrib/issues/detail?id=32
  (require 'clojure.java.browse-ui)
  ((find-var 'clojure.java.browse-ui/open-url-in-swing) url))

(defn browse-url
  "Open url in a browser"
  {:added "1.2"}
  [url]
  (or (open-url-in-browser url)
      (when *open-url-script* (sh/sh *open-url-script* (str url)) true)
      (open-url-in-swing url)))
