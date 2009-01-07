;;; browse.clj -- start a web browser from Clojure

;   Copyright (c) Christophe Grand, December 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.contrib.javadoc.browse
  (:require [clojure.contrib.shell-out :as sh]) 
  (:import (java.net URI)))

(defn- macosx? []
  (-> "os.name" System/getProperty .toLowerCase
    (.startsWith "mac os x")))

(def *open-url-script* (when (macosx?) "/usr/bin/open"))

(defn open-url-in-browser
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

(defn open-url-in-swing
  "Opens url (a string) in a Swing window."
  [url]
  (let [htmlpane (javax.swing.JEditorPane. url)]
    (.setEditable htmlpane false)
    (.addHyperlinkListener htmlpane
      (proxy [javax.swing.event.HyperlinkListener] []
        (hyperlinkUpdate [#^javax.swing.event.HyperlinkEvent e]
          (when (= (.getEventType e) (. javax.swing.event.HyperlinkEvent$EventType ACTIVATED))
            (if (instance? javax.swing.text.html.HTMLFrameHyperlinkEvent e)
              (-> htmlpane .getDocument (.processHTMLFrameHyperlinkEvent e))
              (.setPage htmlpane (.getURL e)))))))
    (doto (javax.swing.JFrame.)
      (.setContentPane (javax.swing.JScrollPane. htmlpane))
      (.setBounds 32 32 700 900)
      (.show))))

(defn browse-url [url]
  (or (open-url-in-browser url) (when *open-url-script* (sh/sh *open-url-script* (str url)) true) (open-url-in-swing url)))
