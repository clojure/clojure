;   Copyright (c) Christophe Grand, November 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; a repl helper to quickly open JDK javadocs.

(ns clojure.contrib.javadoc)

(defn- open-url-in-browser [url]
  (try 
    (when (clojure.lang.Reflector/invokeStaticMethod "java.awt.Desktop" 
      "isDesktopSupported" (to-array nil))
      (-> (clojure.lang.Reflector/invokeStaticMethod "java.awt.Desktop" 
            "getDesktop" (to-array nil))
        (.browse (java.net.URI. url)))
      url)
    (catch ClassNotFoundException e
      nil)))
        
(defn- open-url-in-swing [url]
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

(defn javadoc
 "Opens a browser window displaying the javadoc for the argument."
 [class-or-object]
  (let [c (if (instance? Class class-or-object) 
            class-or-object 
            (class class-or-object))
        url (str "http://java.sun.com/javase/6/docs/api/"
              (-> c .getName (.replace \. \/) (.replace \$ \.)) 
              ".html")]
    (or (open-url-in-browser url) (open-url-in-swing url))))

