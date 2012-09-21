;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns
    ^{:author "Christophe Grand",
      :doc "Helper namespace for clojure.java.browse.
            Prevents console apps from becoming GUI unnecessarily."}
  clojure.java.browse-ui)

(defn- open-url-in-swing
  [url]
  (let [htmlpane (javax.swing.JEditorPane. url)]
    (.setEditable htmlpane false)
    (.addHyperlinkListener htmlpane
      (proxy [javax.swing.event.HyperlinkListener] []
        (hyperlinkUpdate [^javax.swing.event.HyperlinkEvent e]
          (when (= (.getEventType e) (. javax.swing.event.HyperlinkEvent$EventType ACTIVATED))
            (if (instance? javax.swing.text.html.HTMLFrameHyperlinkEvent e)
              (-> htmlpane .getDocument (.processHTMLFrameHyperlinkEvent e))
              (.setPage htmlpane (.getURL e)))))))
    (doto (javax.swing.JFrame.)
      (.setContentPane (javax.swing.JScrollPane. htmlpane))
      (.setBounds 32 32 700 900)
      (.show))))
      
