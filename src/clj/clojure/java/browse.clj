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
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str])
  (:import (java.net URI)))

(defn- macosx? []
  (-> "os.name" System/getProperty .toLowerCase
    (.startsWith "mac os x")))

(defn- xdg-open-loc []
  ;; try/catch needed to mask exception on Windows without Cygwin
  (let [which-out (try (:out (sh/sh "which" "xdg-open"))
                       (catch Exception e ""))]
    (if (= which-out "")
      nil
      (str/trim-newline which-out))))

(defn- open-url-script-val []
  (if (macosx?)
    "/usr/bin/open"
    (xdg-open-loc)))

;; We could assign (open-url-script-val) to *open-url-script* right
;; away in the def below, but clojure.java.shell/sh creates a future
;; that causes a long wait for the JVM to exit during Clojure compiles
;; (unless we can somehow here make it call (shutdown-agents) later).
;; Better to initialize it when we first need it, in browse-url.

(def ^:dynamic *open-url-script* (atom :uninitialized))

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
  (let [script @*open-url-script*
        script (if (= :uninitialized script)
                 (reset! *open-url-script* (open-url-script-val))
                 script)]
    (or (when script (sh/sh script (str url)) true)
        (open-url-in-browser url)
        (open-url-in-swing url))))
