;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  clojure.contrib.swing-utils
;;
;;  Useful functions for interfacing Clojure to Swing
;;
;;  scgilardi (gmail)
;;  Created 31 May 2009

(ns clojure.contrib.swing-utils
  (:import java.awt.event.ActionListener))

(defn add-action-listener
  "Adds an ActionLister to component. When the action fires, f will be
  invoked with the event as its first argument followed by args"
  [component f & args]
  (.addActionListener component (proxy [ActionListener] []
    (actionPerformed [evt] (apply f evt args)))))
