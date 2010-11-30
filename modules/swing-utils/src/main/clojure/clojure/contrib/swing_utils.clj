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
  (:import (java.awt.event ActionListener KeyAdapter)
           (javax.swing AbstractAction Action 
                        JMenu JMenuBar JMenuItem
                        SwingUtilities))
  (:use [clojure.contrib.def :only (defvar)]))

(defn add-action-listener
  "Adds an ActionLister to component. When the action fires, f will be
  invoked with the event as its first argument followed by args.
  Returns the listener."
  [component f & args]
  (let [listener (proxy [ActionListener] []
                   (actionPerformed [event] (apply f event args)))]
    (.addActionListener component listener)
    listener))

(defn add-key-typed-listener
  "Adds a KeyListener to component that only responds to KeyTyped events.
  When a key is typed, f is invoked with the KeyEvent as its first argument
  followed by args. Returns the listener."
  [component f & args]
  (let [listener (proxy [KeyAdapter] []
                   (keyTyped [event] (apply f event args)))]
    (.addKeyListener component listener)
    listener))

;; ----------------------------------------------------------------------
;; Meikel Brandmeyer

(defn do-swing*
  "Runs thunk in the Swing event thread according to schedule:
    - :later => schedule the execution and return immediately
    - :now   => wait until the execution completes."
  [schedule thunk]
  (cond
   (= schedule :later) (SwingUtilities/invokeLater thunk)
   (= schedule :now) (if (SwingUtilities/isEventDispatchThread)
                       (thunk)
                       (SwingUtilities/invokeAndWait thunk)))
  nil)

(defmacro do-swing
  "Executes body in the Swing event thread asynchronously. Returns
  immediately after scheduling the execution."
  [& body]
  `(do-swing* :later (fn [] ~@body)))

(defmacro do-swing-and-wait
  "Executes body in the Swing event thread synchronously. Returns
  after the execution is complete."
  [& body]
  `(do-swing* :now (fn [] ~@body)))

(defvar action-translation-table
  (atom {:name        Action/NAME
         :accelerator Action/ACCELERATOR_KEY
         :command-key Action/ACTION_COMMAND_KEY
         :long-desc   Action/LONG_DESCRIPTION
         :short-desc  Action/SHORT_DESCRIPTION
         :mnemonic    Action/MNEMONIC_KEY
         :icon        Action/SMALL_ICON})
  "Translation table for the make-action constructor.")

(defn make-action
  "Create an Action proxy from the given action spec. The standard keys
  recognised are: :name, :accelerator, :command-key, :long-desc,
  :short-desc, :mnemonic and :icon - corresponding to the similar named
  Action properties.  The :handler value is used in the actionPerformed
  method of the proxy to pass on the event."
  [spec]
  (let [t-table @action-translation-table
        handler (:handler spec)
        spec    (dissoc spec :handler)
        spec    (map (fn [[k v]] [(t-table k) v]) spec)
        action  (proxy [AbstractAction] []
                  (actionPerformed [evt] (handler evt)))]
    (doseq [[k v] spec]
      (.putValue action k v))
    action))

(defvar menu-constructor-dispatch
  (atom #{:action :handler :items})
  "An atom containing the dispatch set for the add-menu-item method.")

(defmulti add-menu-item
  "Adds a menu item to the parent according to the item description.
   The item description is a map of the following structure.

 Either:
   - one single :action specifying a javax.swing.Action to be associated
     with the item.
   - a specification suitable for make-action
   - a set of :name, :mnemonic and :items keys, specifying a submenu with
     the given sequence of item entries.
   - an empty map specifying a separator."
  {:arglists '([parent item])}
  (fn add-menu-item-dispatch [_ item]
    (some @menu-constructor-dispatch (keys item))))

(defmethod add-menu-item :action
  add-menu-item-action
  [parent {:keys [action]}]
  (let [item (JMenuItem. action)]
    (.add parent item)))

(defmethod add-menu-item :handler
  add-menu-item-handler
  [parent spec]
  (add-menu-item parent {:action (make-action spec)}))

(defmethod add-menu-item :items
  add-menu-item-submenu
  [parent {:keys [items mnemonic name]}]
  (let [menu (JMenu. name)]
    (when mnemonic
      (.setMnemonic menu mnemonic))
    (doseq [item items]
      (add-menu-item menu item))
    (.add parent menu)))

(defmethod add-menu-item nil ; nil meaning separator
  add-menu-item-separator
  [parent _]
  (.addSeparator parent))

(defn make-menubar
  "Create a menubar containing the given sequence of menu items. The menu
  items are described by a map as is detailed in the docstring of the
  add-menu-item function."
  [menubar-items]
  (let [menubar (JMenuBar.)]
    (doseq [item menubar-items]
      (add-menu-item menubar item))
    menubar))

;; ----------------------------------------------------------------------
