;;  Copyright (c) Craig McDaniel, Jan 2009. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;  which can be found in the file epl-v10.html at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.

;;  Server socket library - includes REPL on socket

(ns 
  ^{:author "Craig McDaniel",
     :doc "Server socket library - includes REPL on socket"}
  clojure.contrib.server-socket
  (:import (java.net InetAddress ServerSocket Socket SocketException)
           (java.io InputStreamReader OutputStream OutputStreamWriter PrintWriter)
           (clojure.lang LineNumberingPushbackReader))
  (:use [clojure.main :only (repl)]))
 
(defn- on-thread [f]
  (doto (Thread. ^Runnable f) 
    (.start)))

(defn- close-socket [^Socket s]
  (when-not (.isClosed s)    
    (doto s
      (.shutdownInput)
      (.shutdownOutput)
      (.close))))

(defn- accept-fn [^Socket s connections fun]
  (let [ins (.getInputStream s)
        outs (.getOutputStream s)]
    (on-thread #(do
                  (dosync (commute connections conj s))
                  (try
                   (fun ins outs)
                   (catch SocketException e))
                  (close-socket s)
                  (dosync (commute connections disj s))))))

(defstruct server-def :server-socket :connections)

(defn- create-server-aux [fun ^ServerSocket ss]
  (let [connections (ref #{})]
    (on-thread #(when-not (.isClosed ss)
                  (try 
                   (accept-fn (.accept ss) connections fun)
                   (catch SocketException e))
                  (recur)))
    (struct-map server-def :server-socket ss :connections connections)))
 
(defn create-server 
  "Creates a server socket on port. Upon accept, a new thread is
  created which calls:

  (fun input-stream output-stream)

  Optional arguments support specifying a listen backlog and binding
  to a specific endpoint."
  ([port fun backlog ^InetAddress bind-addr] 
     (create-server-aux fun (ServerSocket. port backlog bind-addr)))
  ([port fun backlog]
     (create-server-aux fun (ServerSocket. port backlog)))
  ([port fun]
     (create-server-aux fun (ServerSocket. port))))

(defn close-server [server]
  (doseq [s @(:connections server)]
    (close-socket s))
  (dosync (ref-set (:connections server) #{}))
  (.close ^ServerSocket (:server-socket server)))

(defn connection-count [server]
  (count @(:connections server)))

;;;; 
;;;; REPL on a socket
;;;; 

(defn- socket-repl [ins outs]
  (binding [*in* (LineNumberingPushbackReader. (InputStreamReader. ins))
            *out* (OutputStreamWriter. outs)
            *err* (PrintWriter. ^OutputStream outs true)]
    (repl)))

(defn create-repl-server 
  "create a repl on a socket"
  ([port backlog ^InetAddress bind-addr] 
     (create-server port socket-repl backlog bind-addr))
  ([port backlog] 
     (create-server port socket-repl backlog))
  ([port] 
     (create-server port socket-repl)))
