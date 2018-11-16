;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Socket server support"
      :author "Alex Miller"}
  clojure.core.server
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.main :as m])
  (:import
   [clojure.lang LineNumberingPushbackReader]
   [java.net InetAddress Socket ServerSocket SocketException]
   [java.io Reader Writer PrintWriter BufferedWriter BufferedReader InputStreamReader OutputStreamWriter]
   [java.util.concurrent.locks ReentrantLock]))

(set! *warn-on-reflection* true)

(def ^:dynamic *session* nil)

;; lock protects servers
(defonce ^:private lock (ReentrantLock.))
(defonce ^:private servers {})

(defmacro ^:private with-lock
  [lock-expr & body]
  `(let [lockee# ~(with-meta lock-expr {:tag 'java.util.concurrent.locks.ReentrantLock})]
     (.lock lockee#)
     (try
       ~@body
       (finally
         (.unlock lockee#)))))

(defmacro ^:private thread
  [^String name daemon & body]
  `(doto (Thread. (fn [] ~@body) ~name)
    (.setDaemon ~daemon)
    (.start)))

(defn- required
  "Throw if opts does not contain prop."
  [opts prop]
  (when (nil? (get opts prop))
    (throw (ex-info (str "Missing required socket server property " prop) opts))))

(defn- validate-opts
  "Validate server config options"
  [{:keys [name port accept] :as opts}]
  (doseq [prop [:name :port :accept]] (required opts prop))
  (when (or (not (integer? port)) (not (< -1 port 65535)))
    (throw (ex-info (str "Invalid socket server port: " port) opts))))

(defn- accept-connection
  "Start accept function, to be invoked on a client thread, given:
    conn - client socket
    name - server name
    client-id - client identifier
    in - in stream
    out - out stream
    err - err stream
    accept - accept fn symbol to invoke
    args - to pass to accept-fn"
  [^Socket conn name client-id in out err accept args]
  (try
    (binding [*in* in
              *out* out
              *err* err
              *session* {:server name :client client-id}]
      (with-lock lock
        (alter-var-root #'servers assoc-in [name :sessions client-id] {}))
      (require (symbol (namespace accept)))
      (let [accept-fn (resolve accept)]
        (apply accept-fn args)))
    (catch SocketException _disconnect)
    (finally
      (with-lock lock
        (alter-var-root #'servers update-in [name :sessions] dissoc client-id))
      (.close conn))))

(defn start-server
  "Start a socket server given the specified opts:
    :address Host or address, string, defaults to loopback address
    :port Port, integer, required
    :name Name, required
    :accept Namespaced symbol of the accept function to invoke, required
    :args Vector of args to pass to accept function
    :bind-err Bind *err* to socket out stream?, defaults to true
    :server-daemon Is server thread a daemon?, defaults to true
    :client-daemon Are client threads daemons?, defaults to true
   Returns server socket."
  [opts]
  (validate-opts opts)
  (let [{:keys [address port name accept args bind-err server-daemon client-daemon]
         :or {bind-err true
              server-daemon true
              client-daemon true}} opts
         address (InetAddress/getByName address)  ;; nil returns loopback
         socket (ServerSocket. port 0 address)]
    (with-lock lock
      (alter-var-root #'servers assoc name {:name name, :socket socket, :sessions {}}))
    (thread
      (str "Clojure Server " name) server-daemon
      (try
        (loop [client-counter 1]
          (when (not (.isClosed socket))
            (try
              (let [conn (.accept socket)
                    in (LineNumberingPushbackReader. (InputStreamReader. (.getInputStream conn)))
                    out (BufferedWriter. (OutputStreamWriter. (.getOutputStream conn)))
                    client-id (str client-counter)]
                (thread
                  (str "Clojure Connection " name " " client-id) client-daemon
                  (accept-connection conn name client-id in out (if bind-err out *err*) accept args)))
              (catch SocketException _disconnect))
            (recur (inc client-counter))))
        (finally
          (with-lock lock
            (alter-var-root #'servers dissoc name)))))
    socket))

(defn stop-server
  "Stop server with name or use the server-name from *session* if none supplied.
  Returns true if server stopped successfully, nil if not found, or throws if
  there is an error closing the socket."
  ([]
   (stop-server (:server *session*)))
  ([name]
   (with-lock lock
     (let [server-socket ^ServerSocket (get-in servers [name :socket])]
       (when server-socket
         (alter-var-root #'servers dissoc name)
         (.close server-socket)
         true)))))

(defn stop-servers
  "Stop all servers ignores all errors, and returns nil."
  []
  (with-lock lock
    (doseq [name (keys servers)]
      (future (stop-server name)))))

(defn- parse-props
  "Parse clojure.server.* from properties to produce a map of server configs."
  [props]
  (reduce
    (fn [acc [^String k ^String v]]
      (let [[k1 k2 k3] (str/split k #"\.")]
        (if (and (= k1 "clojure") (= k2 "server"))
          (conj acc (merge {:name k3} (edn/read-string v)))
          acc)))
    [] props))

(defn start-servers
  "Start all servers specified in the system properties."
  [system-props]
  (doseq [server (parse-props system-props)]
    (start-server server)))

(defn repl-init
  "Initialize repl in user namespace and make standard repl requires."
  []
  (in-ns 'user)
  (apply require clojure.main/repl-requires))

(defn repl-read
  "Enhanced :read hook for repl supporting :repl/quit."
  [request-prompt request-exit]
  (or ({:line-start request-prompt :stream-end request-exit}
        (m/skip-whitespace *in*))
      (let [input (read {:read-cond :allow} *in*)]
        (m/skip-if-eol *in*)
        (case input
          :repl/quit request-exit
          input))))

(defn repl
  "REPL with predefined hooks for attachable socket server."
  []
  (m/repl
    :init repl-init
    :read repl-read))

(defn- ex->data
  [ex phase]
  (assoc (Throwable->map ex) :phase phase))

(defn prepl
  "a REPL with structured output (for programs)
  reads forms to eval from in-reader (a LineNumberingPushbackReader)
  Closing the input or passing the form :repl/quit will cause it to return

  Calls out-fn with data, one of:
  {:tag :ret
   :val val ;;eval result
   :ns ns-name-string
   :ms long ;;eval time in milliseconds
   :form string ;;iff successfully read
   :clojure.error/phase (:execution et al per clojure.main/ex-triage) ;;iff error occurred
  }
  {:tag :out
   :val string} ;chars from during-eval *out*
  {:tag :err
   :val string} ;chars from during-eval *err*
  {:tag :tap
   :val val} ;values from tap>

  You might get more than one :out or :err per eval, but exactly one :ret
  tap output can happen at any time (i.e. between evals)
  If during eval an attempt is made to read *in* it will read from in-reader unless :stdin is supplied

  Alpha, subject to change."
  {:added "1.10"}
  [in-reader out-fn & {:keys [stdin]}]
  (let [EOF (Object.)
        tapfn #(out-fn {:tag :tap :val %1})]
    (m/with-bindings
      (in-ns 'user)
      (binding [*in* (or stdin in-reader)
                *out* (PrintWriter-on #(out-fn {:tag :out :val %1}) nil)
                *err* (PrintWriter-on #(out-fn {:tag :err :val %1}) nil)]
        (try
          (add-tap tapfn)
          (loop []
            (when (try
                    (let [[form s] (read+string in-reader false EOF)]
                      (try
                        (when-not (identical? form EOF)
                          (let [start (System/nanoTime)
                                ret (eval form)
                                ms (quot (- (System/nanoTime) start) 1000000)]
                            (when-not (= :repl/quit ret)
                              (set! *3 *2)
                              (set! *2 *1)
                              (set! *1 ret)
                              (out-fn {:tag :ret
                                       :val (if (instance? Throwable ret)
                                              (Throwable->map ret)
                                              ret)
                                       :ns (str (.name *ns*))
                                       :ms ms
                                       :form s})
                              true)))
                        (catch Throwable ex
                          (set! *e ex)
                          (out-fn {:tag :ret :val (ex->data ex (or (-> ex ex-data :clojure.error/phase) :execution))
                                   :ns (str (.name *ns*)) :form s
                                   :exception true})
                          true)))
                    (catch Throwable ex
                      (set! *e ex)
                      (out-fn {:tag :ret :val (ex->data ex :read-source)
                               :ns (str (.name *ns*))
                               :exception true})
                      true))
              (recur)))
          (finally
           (remove-tap tapfn)))))))

(defn- resolve-fn [valf]
  (if (symbol? valf)
    (or (resolve valf)
        (when-let [nsname (namespace valf)]
          (require (symbol nsname))
          (resolve valf))
        (throw (Exception. (str "can't resolve: " valf))))
    valf))

(defn io-prepl
  "prepl bound to *in* and *out*, suitable for use with e.g. server/repl (socket-repl).
  :ret and :tap vals will be processed by valf, a fn of one argument
  or a symbol naming same (default pr-str)

  Alpha, subject to change."
  {:added "1.10"}
  [& {:keys [valf] :or {valf pr-str}}]
  (let [valf (resolve-fn valf)
        out *out*
        lock (Object.)]
    (prepl *in*
           (fn [m]
             (binding [*out* out, *flush-on-newline* true, *print-readably* true]
               (locking lock
                 (prn (if (#{:ret :tap} (:tag m))
                        (try
                          (assoc m :val (valf (:val m)))
                          (catch Throwable ex
                            (assoc m :val (ex->data ex :print-eval-result)
                                     :exception true)))
                        m))))))))

(defn remote-prepl
  "Implements a prepl on in-reader and out-fn by forwarding to a
  remote [io-]prepl over a socket.  Messages will be read by readf, a
  fn of a LineNumberingPushbackReader and EOF value or a symbol naming
  same (default #(read %1 false %2)),
  :ret and :tap vals will be processed by valf, a fn of one argument
  or a symbol naming same (default read-string). If that function
  throws, :val will be unprocessed.

  Alpha, subject to change."
  {:added "1.10"}
  [^String host port ^Reader
   in-reader out-fn & {:keys [valf readf] :or {valf read-string, readf #(read %1 false %2)}}]
  (let [valf (resolve-fn valf)
        readf (resolve-fn readf)
        ^long port (if (string? port) (Integer/valueOf ^String port) port)
        socket (Socket. host port)
        rd (-> socket .getInputStream InputStreamReader. BufferedReader. LineNumberingPushbackReader.)
        wr (-> socket .getOutputStream OutputStreamWriter.)
        EOF (Object.)]
    (thread "clojure.core.server/remote-prepl" true
            (try (loop []
                   (let [{:keys [tag val] :as m} (readf rd EOF)]
                     (when-not (identical? m EOF)
                       (out-fn
                        (if (#{:ret :tap} tag)
                          (try
                            (assoc m :val (valf val))
                            (catch Throwable ex
                              (assoc m :val (ex->data ex :read-eval-result)
                                       :exception true)))
                          m))
                       (recur))))
                 (finally
                  (.close wr))))
    (let [buf (char-array 1024)]
      (try (loop []
             (let [n (.read in-reader buf)]
               (when-not (= n -1)
                 (.write wr buf 0 n)
                 (.flush wr)
                 (recur))))
           (finally
            (.close rd))))))
