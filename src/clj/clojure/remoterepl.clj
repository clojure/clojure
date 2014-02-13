(ns clojure.remoterepl
  (:import [java.net ServerSocket Socket]
           [clojure.lang LineNumberingPushbackReader]
           [java.io PrintWriter InputStreamReader OutputStreamWriter]))

(def pending (atom (clojure.lang.PersistentQueue/EMPTY)))

(defn dequeue! [queue]
  (loop []
    (let [q @queue
          value (peek q)
          nq (pop q)]
      (if (compare-and-set! queue q nq)
        value
        (recur)))))

(def in-call-remote (atom false))

(def port 4815)

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defmacro with-socket [[port in out] & body]
  `(loop []
     (let [s# (.accept (ServerSocket. ~port))
           ~out (PrintWriter. (.getOutputStream s#) true)
          ~in (LineNumberingPushbackReader. (InputStreamReader. (.getInputStream s#)))]
       (future
         ~@body))
     (recur)))

(def repl (atom nil))

(def responses (atom {}))

(defn do-pending [f]
  (if (= 2 (count f))
    (let [[id r] f]
      (swap! responses assoc id r))
    (let [[id f args] f]
      (if objc?
        ($ @repl :println (pr-str [id (apply f args)]))
        (future (.println @repl (pr-str [id (apply f args)])))))))

(defn do-pendings []
  (loop [p (dequeue! pending)]
    (when p
      (do-pending p)
      (recur (dequeue! pending)))))

(defn call-remote [sel args]
  (reset! in-call-remote true)
  (let [args (vec args)
        id (keyword (uuid))
        msg (pr-str [id sel args])]
    (if objc?
      ($ @repl :println msg)
      (.println @repl msg))
    (loop []
      (if (some #{id} (keys @responses))
        (let [r (id @responses)]
          (swap! responses dissoc id)
          (reset! in-call-remote false)
          r)
        (do
          (Thread/sleep 10)
          (when objc?
            (do-pendings))
          (recur))))))

(defn listen-objc []
  (let [s ($ ($ ($ NSSocketImpl) :alloc)
             :initWithHost "localhost"
             :withPort (str port))]
    (reset! repl s)
    (loop [f ($ s :read)]
      (swap! pending conj (read-string f))
      (when-not @in-call-remote
        (dispatch-main (do-pendings)))
      (recur ($ s :read)))))

(defn listen-jvm []
  (with-socket [port in out]
    (reset! repl out)
    (loop [f (read in)]
      (swap! pending conj f)
      (do-pendings)
      (recur (read in)))))

(defn listen []
  (clojure.lang.RemoteRepl/setConnected true)
  (future
    (if objc?
      (listen-objc)
      (listen-jvm))))
