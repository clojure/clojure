(ns clojure.remoterepl
  (:import [java.net ServerSocket Socket]
           [clojure.lang LineNumberingPushbackReader]
           [java.io PrintWriter InputStreamReader OutputStreamWriter]))

(defmacro with-socket [[port in out] & body]
  `(future
     (let [s# (.accept (ServerSocket. ~port))
           ~out (PrintWriter. (.getOutputStream s#) true)
           ~in (LineNumberingPushbackReader. (InputStreamReader. (.getInputStream s#)))]
       ~@body)))

(def repl-in (atom nil))
(def repl-out (atom nil))

(defn call-remote [sel args]
  (let [p (promise)
        args (vec args)]
    (future
      (if clojure.lang.ObjC/objc
        (do
          ($ @repl-in :println (pr-str [sel args]))
          (deliver p ($ @repl-in :read)))
        (do
          (.println @repl-out (pr-str [sel args]))
          (deliver p (read @repl-in)))))
    (loop []
      (if (realized? p)
        @p
        (do
          (Thread/sleep 100)
          (recur))))))

(defn listen []
  (clojure.lang.RemoteRepl/setConnected true)
  (if clojure.lang.ObjC/objc
    (do
      (reset! repl-in ($ ($ ($ NSSocketImpl) :alloc)
                         :initWithHost "localhost"
                         :withPort "4444"))
      (future
        (let [s ($ ($ ($ NSSocketImpl) :alloc)
                   :initWithHost "localhost"
                   :withPort "5555")]
          (loop [[f args] (read-string ($ s :read))]
            (clojure.lang.RT/dispatchInMain
             (fn []
               ($ s :println (pr-str (apply f args)))))
            (recur (read-string ($ s :read)))))))
    (do
      (with-socket [4444 in out]
        (loop [[f args] (read in)]
          (.println out (pr-str (apply f args)))
          (recur (read in))))
      (with-socket [5555 in out]
        (reset! repl-out out)
        (reset! repl-in in)))))
