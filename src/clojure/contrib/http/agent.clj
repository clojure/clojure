;;; http/agent.clj: agent-based asynchronous HTTP client

;; by Stuart Sierra, http://stuartsierra.com/
;; June 8, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns #^{:doc "Agent-based asynchronous HTTP client."}
  clojure.contrib.http.agent
  (:require [clojure.contrib.http.connection :as c]
            [clojure.contrib.duck-streams :as duck]))

(defn- setup-http-connection
  [conn options]
  (.setRequestMethod conn (:method options))
  (.setInstanceFollowRedirects (:follow-redirects options))
  (doseq [[name value] (:headers options)]
    (.setRequestProperty conn name value)))

(defn- success? [conn]
  ;; Is the response in the 2xx range?
  (= 2 (unchecked-divide (.getResponseCode conn)) 100))

(defn- do-http-agent-request [state options]
  (let [conn (::connection state)]
    (setup-http-connection conn options)
    (c/start-http-connection conn (:body options))
    (let [bytes (if (success? conn)
                  (duck/to-byte-array (.getInputStream conn))
                  (duck/to-byte-array (.getErrorStream conn)))]
      (.disconnect conn)
      (assoc state
        ::response-body-bytes bytes
        ::state ::completed))))

(def *http-request-defaults*
  {:method "GET"
   :headers {}
   :body nil
   :connect-timeout 0
   :read-timeout 0
   :follow-redirects true})

(defn http-agent
  "Creates (and immediately returns) an Agent representing an HTTP
  request running in a new thread.

  options are key/value pairs:

  :method string

  The HTTP method name.  Default is \"GET\".

  :headers h

  HTTP headers, as a Map or a sequence of pairs like 
  ([key1,value1], [key2,value2])  Default is nil.

  :body b
  
  HTTP request entity body, one of nil, String, byte[], InputStream,
  Reader, or File.  Default is nil.

  :connect-timeout int

  Timeout value, in milliseconds, when opening a connection to the
  URL.  Default is zero, meaning no timeout.

  :read-timeout int

  Timeout value, in milliseconds, when reading data from the
  connection.  Default is zero, meaning no timeout.

  :follow-redirects boolean

  If true, HTTP 3xx redirects will be followed automatically.  Default
  is true.
  "
  ([url & options]
     (let [opts (merge *http-request-defaults* (apply array-map options))]
       (let [a (agent {::connection (c/http-connection url)
                       ::state ::created})]
         (send-off a do-http-agent-request opts)))))

(defn response-body-bytes [a]
  (when (= (::state @a) ::completed)
    (::response-body-bytes @a)))

(defn response-body-str [a]
  (let [da @a]
    (when (= (::state da) ::completed)
      (let [conn (::connection da)
            bytes (::response-body-bytes da)
            encoding (or (.getContentEncoding conn)
                         duck/*default-encoding*)]
        (String. bytes encoding)))))

(defn response-status [a]
  (.getResponseCode (::connection @a)))

(defn response-message [a]
  (.getResponseMessage (::connection @a)))

(defn response-headers [a]
  (.getHeaderFields (::connection @a)))

(defn response-headers-seq
  "Returns the HTTP response headers in order as a sequence of
  [String,String] pairs.  The first 'header' name may be null for the
  HTTP status line."
  [http-agnt]
  (let [conn (::connection @http-agnt)
        f (fn thisfn [i]
            ;; Get value first because first key may be nil.
            (when-let [value (.getHeaderField conn i)]
              (cons [(.getHeaderFieldKey conn i) value]
                    (thisfn (inc i)))))]
    (lazy-seq (f 0))))
