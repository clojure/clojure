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
            [clojure.contrib.duck-streams :as duck])
  (:import (java.io ByteArrayOutputStream)))

(defn- setup-http-connection
  [conn options]
  (.setRequestMethod conn (:method options))
  (.setInstanceFollowRedirects conn (:follow-redirects options))
  (doseq [[name value] (:headers options)]
    (.setRequestProperty conn name value)))

(defn- connection-success? [conn]
  ;; Is the response in the 2xx range?
  (= 2 (unchecked-divide (.getResponseCode conn) 100)))

(defn- start-request [state options]
  (prn "start-request")
  (let [conn (::connection state)]
    (setup-http-connection conn options)
    (c/start-http-connection conn (:body options))
    (assoc state ::state ::started)))

(defn- open-response [state options]
  (prn "open-response")
  (let [conn (::connection state)]
    (assoc state
      ::response-stream (if (connection-success? conn)
                          (.getInputStream conn)
                          (.getErrorStream conn))
      ::state ::receiving)))

(defn- handle-response [state success-handler failure-handler options]
  (prn "handle-response")
  (let [conn (::connection state)]
    (assoc state
      ::result (if (connection-success? conn)
                   (success-handler)
                   (failure-handler))
      ::state ::finished)))

(defn- disconnect [state options]
  (prn "disconnect")
  (.close (::response-stream state))
  (.disconnect (::connection state))
  (assoc state
    ::response-stream nil
    ::state ::disconnected))

(defn response-body-stream
  "Returns an InputStream of the HTTP response body."
  [http-agnt]
  (let [a @http-agnt]
    (if (= (::state a) ::receiving)
      (::response-stream a)
      (throw (Exception. "response-body-stream may only be called from a callback function passed to http-agent")))))

(defn buffer-bytes
  "The default HTTP agent result handler; it collects the response
  body in a java.io.ByteArrayOutputStream."
  [http-agnt]
  (let [output (ByteArrayOutputStream.)]
    (duck/copy (response-body-stream http-agnt) output)
    output))

(def *http-agent-defaults*
  {:method "GET"
   :headers {}
   :body nil
   :connect-timeout 0
   :read-timeout 0
   :follow-redirects true
   :on-success buffer-bytes
   :on-failure buffer-bytes})

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

  :on-success f

  Function to be called when the request succeeds with a 2xx response
  code.  The function will be called with the HTTP agent as its
  argument, and can use the response-body-stream function to read the
  response body.  The return value of this function will be stored in
  the state of the agent and can be retrieved with the 'result'
  function.  Any exceptions thrown by this function will be added to
  the agent's error queue (see agent-errors).  The default function
  collects the response stream into a byte array.

  :on-failure f

  Like :on-success but this function will be called when the request
  fails with a 4xx or 5xx response code.
  "
  ([url & options]
     (let [opts (merge *http-agent-defaults* (apply array-map options))]
       (let [a (agent {::connection (c/http-connection url)
                       ::state ::created
                       ::url url
                       ::options opts})]
         (send-off a start-request opts)
         (send-off a open-response opts)
         (send-off a handle-response
                   (partial (:on-success opts) a)
                   (partial (:on-failure opts) a)
                   opts)
         (send-off a disconnect opts)))))

(defn result
  "Returns the value returned by the :on-success or :on-failure
  handler function of the HTTP agent; nil if the handler function has
  not yet finished.  The default handler function returns a
  ByteArrayOutputStream."
  [http-agnt]
  (when (#{::disconnected ::finished} (::state @http-agnt))
    (::result @http-agnt)))

(defn response-body-bytes
  "Returns a Java byte array of the content returned by the server;
  nil if the content is not yet available."
  [http-agnt]
  (when-let [buffer (result http-agnt)]
    (if (instance? ByteArrayOutputStream buffer)
      (.toByteArray buffer)
      (throw (Exception. "Result was not a ByteArrayOutputStream")))))

(defn response-body-str
  "Returns the HTTP response body as a string, using the given
  encoding.

  If no encoding is given, uses the encoding specified in the server
  headers, or clojure.contrib.duck-streams/*default-encoding* if it is
  not specified."
  ([http-agnt]
     (response-body-str http-agnt
                        (or (.getContentEncoding (::connection @http-agnt))
                            duck/*default-encoding*)))
  ([http-agnt encoding]
     (when-let [buffer (result http-agnt)]
       (if (instance? ByteArrayOutputStream buffer)
         (.toString buffer encoding)
         (throw (Exception. "Result was not a ByteArrayOutputStream"))))))

(defn response-status
  "Returns the Integer response status code (e.g. 200, 404) for this request."
  [a]
  (when (= (::state @a) ::completed)
    (.getResponseCode (::connection @a))))

(defn response-message
  "Returns the HTTP response message (e.g. 'Not Found'), for this request."
  [a]
  (when (= (::state @a) ::completed)
    (.getResponseMessage (::connection @a))))

(defn response-headers
  "Returns a String=>String map of HTTP response headers.  Header
  names are converted to all lower-case.  If a header appears more
  than once, only the last value is returned."
  [a]
  (reduce (fn [m [#^String k v]]
            (assoc m (when k (.toLowerCase k)) (last v)))
          {} (.getHeaderFields (::connection @a))))

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

(defn- response-in-range? [digit http-agnt]
  (= digit (unchecked-divide (.getResponseCode (::connection @http-agnt))
                             100)))

(defn success?
  "Returns true if the HTTP response code was in the 200-299 range."
  [http-agnt]
  (response-in-range? 2 http-agnt))

(defn redirect?
  "Returns true if the HTTP response code was in the 300-399 range.

  Note: if the :follow-redirects option was true (the default),
  redirects will be followed automatically and a the agent will never
  return a 3xx response code."
  [http-agnt]
  (response-in-range? 3 http-agnt))

(defn client-error?
  "Returns true if the HTTP response code was in the 400-499 range."
  [http-agnt]
  (response-in-range? 4 http-agnt))

(defn server-error?
  "Returns true if the HTTP response code was in the 500-599 range."
  [http-agnt]
  (response-in-range? 5 http-agnt))

(defn error?
  "Returns true if the HTTP response code was in the 400-499 range OR
  the 500-599 range."
  [http-agnt]
  (or (client-error? http-agnt)
      (server-error? http-agnt)))
