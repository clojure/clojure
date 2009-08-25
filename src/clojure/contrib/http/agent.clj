;;; http/agent.clj: agent-based asynchronous HTTP client

;; by Stuart Sierra, http://stuartsierra.com/
;; August 17, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns #^{:doc "Agent-based asynchronous HTTP client.

  This is a HTTP client library based on Java's HttpURLConnection
  class and Clojure's Agent system.  It allows you to make multiple
  HTTP requests in parallel.

  Start an HTTP request with the 'http-agent' function, which
  immediately returns a Clojure Agent.  You will never deref this
  agent; that is handled by the accessor functions.  The agent will
  execute the HTTP request on a separate thread.

  If you pass a :handler function to http-agent, that function will be
  called as soon as the HTTP response body is ready.  The handler
  function is called with one argument, the HTTP agent itself.  The
  handler can read the response body by calling the 'stream' function
  on the agent.

  The value returned by the handler function becomes part of the state
  of the agent, and you can retrieve it with the 'result' function.
  If you call 'result' before the HTTP request has finished, it will
  block until the handler function returns.

  If you don't provide a handler function, the default handler will
  buffer the entire response body in memory, which you can retrieve
  with the 'bytes', 'string', or 'stream' functions.  Like 'result',
  these functions will block until the HTTP request is completed.

  If you want to check if an HTTP request is finished without
  blocking, use the 'done?' function.

  A single GET request could be as simple as:

    (string (http-agent \"http://www.stuartsierra.com/\"))

  A simple POST might look like:

    (http-agent \"http...\" :method \"POST\" :body \"foo=1\")

  And you could write the response directly to a file like this:

    (require '[clojure.contrib.duck-streams :as d])

    (http-agent \"http...\"
                :handler (fn [agnt] 
                           (with-open [w (d/writer \"/tmp/out\")] 
                             (d/copy (stream agnt) w))))
"
       :author "Stuart Sierra"
       }

  clojure.contrib.http.agent
  (:require [clojure.contrib.http.connection :as c]
            [clojure.contrib.duck-streams :as duck])
  (:import (java.io InputStream ByteArrayOutputStream
                    ByteArrayInputStream)
           (java.net HttpURLConnection)))


;;; PRIVATE

(declare result stream)

(defn- setup-http-connection
  "Sets the instance method, redirect behavior, and request headers of
  the HttpURLConnection."
  [#^HttpURLConnection conn options]
  (.setRequestMethod conn (:method options))
  (.setInstanceFollowRedirects conn (:follow-redirects options))
  (doseq [[name value] (:headers options)]
    (.setRequestProperty conn name value)))

(defn- start-request
  "Agent action that starts sending the HTTP request."
  [state options]
  (let [conn (::connection state)]
    (setup-http-connection conn options)
    (c/start-http-connection conn (:body options))
    (assoc state ::state ::started)))

(defn- connection-success? [#^HttpURLConnection conn]
  "Returns true if the HttpURLConnection response code is in the 2xx
  range."
  (= 2 (unchecked-divide (.getResponseCode conn) 100)))

(defn- open-response
  "Agent action that opens the response body stream on the HTTP
  request; this will block until the response stream is available." ;
  [state options]
  (let [#^HttpURLConnection conn (::connection state)]
    (assoc state
      ::response-stream (if (connection-success? conn)
                          (.getInputStream conn)
                          (.getErrorStream conn))
      ::state ::receiving)))

(defn- handle-response
  "Agent action that calls the provided handler function, with no
  arguments, and sets the ::result key of the agent to the handler's
  return value."
  [state handler options]
  (let [conn (::connection state)]
    (assoc state
      ::result (handler)
      ::state ::finished)))

(defn- disconnect
  "Agent action that closes the response body stream and disconnects
  the HttpURLConnection."
  [state options]
  (when (::response-stream state)
    (.close #^InputStream (::response-stream state)))
  (.disconnect #^HttpURLConnection (::connection state))
  (assoc state
    ::response-stream nil
    ::state ::disconnected))

(defn- status-in-range?
  "Returns true if the response status of the HTTP agent begins with
  digit, an Integer."
  [digit http-agnt]
  (= digit (unchecked-divide (.getResponseCode
                              #^HttpURLConnection (::connection @http-agnt))
                             100)))

(defn- #^ByteArrayOutputStream get-byte-buffer [http-agnt]
  (let [buffer (result http-agnt)]
    (if (instance? ByteArrayOutputStream buffer)
      buffer
      (throw (Exception. "Handler result was not a ByteArrayOutputStream")))))


(defn buffer-bytes
  "The default HTTP agent result handler; it collects the response
  body in a java.io.ByteArrayOutputStream, which can later be
  retrieved with the 'stream', 'string', and 'bytes' functions."
  [http-agnt]
  (let [output (ByteArrayOutputStream.)]
    (duck/copy (or (stream http-agnt) "") output)
    output))


;;; CONSTRUCTOR

(def *http-agent-defaults*
     {:method "GET"
      :headers {}
      :body nil
      :connect-timeout 0
      :read-timeout 0
      :follow-redirects true
      :handler buffer-bytes})

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

  :handler f

  Function to be called when the HTTP response body is ready.  If you
  do not provide a handler function, the default is to buffer the
  entire response body in memory.

  The handler function will be called with the HTTP agent as its
  argument, and can use the 'stream' function to read the response
  body.  The return value of this function will be stored in the state
  of the agent and can be retrieved with the 'result' function.  Any
  exceptions thrown by this function will be added to the agent's
  error queue (see agent-errors).  The default function collects the
  response stream in a memory buffer.
  "
  ([uri & options]
     (let [opts (merge *http-agent-defaults* (apply array-map options))]
       (let [a (agent {::connection (c/http-connection uri)
                       ::state ::created
                       ::uri uri
                       ::options opts})]
         (send-off a start-request opts)
         (send-off a open-response opts)
         (send-off a handle-response (partial (:handler opts) a) opts)
         (send-off a disconnect opts)))))


;;; RESPONSE BODY ACCESSORS

(defn result
  "Returns the value returned by the :handler function of the HTTP
  agent; blocks until the HTTP request is completed.  The default
  handler function returns a ByteArrayOutputStream."
  [http-agnt]
  (await http-agnt)
  (::result @http-agnt))

(defn stream
  "Returns an InputStream of the HTTP response body.  When called by
  the handler function passed to http-agent, this is the raw
  HttpURLConnection stream.

  If the default handler function was used, this function returns a
  ByteArrayInputStream on the buffered response body."
  [http-agnt]
  (let [a @http-agnt]
    (if (= (::state a) ::receiving)
      (::response-stream a)
      (ByteArrayInputStream.
       (.toByteArray (get-byte-buffer http-agnt))))))

(defn bytes
  "Returns a Java byte array of the content returned by the server;
  nil if the content is not yet available."
  [http-agnt]
  (.toByteArray (get-byte-buffer http-agnt)))

(defn string
  "Returns the HTTP response body as a string, using the given
  encoding.

  If no encoding is given, uses the encoding specified in the server
  headers, or clojure.contrib.duck-streams/*default-encoding* if it is
  not specified."
  ([http-agnt]
     (string http-agnt (or (.getContentEncoding
                            #^HttpURLConnection (::connection @http-agnt))
                           duck/*default-encoding*)))
  ([http-agnt #^String encoding]
     (.toString (get-byte-buffer http-agnt) encoding)))


;;; REQUEST ACCESSORS

(defn request-uri
  "Returns the URI/URL requested by this HTTP agent, as a String."
  [http-agnt]
  (::uri @http-agnt))

(defn request-headers
  "Returns the request headers specified for this HTTP agent."
  [http-agnt]
  (:headers (::options @http-agnt)))

(defn method
  "Returns the HTTP method name used by this HTTP agent, as a String."
  [http-agnt]
  (:method (::options @http-agnt)))

(defn request-body
  "Returns the HTTP request body given to this HTTP agent.  

  Note: if the request body was an InputStream or a Reader, it will no
  longer be usable."
  [http-agnt]
  (:body (::options @http-agnt)))


;;; RESPONSE ACCESSORS

(defn done?
  "Returns true if the HTTP request/response has completed."
  [http-agnt]
  (if (#{::finished ::disconnected} (::state @http-agnt))
    true false))

(defn status
  "Returns the HTTP response status code (e.g. 200, 404) for this
  request, as an Integer, or nil if the status has not yet been
  received."
  [http-agnt]
  (when (done? http-agnt)
    (.getResponseCode #^HttpURLConnection (::connection @http-agnt))))

(defn message
  "Returns the HTTP response message (e.g. 'Not Found'), for this
  request, or nil if the response has not yet been received."
  [http-agnt]
  (when (done? http-agnt)
    (.getResponseMessage #^HttpURLConnection (::connection @http-agnt))))

(defn headers
  "Returns a map of HTTP response headers.  Header names are converted
  to keywords in all lower-case Header values are strings.  If a
  header appears more than once, only the last value is returned."
  [http-agnt]
  (reduce (fn [m [#^String k v]]
            (assoc m (when k (keyword (.toLowerCase k))) (last v)))
          {} (.getHeaderFields
              #^HttpURLConnection (::connection @http-agnt))))

(defn headers-seq
  "Returns the HTTP response headers in order as a sequence of
  [String,String] pairs.  The first 'header' name may be null for the
  HTTP status line."
  [http-agnt]
  (let [#^HttpURLConnection conn (::connection @http-agnt)
        f (fn thisfn [#^Integer i]
            ;; Get value first because first key may be nil.
            (when-let [value (.getHeaderField conn i)]
              (cons [(.getHeaderFieldKey conn i) value]
                    (thisfn (inc i)))))]
    (lazy-seq (f 0))))


;;; RESPONSE STATUS CODE ACCESSORS

(defn success?
  "Returns true if the HTTP response code was in the 200-299 range."
  [http-agnt]
  (status-in-range? 2 http-agnt))

(defn redirect?
  "Returns true if the HTTP response code was in the 300-399 range.

  Note: if the :follow-redirects option was true (the default),
  redirects will be followed automatically and a the agent will never
  return a 3xx response code."
  [http-agnt]
  (status-in-range? 3 http-agnt))

(defn client-error?
  "Returns true if the HTTP response code was in the 400-499 range."
  [http-agnt]
  (status-in-range? 4 http-agnt))

(defn server-error?
  "Returns true if the HTTP response code was in the 500-599 range."
  [http-agnt]
  (status-in-range? 5 http-agnt))

(defn error?
  "Returns true if the HTTP response code was in the 400-499 range OR
  the 500-599 range."
  [http-agnt]
  (or (client-error? http-agnt)
      (server-error? http-agnt)))
