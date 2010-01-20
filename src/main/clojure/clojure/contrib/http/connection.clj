;;; http/connection.clj: low-level HTTP client API around HttpURLConnection

;; by Stuart Sierra, http://stuartsierra.com/
;; June 8, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns #^{:doc "Low-level HTTP client API around HttpURLConnection"}
  clojure.contrib.http.connection
  (:require [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.java-utils :as j])
  (:import (java.net URI URL HttpURLConnection)
           (java.io File InputStream Reader)))

(defn http-connection
  "Opens an HttpURLConnection at the URL, handled by as-url."
  [url]
  (.openConnection (j/as-url url)))

(defmulti
  #^{:doc "Transmits a request entity body."}
  send-request-entity (fn [conn entity] (type entity)))

(defmethod send-request-entity duck/*byte-array-type* [#^HttpURLConnection conn entity]
  (.setFixedLengthStreamingMode conn (count entity))
  (.connect conn)
  (duck/copy entity (.getOutputStream conn)))

(defmethod send-request-entity String [conn #^String entity]
  (send-request-entity conn (.getBytes entity duck/*default-encoding*)))

(defmethod send-request-entity File [#^HttpURLConnection conn #^File entity]
  (.setFixedLengthStreamingMode conn (.length entity))
  (.connect conn)
  (duck/copy entity (.getOutputStream conn)))

(defmethod send-request-entity InputStream [#^HttpURLConnection conn entity]
  (.setChunkedStreamingMode conn -1)
  (.connect conn)
  (duck/copy entity (.getOutputStream conn)))

(defmethod send-request-entity Reader [#^HttpURLConnection conn entity]
  (.setChunkedStreamingMode conn -1)
  (.connect conn)
  (duck/copy entity (.getOutputStream conn)))

(defn start-http-connection
  ([#^HttpURLConnection conn] (.connect conn))
  ([#^HttpURLConnection conn request-entity-body]
     (if request-entity-body
       (do (.setDoOutput conn true)
           (send-request-entity conn request-entity-body))
       (.connect conn))))
