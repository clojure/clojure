;;; xml-stream-reader.clj -- StAX XML reading for Clojure

;; by Stuart Sierra
;; Version 1; April 21, 2008

;; THIS IS AN 'ALPHA' RELEASE AND THE API IS SUBJECT TO CHANGE.


;; Copyright (c) Stuart Sierra.  All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of the Clojure
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; This file defines a stream-based parser for XML, based on the StAX
;; API, included with the JDK since version 6.
;;
;; StAX documentation is available at
;; https://jaxp-sources.dev.java.net/nonav/docs/api/javax/xml/stream/XMLStreamReader.html
;;
;; The operation of the parser is similar to SAX event-based parsers,
;; but the API is simpler and (supposedly) faster than SAX.  It also
;; provides XML namespace support.
;;
;; This is a lower-level interface than Clojure's xml.clj.  It does
;; not build up a data structure representing the XML.  It merely
;; calls a handler function, which you must provide, for each event in
;; the XML stream, and provides some convenient macros to query the
;; current event.
;;
;; See the "PUBLIC API", below, for more instructions.
;;
;; This parser will use the Apache XML Commons resolver, if it is
;; available, to avoid downloading DTDs from the web.  See
;; http://xml.apache.org/commons/components/resolver/index.html
;;
;; If the XML Commons resolver jar is on your classpath, and your XML
;; catalogs are properly configured, this library will use local
;; versions of the XML DTDs.


(clojure/in-ns 'xml-stream-reader)
(clojure/refer 'clojure)


;;; PRIVATE

(import '(javax.xml.stream XMLInputFactory XMLStreamConstants
			   XMLStreamReader))

(def #^XMLStreamReader *xml-input-stream*)  ; dynamically bound during parsing

;; Use Apache's DTD Resolver if it is available.
(try
 (def
  #^{:private true
     :doc "Instance of ResolvingXMLReader from the Apach XML Commons,
     or nil if that class cannot be found."}
  *dtd-resolver-impl*
  (.. Class (forName "org.apache.xml.resolver.tools.ResolvingXMLReader")
      (newInstance)))
 ;; XMLStreamReader uses a non-SAX interface, so we have to wrap
 ;; the entity resolver in a different interface.
 (def
  #^{:private true
     :doc "Proxy class to implement the StAX XMLResolver interface."}
  *dtd-resolver*
  (proxy [javax.xml.stream.XMLResolver] []
    ;; Uncomment (prn ... ) lines below to debug the resolver.
    (resolveEntity [publicID systemID baseURI namespace]
                   ;;(prn "Resolving" publicID systemID baseURI namespace)
                   (let [entity (. *dtd-resolver-impl* (resolveEntity publicID systemID))]
                     (when entity
                       (let [#^String systemid (. entity (getSystemId))]
                         (when (and systemid (. systemid (startsWith "file:")))
                           ;;(prn "Found local system ID" systemid)
                           (new java.io.FileInputStream (subs systemid 5)))))))))
 (catch Exception e  ;; could not find ResolvingXMLReader
	(def *dtd-resolver-impl* nil)
	(def *dtd-resolver* nil)))

(def
 #^{:private true
    :doc "Global XMLInputFactory.  Note: the API documentation does
    not specify if XMLInputFactory is thread-safe."}
 *xml-input-factory*
 (. XMLInputFactory (newInstance)))

;; Use the Apache DTD Resolver if it's available.
(when *dtd-resolver* (. *xml-input-factory* (setXMLResolver *dtd-resolver*)))

(def
 #^{:private true
    :doc "Pre-allocated Throwable used to quit XML stream parsing."}
 +stop-parsing+ (new Throwable "Finished parsing XML."))


;;; PUBLIC API

(defn xml-stream-reader
  "Creates an XMLStreamReader from the source (a Reader or
  InputStream).  You should call close() on the XMLStreamReader when
  you are finished with it.  Normally this is called automatically by
  'parse-xml-stream'."
  [input]
  (. *xml-input-factory* (createXMLStreamReader input)))


(defn parse-xml-stream
  "Parses 'source' (a Reader or InputStream) with XMLStreamReader.
  Calls 'handler-function' once for each event in the XML stream, with
  the event type (one of the XMLStreamConstants) as the argument.

  Use the accessor functions like (lname), (text), and (attr-value...)
  to get information about the current XML event.

  Use (stop-xml-parse) to quit parsing before the entire document has
  been read.

  This function ensures that the XMLStreamReader is closed when
  parsing is completed or stopped, but it does NOT close the provided
  input source."
  [handler-function source]
  (let [xml-stream (xml-stream-reader source)]
    (try
     (binding [*xml-input-stream* xml-stream]
       (loop [] ; while
	 (when (. *xml-input-stream* (hasNext))
	   (handler-function (. *xml-input-stream* (next)))
	   (recur))))
     (catch Throwable t
	    (when-not (identical? t +stop-parsing+)
	      (throw t)))
     (finally (. xml-stream (close))))))


;;; XML EVENT TYPE CONSTANTS
;; Use these to dispatch based on event type in your handler function.

(def ATTRIBUTE (. XMLStreamConstants ATTRIBUTE))
(def CDATA (. XMLStreamConstants CDATA))
(def CHARACTERS (. XMLStreamConstants CHARACTERS))
(def COMMENT (. XMLStreamConstants COMMENT))
(def DTD (. XMLStreamConstants DTD))
(def END_DOCUMENT (. XMLStreamConstants END_DOCUMENT))
(def END_ELEMENT (. XMLStreamConstants END_ELEMENT))
(def ENTITY_DECLARATION (. XMLStreamConstants ENTITY_DECLARATION))
(def ENTITY_REFERENCE (. XMLStreamConstants ENTITY_REFERENCE))
(def NAMESPACE (. XMLStreamConstants NAMESPACE))
(def NOTATION_DECLARATION (. XMLStreamConstants NOTATION_DECLARATION))
(def PROCESSING_INSTRUCTION (. XMLStreamConstants PROCESSING_INSTRUCTION))
(def SPACE (. XMLStreamConstants SPACE))
(def START_DOCUMENT (. XMLStreamConstants START_DOCUMENT))
(def START_ELEMENT (. XMLStreamConstants START_ELEMENT))


;;; PUBLIC XML EVENT ACCESSORS

(defn stop-xml-parse
  "Stop the XML stream parser and return from 'parse-xml-stream'."
  [] (throw +stop-parsing+))

(defmacro event-type
  "Returns the event type (a static in XMLStreamConstants) of the
  current XML event.  Normally not needed, because your handler
  function will receive the event type as its argument."
  []
  '(. *xml-input-stream* (getEventType)))

(defmacro pi-target 
  "Returns the target of an XML processing instruction."
  []
  '(. *xml-input-stream* (getPITarget)))

(defmacro pi-data
  "Returns the data of an XML processing instruction."
  []
  '(. *xml-input-stream* (getPIData)))

(defmacro prefix
  "Returns the prefix of a namespace-qualified XML element."
  []
  '(let [p (. *xml-input-stream* (getPrefix))]
     (if (. p (isEmpty)) nil (keyword p))))

(defmacro lname
  "Returns the local name of an XML element."
  []
  '(. *xml-input-stream* (getLocalName)))

(defmacro xmlns
  "Returns the namespace URI of a namespace-qualified XML element."
  []
  '(. *xml-input-stream* (getNamespaceURI)))

(defmacro whitespace?
  "Returns true if the current XML character node contains only
  whitespace.  Implementation-dependent."
  []
  '(. *xml-input-stream* (isWhiteSpace)))

(defmacro text
  "Returns the text of the current XML character node."
  []
  '(. *xml-input-stream* (getText)))

(defmacro attr-count
  "Returns the number of attributes on the current XML element."
  []
  '(. *xml-input-stream* (getAttributeCount)))

(defn attr-value
  "Returns the value of the attribute on the current XML element."
  ([local-name] (. *xml-input-stream* (getAttributeValue nil local-name)))
  ([local-name xmlns] (. *xml-input-stream* (getAttributeValue xmlns local-name))))

(defstruct attribute :lname :xmlns :prefix :value)

(defn nth-attr
  "Returns a struct representing the nth attribute of the current XML
  element.  The struct has 4 parts, :lname (local name), :xmlns,
  :prefix, and :value."
  [n]
  (struct attribute 
	  (. *xml-input-stream* (getAttributeLocalName n))
	  (. *xml-input-stream* (getAttributeNamespace n))
	  (. *xml-input-stream* (getAttributePrefix n))
	  (. *xml-input-stream* (getAttributeValue n))))

(defn attrs
  "Returns a seq of attribute structures for all attributes on the
  current XML element."
  []
  (doall  ; have to get all attributes before next XML event
   (for [index (range (attr-count))]
     (nth-attr index))))



;; Valid methods for each state:
;; from https://jaxp-sources.dev.java.net/nonav/docs/api/javax/xml/stream/XMLStreamReader.html
;;
;; All States	getProperty(), hasNext(), require(), close(),
;;		getNamespaceURI(), isStartElement(), isEndElement(),
;;		isCharacters(), isWhiteSpace(), getNamespaceContext(),
;;		getEventType(),getLocation(), hasText()
;;
;; START_ELEMENT next(), getName(), getLocalName(), hasName(),
;;		getPrefix(), getAttributeXXX(),
;;		isAttributeSpecified(), getNamespaceXXX(),
;;		getElementText(), nextTag()
;;
;; ATTRIBUTE	next(), nextTag() getAttributeXXX(),
;;		isAttributeSpecified(),
;;
;; NAMESPACE	next(), nextTag() getNamespaceXXX()
;;
;; END_ELEMENT	next(), getName(), getLocalName(), hasName(),
;;		getPrefix(), getNamespaceXXX(), nextTag()
;;
;; CHARACTERS	next(), getTextXXX(), nextTag()
;;
;; CDATA	next(), getTextXXX(), nextTag()
;;
;; COMMENT	next(), getTextXXX(), nextTag()
;;
;; SPACE	next(), getTextXXX(), nextTag()
;;
;; START_DOCUMENT next(), getEncoding(), next(), getPrefix(),
;;		getVersion(), isStandalone(), standaloneSet(),
;;		getCharacterEncodingScheme(), nextTag()
;;
;; END_DOCUMENT	close()
;;
;; PROCESSING_INSTRUCTION  next(), getPITarget(), getPIData(), nextTag()
;;
;; ENTITY_REFERENCE 	next(), getLocalName(), getText(), nextTag()
;;
;; DTD		next(), getText(), nextTag()
