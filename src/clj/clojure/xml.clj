;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "XML reading/writing."
       :author "Rich Hickey"}
  clojure.xml
  (:import (org.xml.sax ContentHandler Attributes SAXException)
           (javax.xml.parsers SAXParser SAXParserFactory)))

(def ^:dynamic *stack*)
(def ^:dynamic *current*)
(def ^:dynamic *state*) ; :element :chars :between
(def ^:dynamic *sb*)

(defstruct element :tag :attrs :content)

(def tag (accessor element :tag))
(def attrs (accessor element :attrs))
(def content (accessor element :content))

(def content-handler
  (let [push-content (fn [e c]
                       (assoc e :content (conj (or (:content e) []) c)))
        push-chars (fn []
                     (when (and (= *state* :chars)
                                (some (complement #(Character/isWhitespace (char %))) (str *sb*)))
                       (set! *current* (push-content *current* (str *sb*)))))]
    (new clojure.lang.XMLHandler
         (proxy [ContentHandler] []
           (startElement [uri local-name q-name ^Attributes atts]
             (let [attrs (fn [ret i]
                           (if (neg? i)
                             ret
                             (recur (assoc ret
                                           (clojure.lang.Keyword/intern (symbol (.getQName atts i)))
                                           (.getValue atts (int i)))
                                    (dec i))))
                   e (struct element
                             (. clojure.lang.Keyword (intern (symbol q-name)))
                             (when (pos? (.getLength atts))
                               (attrs {} (dec (.getLength atts)))))]
               (push-chars)
               (set! *stack* (conj *stack* *current*))
               (set! *current* e)
               (set! *state* :element))
             nil)
           (endElement [uri local-name q-name]
             (push-chars)
             (set! *current* (push-content (peek *stack*) *current*))
             (set! *stack* (pop *stack*))
             (set! *state* :between)
             nil)
           (characters [^chars ch start length]
             (when-not (= *state* :chars)
               (set! *sb* (new StringBuilder)))
             (let [^StringBuilder sb *sb*]
               (.append sb ch (int start) (int length))
               (set! *state* :chars))
             nil)
           (setDocumentLocator [locator])
           (startDocument [])
           (endDocument [])
           (startPrefixMapping [prefix uri])
           (endPrefixMapping [prefix])
           (ignorableWhitespace [ch start length])
           (processingInstruction [target data])
           (skippedEntity [name])
           ))))

(defn sax-parser
  "Create a new SAXParser"
  {:added "1.11"}
  ^SAXParser []
  (.newSAXParser (SAXParserFactory/newInstance)))

(defn disable-external-entities
  "Modifies a SAXParser to disable external entity resolution to prevent XXE attacks"
  {:added "1.11"}
  ^SAXParser [^SAXParser parser]
  (let [reader (.getXMLReader parser)]
    ;; as per https://cheatsheetseries.owasp.org/cheatsheets/XML_External_Entity_Prevention_Cheat_Sheet.html
    (.setFeature reader "http://apache.org/xml/features/nonvalidating/load-external-dtd" false)
    (.setFeature reader "http://xml.org/sax/features/external-general-entities", false)
    (.setFeature reader "http://xml.org/sax/features/external-parameter-entities" false)
    parser))

(defn startparse-sax
  "A startparse function suitable for use with clojure.xml/parse.
  Note that this function is open to XXE entity attacks, see startparse-sax-safe."
  {:added "1.0"}
  [s ch]
  (.parse (sax-parser) s ch))

(defn startparse-sax-safe
  "A startparse function suitable for use with clojure.xml/parse.
  External entity resolution is disabled to prevent XXE entity attacks."
  {:added "1.11"}
  [s ch]
  (.parse (disable-external-entities (sax-parser)) s ch))

(defn parse
  "Parses and loads the source s, which can be a File, InputStream or
  String naming a URI. Returns a tree of the xml/element struct-map,
  which has the keys :tag, :attrs, and :content. and accessor fns tag,
  attrs, and content. Other parsers can be supplied by passing
  startparse, a fn taking a source and a ContentHandler and returning
  a parser.

  Prior to 1.11, used startparse-sax by default. As of 1.11, uses
  startparse-sax-safe, which disables XXE (XML External Entity)
  processing. Pass startparse-sax to revert to prior behavior."
  {:added "1.0"}
  ([s] (parse s startparse-sax-safe))
  ([s startparse]
    (binding [*stack* nil
              *current* (struct element)
              *state* :between
              *sb* nil]
      (startparse s content-handler)
      ((:content *current*) 0)))) 

(defn emit-element [e]
  (if (instance? String e)
    (println e)
    (do
      (print (str "<" (name (:tag e))))
      (when (:attrs e)
	(doseq [attr (:attrs e)]
	  (print (str " " (name (key attr)) "='" (val attr)"'"))))
      (if (:content e)
	(do
	  (println ">")
	  (doseq [c (:content e)]
	    (emit-element c))
	  (println (str "</" (name (:tag e)) ">")))
	(println "/>")))))

(defn emit [x]
  (println "<?xml version='1.0' encoding='UTF-8'?>")
  (emit-element x))

;(export '(tag attrs content parse element emit emit-element))

;(load-file "/Users/rich/dev/clojure/src/xml.clj")
;(def x (xml/parse "http://arstechnica.com/journals.rssx"))
