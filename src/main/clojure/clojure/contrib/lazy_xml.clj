;   Copyright (c) Chris Houser, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Functions to parse xml lazily and emit back to text.

(ns 
    #^{:author "Chris Houser",
       :doc "Functions to parse xml lazily and emit back to text."}
    clojure.contrib.lazy-xml
    (:use [clojure.xml :as xml :only []]
          [clojure.contrib.seq :only [fill-queue]])
    (:import (org.xml.sax Attributes InputSource)
             (org.xml.sax.helpers DefaultHandler)
             (javax.xml.parsers SAXParserFactory)
             (java.util.concurrent LinkedBlockingQueue TimeUnit)
             (java.lang.ref WeakReference)
             (java.io Reader)))

(defstruct node :type :name :attrs :str)

; http://www.extreme.indiana.edu/xgws/xsoap/xpp/
(def has-pull false)
(defn- parse-seq-pull [& _])
(try
  (load "lazy_xml/with_pull")
  (catch Exception e
    (when-not (re-find #"XmlPullParser" (str e))
      (throw e))))

(defn startparse-sax [s ch]
  (.. SAXParserFactory newInstance newSAXParser (parse s ch)))

(defn parse-seq
  "Parses the source s, which can be a File, InputStream or String
  naming a URI. Returns a lazy sequence of maps with two or more of
  the keys :type, :name, :attrs, and :str. Other SAX-compatible
  parsers can be supplied by passing startparse, a fn taking a source
  and a ContentHandler and returning a parser. If a parser is
  specified, it will be run in a separate thread and be allowed to get
  ahead by queue-size items, which defaults to maxint.  If no parser
  is specified and org.xmlpull.v1.XmlPullParser is in the classpath,
  this superior pull parser will be used."
  ([s] (if has-pull
         (parse-seq-pull s)
         (parse-seq s startparse-sax)))
  ([s startparse] (parse-seq s startparse Integer/MAX_VALUE))
  ([s startparse queue-size]
   (let [s (if (instance? Reader s) (InputSource. s) s)
         f (fn filler-func [fill]
             (startparse s (proxy [DefaultHandler] []
               (startElement [uri local-name q-name #^Attributes atts]
                 ;(prn :start-element q-name)(flush)
                 (let [attrs (into {} (for [i (range (.getLength atts))]
                                           [(keyword (.getQName atts i))
                                            (.getValue atts i)]))]
                   (fill (struct node :start-element (keyword q-name) attrs))))
               (endElement [uri local-name q-name]
                 ;(prn :end-element q-name)(flush)
                 (fill (struct node :end-element (keyword q-name))))
               (characters [ch start length]
                 ;(prn :characters)(flush)
                 (let [st (String. ch start length)]
                   (when (seq (.trim st))
                     (fill (struct node :characters nil nil st))))))))]
     (fill-queue f :queue-size queue-size))))


(defstruct element :tag :attrs :content)
(declare mktree)

(defn- siblings [coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (let [event (first s)]
        (condp = (:type event)
          :characters    (cons (:str event) (siblings (rest s)))
          :start-element (let [t (mktree s)]
                           (cons (first t) (siblings (rest t))))
          :end-element   [(rest s)])))))

(defn- mktree
  [[elem & events]]
    (lazy-seq
      (let [sibs (siblings events)]
        ;(prn :elem elem)
        (cons
          (struct element (:name elem) (:attrs elem) (drop-last sibs))
          (lazy-seq (last sibs))))))

(defn parse-trim
  "Parses the source s, which can be a File, InputStream or String
  naming a URI. Returns a lazy tree of the clojure.xml/element
  struct-map, which has the keys :tag, :attrs, and :content and
  accessor fns tag, attrs, and content, with the whitespace trimmed
  from around each content string. This format is compatible with what
  clojure.xml/parse produces, except :content is a lazy seq instead of
  a vector.  Other SAX-compatible parsers can be supplied by passing
  startparse, a fn taking a source and a ContentHandler and returning
  a parser. If a parser is specified, it will be run in a separate
  thread and be allowed to get ahead by queue-size items, which
  defaults to maxing.  If no parser is specified and
  org.xmlpull.v1.XmlPullParser is in the classpath, this superior pull
  parser will be used."
  ([s] (first (mktree (parse-seq s))))
  ([s startparse queue-size]
    (first (mktree (parse-seq s startparse queue-size)))))

(defn attributes [e]
  (let [v (vec (:attrs e))]
    (reify org.xml.sax.Attributes
      (getLength [_] (count v))
      (getURI [_ i] (namespace (key (v i))))
      (getLocalName [_ i] (name (key (v i))))
      (getQName [_ i] (name (key (v i))))
      (getValue [_ uri name] (get (:attrs e) name))
      (#^String getValue [_ #^int i] (val (v i)))
      (#^String getType [_ #^int i] "CDATA"))))

(defn- emit-element
  "Recursively prints as XML text the element struct e.  To have it
  print extra whitespace like clojure.xml/emit, use the :pad true
  option."
  [e #^org.xml.sax.ContentHandler ch]
  (if (instance? String e)
    (.characters ch (.toCharArray #^String e) 0 (count e))
    (let [nspace (namespace (:tag e))
          qname (name (:tag e))]
      (.startElement ch (or nspace "") qname qname (attributes e))
      (doseq [c (:content e)]
        (emit-element c ch))
      (.endElement ch (or nspace "") qname qname))))
  

(defn emit
  [e & {:as opts}]
  (let [content-handler (atom nil)
        trans (-> (javax.xml.transform.TransformerFactory/newInstance)
                .newTransformer)]

    (when (:indent opts)
      (.setOutputProperty trans "indent" "yes")
      (.setOutputProperty trans "{http://xml.apache.org/xslt}indent-amount"
                          (str (:indent opts))))

    (when (contains? opts :xml-declaration)
      (.setOutputProperty trans "omit-xml-declaration" 
                          (if (:xml-declaration opts) "no" "yes")))

    (when (:encoding opts)
      (.setOutputProperty trans "encoding" (:encoding opts)))

    (.transform
      trans
      (javax.xml.transform.sax.SAXSource.
        (reify org.xml.sax.XMLReader
          (getContentHandler [_] @content-handler)
          (setDTDHandler [_ handler])
          (setFeature [_ name value])
          (setProperty [_ name value])
          (setContentHandler [_ ch] (reset! content-handler ch))
          (#^void parse [_ #^org.xml.sax.InputSource _]
            (when @content-handler
              (.startDocument @content-handler)
              (emit-element e @content-handler)
              (.endDocument @content-handler))))
        (org.xml.sax.InputSource.))
      (javax.xml.transform.stream.StreamResult. *out*))))

(comment

(def atomstr "<?xml version='1.0' encoding='UTF-8'?>
<feed xmlns='http://www.w3.org/2005/Atom'>
  <id>tag:blogger.com,1999:blog-28403206</id>
  <updated>2008-02-14T08:00:58.567-08:00</updated>
  <title type='text'>n01senet</title>
  <link rel='alternate' type='text/html' href='http://n01senet.blogspot.com/'/>
  <entry xmlns:foo='http://foo' xmlns:bar='http://bar'>
    <id>1</id>
    <published>2008-02-13</published>
    <title type='text'>clojure is the best lisp yet</title>
    <author><name>Chouser</name></author>
  </entry>
  <entry>
    <id>2</id>
    <published>2008-02-07</published>
    <title type='text'>experimenting with vnc</title>
    <author><name>agriffis</name></author>
  </entry>
</feed>
")

(def tree (parse-trim (java.io.StringReader. atomstr)
                 startparse-sax
                 1))
(println "\nsax")
(emit tree)

(def tree (parse-trim (java.io.StringReader. atomstr)))
(println "\ndefault")
(emit tree)

(def tree (xml/parse (org.xml.sax.InputSource. (java.io.StringReader. atomstr))))
(println "\norig")
(emit tree)

; When used with zip and zip-filter, you can get do queries like this
; without parsing more than the first few tags:
; (zip/node (first (xml-> (zip/xml-zip tree) :id)))

)
