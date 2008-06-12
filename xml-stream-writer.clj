;;; xml-stream-writer.clj -- Clojure interface to XMLStreamWriter

;; by Stuart Sierra <mail@stuartsierra.com>
;; Version 1; April 21, 2008

;; THIS IS AN 'ALPHA' RELEASE AND THE API IS SUBJECT TO CHANGE.


;; Copyright (c) 2008 Stuart Sierra. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; This file defines a Clojure interface to the StAX XML writer API,
;; defined in javax.xml.stream.XMLOutputStream, included with the JDK
;; since version 6.
;;
;; The main entry point is the 'with-xml-out' macro, which takes
;; one argument, an open java.io.Writer, and a body.  This macro opens
;; a new XMLStreamWriter on the given output stream and ensures that
;; all document tags are closed.  It does NOT close the underlying
;; Writer.
;;
;; Within the body of 'with-xml-out', certain forms will be
;; translated into XML:
;;
;;     "This is a string."    =>  This is a string.
;;     [:foo "this & that"]   =>  <foo>this &amp; that</foo>
;;     [:foo]                 =>  <foo/>
;;
;; Attributes are specified as maps:
;;
;;    [:foo {:bar "this & that"}]  =>  <foo bar="this &amp; that"/>
;;
;; Declare XML namespace prefixes inside 'emit-xml' with
;; (set-xmlns prefix uri), where 'prefix' is a keyword.
;;
;; Once a namespace has been declared, create elements in that
;; namespace by using a 2-element vector [prefix localname] as the tag
;; name:
;;
;;     (with-xml-out *out*
;;       (set-xmlns :me "http://xmlns.com/mine")
;;       [[:me :foo] "content"])
;;
;;   =>  <me:foo xmlns:me="http://xmlns.com/mine">content</me:foo>
;;
;; Same with attributes: 
;;
;;     (with-xml-out *out*
;;       (set-xmlns :me "http://xmlns.com/mine")
;;       [:foo {[:me bar] "value"}])
;;
;;   =>  <foo me:bar="value" xmlns:me="http://xmlns.com/mine"/>
;;
;; Undeclared namespaces will get an auto-generated prefix:
;;
;;     (with-xml-out *out* [["http://myns.com/" :foo]])
;;
;;   =>  <zdef18:foo xmlns:zdef18="http://myns.com/"/>
;;
;; Normal function calls and symbols may be placed in the body of
;; 'with-xml-out'; they will be evaluated normally and their return
;; values will be included as text in the XML output.  To avoid adding
;; text to the output, functions should return nil.
;;
;; To emit XML tags from a function without creating a new
;; XMLStreamWriter, use the 'emit-xml' macro:
;;
;;     (defn foo [name] (emit-xml [:mytag {:id name}]))
;;
;;     (with-xml-out *out*
;;       [:root (foo 24) (foo 24)])
;;
;;  =>  <root><mytag id="24"/><mytag id="24"/></root>
;;
;; To create the default XML declaration (version 1.0 and UTF-8
;; encoding), use the (xmldecl) macro:
;;
;;    (with-xml-out *out* (xmldecl))
;;
;;  =>  <?xml version="1.0" encoding="UTF-8"?>
;;



(clojure/in-ns 'xml-stream-writer)
(clojure/refer 'clojure)

(import '(javax.xml.stream XMLOutputFactory))

(defn- to-name [x]
  (cond
   (keyword? x) (name x)
   (string? x) x
   (symbol? x) (name x)
   true (str x)))

(def
 *xml-output-factory*
 (let [factory (. XMLOutputFactory (newInstance))]
   (. factory (setProperty
               "javax.xml.stream.isRepairingNamespaces" true))
   factory))

(def *xml-stream-writer*)

(def *xmlns-prefixes*)

(def *xml-indent* nil)

(def *xml-tag-depth*)

(defn apply-indent []
  (when *xml-indent*
    (. *xml-stream-writer* (writeCharacters "\n"))
    (dotimes i (* *xml-indent* *xml-tag-depth*)
      (. *xml-stream-writer* (writeCharacters " ")))))

(defn start-tag
  ([name]
     (apply-indent)
     (set! *xml-tag-depth* (inc *xml-tag-depth*))
     (. *xml-stream-writer*
        (writeStartElement (to-name name))))
  ([namespace name]
     (apply-indent)
     (set! *xml-tag-depth* (inc *xml-tag-depth*))
     (. *xml-stream-writer*
        (writeStartElement namespace (to-name name)))))

(defn empty-tag
  ([name]
     (. *xml-stream-writer*
        (writeEmptyElement (to-name name))))
  ([namespace name]
     (. *xml-stream-writer*
        (writeEmptyElement namespace (to-name name)))))

(defn xml-attr
  ([name value]
     (. *xml-stream-writer* (writeAttribute (to-name name) (str value))))
  ([namespace name value]
     (. *xml-stream-writer* (writeAttribute namespace (to-name name) (str value)))))

(defn xml-text [t]
  (when t
    (apply-indent)
    (. *xml-stream-writer* (writeCharacters (str t)))))

(defn end-tag []
  (set! *xml-tag-depth* (dec *xml-tag-depth*))
  (apply-indent)
  (. *xml-stream-writer* (writeEndElement)))

(defn entity [name]
  (. *xml-stream-writer* (writeEntityRef (to-name name))))

(def emit-form)

(defn resolve-xmlns [x]
  (if (keyword? x)
    (*xmlns-prefixes* x)
    x))

(defn- emit-tag [form]
  (if (or (= 1 (count form))
          (and (= 2 (count form))
               (map? (second form))))         ; is the element empty?
    (apply list 'do
           (let [tag (first form)]
             (if (vector? tag)          ; is the element namespaced?
               (list 'empty-tag (list 'resolve-xmlns (first tag)) (second tag))
               (list 'empty-tag tag)))
           (map emit-form (rest form))) ; still have to handle attributes
    (apply list 'do
           (let [tag (first form)]        ; non-empty element
             (if (vector? tag)            ; is the element namespaced?
               (list 'start-tag (list 'resolve-xmlns (first tag)) (second tag))
               (list 'start-tag tag)))
           (concat (map emit-form (rest form))
                   (list '(end-tag))))))

(defn- emit-xml-attr [name-value]
  (let [[name value] name-value]
    (if (vector? name)
      (list 'xml-attr (list 'resolve-xmlns (first name)) (second name) value)
      (list 'xml-attr name value))))

(defn- emit-xml-attrs [xml-attr-map]
  (apply list 'do
         (map emit-xml-attr xml-attr-map)))

(defn- emit-form [form]
  (cond
   (vector? form) (emit-tag form)
   (map? form) (emit-xml-attrs form)
   true `(xml-text ~form)))

(defmacro xmldecl []
  '(. *xml-stream-writer* (writeStartDocument "UTF-8" "1.0")))

(defmacro set-xmlns [prefix namespace]
  `(sync nil
     (set! *xmlns-prefixes* (assoc *xmlns-prefixes* ~prefix ~namespace))
     (. *xml-stream-writer* (setPrefix ~(to-name prefix) ~namespace))))

(defmacro emit-xml [& forms]
  `(do ~@(map emit-form forms)))

(defmacro with-xml-out [writer & body]
  `(binding [*xml-stream-writer*
             (. *xml-output-factory* (createXMLStreamWriter ~writer))
             *xmlns-prefixes* {}
             *xml-tag-depth* 0]
     (try
      (emit-xml ~@body)
      (finally
       (. *xml-stream-writer* (writeEndDocument))
       (. *xml-stream-writer* (close))
       nil))))
