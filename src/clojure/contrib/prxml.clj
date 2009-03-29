;;; prxml.clj -- compact syntax for generating XML

;; by Stuart Sierra, http://stuartsierra.com/
;; March 29, 2009

;; Copyright (c) 2009 Stuart Sierra. All rights reserved.  The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; Change Log
;;
;; March 29, 2009: added *prxml-indent*
;;
;; January 4, 2009: initial version


;; See function "prxml" at the bottom of this file for documentation.


(ns clojure.contrib.prxml
  (:use [clojure.contrib.lazy-xml :only (escape-xml)]))

(def
 #^{:doc "If true, empty tags will have a space before the closing />"}
 *html-compatible* false)

(def
 #^{:doc "The number of spaces to indent sub-tags.  nil for no indent
  and no extra line-breaks."}
 *prxml-indent* nil)

(defn- namestr [x]
  (if (or (symbol? x) (keyword? x)) (name x) (str x)))

(def #^{:private true} *prxml-tag-depth* 0)

(def #^{:private true} print-xml)  ; forward declaration

(defn- prxml-attribute [name value]
  (print " ")
  (print (namestr name))
  (print "=\"")
  (print (escape-xml (str value)))
  (print "\""))

(defmulti #^{:private true} print-xml-tag (fn [tag attrs content] tag))

(defmethod print-xml-tag :raw! [tag attrs contents]
  (doseq [c contents] (print c)))

(defmethod print-xml-tag :comment! [tag attrs contents]
  (print "<!-- ")
  (doseq [c contents] (print c))
  (print " -->"))

(defmethod print-xml-tag :decl! [tag attrs contents]
  (let [attrs (merge {:version "1.0" :encoding "UTF-8"}
                     attrs)]
    ;; Must enforce ordering of pseudo-attributes:
    (print "<?xml version=\"")
    (print (:version attrs))
    (print "\" encoding=\"")
    (print (:encoding attrs))
    (print "\"")
    (when (:standalone attrs)
      (print " standalone=\"")
      (print (:standalone attrs))
      (print "\""))
    (print "?>")))

(defmethod print-xml-tag :cdata! [tag attrs contents]
  (print "<![CDATA[")
  (doseq [c contents] (print c))
  (print "]]>"))

(defmethod print-xml-tag :doctype! [tag attrs contents]
  (print "<!DOCTYPE ")
  (doseq [c contents] (print c))
  (print ">"))

(defmethod print-xml-tag :default [tag attrs contents]
  (let [tag-name (namestr tag)]
    (when *prxml-indent*
      (newline)
      (dotimes [n (* *prxml-tag-depth* *prxml-indent*)] (print " ")))
    (print "<")
    (print tag-name)
    (doseq [[name value] attrs]
      (prxml-attribute name value))
    (if (seq contents)
      (do  ;; not an empty tag
        (print ">")
        (if (every? string? contents)
          ;; tag only contains strings:
          (do (doseq [c contents] (print-xml c))
              (print "</") (print tag-name) (print ">"))
          ;; tag contains sub-tags:
          (do (binding [*prxml-tag-depth* (inc *prxml-tag-depth*)]
                (doseq [c contents] (print-xml c)))
              (when *prxml-indent*
                (newline)
                (dotimes [n (* *prxml-tag-depth* *prxml-indent*)] (print " ")))
              (print "</") (print tag-name) (print ">"))))
      ;; empty tag:
      (print (if *html-compatible* " />" "/>")))))


(defmulti #^{:private true} print-xml class)

(defmethod print-xml clojure.lang.IPersistentVector [x]
  (let [[tag & contents] x
        [attrs content] (if (map? (first contents))
                          [(first contents) (rest contents)]
                          [{} contents])]
    (print-xml-tag tag attrs content)))

(defmethod print-xml clojure.lang.ISeq [x]
  ;; Recurse into sequences, so we can use (map ...) inside prxml.
  (doseq [c x] (print-xml c)))

(defmethod print-xml clojure.lang.Keyword [x]
  (print-xml-tag x {} nil))

(defmethod print-xml String [x]
  (print (escape-xml x)))

(defmethod print-xml nil [x])

(defmethod print-xml :default [x]
  (print x))


(defn prxml
  "Print XML to *out*.  Vectors become XML tags: the first item is the
  tag name; optional second item is a map of attributes.

  Sequences are processed recursively, so you can use map and other
  sequence functions inside prxml.

    (prxml [:p {:class \"greet\"} [:i \"Ladies & gentlemen\"]])
    ; => <p class=\"greet\"><i>Ladies &amp; gentlemen</i></p>

  PSEUDO-TAGS: some keywords have special meaning:

    :raw!      do not XML-escape contents
    :comment!  create an XML comment
    :decl!     create an XML declaration, with attributes
    :cdata!    create a CDATA section
    :doctype!  create a DOCTYPE!

    (prxml [:p [:raw! \"<i>here & gone</i>\"]])
    ; => <p><i>here & gone</i></p>

    (prxml [:decl! {:version \"1.1\"}])
    ; => <?xml version=\"1.1\" encoding=\"UTF-8\"?>"
  [& args]
  (doseq [arg args] (print-xml arg)))
