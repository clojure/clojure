;;; xml.clj -- a pretty print dispatch version of prxml.clj -- a compact syntax for generating XML

;; by Tom Faulhaber, based on the original by Stuart Sierra, http://stuartsierra.com/
;; May 13, 2009

;; Copyright (c) 2009 Tom Faulhaber/Stuart Sierra. All rights reserved.  The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; See function "prxml" at the bottom of this file for documentation.


(ns 
  #^{:author "Tom Faulhaber, based on the original by Stuart Sierra",
     :doc "A version of prxml that uses a pretty print dispatch function."}
  clojure.contrib.pprint.examples.xml
  (:use [clojure.contrib.lazy-xml :only (escape-xml)]
        [clojure.contrib.java-utils :only (as-str)]
        [clojure.contrib.pprint :only (formatter-out write)]
        [clojure.contrib.pprint.utilities :only (prlabel)]))

(def
 #^{:doc "If true, empty tags will have a space before the closing />"}
 *html-compatible* false)

(def
 #^{:doc "The number of spaces to indent sub-tags."}
 *prxml-indent* 2)

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
    ((formatter-out "<?xml version=\"~a\" encoding=\"~a\"~@[ standalone=\"~a\"~]?>") 
     (:version attrs) (:encoding attrs) (:standalone attrs))))

(defmethod print-xml-tag :cdata! [tag attrs contents]
  ((formatter-out "<[!CDATA[~{~a~}]]>") contents))

(defmethod print-xml-tag :doctype! [tag attrs contents]
  ((formatter-out "<[!DOCTYPE [~{~a~}]]>") contents))

(defmethod print-xml-tag :default [tag attrs contents]
  (let [tag-name (as-str tag)]
    (if (seq? contents)
      ((formatter-out "~<~<<~a~1:i~{ ~:_~{~a=\"~a\"~}~}>~:>~vi~{~_~w~}~0i~_</~a>~:>")
       [[tag-name (map #(vector (as-str (key %)) (as-str (val %))) attrs)] *prxml-indent* contents tag-name])
      ((formatter-out "~<<~a~1:i~{~:_ ~{~a=\"~a\"~}~}/>~:>") [tag-name attrs]))))


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
  (doseq [arg args] (write arg :dispatch print-xml))
  (when (pos? (count args)) (newline)))
