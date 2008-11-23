;   Copyright (c) Chris Houser, April 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Specialization of zip-filter for xml trees.

(ns clojure.contrib.zip-filter.xml
    (:require [clojure.contrib.zip-filter :as zf]
              [clojure.zip :as zip]
              [clojure.xml :as xml]))

(declare xml->)

(defn attr
  "Returns the xml attribute named attrname, of the xml node at location loc."
  ([attrname]     (fn [loc] (attr loc attrname)))
  ([loc attrname] (when (zip/branch? loc) (-> loc zip/node :attrs attrname))))

(defn attr=
  "Returns a query predicate that matches a node when it has an
  attribute named attrname whose value is attrval."
  [attrname attrval] (fn [loc] (= attrval (attr loc attrname))))

(defn tag=
  "Returns a query predicate that matches a node when its is a tag
  named tagname."
  [tagname]
    (fn [loc]
      (filter #(and (zip/branch? %) (= tagname ((zip/node %) :tag)))
              (if (zf/auto? loc)
                (zf/children-auto loc)
                (list (zf/auto true loc))))))

(defn text
  "Returns the textual contents of the given location, similar to
  xpaths's value-of"
  [loc]
    (.replaceAll
      #^String (apply str (xml-> loc zf/descendants zip/node string?))
      (str "[\\s" (char 160) "]+") " "))

(defn text=
  "Returns a query predicate that matches a node when its textual
  content equals s."
  [s] (fn [loc] (= (text loc) s)))

(defn seq-test
  "Returns a query predicate that matches a node when its xml content
  matches the query expresions given."
  #^{:private true}
  [preds] (fn [loc] (and (apply xml-> loc preds) (list loc))))

(defn xml->
  "The loc is passed to the first predicate.  If the predicate returns
  a collection, each value of the collection is passed to the next
  predicate.  If it returns a location, the location is passed to the
  next predicate.  If it returns true, the input location is passed to
  the next predicate.  If it returns false or nil, the next predicate
  is not called.

  This process is repeated, passing the processed results of each
  predicate to the next predicate.  xml-> returns the final sequence.
  The entire chain is evaluated lazily.

  There are also special predicates: keywords are converted to tag=,
  strings to text=, and vectors to sub-queries that return true if
  they match.

  See the footer of zip-query.clj for examples."
  [loc & preds]
    (zf/mapcat-chain loc preds
                     #(cond (keyword? %) (tag= %)
                            (string?  %) (text= %)
                            (vector?  %) (seq-test %))))

(defn xml1->
  "Returns the first item from loc based on the query predicates
  given.  See xml->"
  [loc & preds] (first (apply xml-> loc preds)))


; === examples ===

(comment

(defn parse-str [s]
  (zip/xml-zip (xml/parse (new org.xml.sax.InputSource
                               (new java.io.StringReader s)))))

(def atom1 (parse-str "<?xml version='1.0' encoding='UTF-8'?>
<feed xmlns='http://www.w3.org/2005/Atom'>
  <id>tag:blogger.com,1999:blog-28403206</id>
  <updated>2008-02-14T08:00:58.567-08:00</updated>
  <title type='text'>n01senet</title>
  <link rel='alternate' type='text/html' href='http://n01senet.blogspot.com/'/>
  <entry>
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
"))

; simple single-function filter
(assert (= (xml-> atom1 #((zip/node %) :tag))
           '(:feed)))

; two-stage filter using helpful query prediates
(assert (= (xml-> atom1 (tag= :title) text)
           '("n01senet")))

; same filter as above, this time using keyword shortcut
(assert (= (xml-> atom1 :title text)
           '("n01senet")))

; multi-stage filter
(assert (= (xml-> atom1 :entry :author :name text)
           '("Chouser" "agriffis")))

; test xml1->
(assert (= (xml1-> atom1 :entry :author :name text)
           "Chouser"))

; multi-stage filter with subquery specified using a vector
(assert (= (xml-> atom1 :entry [:author :name (text= "agriffis")]
                        :id text)
           '("2")))

; same filter as above, this time using a string shortcut
(assert (= (xml-> atom1 :entry [:author :name "agriffis"] :id text)
           '("2")))

; attribute access
(assert (= (xml-> atom1 :title (attr :type))
           '("text")))

; attribute filtering
(assert (= (xml-> atom1 :link [(attr= :rel "alternate")] (attr :type))
           '("text/html")))

; ancestors
(assert (= (xml-> atom1 zf/descendants :id "2" zf/ancestors zip/node #(:tag %))
           '(:id :entry :feed)))

; ancestors with non-auto tag= (:entry), followed by auto tag= (:id)
(assert (= (xml-> atom1 zf/descendants :name "Chouser" zf/ancestors
                  :entry :id text)
           '("1")))

; left-locs and detection of returning a single loc (zip/up)
(assert (= (xml-> atom1 zf/descendants :name "Chouser" zip/up
                  zf/left-locs :id text)
           '("1")))

; right-locs
(assert (= (xml-> atom1 zf/descendants :id zf/right-locs :author text)
           '("Chouser" "agriffis")))

)
