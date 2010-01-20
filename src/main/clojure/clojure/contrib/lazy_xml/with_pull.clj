;   Copyright (c) Chris Houser, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; optional module to allow lazy-xml to use pull parser instead of sax

(in-ns 'clojure.contrib.lazy-xml)
(import '(org.xmlpull.v1 XmlPullParser XmlPullParserFactory))

(defn- attrs [xpp]
  (for [i (range (.getAttributeCount xpp))]
    [(keyword (.getAttributeName xpp i))
     (.getAttributeValue xpp i)]))

(defn- ns-decs [xpp]
  (let [d (.getDepth xpp)]
    (for [i (range (.getNamespaceCount xpp (dec d)) (.getNamespaceCount xpp d))]
      (let [prefix (.getNamespacePrefix xpp i)]
        [(keyword (str "xmlns" (when prefix (str ":" prefix))))
         (.getNamespaceUri xpp i)]))))

(defn- attr-hash [xpp]
  (into {} (concat (ns-decs xpp) (attrs xpp))))

(defn- pull-step [xpp]
  (let [step (fn [xpp]
               (condp = (.next xpp)
                 XmlPullParser/START_TAG
                   (cons (struct node :start-element
                                 (keyword (.getName xpp))
                                 (attr-hash xpp))
                         (pull-step xpp))
                   XmlPullParser/END_TAG
                   (cons (struct node :end-element
                                 (keyword (.getName xpp)))
                         (pull-step xpp))
                   XmlPullParser/TEXT
                   (let [text (.trim (.getText xpp))]
                     (if (empty? text)
                       (recur xpp)
                       (cons (struct node :characters nil nil text)
                             (pull-step xpp))))))]
    (lazy-seq (step xpp))))

(def #^{:private true} factory
  (doto (XmlPullParserFactory/newInstance)
    (.setNamespaceAware true)))

(defn- parse-seq-pull [s]
  (let [xpp (.newPullParser factory)]
    (.setInput xpp s)
    (pull-step xpp)))

(def has-pull true)
