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
  (case (.next xpp)
     XmlPullParser/START_TAG
        (lazy-cons (struct node :start-element
                           (keyword (.getName xpp))
                           (attr-hash xpp))
                   (pull-step xpp))
     XmlPullParser/END_TAG
        (lazy-cons (struct node :end-element
                           (keyword (.getName xpp)))
                   (pull-step xpp))
     XmlPullParser/TEXT
        (let [text (.trim (.getText xpp))]
          (if (seq text)
            (lazy-cons (struct node :characters nil nil text)
                       (pull-step xpp))
            (recur xpp)))))

(def #^{:private true} factory
  (doto (XmlPullParserFactory/newInstance)
    (setNamespaceAware true)))

(defn- parse-seq-pull [s]
  (let [xpp (.newPullParser factory)]
    (.setInput xpp s)
    (pull-step xpp)))

(def has-pull true)
