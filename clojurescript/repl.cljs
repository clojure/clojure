(ns jsrepl)

(def append-dom)

(defn dom [o]
  (if (coll? o)
    (let [[tag attrs & body] o]
      (cond
        (coll? tag) (mapcat dom o)
        (keyword? tag)
          (let [elem (.createElement document (name tag))]
            (when (map? attrs)
              (doseq [k v] attrs
                (.setAttribute elem (name k) v)))
            [(append-dom elem (if (map? attrs) body (cons attrs body)))])
        :else (lazy-cons (.createTextNode document (str (first o)))
                         (dom (rest o)))))
    [(.createTextNode document (str o))]))

(defn append-dom [parent v]
  (doseq i (dom v)
    (.appendChild parent i))
  parent)

(def elems)

(defn repl-print [text]
  (let [log (:log elems)]
    (doseq line (.split text #"\n")
      (append-dom log [:div {:class "cg"} line]))
    (set! (.scrollTop log) (.scrollHeight log))))

(defn postexpr []
  (append-dom (:log elems)
    [:table
     [:tbody
      [:tr
       [:td {:class "cg"} "user=> "]
       [:td (-> :input elems .value (.replace #"\n$" ""))]]]])
  (set! (-> :scripts elems .innerHTML) "")
  (set! (-> :input elems .value) ""))

(defn state [status, msg]
  (set! (.innerHTML (:status elems)) status)
  (cond
    (= status "error") (do (postexpr) (print msg))
    (= status "compiled") (postexpr)))

(defn err [e]
  (println e)
  (set! *e e))

(set! *print-length* 103)

(set! (.onload window) (fn []
  ; no refs yet, so just re-def
  (set! elems (into {} (for [n '(log input status scripts)]
                      [(keyword n) (.getElementById document (str n))])))

  (set! (.print window) repl-print)

  (set! (.onkeypress (:input elems))
        (fn [e]
          (let [e (or e event)]
            (when (== (.keyCode e) 13)
              (let [s (.createElement document "script")]
                (set! (.src s) (str "http://localhost:8081/"
                                    (-> :input elems .value escape
                                        (.replace #"\+" "%2b"))))
                (.appendChild (:scripts elems) s))))))

  (println "ClojureScript")

  (.focus (:input elems))))
