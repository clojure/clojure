(ns jsrepl)

(def append-dom)

(defn dom [o]
  (if (coll? o)
    (let [[tag attrs & body] o]
      (if (keyword? tag)
        (let [elem (.createElement document (name tag))]
          (when (map? attrs)
            (doseq [k v] attrs
              (when v (.setAttribute elem (name k) v))))
          [(append-dom elem (if (map? attrs) body (cons attrs body)))])
        (mapcat dom o)))
    (when o
      [(.createTextNode document (str o))])))

(defn append-dom [parent v]
  (doseq i (dom v)
    (.appendChild parent i))
  parent)

(def elems)
(def lastval)
(def *print-color* nil)

(defn repl-print [text]
  (let [log (:log elems)]
    (doseq line (.split text #"\n")
      (append-dom log
        [:div {:class "cg"
               :style (when *print-color*
                        (str "color:" *print-color*))}
         line]))
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

(defn print-err [m]
  (binding [*print-color* "#a00"]
    (println m)))

(defn show-state [url]
  (set! (-> :status elems .src) url))

(defn state [status msg]
  (cond
    (= status "incomplete") (show-state "dots.png")
    (= status "done") (prn lastval)
    (= status "error") (do
                         (postexpr)
                         (show-state "blank.gif")
                         (print-err msg))
    (= status "compiled") (do
                            (postexpr)
                            (setTimeout #(show-state "blank.gif") 0))))

(defn err [e]
  (print-err e)
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
              (set! (-> :status elems .src) "clojure-logo-anim-03.gif")
              (append-dom (:scripts elems)
                [:script {:src (str "http://localhost:8081/"
                                    (-> :input elems .value escape
                                        (.replace #"\+" "%2b")))}])))))

  (println "ClojureScript")

  (.focus (:input elems))))
