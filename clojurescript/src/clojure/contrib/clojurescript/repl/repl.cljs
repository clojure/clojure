(ns jsrepl)

(def append-dom)

(defn dom [o]
  (if (coll? o)
    (let [[tag attrs & body] o]
      (if (keyword? tag)
        (let [elem (.createElement document (name tag))]
          (when (map? attrs)
            (doseq [[k v] attrs]
              (when v (.setAttribute elem (name k) v))))
          [(append-dom elem (if (map? attrs) body (cons attrs body)))])
        (mapcat dom o)))
    (when o
      [(.createTextNode document (str o))])))

(defn append-dom [parent v]
  (doseq [i (dom v)]
    (.appendChild parent i))
  parent)

(def *print-class* nil)

(defn repl-print [log text]
  (doseq [line (.split text #"\n")]
    (append-dom log
      [:div {:class (str "cg "
                         (when *print-class*
                           (str " " *print-class*)))}
       line]))
  (set! (.scrollTop log) (.scrollHeight log)))

(defn postexpr [log input]
  (append-dom log
    [:table
     [:tbody
      [:tr
       [:td {:class "cg"} "user=> "]
       [:td (.replace (.value input) #"\n$" "")]]]]))

(defmacro print-with-class [c m]
  `(binding [*print-class* ~c]
     (println ~m)))

(set! *print-length* 103)

(defmacro let-elem-ids [ids & body]
  `(let ~(vec (mapcat #(list % (list '.getElementById 'document (str %))) ids))
     ~@body))

(set! (.onload window) (fn []
  (let-elem-ids [log input status applet]
    (set! (.print window) #(repl-print log %))

    (set! (.onkeypress input)
          (fn [ev]
            (when (== (.keyCode (or ev event)) 13)
              (let [[status-name text] (.tojs applet (.value input))]
                (if (= status-name "incomplete")
                  (set! (.src status) "dots.png")
                  (do
                    (postexpr log input)
                    (if (= status-name "js")
                      (try (prn (.eval window text))
                        (catch Exception e
                          (print-with-class "err" e)
                          (set! *e e)))
                      (print-with-class "err" text))
                    (setTimeout #(set! (.value input) "") 0)
                    (set! (.src status) "blank.gif")))))))

    (println "ClojureScript")

    (.focus input))))
