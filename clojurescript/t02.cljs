; This may look like Clojure, but it's actually ClojureScript.  Macros
; may be used here, but should be defined elsewhere, in regular
; Clojure code.
(ns n01se)

(defn my-take
  "Returns a lazy seq of the first n items in coll, or all items if
  there are fewer than n."  
  [n coll]
    (when (and (pos? n) (seq coll))
      (lazy-cons (first coll) (my-take (dec n) (rest coll)))))

(defn script-src []
  (for [elem (.getElementsByTagName document "script")]
    (do
      (prn :next)
      (if-let [src (.src elem)]
        src
        "--none--"))))

(doseq [src (my-take 2 (script-src))]
  (prn src))
