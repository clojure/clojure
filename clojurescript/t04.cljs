; This may look like Clojure, but it's actually ClojureScript.  Macros
; may be used here, but should be defined elsewhere, in regular
; Clojure code.
(ns n01se)

(defn script-src []
  (for [elem (.getElementsByTagName document "script")]
    (do
      (prn :next)
      (if-let src (.src elem)
        src
        "--none--"))))

(doseq src (take 2 (script-src))
  (prn src))
