(ns clojure.contrib.expect.util
  (:use clojure.contrib.seq-utils))

(defmacro assert-args [fnname & pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  ~(str fnname " requires " (second pairs)))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args fnname more)))))

(defn index-of
  "Returns the first index of value v in the collection or nil."
  [coll v]
  (first (positions #{v} coll)))
