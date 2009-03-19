(ns clojure.contrib.greatest-least)

(defn- boundary
  [cmp-fn f & args]
  (when args
    (reduce (fn [a b] (if (cmp-fn (compare (f b) (f a)))
                        b
                        a)) args)))

(defn greatest-by
  "Return the argument for which f yields the greatest value."
  [f & args]
  (apply boundary pos? f args))

(defn greatest
  "Return the greatest argument."
  [& args]
  (apply greatest-by identity args))

(defn least-by
  "Return the argument for which f yields the smallest value."
  [f & args]
  (apply boundary neg? f args))

(defn least
  "Return the smallest element."
  [& args]
  (apply least-by identity args))


(defn- boundary-all
  [cmp-fn f & args]
  (when args
    (reduce (fn [a b]
              (if (nil? a)
                (cons b nil)
                (let [x (compare (f b) (f (first a)))]
                  (cond (zero? x) (cons b a)
                        (cmp-fn x) (cons b nil)
                        :else a))))
            nil
            args)))

(defn all-greatest-by
  "Return all the elements for which f yields the greatest value."
  [f & args]
  (apply boundary-all pos? f args))

(defn all-greatest
  "Returns all the greatest elements."
  [& args]
  (apply all-greatest-by identity args))

(defn all-least-by
  "Return all the elements for which f yields the least value."
  [f & args]
  (apply boundary-all neg? f args))

(defn all-least
  "Returns all the least elements."
  [& args]
  (apply all-least-by identity args))
