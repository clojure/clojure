(in-module "clojure")

(defn f0 ())

(defn f1 (x) x)

(defn f2 (x y) y)

(defn f5 (a b c d e) (d e) (f1 a))


(defn* f01
       (())
       ((x) x))

(defn fa (x)
      (.foo x))

(defn fk (x)
      (:foo x))

(defn fl (a b c)
      (let ((d (let ((x a)) x)))
            d)
      (let ((e c))
            e))

(defn fl* (a b c)
      (let* ((d b)
             (e d))
            e))

(defn fr (a b & c) c)