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
      (let ((d (let ((x a))
                 x)))
        d)
      (let ((e c))
        e))

(defn fl* (a b c)
      (let* ((d b)
             (e d))
        e))

(defn always (x)
      (fn () x))

(defn fletfn (x)
      (letfn ((a (b) b)
              (c (d) (a d))
              (d (x) (d a)))
             (c x)))


(defn fif (a b x y z)
      (if a
           (if (if x y z)
               y
             z)
         b))

(defn fr (a b & c) c)

(defn fnot (x y z)
      (if (not x)
          (not y)
        (not z)))

(defn forf (x y z)
      (if (or x y)
          x
        (or x y z)))


(defn fand (x y z)
      (if (and x y)
          x
        (and x y z)))

(defn fset (x y z)
      (set x a)
      (set b y)
      (if (set (:foo x) z)
          (set (.bar y) z)
        (set (foo x y) z)))

(defn fdo (a b c)
      (do ((a b a)
           (b c b))
          (c)
        a b c)
      (do ((a b a)
           (b c b))
          (c b)
        a b c))

(defn fg (x)
      y)
