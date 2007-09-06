(def list (fn [& args] args))
(def cons (fn [x seq] (. RT (cons x seq))))

(def defn (fn [name & fdecl]
              (list 'def name (cons 'fn fdecl))))

(. (the-var defn) (setMacro))

(def defmacro (fn [name & args]
                  (list 'do
                        (cons 'defn (cons name args))
                        (list '. (list 'the-var name) '(setMacro)))))

(. (the-var defmacro) (setMacro))

(defmacro when [test & body]
   (list 'if test (cons 'do body)))

(defmacro when-not [test & body]
   (list 'if test null (cons 'do body)))

(def t (. RT T))

(defn vector
      ([] (. clojure.lang.PersistentVector EMPTY))
      ([& args]
          (. clojure.lang.PersistentVector (create args))))

(defn null? [x] (if x null t))

(defn not [x] (if x null t))

(defn first [x] (. RT (first x)))

(defn rest [x] (. RT (rest x)))

(defn second [x] (. RT (second x)))

(defn eql [x y] (. RT (equal x y)))

(defn strcat [x y] (. x (concat y)))

(defn str [x] (. x (toString)))

(defn gensym 
  ([] (thisfn "G__"))
  ([prefix-string] (. clojure.lang.Symbol (intern (strcat prefix-string (str (. RT (nextID))))))))

(defmacro cond [& clauses]
  (when clauses
    (list 'if (first clauses)
            (second clauses)
            (cons 'cond (rest (rest clauses))))))

(defmacro and
  ([] t)
  ([x] x)
  ([x & rest] (list 'if x (cons 'and rest))))

(defmacro or
  ([] null)
  ([x] x)
  ([x & rest]
      (let [gor (gensym "or__")]
        (list 'let (vector gor x)
              (list 'if gor gor (cons 'or rest))))))

(defn apply
  ([f & args]
      (let [spread (fn [arglist]
                       (cond
                        (null? arglist) null
                        (null? (rest arglist)) (first arglist)
                        :else (cons (first arglist) (thisfn (rest arglist)))))]
        (. f (applyTo (spread args))))))

(defn +
      ([] 0)
      ([x] x)
      ([x y] (. Num (add x y)))
      ([x y & rest]
          (apply thisfn (thisfn x y) rest)))

(defn *
      ([] 1)
      ([x] x)
      ([x y] (. Num (multiply x y)))
      ([x y & rest]
          (apply thisfn (thisfn x y) rest)))

(defn /
      ([x] (thisfn 1 x))
      ([x y] (. Num (divide x y)))
      ([x y & rest]
          (apply thisfn (thisfn x y) rest)))

(defn -
      ([x] (. Num (negate x)))
      ([x y] (. Num (subtract x y)))
      ([x y & rest]
          (apply thisfn (thisfn x y) rest)))

(defn <
      ([x] t)
      ([x y] (. Num (lt x y)))
      ([x y & rest]
          (and (thisfn x y) (apply thisfn y rest))))

(defn <=
      ([x] t)
      ([x y] (. Num (lte x y)))
      ([x y & rest]
          (and (thisfn x y) (apply thisfn y rest))))

(defn >
      ([x] t)
      ([x y] (. Num (gt x y)))
      ([x y & rest]
          (and (thisfn x y) (apply thisfn y rest))))

(defn >=
      ([x] t)
      ([x y] (. Num (gte x y)))
      ([x y & rest]
          (and (thisfn x y) (apply thisfn y rest))))

(defn ==
      ([x] t)
      ([x y] (. Num (equiv x y)))
      ([x y & rest]
          (and (thisfn x y) (apply thisfn y rest))))

(defn inc [x]
      (. x (onePlus)))

(defn dec [x]
      (. x (oneMinus)))

(defn pos? [x]
      (. Num (plusp x)))

(defn neg? [x]
      (. Num (minusp x)))

(defn zero? [x]
      (== x 0))

(defn complement [f]
  (fn [& args]
    (not (apply f args))))

(defn constantly [x]
  (fn [& args] x))

(defn identity [x] x)

