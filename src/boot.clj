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
   (list 'if test nil (cons 'do body)))

(def t (. RT T))

(defn vector
      ([] (. clojure.lang.PersistentVector EMPTY))
      ([& args]
          (. clojure.lang.PersistentVector (create args))))

(defn nil? [x] (if x nil t))

(defn not [x] (nil? x))

(defn first [x] (. RT (first x)))

(defn rest [x] (. RT (rest x)))

(defn second [x] (. RT (second x)))

(defn eql? [x y] (. RT (equal x y)))

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
  ([] nil)
  ([x] x)
  ([x & rest]
      (let [gor (gensym "or__")]
        (list 'let (vector gor x)
              (list 'if gor gor (cons 'or rest))))))

(defn spread [arglist]
      (cond
       (nil? arglist) nil
       (nil? (rest arglist)) (first arglist)
       :else (cons (first arglist) (thisfn (rest arglist)))))
 
(defn apply [f & args]
      (. f (applyTo (spread args))))

(defn list* [& args]
      (spread args))

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

(defmacro locking [x & body]
  (let [gsym (gensym)]
    (list 'let [gsym x]
          (list 'try-finally
                (cons 'do (cons (list 'monitor-enter gsym) body))
                (list 'monitor-exit gsym)))))

(defmacro delay [& body]
  (list '. 'clojure.lang.Delay (list 'new (list* 'fn [] body))))

(defmacro lazy-cons [x & body]
  (list '. 'clojure.lang.FnSeq (list 'new x (list* 'delay body))))

(defn concat
      ([] nil)
      ([x & xs]
          (cond
           (nil? xs) x
           (nil? x) (recur (first xs) (rest xs))
           :else (lazy-cons (first x) (apply concat (rest x) xs)))))

(defn andf [& args]
      (if (nil? (rest args))
          (first args)
        (and (first args) (recur (rest args)))))

(defn orf [& args]
      (if (nil? args)
          nil
        (or (first args) (recur (rest args)))))