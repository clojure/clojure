(def list (fn [& args] args))
(def cons (fn [x seq] (. RT (cons x seq))))
(def conj (fn [coll x] (. RT (conj coll x))))

(def defn (fn [name & fdecl]
              (list 'def name (cons 'fn fdecl))))

(. (the-var defn) (setMacro))

(defn vector
      ([] [])
      ([& args]
          (. clojure.lang.PersistentVector (create args))))

(defn hash-map
      ([] {})
      ([& args]
          (. clojure.lang.PersistentHashMap (create args))))

(defn sorted-map
      ([& args]
          (. clojure.lang.PersistentTreeMap (create args))))

(defn sorted-map-by
      ([comparator & args]
          (. clojure.lang.PersistentTreeMap (create comparator args))))

(defn meta [#^IObj x]
 (. x (meta)))

(defn with-meta [#^IObj x m]
  (. x (withMeta m)))

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

(defn nil? [x] (if x nil t))

(defn not [x] (nil? x))

(defn first [x] (. RT (first x)))

(defn rest [x] (. RT (rest x)))

(defn second [x] (. RT (second x)))

(defn eql? [x y] (. RT (equal x y)))

(defn strcat [#^String x y] (. x (concat y)))

(defn str [#^Object x] (. x (toString)))

(defn gensym 
  ([] (thisfn "G__"))
  ([prefix-string] (. Symbol (intern (strcat prefix-string (str (. RT (nextID))))))))

(defmacro cond [& clauses]
  (when clauses
    (list 'if (first clauses)
            (second clauses)
            (cons 'cond (rest (rest clauses))))))

(defn spread [arglist]
      (cond
       (nil? arglist) nil
       (nil? (rest arglist)) (first arglist)
       :else (cons (first arglist) (thisfn (rest arglist)))))

(defn apply [#^IFn f & args]
      (. f (applyTo (spread args))))

(defn list* [& args]
      (spread args))

(defmacro delay [& body]
  (list 'new 'clojure.lang.Delay (list* 'fn [] body)))

(defmacro lazy-cons [x & body]
  (list 'new 'clojure.lang.FnSeq x (list* 'delay body)))

(defn concat
      ([] nil)
      ([x & xs]
          (cond
           (nil? xs) x
           (nil? x) (recur (first xs) (rest xs))
           :else (lazy-cons (first x) (apply concat (rest x) xs)))))

;;at this point all the support for syntax-quote exists

(defmacro and
  ([] t)
  ([x] x)
  ([x & rest] `(if ~x (and ~@rest))))

(defmacro or
  ([] nil)
  ([x] x)
  ([x & rest]
      (let [gor (gensym "or__")]
        `(let [~gor ~x]
              (if ~gor ~gor (or ~@rest))))))

;;math stuff
(defn +
      ([] 0)
      ([x] x)
      ([x y] (. Num (add x y)))
      ([x y & more]
          (apply thisfn (thisfn x y) more)))

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
      (. Num (inc x)))

(defn dec [x]
      (. Num (dec x)))

(defn pos? [x]
      (. Num (posPred x)))

(defn neg? [x]
      (. Num (negPred x)))

(defn zero? [x]
      (. Num (zeroPred x)))

(defn complement [f]
  (fn [& args]
    (not (apply f args))))

(defn constantly [x]
  (fn [& args] x))

(defn identity [x] x)

;;Collection stuff

(defn seq [coll]
  (. RT (seq coll)))

(defn count [coll]
  (. RT (count coll)))

;;map stuff

(defn contains [coll key]
 (. RT (contains coll key)))

(defn get [coll key]
 (. RT (get coll key)))

(defn assoc [coll key val]
 (. RT (assoc coll key val)))

(defn dissoc [coll key]
 (. RT (dissoc coll key)))

(defn find [coll key]
 (. RT (find coll key)))

(defn keys [map]
  (. RT (keys map)))

(defn vals [map]
  (. RT (vals map)))

(defn andfn [& args]
      (if (nil? (rest args))
          (first args)
        (and (first args) (recur (rest args)))))

(defn orfn [& args]
      (if (nil? args)
          nil
        (or (first args) (recur (rest args)))))


(defmacro locking [x & body]
  (let [gsym (gensym)]
    `(let [~gsym ~x]
          (try-finally
                (do (monitor-enter ~gsym) ~@body)
                (monitor-exit ~gsym)))))

(defmacro ..
  ([x form] `(. ~x ~form))
  ([x form & more] `(.. (. ~x ~form) ~@more)))

;;polyfns
(defmacro defpolyfn [name dispatch-fn]
  `(def ~name (new clojure.lang.PolyFn ~dispatch-fn)))

(defmacro defmethod [polyfn dispatch-val & fn-tail]
  (let [pvar (gensym)]
    `(let [~pvar (the-var ~polyfn)]
       (locking ~pvar
                (. ~pvar (bindRoot (.. ~pvar (getRoot) (assoc ~dispatch-val (fn ~@fn-tail)))))))))
