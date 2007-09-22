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
      `(let [or# ~x]
         (if or# or# (or ~@rest)))))

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

;;list stuff
(defn peek [list]
  (. RT (peek list)))

(defn pop [list]
  (. RT (pop list)))

(defn nth [coll index]
 (. RT (nth coll index)))

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

(defn rseq [smap]
  (. smap (rseq)))

(defn name [#^clojure.lang.Named x]
  (. x (getName)))

(defn namespace [#^clojure.lang.Named x]
  (. x (getNamespace)))

(defn andfn [& args]
      (if (nil? (rest args))
          (first args)
        (and (first args) (recur (rest args)))))

(defn orfn [& args]
      (if (nil? args)
          nil
        (or (first args) (recur (rest args)))))


(defmacro locking [x & body]
  `(let [lockee# ~x]
        (try-finally
           (do (monitor-enter lockee#) ~@body)
           (monitor-exit lockee#))))

(defmacro ..
  ([x form] `(. ~x ~form))
  ([x form & more] `(.. (. ~x ~form) ~@more)))

;;polyfns
(defmacro defpolyfn [name dispatch-fn]
  `(def ~name (new clojure.lang.PolyFn ~dispatch-fn)))

(defmacro defmethod [polyfn dispatch-val & fn-tail]
  `(let [pvar# (the-var ~polyfn)]
     (locking pvar#
        (. pvar# (bindRoot (.. pvar# (getRoot) (assoc ~dispatch-val (fn ~@fn-tail))))))))

(defmacro binding [bindings & body]
  (let [var-ize (fn [var-vals]
                    (loop [ret [] vvs (seq var-vals)]
                          (if vvs
                              (recur  (conj (conj ret `(the-var ~(first vvs))) (second vvs))
                                      (rest (rest vvs)))
                            (seq ret))))]
    `(try-finally
      (do
          (. Var (pushThreadBindings (hash-map ~@(var-ize bindings))))
          ~@body)
      (. Var (popThreadBindings)))))

;;Refs
(defn ref [x]
 (new Ref x))

(defn deref [#^Ref ref]
  (. ref (get)))

(defn deref! [#^Ref ref]
  (. ref (currentVal)))

(defn commute [#^Ref ref fun]
  (. ref (commute fun)))

(defn set
  ([#^Ref ref]
    (. ref (touch))
    (. ref (get)))
  ([#^Ref ref val]
    (. ref (set val))))

(defmacro sync [flags-ignored-for-now & body]
  `(. clojure.lang.LockingTransaction
    (runInTransaction (fn [] ~@body))))

