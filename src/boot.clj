;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-namespace 'clojure)

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

(defn str [#^Object x] (. x (toString)))

(defn strcat [x & ys]
  (let [#^String s (str x)]
    (if ys
        (recur (. s  (concat (str (first ys)))) (rest ys))
      s)))

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

(defn contains [map key]
 (. RT (contains map key)))

(defn get [map key]
 (. RT (get map key)))

(defn assoc [map key val]
 (. RT (assoc map key val)))

(defn dissoc [map key]
 (. RT (dissoc map key)))

(defn find [map key]
 (. RT (find map key)))

(defn select [map keyseq]
 (loop [ret {} keys (seq keyseq)]
   (if keys
        (let [entry (. RT (find map (first keys)))]
            (recur
                (if entry
                    (conj ret entry)
                   ret)
                (rest keys)))
      ret)))

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

(defn find-var [sym]
 (. Var (find sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Refs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;; sequence fns  ;;;;;;;;;;;;;;;;;;;;;;;

(defn reduce
  ([f coll]
     (if (seq coll)
       (thisfn f (rest coll) (first coll))
      (f)))
  ([f coll val]
    (if (seq coll)
       (recur f (rest coll) (f val (first coll)))
      val)))

(defn reverse [coll]
  (reduce conj coll nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fn stuff ;;;;;;;;;;;;;;;;


(defn comp [& fs]
  (let [fs (reverse fs)]
     (fn [& args]
       (loop [ret (apply (first fs) args) fs (rest fs)]
          (if fs
              (recur ((first fs) ret) (rest fs))
             ret)))))

(defn appl
	([f arg1]
	   (fn [& args] (apply f arg1 args)))
	([f arg1 arg2]
	   (fn [& args] (apply f arg1 arg2 args)))
	([f arg1 arg2 arg3]
	   (fn [& args] (apply f arg1 arg2 arg3 args)))
	([f arg1 arg2 arg3 & more]
	  (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))

;;;;;;;;;;;;;;;;;;; sequence fns  ;;;;;;;;;;;;;;;;;;;;;;;


  
(defn every [pred coll]
  (if (seq coll)
     (and (pred (first coll))
          (recur pred (rest coll)))
    t))

(def not-every (comp not every))

(defn any [pred coll]
  (when (seq coll)
    (or (pred (first coll)) (recur pred (rest coll)))))

(def not-any (comp not any))

(defn map
  ([f coll]
    (when (seq coll)
       (lazy-cons (f (first coll)) (map f (rest coll)))))
  ([f coll & colls]
    (when (and (seq coll) (every seq colls))
      (lazy-cons (apply f (first coll) (map first colls))
                 (apply map f (rest coll) (map rest colls))))))

(defn mapcat [f & colls]
   (apply concat (apply map f colls)))

(defn filter [pred coll]
  (when (seq coll)
     (if (pred (first coll))
         (lazy-cons (first coll) (filter pred (rest coll)))
       (recur pred (rest coll)))))

(defn take [n coll]
  (when (and (pos? n) (seq coll))
    (lazy-cons (first coll) (take (dec n) (rest coll)))))

(defn take-while [pred coll]
  (when (and (seq coll) (pred (first coll)))
     (lazy-cons (first coll) (take-while pred (rest coll)))))

(defn drop [n coll]
  (if (and (pos? n) (seq coll))
      (recur (dec n) (rest coll))
     coll))

(defn drop-while [pred coll]
  (if (and (seq coll) (pred (first coll)))
      (recur pred (rest coll))
     coll))

(defn cycle-rep [xs ys]
  (if xs
      (lazy-cons (first xs) (cycle-rep (rest xs) ys))
     (recur ys ys)))
     
(defn cycle [coll]
  (when (seq coll)
     (cycle-rep (seq coll) (seq coll))))

(defn split-at [n coll]
  [(take n coll) (drop n coll)])

(defn split-with [pred coll]
  [(take-while pred coll) (drop-while pred coll)])

(defn repeat [x]
  (lazy-cons x (repeat x)))

(defn replicate [n x]
  (take n (repeat x)))
  
(defn iterate [f x]
 (let [v (f x)]
   (lazy-cons v (iterate f v))))


(defn merge [& maps]
  (reduce conj maps))

;; evaluation
(defn eval [form]
  (. clojure.lang.Compiler (eval form)))
  
(defmacro import [pkg & class-names]
  (loop [ret () classes class-names]
        (if classes
            (let [c (first classes)]
              (recur (conj ret
                           (if (instance? c Symbol)
                               `(set! *imports* (assoc *imports* '~c ~(strcat pkg "." c)))
                             `(set! *imports* (assoc *imports* '~(second c) ~(strcat pkg "." (first c))))))
                     (rest classes)))
          (cons `do ret))))

;(defmacro refer-to [ns & names]
;  (loop [ret () names names]
;        (if names
;            (let [v (first names)]
;              (recur (conj ret
;                           (if (instance? v Symbol)
;                               `(set! *refers* (assoc *refers* '~v (the-var ~(. Symbol (intern (str ns) (str v))))))
;                             `(set! *refers* (assoc *refers* '~(second v) (the-var ~(. Symbol (intern (str ns) (str (first v)))))))))
;                     (rest names)))
;          (cons `do ret))))

(defn refer-to [& export-maps]
  (set! *refers* (apply merge export-maps)))

(defn make-export-map [var-syms]
  (loop [ret {}
         vs (seq var-syms)]
    (if vs
         (let [s (first vs) v (find-var s)]
            (if v
                (recur (assoc ret (. Symbol (intern (name s))) v) (rest vs))
               (throw (new Exception (strcat "Can't find Var: " s)))))
       ret)))

(def *exports*
  (make-export-map
	`(  load-file eql-ref?
		list cons conj defn
		vector hash-map sorted-map sorted-map-by
		meta with-meta defmacro when when-not
		nil? not first rest second
		eql? str strcat gensym cond
		apply list* delay lazy-cons concat
		and or + * / - == < <= > >=
		inc dec pos? neg? zero?
		complement constantly identity seq count
		peek pop nth contains get
		assoc dissoc find keys vals merge
		rseq name namespace locking ..
		defpolyfn defmethod binding find-var
		ref deref deref! commute set sync
		reduce reverse comp appl
		every not-every any not-any
		map mapcat filter take take-while drop drop-while
		cycle split-at split-with repeat replicate iterate
		eval import refer-to in-namespace
	)))