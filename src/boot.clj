;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure)

(def #^{:arglists '([& args])} list (. clojure.lang.PersistentList creator))
(def #^{:arglists '([x seq])} cons (fn* [x seq] (. clojure.lang.RT (cons x seq))))

;during bootstrap we don't have destructuring let or fn, will redefine later
(def #^{:macro true}
	let (fn* [& decl] (cons 'let* decl)))

(def #^{:macro true}
	fn (fn* [& decl] (cons 'fn* decl))) 

(def #^{:arglists '([coll x])} conj (fn [coll x] (. clojure.lang.RT (conj coll x))))
(def #^{:arglists '([x])} first (fn [x] (. clojure.lang.RT (first x))))
(def #^{:arglists '([x])} rest (fn [x] (. clojure.lang.RT (rest x))))
(def #^{:arglists '([coll])} seq (fn [coll] (. clojure.lang.RT (seq coll))))
(def #^{:arglists '([#^Class c x])} instance? (fn [#^Class c x] (. c (isInstance x))))
(def #^{:arglists '([x])} seq? (fn [x] (instance? clojure.lang.ISeq x)))
(def #^{:private true}
  sigs
  (fn [fdecl]
    (if (seq? (first fdecl))
      (loop [ret [] fdecl fdecl]
        (if fdecl
	      (recur (conj ret (first (first fdecl))) (rest fdecl))
	      (seq ret)))
      (list (first fdecl)))))
(def #^{:arglists '([map key val])} assoc (fn [map key val] (. clojure.lang.RT (assoc map key val))))

;;;;;;;;;;;;;;;;; metadata ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def #^{:arglists '([x])} meta (fn [x]
  (if (instance? clojure.lang.IObj x)
    (. #^clojure.lang.IObj x (meta)))))

(def #^{:arglists '([#^clojure.lang.IObj x m])} with-meta (fn [#^clojure.lang.IObj x m]
  (. x (withMeta m))))


(def defn (fn [name & fdecl]
              (list 'def (with-meta name (assoc (meta name) :arglists (list 'quote (sigs fdecl))))
              (cons `fn (cons name fdecl)))))

(. (var defn) (setMacro))


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


  
;;;;;;;;;;;;;;;;;;;;
(def defmacro (fn [name & args]
                  (list 'do
                        (cons `defn (cons name args))
                        (list '. (list 'var name) '(setMacro)))))

(. (var defmacro) (setMacro))

(defmacro when [test & body]
   (list 'if test (cons 'do body)))

(defmacro when-not [test & body]
   (list 'if test nil (cons 'do body)))



(defn #^Boolean nil? [x] (identical? x nil))
(defn #^Boolean false? [x] (identical? x false))
(defn #^Boolean true? [x] (identical? x true))

(defn not [x] (if x false true))


(defn second [x] (. clojure.lang.RT (second x)))

(defn ffirst [x] (first (first x)))
(defn rfirst [x] (rest (first x)))
(defn frest [x] (first (rest x)))
(defn rrest [x] (rest (rest x)))

(defn #^Boolean = [x y] (. clojure.lang.RT (equal x y)))
(defn #^Boolean not= [x y] (not (= x y)))

(defn #^String str
  ([] "")
  ([#^Object x]
   (if x (. x (toString)) ""))
  ([x & ys]
    (loop [sb (new StringBuilder (str x)) more ys]
      (if more
          (recur (. sb  (append (str (first more)))) (rest more))
        (str sb)))))

(defn symbol
  ([name] (. clojure.lang.Symbol (intern name)))
  ([ns name] (. clojure.lang.Symbol (intern ns name))))

(defn keyword
  ([name] (. clojure.lang.Keyword (intern nil name)))
  ([ns name] (. clojure.lang.Keyword (intern ns name))))

(defn gensym 
  ([] (gensym "G__"))
  ([prefix-string] (. clojure.lang.Symbol (intern (str prefix-string (str (. clojure.lang.RT (nextID))))))))

(defmacro cond [& clauses]
  (when clauses
    (list 'if (first clauses)
            (second clauses)
            (cons 'cond (rest (rest clauses))))))

(defn spread [arglist]
      (cond
       (nil? arglist) nil
       (nil? (rest arglist)) (seq (first arglist))
       :else (cons (first arglist) (spread (rest arglist)))))

(defn apply [#^clojure.lang.IFn f & args]
      (. f (applyTo (spread args))))

(defn list* [arg & args]
      (spread (cons arg args)))

(defmacro delay [& body]
  (list 'new 'clojure.lang.Delay (list* `fn [] body)))

(defn fnseq [x restfn]
  (new clojure.lang.FnSeq x restfn))

(defmacro lazy-cons [x & body]
  (list 'fnseq x (list* `fn [] body)))


  
(defn concat
      ([] nil)
      ([x & xs]
          (cond
           (nil? xs) (seq x)
           (nil? (seq x)) (recur (first xs) (rest xs))
           :else (lazy-cons (first x) (apply concat (rest x) xs)))))

;;at this point all the support for syntax-quote exists

(defmacro and
  ([] true)
  ([x] x)
  ([x & rest]
    `(let [and# ~x]
       (if and# (and ~@rest) and#))))

(defmacro or
  ([] nil)
  ([x] x)
  ([x & rest]
      `(let [or# ~x]
         (if or# or# (or ~@rest)))))

;;;;;;;;;;;;;;;;;;; sequence fns  ;;;;;;;;;;;;;;;;;;;;;;;

(defn reduce
  ([f coll]
     (if (seq coll)
       (reduce f (first coll) (rest coll))
      (f)))
  ([f val coll]
    (if (seq coll)
       (recur f (f val (first coll)) (rest coll))
      val)))

(defn reverse [coll]
  (reduce conj nil coll))
  
;;math stuff
(defn +
      ([] 0)
      ([x] x)
      ([x y] (. clojure.lang.Num (add x y)))
      ([x y & more]
          (reduce + (+ x y) more)))

(defn *
      ([] 1)
      ([x] x)
      ([x y] (. clojure.lang.Num (multiply x y)))
      ([x y & more]
          (reduce * (* x y) more)))

(defn /
      ([x] (/ 1 x))
      ([x y] (. clojure.lang.Num (divide x y)))
      ([x y & more]
          (reduce / (/ x y) more)))

(defn -
      ([x] (. clojure.lang.Num (negate x)))
      ([x y] (. clojure.lang.Num (subtract x y)))
      ([x y & more]
          (reduce - (- x y) more)))

(defn <
      ([x] true)
      ([x y] (. clojure.lang.Num (lt x y)))
      ([x y & more]
          (when (< x y)
            (if (rest more)
                (recur y (first more) (rest more))
                (< y (first more))))))

(defn <=
      ([x] true)
      ([x y] (. clojure.lang.Num (lte x y)))
      ([x y & more]
          (when (<= x y)
            (if (rest more)
                (recur y (first more) (rest more))
                (<= y (first more))))))

(defn >
      ([x] true)
      ([x y] (. clojure.lang.Num (gt x y)))
      ([x y & more]
          (when (> x y)
            (if (rest more)
                (recur y (first more) (rest more))
                (> y (first more))))))

(defn >=
      ([x] true)
      ([x y] (. clojure.lang.Num (gte x y)))
      ([x y & more]
          (when (>= x y)
            (if (rest more)
                (recur y (first more) (rest more))
                (>= y (first more))))))

(defn ==
      ([x] true)
      ([x y] (. clojure.lang.Num (equiv x y)))
      ([x y & more]
          (when (== x y)
            (if (rest more)
                (recur y (first more) (rest more))
                (== y (first more))))))

(defn max
  ([x] x)
  ([x y] (if (> x y) x y))
  ([x y & more]
   (reduce max (max x y) more)))

(defn min
  ([x] x)
  ([x y] (if (< x y) x y))
  ([x y & more]
   (reduce min (min x y) more)))

(defn inc [x]
      (. clojure.lang.Num (inc x)))

(defn dec [x]
      (. clojure.lang.Num (dec x)))

(defn #^Boolean pos? [x]
      (. clojure.lang.Num (posPred x)))

(defn #^Boolean neg? [x]
      (. clojure.lang.Num (negPred x)))

(defn #^Boolean zero? [x]
      (. clojure.lang.Num (zeroPred x)))

(defn quot [num div]
  (. clojure.lang.Num (quotient num div)))

(defn rem [num div]
  (. clojure.lang.Num (remainder num div)))

;;Bit ops

(defn bit-shift-left [x y]
  (. clojure.lang.IntegerNum (shiftLeft x y)))

(defn bit-shift-right [x y]
  (. clojure.lang.IntegerNum (shiftRight x y)))

(defn bit-and [x y]
  (. clojure.lang.IntegerNum (bitAnd x y)))

(defn bit-or [x y]
  (. clojure.lang.IntegerNum (bitOr x y)))

(defn bit-xor [x y]
  (. clojure.lang.IntegerNum (bitXor x y)))

(defn bit-not [x]
  (. clojure.lang.IntegerNum (bitNot x)))

(defn complement [f]
  (fn [& args]
    (not (apply f args))))

(defn constantly [x]
  (fn [& args] x))

(defn identity [x] x)

;;Collection stuff



(defn count [coll]
  (. clojure.lang.RT (count coll)))

;;list stuff
(defn peek [list]
  (. clojure.lang.RT (peek list)))

(defn pop [list]
  (. clojure.lang.RT (pop list)))

(defn nth [coll index]
 (. clojure.lang.RT (nth coll index)))

;;map stuff

(defn contains? [map key]
 (. clojure.lang.RT (contains map key)))

(defn get
  ([map key]
    (. clojure.lang.RT (get map key)))
  ([map key not-found]
    (. clojure.lang.RT (get map key not-found))))



(defn dissoc [map key]
 (. clojure.lang.RT (dissoc map key)))

(defn find [map key]
 (. clojure.lang.RT (find map key)))

(defn select [map keyseq]
 (loop [ret {} keys (seq keyseq)]
   (if keys
        (let [entry (. clojure.lang.RT (find map (first keys)))]
            (recur
                (if entry
                    (conj ret entry)
                   ret)
                (rest keys)))
      ret)))

(defn keys [map]
  (. clojure.lang.RT (keys map)))

(defn vals [map]
  (. clojure.lang.RT (vals map)))

(defn key [#^java.util.Map$Entry e]
 (. e (getKey)))

(defn val [#^java.util.Map$Entry e]
 (. e (getValue)))

(defn rseq [#^clojure.lang.Reversible rev]
  (. rev (rseq)))

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
        (try
           (monitor-enter lockee#)
           ~@body
           (finally
             (monitor-exit lockee#)))))

(defmacro ..
  ([x form] `(. ~x ~form))
  ([x form & more] `(.. (. ~x ~form) ~@more)))

(defmacro ->
  ([x form] `(~(first form) ~x ~@(rest form)))
  ([x form & more] `(-> (-> ~x ~form) ~@more)))

;;multimethods
(defmacro defmulti
  ([name dispatch-fn] `(defmulti ~name ~dispatch-fn :default))
  ([name dispatch-fn default-val]
    `(def ~name (new clojure.lang.MultiFn ~dispatch-fn ~default-val))))

(defmacro defmethod [multifn dispatch-val & fn-tail]
  `(let [pvar# (var ~multifn)]
      (. pvar# (commuteRoot (fn [mf#] (. mf# (assoc ~dispatch-val (fn ~@fn-tail))))))))

(defmacro remove-method [multifn dispatch-val]
  `(let [pvar# (var ~multifn)]
      (. pvar# (commuteRoot (fn [mf#] (. mf# (dissoc ~dispatch-val)))))))

;;;;;;;;; var stuff      

(defmacro binding [bindings & body]
  (let [var-ize (fn [var-vals]
                    (loop [ret [] vvs (seq var-vals)]
                          (if vvs
                              (recur  (conj (conj ret `(var ~(first vvs))) (second vvs))
                                      (rest (rest vvs)))
                            (seq ret))))]
    `(try
      (. clojure.lang.Var (pushThreadBindings (hash-map ~@(var-ize bindings))))
      ~@body
      (finally
        (. clojure.lang.Var (popThreadBindings))))))

(defn find-var [sym]
 (. clojure.lang.Var (find sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Refs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn agent [state]
 (new clojure.lang.Agent state))

(defn agent-of [state]
 (:agent ^state))

(defn ! [#^clojure.lang.Agent a f & args]
  (. a (dispatch f args)))

(defn agent-errors [#^clojure.lang.Agent a]
  (. a (getErrors)))

(defn clear-agent-errors [#^clojure.lang.Agent a]
  (. a (clearErrors)))

(defn ref [x]
 (new clojure.lang.Ref x))

(defn deref [#^clojure.lang.IRef ref]
  (. ref (get)))

(defn commute [#^clojure.lang.Ref ref fun & args]
  (. ref (commute fun args)))

(defn alter [#^clojure.lang.Ref ref fun & args]
  (. ref (alter fun args)))

(defn set [#^clojure.lang.Ref ref val]
    (. ref (set val)))

(defn ensure [#^clojure.lang.Ref ref]
    (. ref (touch))
    (. ref (get)))

(defmacro sync [flags-ignored-for-now & body]
  `(. clojure.lang.LockingTransaction
    (runInTransaction (fn [] ~@body))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fn stuff ;;;;;;;;;;;;;;;;


(defn comp [& fs]
  (let [fs (reverse fs)]
     (fn [& args]
       (loop [ret (apply (first fs) args) fs (rest fs)]
          (if fs
              (recur ((first fs) ret) (rest fs))
             ret)))))

(defn partial
	([f arg1]
	   (fn [& args] (apply f arg1 args)))
	([f arg1 arg2]
	   (fn [& args] (apply f arg1 arg2 args)))
	([f arg1 arg2 arg3]
	   (fn [& args] (apply f arg1 arg2 arg3 args)))
	([f arg1 arg2 arg3 & more]
	  (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))

;;;;;;;;;;;;;;;;;;; sequence fns  ;;;;;;;;;;;;;;;;;;;;;;;


  
(defn #^Boolean every? [pred coll]
  (if (seq coll)
     (and (pred (first coll))
          (recur pred (rest coll)))
    true))

(def #^Boolean not-every? (comp not every?))

(defn some [pred coll]
  (when (seq coll)
    (or (pred (first coll)) (recur pred (rest coll)))))

(def #^Boolean not-any? (comp not some))

(defn map
  ([f coll]
    (when (seq coll)
       (lazy-cons (f (first coll)) (map f (rest coll)))))
  ([f coll & colls]
    (when (and (seq coll) (every? seq colls))
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

(defn cycle [coll]
  (when (seq coll)
    (let [rep (fn this [xs]
                  (if xs
                    (lazy-cons (first xs) (this (rest xs)))
                    (recur (seq coll))))]
      (rep (seq coll)))))

(defn split-at [n coll]
  [(take n coll) (drop n coll)])

(defn split-with [pred coll]
  [(take-while pred coll) (drop-while pred coll)])

(defn repeat [x]
  (lazy-cons x (repeat x)))

(defn replicate [n x]
  (take n (repeat x)))
  
(defn iterate [f x]
   (lazy-cons x (iterate f (f x))))

(defn range
 ([end] (take end (iterate inc 0)))
 ([start end] (take (- end start) (iterate inc start)))
 ([start end step]
   (take-while (partial (if (pos? step) > <) end) (iterate (partial + step) start))))

(defn merge [& maps]
  (reduce conj maps))

(defn merge-with [f & maps]
  (let [merge-entry (fn [m e]
			(let [k (key e) v (val e)]
			  (if (contains? m k)
			    (assoc m k (f (m k) v))
			    (assoc m k v))))
	merge2 (fn [m1 m2]
		   (reduce merge-entry m1 (seq m2)))]
    (reduce merge2 maps)))



(defn zipmap [keys vals]
  (loop [map {}
         ks (seq keys)
         vs (seq vals)]
    (if (and ks vs)
        (recur (assoc map (first ks) (first vs))
               (rest ks)
               (rest vs))
       map)))

(defn line-seq [#^java.io.BufferedReader rdr]
  (let [line  (. rdr (readLine))]
    (when line
      (lazy-cons line (line-seq rdr)))))

(defn comparator [pred]
  (fn [x y] (cond (pred x y) -1 (pred y x) 1 :else 0)))
  
(defn sort
  ([#^java.util.Collection coll]
   (when (and coll (not (. coll (isEmpty))))
     (let [a (. coll (toArray))]
       (. java.util.Arrays (sort a))
       (seq a))))
  ([#^java.util.Comparator comp #^java.util.Collection coll]
   (when (and coll (not (. coll (isEmpty))))
     (let [a (. coll (toArray))]
       (. java.util.Arrays (sort a comp))
       (seq a)))))

(defn sort-by
  ([keyfn coll]
    (sort (fn [x y] (. #^Comparable (keyfn x) (compareTo (keyfn y)))) coll))
  ([keyfn #^java.util.Comparator comp coll]
    (sort (fn [x y] (. comp (compare (keyfn x) (keyfn y)))) coll)))

;; evaluation

(defn eval [form]
  (. clojure.lang.Compiler (eval form)))

(defn defimports [& imports-maps]
  (def *imports* (apply merge imports-maps)))

(defmacro doseq [item list & body]
  `(loop [list# (seq ~list)]
     (when list#
       (let [~item (first list#)]
         ~@body)
       (recur (rest list#)))))

(defn scan
  ([coll]
    (when (seq coll)
      (recur (rest coll))))
  ([n coll]
    (when (and (seq coll) (pos? n))
      (recur (dec n) (rest coll)))))

(defn touch
  ([coll]
   (scan coll)
   coll)
  ([n coll]
   (scan n coll)
   coll))

(defn await [& agents]
  (let [latch (new java.util.concurrent.CountDownLatch (count agents))
	count-down (fn [agent] (. latch (countDown)) agent)]
    (doseq agent agents
      (! agent count-down))
    (. latch (await))))

(defn await-for [timeout-ms & agents]
  (let [latch (new java.util.concurrent.CountDownLatch (count agents))
	count-down (fn [agent] (. latch (countDown)) agent)]
    (doseq agent agents
      (! agent count-down))
    (. latch (await  timeout-ms (. java.util.concurrent.TimeUnit MILLISECONDS)))))
  
(defmacro dotimes [i n & body]
  `(loop [~i 0 n# ~n]
     (when (< ~i n#)
       ~@body
       (recur (inc ~i) n#))))

(defn import [& import-lists]
 (when import-lists
   (let [#^clojure.lang.Namespace ns *ns*
         pkg (ffirst import-lists)
         classes (rfirst import-lists)]
       (doseq c classes
         (. ns (importClass c (. Class (forName (str pkg "." c)))))) )
   (apply import (rest import-lists))))

(defn into-array [aseq]
  (. clojure.lang.RT (seqToTypedArray (seq aseq))))

(defn into [to from]
  (let [ret to items (seq from)]
    (if items
       (recur (conj ret (first items)) (rest items))
      ret)))

(defn array [& items]
  (into-array items))

(defn make-proxy [classes method-map]
  (. java.lang.reflect.Proxy
    (newProxyInstance (. (identity clojure.lang.Compiler) (getClassLoader))
                      (into-array classes)
                      (new clojure.lang.ProxyHandler method-map))))

(defmacro implement [classes & fs]
  `(make-proxy
      ~classes
      ~(loop [fmap {} fs fs]
              (if fs
                  (recur (assoc fmap (name (ffirst fs))
                                     (cons `fn (rfirst fs)))
                         (rest fs))
                 fmap))))

(defn pr
  ([] nil)
  ([x]
   (. clojure.lang.RT (print x *out*))
   nil)
  ([x & more]
   (pr x)
   (. *out* (append \space))
   (apply pr more)))

(defn newline []
  (. *out* (append \newline))
  nil)

(defn prn [& more]
  (apply pr more)
  (newline))

(defn print [& more]
  (binding [*print-readably* nil]
    (apply pr more)))

(defn println [& more]
  (binding [*print-readably* nil]
    (apply prn more)))

(defn read
  ([]
    (read *in*))
  ([stream]
    (read stream true nil))
  ([stream eof-error? eof-value]
    (read stream eof-error? eof-value false))
  ([stream eof-error? eof-value recursive?]
    (. clojure.lang.LispReader (read stream eof-error? eof-value recursive?))))

(defmacro with-open [rdr init & body]
  `(let [~rdr ~init]
     (try
      ~@body
      (finally
        (. ~rdr (close))))))

(defmacro doto [x & members]
  (let [gx (gensym)]
    `(let [~gx ~x]
       (do
         ~@(map (fn [m] (list '. gx m))
                members))
       ~gx)))

(defmacro memfn [name & args]
  `(fn [target# ~@args]
      (. target# (~name ~@args))))

(defmacro time [expr]
   `(let [start# (. System (nanoTime))
          ret# ~expr]
       (prn (str "Elapsed time: " (/ (- (. System (nanoTime)) start#) 1000000.0) " msecs"))
       ret#))


(defn #^Integer int [x]
  (. clojure.lang.RT (intCast x)))

(defn #^Long long [#^Number x]
  (. x (longValue)))

(defn #^Float float [#^Number x]
  (. x (floatValue)))

(defn #^Double double [#^Number x]
  (. x (doubleValue)))

(defn #^Short short [#^Number x]
  (. x (shortValue)))

(defn #^Byte byte [#^Number x]
  (. x (byteValue)))

(defn #^Character char [x]
  (. clojure.lang.RT (charCast x)))

(defn #^Boolean boolean [x]
  (if x true false))

(import '(java.lang.reflect Array))

(defn alength [array]
  (. Array (getLength array)))

(defn aget 
  ([array idx]
   (. Array (get array idx)))
  ([array idx & idxs]
   (apply aget (aget array idx) idxs)))

(defn aset
  ([array idx val]
   (. Array (set array idx val))
   val)
  ([array idx idx2 & idxv]
   (apply aset (aget array idx) idx2 idxv)))

(defmacro def-aset [name method coerce]
  `(defn ~name
    ([array# idx# val#]
     (. Array (~method array# idx# (~coerce val#)))
     val#)
    ([array# idx# idx2# & idxv#]
     (apply ~name (aget array# idx#) idx2# idxv#))))

(def-aset aset-int setInt int)
(def-aset aset-long setLong long)
(def-aset aset-boolean setBoolean boolean)
(def-aset aset-float setFloat float)
(def-aset aset-double setDouble double)
(def-aset aset-short setShort short)
(def-aset aset-byte setByte byte)
(def-aset aset-char setChar char)

(defn make-array 
  ([#^Class type len]
    (. Array (newInstance type (int len))))
  ([#^Class type dim & more-dims]
    (let [dims (cons dim more-dims)
          #^"[I" dimarray (make-array (. Integer TYPE)  (count dims))]
      (dotimes i (alength dimarray)
        (aset-int dimarray i (nth dims i)))
      (. Array (newInstance type dimarray)))))

(defn to-array [#^java.util.Collection coll]
  (. coll (toArray)))

(defn to-array-2d [#^java.util.Collection coll]
  (let [ret (make-array (. Class (forName "[Ljava.lang.Object;")) (. coll (size)))]
    (loop [i 0 xs (seq coll)]
      (when xs
        (aset ret i (to-array (first xs)))
        (recur (inc i) (rest xs))))
    ret))

(import '(java.util.concurrent Executors LinkedBlockingQueue))

(defn pmap
  ([f coll]
   (let [nthreads (.. Runtime (getRuntime) (availableProcessors))
         exec (. Executors (newFixedThreadPool nthreads))
         todo (ref (seq coll))
         out (ref 0)
         q (new LinkedBlockingQueue)
         produce (fn []
                     (let [job (sync nil
                                 (when @todo
                                   (let [item (first @todo)]
                                     (alter todo rest)
                                     (commute out inc)
                                     (list item))))]
                       (when job
                         (. q (put (f (first job))))
                         (recur))))
         tasks (doseq dnu (map (fn [task]
                                   (. exec (submit #^java.util.concurrent.Callable task)))
                               (replicate nthreads produce)))
         consume (fn this []
                     (if (sync nil (and (or @todo (pos? @out))
                                        (commute out dec)))
                       (fnseq (. q (take)) this)
                       (do
                         (. exec (shutdown))
                         (doseq x tasks)
                         nil)))]
     (consume)))
  ([f coll & colls]
   (pmap (fn [items] (apply f items))
         (let [encl-fn (fn this [collseq]
                           (when (every? seq collseq)
                             (lazy-cons (map first collseq)
                                        (this (map rest collseq)))))]
           (encl-fn (cons coll colls))))))

(defn macroexpand-1 [form]
  (let [v (. clojure.lang.Compiler (isMacro (first form)))]
    (if v
      (apply @v (rest form))
      form)))

(defn macroexpand [form]
   (let [ex (macroexpand-1 form)
	 v  (. clojure.lang.Compiler (isMacro (first ex)))]
     (if v
       (macroexpand ex)
       ex)))

(defn create-struct [& keys]
   (. clojure.lang.PersistentStructMap (createSlotMap keys)))

(defmacro defstruct [name & keys]
  `(def ~name (create-struct ~@keys)))
  
(defn struct-map [s & inits]
  (. clojure.lang.PersistentStructMap (create s inits)))

(defn struct [s & vals]
  (. clojure.lang.PersistentStructMap (construct s vals)))

(defn accessor [s key]
   (. clojure.lang.PersistentStructMap (getAccessor s key)))

(defn subvec
  ([v start]
    (subvec v start (count v)))
  ([v start end]
    (. clojure.lang.RT (subvec v start end))))

(defn load [rdr]
  (. clojure.lang.Compiler (load rdr)))

(defn resultset-seq [#^java.sql.ResultSet rs]
  (let [rsmeta (. rs (getMetaData))
	idxs (range 1 (inc (. rsmeta (getColumnCount))))
	keys (map (comp keyword (memfn toLowerCase))
	       (map (fn [i] (. rsmeta (getColumnName i))) idxs))
	row-struct (apply create-struct keys)
	row-values (fn [] (map (fn [#^Integer i] (. rs (getObject i))) idxs))
	rows (fn this []
	       (when (. rs (next))
		     (fnseq (apply struct row-struct (row-values)) this)))]
    (rows)))

(defn to-set [coll]
  (loop [ret {} keys (seq coll)]
    (if keys
      (recur (if (contains? ret (first keys))
	       ret
	       (assoc ret (first keys) true))
	     (rest keys))
      ret)))

(defn distinct [coll]
  (keys (to-set coll)))

(defn filter-key [keyfn pred amap]
  (loop [ret {} es (seq amap)]
    (if es
      (if (pred (keyfn (first es)))
	(recur (assoc ret (key (first es)) (val (first es))) (rest es))
	(recur ret (rest es)))
      ret)))

(defn find-ns [sym]
  (. clojure.lang.Namespace (find sym)))

(defn create-ns [sym]
  (. clojure.lang.Namespace (findOrCreate sym)))

(defn remove-ns [sym]
  (. clojure.lang.Namespace (remove sym)))

(defn all-ns []
  (. clojure.lang.Namespace (all)))

(defn ns-name [#^clojure.lang.Namespace ns]
  (. ns (getName)))

(defn ns-map [#^clojure.lang.Namespace ns]
  (. ns (getMappings)))

(defn ns-unmap [#^clojure.lang.Namespace ns sym]
  (. ns (unmap sym)))

;(defn export [syms]
;  (doseq sym syms
;   (.. *ns* (intern sym) (setExported true))))

(defn ns-publics [#^clojure.lang.Namespace ns]
  (filter-key val (fn [v] (and (instance? clojure.lang.Var v)
                               (= ns (. v ns))
                               (. v (isPublic))))
          (ns-map ns)))

(defn ns-imports [#^clojure.lang.Namespace ns]
  (filter-key val (partial instance? Class) (ns-map ns)))

(defn refer [ns-sym & filters]
  (let [ns (find-ns ns-sym)
	    fs (apply hash-map filters)
	    nspublics (ns-publics ns)
	    rename (or (:rename fs) {})
	    exclude (to-set (:exclude fs))
	    to-do (or (:only fs) (keys nspublics))]
    (doseq sym to-do
      (when-not (exclude sym)
	    (let [v (nspublics sym)]
	      (when-not v
	        (throw (new java.lang.IllegalAccessError (str sym " is not public"))))
	      (. *ns* (refer (or (rename sym) sym) v)))))))

(defn ns-refers [#^clojure.lang.Namespace ns]
  (filter-key val (fn [v] (and (instance? clojure.lang.Var v)
			                (not= ns (. v ns))))
          (ns-map ns)))

(defn ns-interns [#^clojure.lang.Namespace ns]
  (filter-key val (fn [v] (and (instance? clojure.lang.Var v)
			                (= ns (. v ns))))
          (ns-map ns)))

(defn take-nth [n coll]
  (when (seq coll)
    (lazy-cons (first coll) (take-nth n (drop n coll)))))

(defn interleave [& colls]
  (apply concat (apply map list colls)))

(defn var-get [#^clojure.lang.Var x]
  (. x (get)))

(defn var-set [#^clojure.lang.Var x val]
  (. x (set val)))

(defmacro with-local-vars [name-vals-vec & body]
  `(let [~@(interleave (take-nth 2 name-vals-vec)
                       (repeat '(. clojure.lang.Var (create))))]
     (try
      (. clojure.lang.Var (pushThreadBindings (hash-map ~@name-vals-vec)))
      ~@body
      (finally (. clojure.lang.Var (popThreadBindings))))))

(defn ns-resolve [ns sym]
  (. clojure.lang.Compiler (resolveIn ns sym)))

(defn resolve [sym]
  (ns-resolve *ns* sym))

(defn array-map
	([] (. clojure.lang.PersistentArrayMap EMPTY))
	([& args] (new clojure.lang.PersistentArrayMap (to-array args))))

(defn nthrest [coll n]
  (loop [n n xs (seq coll)]
    (if (and xs (pos? n))
      (recur (dec n) (rest xs))
      xs)))

(defn string? [x]
  (instance? String x))

(defn symbol? [x]
  (instance? clojure.lang.Symbol x))

(defn map? [x]
  (instance? clojure.lang.IPersistentMap x))


(defn vector? [x]
  (instance? clojure.lang.IPersistentVector x))

;redefine let with destructuring
(defmacro let [bindings & body]
  (let [bmap (apply array-map bindings)
        pb (fn pb [bvec b v]
               (let [pvec
                     (fn [bvec b val]
                         (let [gvec (gensym "vec__")]
                           (loop [ret (-> bvec (conj gvec) (conj val))
                                  n 0
                                  bs b
                                  seen-rest? false]
                             (if bs
                               (let [firstb (first bs)]
                                 (cond
                                  (= firstb '&) (recur (pb ret (second bs) (list `nthrest gvec n))
                                                       n
                                                       (rrest bs)
                                                       true)
                                  (= firstb :as) (pb ret (second bs) gvec)
                                  :else (if seen-rest?
                                          (throw (new Exception "Unsupported binding form, only :as can follow & parameter"))
                                          (recur (pb ret firstb  (list `nth gvec n))
                                                 (inc n)
                                                 (rest bs)
                                                 seen-rest?))))
                               ret))))
                     pmap
                     (fn [bvec b v]
                         (let [gmap (or (:as b) (gensym "map__"))
                               defaults (:or b)]
                           (loop [ret (-> bvec (conj gmap) (conj v))
                                  bes (seq (-> b (dissoc :as) (dissoc :or)))]
                             (if bes
                               (let [bb (key (first bes))
                                     bk (val (first bes))
                                     has-default (contains? defaults bb)]
                                 (recur (pb ret bb (if has-default
                                                     (list `get gmap bk (defaults bb))
                                                     (list `get gmap bk)))
                                        (rest bes)))
                               ret))))]
                 (cond
                  (symbol? b) (-> bvec (conj b) (conj v))
                  (vector? b) (pvec bvec b v)
                  (map? b) (pmap bvec b v)
                  :else (throw (new Exception "Unsupported binding form")))))
        process-entry (fn [bvec b] (pb bvec (key b) (val b)))]
    (if (every? symbol? (keys bmap))
      `(let* ~bindings ~@body)
      `(let* ~(reduce process-entry [] bmap) ~@body))))

(defmacro when-first [x xs & body]
  `(when ~xs
     (let [~x (first ~xs)]
       ~@body)))

(defmacro lazy-cat
  ([coll] `(seq ~coll))
  ([coll & colls]
   `(let [iter# (fn iter# [coll#]
		    (if (seq coll#)
		      (lazy-cons (first coll#) (iter# (rest coll#)))
		      (lazy-cat ~@colls)))]
      (iter# ~coll))))
      
(defmacro for
  ([seq-expr expr] (list `for seq-expr `true expr))
  ([seq-exprs filter-expr expr]
   (let [emit (fn emit [ses]
		  (let [x (key (first ses)) xs (val (first ses))
			giter (gensym "iter__") gxs (gensym "s__")]
		    `(fn ~giter [~gxs]
			 (when-first ~x ~gxs
			    ~(if (rest ses)
			       `(let [iterys# ~(emit (rest ses))]
				  (lazy-cat (iterys# ~(val (second ses)))
					    (~giter (rest ~gxs))))
			       `(if ~filter-expr
				  (lazy-cons ~expr (~giter (rest ~gxs)))
				  (recur (rest ~gxs))))))))]
     `(let [iter# ~(emit (seq (apply array-map seq-exprs)))]
	(iter# ~(second seq-exprs))))))

;redefine fn with destructuring
(defmacro fn [& sigs]
  (let [name (if (symbol? (first sigs)) (first sigs) nil)
        sigs (if name (rest sigs) sigs)
        sigs (if (vector? (first sigs)) (list sigs) sigs)
        psig (fn [sig]
               (let [[params & body] sig]
                 (if (every? symbol? params)
                   sig
                   (loop [params params
                          new-params []
                          lets []]
                     (if params
                       (if (symbol? (first params))
                         (recur (rest params) (conj new-params (first params)) lets)
                         (let [gparam (gensym "p__")]
                            (recur (rest params) (conj new-params gparam) (-> lets (conj (first params)) (conj gparam)))))
                       `(~new-params
                         (let ~lets
                            ~@body)))))))
        new-sigs (map psig sigs)]
    (if name
      (list* 'fn* name new-sigs)
      (cons 'fn* new-sigs))))

(defn bean [#^Object x]
  (let [c (. x (getClass))
	pmap (reduce (fn [m #^java.beans.PropertyDescriptor pd]
			 (let [name (. pd (getName))
			       method (. pd (getReadMethod))]
			   (if (and method (zero? (alength (. method (getParameterTypes)))))
			     (assoc m (keyword name) (fn [] (. method (invoke x nil))))
			     m)))
		     {}
		     (seq (.. java.beans.Introspector
			      (getBeanInfo c)
			      (getPropertyDescriptors))))
	v (fn [k] ((pmap k)))
    snapshot (fn []
                (reduce (fn [m e]
                            (assoc m (key e) ((val e))))
                        {} (seq pmap)))]
    (implement [clojure.lang.IPersistentMap]
      (containsKey [k] (contains? pmap k))
      (entryAt [k] (when (contains? pmap k) (new clojure.lang.MapEntry k (v k))))
      (valAt ([k] (v k))
	     ([k default] (if (contains? pmap k) (v k) default)))
      (cons [m] (conj (snapshot) m))
      (count [] (count pmap))
      (assoc [k v] (assoc (snapshot) k v))
      (without [k] (dissoc (snapshot) k))
      (seq [] ((fn this [pseq]
		  (when pseq
		    (lazy-cons (new clojure.lang.MapEntry (first pseq) (v (first pseq)))
			       (this (rest pseq))))) (keys pmap))))))

(defmacro comment [& body])

(defmacro with-out-str [& body]
  `(let [s# (new java.io.StringWriter)]
    (binding [*out* s#]
      ~@body
      (str s#))))

(defn pr-str [& xs]
  (with-out-str
    (apply pr xs)))

(defn prn-str [& xs]
  (with-out-str
    (apply prn xs)))

(defn print-str [& xs]
  (with-out-str
    (apply print xs)))

(defn println-str [& xs]
  (with-out-str
    (apply println xs)))

(defmacro assert [x]
  `(when-not ~x
     (throw (new Exception (str "Assert failed: " (prstr '~x))))))

(defn
#^{:doc "test [v] finds fn at key :test in var metadata and calls it, presuming failure will throw exception"}
test [v]
  (let [f (:test ^v)]
    (if f
      (do (f) :ok)
      :no-test)))

(defn #^java.util.regex.Matcher re-matcher [#^java.util.regex.Pattern re s]
  (. re (matcher s)))

(defn re-groups [#^java.util.regex.Matcher m]
  (let [gc  (. m (groupCount))]
    (if (zero? gc)
      (. m (group))
      (loop [ret [] c 0]
	(if (<= c gc)
	  (recur (conj ret (. m (group c))) (inc c))
	  ret)))))

(defn re-seq [#^java.util.regex.Pattern re s]
  (let [m (re-matcher re s)]
    ((fn step []
	(when (. m (find))
	  (lazy-cons (re-groups m) (step)))))))

(defn re-matches [#^java.util.regex.Pattern re s]
  (let [m (re-matcher re s)]
    (when (. m (matches))
      (re-groups m))))


(defn re-find
  ([#^java.util.regex.Matcher m]
   (when (. m (find))
     (re-groups m)))
  ([#^java.util.regex.Pattern re s]
   (let [m (re-matcher re s)]
     (re-find m))))

