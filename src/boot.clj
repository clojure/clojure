;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure)

(def
	#^{:arglists '([& args])
		:doc "Creates a new list containing the items."}
list (. clojure.lang.PersistentList creator))

(def
	#^{:arglists '([x seq])
	   :doc "Returns a new seq where x is the first element and seq is the rest."}
cons (fn* [x seq] (. clojure.lang.RT (cons x seq))))

;during bootstrap we don't have destructuring let or fn, will redefine later
(def
	#^{:macro true}
let (fn* [& decl] (cons 'let* decl)))

(def
	#^{:macro true}
fn (fn* [& decl] (cons 'fn* decl)))

(def
	#^{:arglists '([coll x])
		:doc "conj[oin]. Returns a new collection with the item 'added'. (conj nil item) returns (item).
		The 'addition' may happen at different 'places' depending on the concrete type."}
conj (fn [coll x] (. clojure.lang.RT (conj coll x))))

(def
	#^{:arglists '([x])
	   :doc "Returns the first item in the collection. Calls seq on its argument. If coll is nil, returns nil."}
first (fn [coll] (. clojure.lang.RT (first coll))))

(def
	#^{:arglists '([x])
	  :doc "Returns a seq of the items after the first. Calls seq on its argument.
	        If there are no more items, returns nil."}
rest (fn [x] (. clojure.lang.RT (rest x))))

(def second (fn [x] (. clojure.lang.RT (second x))))

(def ffirst (fn [x] (first (first x))))
(def rfirst (fn [x] (rest (first x))))
(def frest (fn [x] (first (rest x))))
(def rrest (fn [x] (rest (rest x))))

(def
	#^{:arglists '([coll])
	   :doc "Sequence. Returns a new ISeq on the collection. If the collection is empty, returns nil.
			(seq nil) returns nil. seq also works on Strings, native Java arrays (of reference types)
			and any objects that implement Iterable."}
seq (fn [coll] (. clojure.lang.RT (seq coll))))

(def
	#^{:arglists '([#^Class c x])}
instance? (fn [#^Class c x] (. c (isInstance x))))

(def
	#^{:arglists '([x])
		:doc "Return true if x implements ISeq"}
seq? (fn [x] (instance? clojure.lang.ISeq x)))

(def
	#^{:private true}
sigs
  (fn [fdecl]
    (if (seq? (first fdecl))
      (loop [ret [] fdecl fdecl]
        (if fdecl
	      (recur (conj ret (first (first fdecl))) (rest fdecl))
	      (seq ret)))
      (list (first fdecl)))))

(def
	#^{:arglists '([map key val] [map key val & kvs])
		:doc "assoc[iate]. When applied to a map, returns a new map of the same (hashed/sorted) type,
		that contains the mapping of key(s) to val(s). When applied to a vector, returns a new vector that contains val
		at index. Note - index must be <= (count vector)."}
assoc (fn
       ([map key val] (. clojure.lang.RT (assoc map key val)))
       ([map key val & kvs]
        (let [ret (assoc map key val)]
          (if kvs
            (recur ret (first kvs) (second kvs) (rrest kvs))
            ret)))))

;;;;;;;;;;;;;;;;; metadata ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def
	#^{:arglists '([x])
	   :doc "Returns the metadata of obj, returns nil if there is no metadata."}
meta (fn [x]
  (if (instance? clojure.lang.IObj x)
    (. #^clojure.lang.IObj x (meta)))))

(def
	#^{:arglists '([#^clojure.lang.IObj x m])
	   :doc "Returns an object of the same type and value as obj, with map m as its metadata."}
with-meta (fn [#^clojure.lang.IObj x m]
  (. x (withMeta m))))


(def
	#^{:doc "Same as (def name (fn [params* ] exprs*)) or (def name (fn ([params* ] exprs*)+))"}
defn (fn [name & fdecl]
              (list 'def (with-meta name (assoc (meta name) :arglists (list 'quote (sigs fdecl))))
              (cons `fn (cons name fdecl)))))

(. (var defn) (setMacro))


(defn
	#^{:doc "Creates a new vector containing the args."}
vector
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
(def
	#^{:doc "Like defn, but the resulting function name is declared as a macro
	and will be used as a macro by the compiler when it is called."}
defmacro (fn [name & args]
                  (list 'do
                        (cons `defn (cons name args))
                        (list '. (list 'var name) '(setMacro)))))

(. (var defmacro) (setMacro))

(defmacro
	#^{:doc "Evaluates test. If logical true, evaluates body in an implicit do."}
when [test & body]
   (list 'if test (cons 'do body)))

(defmacro
	#^{:doc "Evaluates test. If logical false, evaluates body in an implicit do."}
when-not [test & body]
   (list 'if test nil (cons 'do body)))



(defn
	#^{:tag Boolean :doc "Returns true if x is nil, false otherwise."}
nil? [x] (identical? x nil))

(defn
	#^{:tag Boolean :doc "Returns true if x is the value false, false otherwise."}
false? [x] (identical? x false))

(defn
	#^{:tag Boolean :doc "Returns true if x is the value true, false otherwise."}
true? [x] (identical? x true))

(defn
	#^{:tag Boolean :doc "Returns true if x is logical false, false otherwise."}
not [x] (if x false true))




(defn
	#^{:tag Boolean
       :doc "Equality. Returns true if obj1 equals obj2, false if not. Same as Java obj1.equals(obj2)
	except it also works for nil, and compares numbers in a type-independent manner.
	Clojure's immutable data structures define equals() (and thus =) as a value, not an identity, comparison."}
= [x y] (. clojure.lang.RT (equal x y)))

(defn
	#^{:tag Boolean
       :doc "Same as (not (= obj1 obj2))"}
not= [x y] (not (= x y)))

(defn
	#^{:tag String
       :doc "With no args, returns the empty string. With one arg x, returns x.toString().
	(str nil) returns the empty string.  With more than one arg, returns the concatenation
	of the str values of the args."}
str
  ([] "")
  ([#^Object x]
   (if x (. x (toString)) ""))
  ([x & ys]
    (loop [sb (new StringBuilder (str x)) more ys]
      (if more
          (recur (. sb  (append (str (first more)))) (rest more))
        (str sb)))))

(defn
	#^{:doc "Returns a Symbol with the given namespace and name."}
symbol
  ([name] (. clojure.lang.Symbol (intern name)))
  ([ns name] (. clojure.lang.Symbol (intern ns name))))

(defn
	#^{:doc "Returns a Keyword with the given namespace and name.
	Do not use : in the keyword strings, it will be added automatically."}
keyword
  ([name] (. clojure.lang.Keyword (intern nil name)))
  ([ns name] (. clojure.lang.Keyword (intern ns name))))

(defn
	#^{:doc "Returns a new symbol with a unique name. If a prefix string is supplied,
	the name is prefix__# where # is some unique number. If prefix is not supplied, the prefix is 'G'."}
gensym
  ([] (gensym "G__"))
  ([prefix-string] (. clojure.lang.Symbol (intern (str prefix-string (str (. clojure.lang.RT (nextID))))))))

(defmacro
	#^{:doc "Takes a set of test/expr pairs. It evaluates each test one at a time.
	If a test returns logical true, cond evaluates and returns the value of the corresponding expr
	and doesn't evaluate any of the other tests or exprs. (cond) returns nil."}
cond [& clauses]
  (when clauses
    (list 'if (first clauses)
            (second clauses)
            (cons 'cond (rest (rest clauses))))))

(defn spread [arglist]
      (cond
       (nil? arglist) nil
       (nil? (rest arglist)) (seq (first arglist))
       :else (cons (first arglist) (spread (rest arglist)))))

(defn 
	#^{:doc "Applies fn f to the argument list formed by prepending args to argseq."}
apply [#^clojure.lang.IFn f & args]
      (. f (applyTo (spread args))))

(defn
	#^{:doc "Creates a new list containing the item prepended to more."}
list* [item & more]
      (spread (cons item more)))

(defmacro delay [& body]
  (list 'new 'clojure.lang.Delay (list* `fn [] body)))

(defn
	#^{:doc "Returns a seq object whose first is first and whose rest is the value produced by calling restfn
	with no arguments. restfn will be called at most once per step in the sequence, e.g. calling rest repeatedly
	on the head of the seq calls restfn once - the value it yields is cached."}
fnseq [x restfn]
  (new clojure.lang.FnSeq x restfn))

(defmacro
	#^{:doc "Expands to code which produces a seq object whose first is first-expr (evaluated) and whose rest is body,
	which is not evaluated until rest is called. rest-expr will be evaluated at most once per step in the sequence,
	e.g. calling rest repeatedly on the head of the seq evaluates rest-expr once - the value it yields is cached."}
lazy-cons [x & body]
  (list 'fnseq x (list* `fn [] body)))


  
(defn
	#^{:doc "Returns a lazy seq representing the concatenation of the elements in colls."}
concat
      ([] nil)
      ([x & xs]
          (cond
           (nil? xs) (seq x)
           (nil? (seq x)) (recur (first xs) (rest xs))
           :else (lazy-cons (first x) (apply concat (rest x) xs)))))

;;at this point all the support for syntax-quote exists

(defmacro
	#^{:doc "Evaluates exprs one at a time, from left to right. If a form returns logical false (nil or false),
	and returns that value and doesn't evaluate any of the other expressions,
	otherwise it returns the value of the last expr. (and) returns true."}
and
  ([] true)
  ([x] x)
  ([x & rest]
    `(let [and# ~x]
       (if and# (and ~@rest) and#))))

(defmacro
	#^{:doc "Evaluates exprs one at a time, from left to right. If a form returns a logical true value,
	or returns that value and doesn't evaluate any of the other expressions, otherwise it returns the value of the
	last expression. (or) returns nil."}
or
  ([] nil)
  ([x] x)
  ([x & rest]
      `(let [or# ~x]
         (if or# or# (or ~@rest)))))

;;;;;;;;;;;;;;;;;;; sequence fns  ;;;;;;;;;;;;;;;;;;;;;;;

(defn
	#^{:doc "f should be a function of 2 arguments. If val is not supplied, returns the result of applying f to the
	first 2 items in coll, then applying f to that result and the 3rd item, etc. If coll contains no items,
	f must accept no arguments as well,	and reduce returns the result of calling f with no arguments.
	If coll has only 1 item, it is returned and f is not called. 
	If val is supplied, returns the result of applying f to val and the first item in coll, then applying f to
	that result and the 2nd item, etc. If coll contains no items, returns val and f is not called."}
reduce
  ([f coll]
     (if (seq coll)
       (reduce f (first coll) (rest coll))
      (f)))
  ([f val coll]
    (if (seq coll)
       (recur f (f val (first coll)) (rest coll))
      val)))

(defn
	#^{:doc "Returns a seq of the items in coll in reverse order. Not lazy."}
reverse [coll]
  (reduce conj nil coll))
  
;;math stuff
(defn
	#^{:doc "Returns the sum of nums. (+) returns 0."}
+
      ([] 0)
      ([x] x)
      ([x y] (. clojure.lang.Num (add x y)))
      ([x y & more]
          (reduce + (+ x y) more)))

(defn
	#^{:doc "Returns the product of nums. (*) returns 1."}
*
      ([] 1)
      ([x] x)
      ([x y] (. clojure.lang.Num (multiply x y)))
      ([x y & more]
          (reduce * (* x y) more)))

(defn
	#^{:doc "If no denominators are supplied, returns 1/numerator,
	else returns numerator divided by all of the denominators."}
/
      ([x] (/ 1 x))
      ([x y] (. clojure.lang.Num (divide x y)))
      ([x y & more]
          (reduce / (/ x y) more)))

(defn
	#^{:doc "If no ys are supplied, returns the negation of x, else subtracts the ys from x and returns the result."}
-
      ([x] (. clojure.lang.Num (negate x)))
      ([x y] (. clojure.lang.Num (subtract x y)))
      ([x y & more]
          (reduce - (- x y) more)))

(defn
	#^{:doc "Returns non-nil if nums are in monotonically increasing order, otherwise nil."}
<
      ([x] true)
      ([x y] (. clojure.lang.Num (lt x y)))
      ([x y & more]
          (when (< x y)
            (if (rest more)
                (recur y (first more) (rest more))
                (< y (first more))))))

(defn
	#^{:doc "Returns non-nil if nums are in monotonically non-decreasing order, otherwise nil."}
<=
      ([x] true)
      ([x y] (. clojure.lang.Num (lte x y)))
      ([x y & more]
          (when (<= x y)
            (if (rest more)
                (recur y (first more) (rest more))
                (<= y (first more))))))

(defn
	#^{:doc "Returns non-nil if nums are in monotonically decreasing order, otherwise nil."}
>
      ([x] true)
      ([x y] (. clojure.lang.Num (gt x y)))
      ([x y & more]
          (when (> x y)
            (if (rest more)
                (recur y (first more) (rest more))
                (> y (first more))))))

(defn
	#^{:doc "Returns non-nil if nums are in monotonically non-increasing order, otherwise nil."}
>=
      ([x] true)
      ([x y] (. clojure.lang.Num (gte x y)))
      ([x y & more]
          (when (>= x y)
            (if (rest more)
                (recur y (first more) (rest more))
                (>= y (first more))))))

(defn
	#^{:doc "Returns non-nil if nums all have the same value, otherwise nil"}
==
      ([x] true)
      ([x y] (. clojure.lang.Num (equiv x y)))
      ([x y & more]
          (when (== x y)
            (if (rest more)
                (recur y (first more) (rest more))
                (== y (first more))))))

(defn
	#^{:doc "Returns the greatest of the nums."}
max
  ([x] x)
  ([x y] (if (> x y) x y))
  ([x y & more]
   (reduce max (max x y) more)))

(defn
	#^{:doc "Returns the least of the nums."}
min
  ([x] x)
  ([x y] (if (< x y) x y))
  ([x y & more]
   (reduce min (min x y) more)))

(defn
	#^{:doc "Returns a number one greater than num."}
inc [x]
      (. clojure.lang.Num (inc x)))

(defn
	#^{:doc "Returns a number one less than num."}
dec [x]
      (. clojure.lang.Num (dec x)))

(defn
	#^{:tag Boolean
	   :doc "Returns true if num is greater than zero, else false"}
pos? [x]
      (. clojure.lang.Num (posPred x)))

(defn
	#^{:tag Boolean
	   :doc "Returns true if num is less than zero, else false"}
neg? [x]
      (. clojure.lang.Num (negPred x)))

(defn 
	#^{:tag Boolean
	   :doc "Returns true if num is zero, else false"}
zero? [x]
      (. clojure.lang.Num (zeroPred x)))

(defn
	#^{:doc "quot[ient] of dividing numerator by denominator."}
quot [num div]
  (. clojure.lang.Num (quotient num div)))

(defn
	#^{:doc "rem[ainder] of dividing numerator by denominator."}
rem [num div]
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

(defn
	#^{:doc "Takes a fn f and returns a fn that takes the same arguments as f,
	has the same effects, if any, and returns the opposite truth value."}
complement [f]
  (fn [& args]
    (not (apply f args))))

(defn
	#^{:doc "Returns a function that takes any number of arguments and returns x."}
constantly [x]
  (fn [& args] x))

(defn
	#^{:doc "Returns its argument."}
identity [x] x)

;;Collection stuff



(defn
	#^{:doc "Returns the number of items in the collection. (count nil) returns 0.
	Also works on strings, arrays, and Java Collections and Maps"}
count [coll]
  (. clojure.lang.RT (count coll)))

;;list stuff
(defn
	#^{:doc "Same as first. Returns the first item in the list. If the list is empty, returns nil."}
peek [list]
  (. clojure.lang.RT (peek list)))

(defn
	#^{:doc "Returns a new list without the first item. If the list is empty, throws an exception.
	Note - not the same as rest."}
pop [list]
  (. clojure.lang.RT (pop list)))

(defn nth [coll index]
 (. clojure.lang.RT (nth coll index)))

;;map stuff

(defn
	#^{:doc "Returns true if key is present, else false."}
contains? [map key]
 (. clojure.lang.RT (contains map key)))

(defn
	#^{:doc "Returns the value mapped to key, not-found or nil if key not present."}
get
  ([map key]
    (. clojure.lang.RT (get map key)))
  ([map key not-found]
    (. clojure.lang.RT (get map key not-found))))

(defn
	#^{:doc "dissoc[iate]. Returns a new map of the same (hashed/sorted) type, that does not contain a mapping for key(s)."}
dissoc
  ([map key]
   (. clojure.lang.RT (dissoc map key)))
  ([map key & ks]
   (let [ret (dissoc map key)]
     (if ks
       (recur ret (first ks) (rest ks))
       ret))))

(defn
	#^{:doc "Returns the map entry for key, or nil if key not present."}
find [map key]
 (. clojure.lang.RT (find map key)))

(defn
	#^{:doc "Returns a map containing only those entries in map whose key is in keys"}
select [map keyseq]
 (loop [ret {} keys (seq keyseq)]
   (if keys
        (let [entry (. clojure.lang.RT (find map (first keys)))]
            (recur
                (if entry
                    (conj ret entry)
                   ret)
                (rest keys)))
      ret)))

(defn
	#^{:doc "Returns a sequence of the map's keys."}
keys [map]
  (. clojure.lang.RT (keys map)))

(defn
	#^{:doc "Returns a sequence of the map's values."}
vals [map]
  (. clojure.lang.RT (vals map)))

(defn
	#^{:doc "Returns the key of the map entry."}
key [#^java.util.Map$Entry e]
 (. e (getKey)))

(defn
	#^{:doc "Returns the value in the map entry."}
val [#^java.util.Map$Entry e]
 (. e (getValue)))

(defn rseq [#^clojure.lang.Reversible rev]
  (. rev (rseq)))

(defn
	#^{:doc "Returns the name String of a symbol or keyword."}
name [#^clojure.lang.Named x]
  (. x (getName)))

(defn
	#^{:doc "Returns the namespace String of a symbol or keyword, or nil if not present."}
namespace [#^clojure.lang.Named x]
  (. x (getNamespace)))

(defn andfn [& args]
      (if (nil? (rest args))
          (first args)
        (and (first args) (recur (rest args)))))

(defn orfn [& args]
      (if (nil? args)
          nil
        (or (first args) (recur (rest args)))))


(defmacro
	#^{:doc "Executes exprs in an implicit do, while holding the monitor of x.
Will release the monitor of x in all circumstances."}
locking [x & body]
  `(let [lockee# ~x]
        (try
           (monitor-enter lockee#)
           ~@body
           (finally
             (monitor-exit lockee#)))))

(defmacro ..
  ([x form] `(. ~x ~form))
  ([x form & more] `(.. (. ~x ~form) ~@more)))

(defmacro
	#^{:doc "Macro. Threads the expr through the forms. Inserts expr as the second item in the first form.
	If there are more forms, inserts the first form as the second item in second form, etc."}
->
  ([x form] `(~(first form) ~x ~@(rest form)))
  ([x form & more] `(-> (-> ~x ~form) ~@more)))

;;multimethods
(defmacro 
	#^{:doc "Creates a new multimethod with the associated dispatch function. If default-dispatch-val is supplied
	it becomes the default dispatch value of the multimethod, otherwise the default dispatch value is :default."}
defmulti
  ([name dispatch-fn] `(defmulti ~name ~dispatch-fn :default))
  ([name dispatch-fn default-val]
    `(def ~name (new clojure.lang.MultiFn ~dispatch-fn ~default-val))))

(defmacro
	#^{:doc "Creates and installs a new method of multimethod associated with dispatch-value. "}
defmethod [multifn dispatch-val & fn-tail]
  `(let [pvar# (var ~multifn)]
      (. pvar# (commuteRoot (fn [mf#] (. mf# (assoc ~dispatch-val (fn ~@fn-tail))))))))

(defmacro
	#^{:doc "Removes the method of multimethod associated with dispatch-value."}
remove-method [multifn dispatch-val]
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


(defn
	#^{:doc "Takes a set of functions and returns a fn that is the composition of those fns.
	The returned fn takes a variable number of args, applies the rightmost of fns to the args,
	the next fn (right-to-left) to the result, etc."}
comp [& fs]
  (let [fs (reverse fs)]
     (fn [& args]
       (loop [ret (apply (first fs) args) fs (rest fs)]
          (if fs
              (recur ((first fs) ret) (rest fs))
             ret)))))

(defn
	#^{:doc "Takes a function f and fewer than the normal arguments to f, and returns a fn that takes
	a variable number of additional args. When called, the returned function calls f with args + additional args."}
partial
	([f arg1]
	   (fn [& args] (apply f arg1 args)))
	([f arg1 arg2]
	   (fn [& args] (apply f arg1 arg2 args)))
	([f arg1 arg2 arg3]
	   (fn [& args] (apply f arg1 arg2 arg3 args)))
	([f arg1 arg2 arg3 & more]
	  (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))

;;;;;;;;;;;;;;;;;;; sequence fns  ;;;;;;;;;;;;;;;;;;;;;;;


  
(defn
	#^{:tag Boolean
		:doc "Returns true if (pred x) is logical true for every x in coll, else false."}
every? [pred coll]
  (if (seq coll)
     (and (pred (first coll))
          (recur pred (rest coll)))
    true))

(def
	#^{:tag Boolean
	   :doc "Returns false if (pred x) is logical true for every x in coll, else true."}
not-every? (comp not every?))

(defn
	#^{:doc "Returns the first logical true value of (pred x) for any x in coll, else nil."}
some [pred coll]
  (when (seq coll)
    (or (pred (first coll)) (recur pred (rest coll)))))

(def 
	#^{:tag Boolean
	   :doc "Returns false if (pred x) is logical true for any x in coll, else true."}
not-any? (comp not some))

(defn
	#^{:doc "Returns a lazy seq consisting of the result of applying f to the set of first items of each coll,
	followed by applying f to the set of second items in each coll, until any one of the colls is exhausted.
	Any remaining items in other colls are ignored. Function f should accept number-of-colls arguments."}
map
  ([f coll]
    (when (seq coll)
       (lazy-cons (f (first coll)) (map f (rest coll)))))
  ([f coll & colls]
    (when (and (seq coll) (every? seq colls))
      (lazy-cons (apply f (first coll) (map first colls))
                 (apply map f (rest coll) (map rest colls))))))

(defn
	#^{:doc "Returns the result of applying concat to the result of applying map to f and colls.
	Thus function f should return a collection."}
mapcat [f & colls]
   (apply concat (apply map f colls)))

(defn
	#^{:doc "Returns a lazy seq of the items in coll for which (pred item) returns true."}
filter [pred coll]
  (when (seq coll)
     (if (pred (first coll))
         (lazy-cons (first coll) (filter pred (rest coll)))
       (recur pred (rest coll)))))

(defn
	#^{:doc "Returns a lazy seq of the first n items in coll, or all items if there are fewer than n."}
take [n coll]
  (when (and (pos? n) (seq coll))
    (lazy-cons (first coll) (take (dec n) (rest coll)))))

(defn
	#^{:doc "Returns a lazy seq of successive items from coll while (pred item) returns true."}
take-while [pred coll]
  (when (and (seq coll) (pred (first coll)))
     (lazy-cons (first coll) (take-while pred (rest coll)))))

(defn
	#^{:doc "Returns a lazy seq of all but the first n items in coll."}
drop [n coll]
  (if (and (pos? n) (seq coll))
      (recur (dec n) (rest coll))
     coll))

(defn
	#^{:doc "Returns a lazy seq of the items in coll starting from the first item for which (pred item) returns nil."}
drop-while [pred coll]
  (if (and (seq coll) (pred (first coll)))
      (recur pred (rest coll))
     coll))

(defn
	#^{:doc "Returns a lazy (infinite!) seq of repetitions of the items in coll."}
cycle [coll]
  (when (seq coll)
    (let [rep (fn thisfn [xs]
                  (if xs
                    (lazy-cons (first xs) (thisfn (rest xs)))
                    (recur (seq coll))))]
      (rep (seq coll)))))

(defn
	#^{:doc "Returns a vector of [(take n coll) (drop n coll)]"}
split-at [n coll]
  [(take n coll) (drop n coll)])

(defn
	#^{:doc "Returns a vector of [(take-while pred coll) (drop-while pred coll)]"}
split-with [pred coll]
  [(take-while pred coll) (drop-while pred coll)])

(defn
	#^{:doc "Returns a lazy (infinite!) seq of xs."}
repeat [x]
  (lazy-cons x (repeat x)))

(defn
	#^{:doc "Returns a lazy seq of n xs."}
replicate [n x]
  (take n (repeat x)))
  
(defn
	#^{:doc "Returns a lazy seq of x, (f x), (f (f x)) etc."}
iterate [f x]
   (lazy-cons x (iterate f (f x))))

(defn
	#^{:doc "Returns a lazy seq of nums from start (inclusive) to end (exclusive), by step,
	where start defaults to 0 and step to 1."}
range
 ([end] (take end (iterate inc 0)))
 ([start end] (take (- end start) (iterate inc start)))
 ([start end step]
   (take-while (partial (if (pos? step) > <) end) (iterate (partial + step) start))))

(defn
	#^{:doc "Returns a map that consists of the rest of the maps conj-ed onto the first.
	If a key occurs in more than one map, the mapping from the latter (left-to-right)
	will be the mapping in the result."}
merge [& maps]
  (reduce conj maps))

(defn
	#^{:doc "Returns a map that consists of the rest of the maps conj-ed onto the first.
	If a key occurs in more than one map, the mapping(s) from the latter (left-to-right) will be combined
	with the mapping in the result by calling (f val-in-result val-in-latter)."}
merge-with [f & maps]
  (let [merge-entry (fn [m e]
			(let [k (key e) v (val e)]
			  (if (contains? m k)
			    (assoc m k (f (m k) v))
			    (assoc m k v))))
	merge2 (fn [m1 m2]
		   (reduce merge-entry m1 (seq m2)))]
    (reduce merge2 maps)))



(defn
	#^{:doc "Returns a map with the keys mapped to the corresponding vals."}
zipmap [keys vals]
  (loop [map {}
         ks (seq keys)
         vs (seq vals)]
    (if (and ks vs)
        (recur (assoc map (first ks) (first vs))
               (rest ks)
               (rest vs))
       map)))

(defn
	#^{:doc "Returns the lines of text from rdr as a lazy sequence of strings.
	rdr must implement java.io.BufferedReader."}
line-seq [#^java.io.BufferedReader rdr]
  (let [line  (. rdr (readLine))]
    (when line
      (lazy-cons line (line-seq rdr)))))

(defn
	#^{:doc "Returns an implementation of java.util.Comparator based upon pred."}
comparator [pred]
  (fn [x y] (cond (pred x y) -1 (pred y x) 1 :else 0)))
  
(defn
	#^{:doc "Returns a sorted sequence of the items in coll. If no comparator is supplied,
	the items must implement Comparable. comparator must implement java.util.Comparator."}
sort
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

(defn
	#^{:doc "Returns a sorted sequence of the items in coll, where the sort order is determined by comparing (keyfn item).
	If no comparator is supplied, the keys must implement Comparable. comparator must implement java.util.Comparator."}
sort-by
  ([keyfn coll]
    (sort (fn [x y] (. #^Comparable (keyfn x) (compareTo (keyfn y)))) coll))
  ([keyfn #^java.util.Comparator comp coll]
    (sort (fn [x y] (. comp (compare (keyfn x) (keyfn y)))) coll)))

;; evaluation

(defn
	#^{:doc "Evaluates the form data structure (not text!) and returns the result."}
eval [form]
  (. clojure.lang.Compiler (eval form)))

(defn defimports [& imports-maps]
  (def *imports* (apply merge imports-maps)))

(defmacro
	#^{:doc "Repeatedly executes body (presumably for side-effects) with binding-form bound to successive items from coll.
	Does not retain the head of the sequence. Returns nil."}
doseq [item list & body]
  `(loop [list# (seq ~list)]
     (when list#
       (let [~item (first list#)]
         ~@body)
       (recur (rest list#)))))

(defn scan [& args] (throw (new Exception "scan is now called dorun")))
(defn touch [& args] (throw (new Exception "touch is now called doall")))

(defn
	#^{:doc "When lazy sequences are produced via functions that have side effects, any effects other than those
	needed to produce the first element in the seq do not occur until the seq is consumed. dorun can be used to force
	any effects. Walks through the successive rests of the seq, does not retain the head and returns nil."}
dorun
  ([coll]
    (when (seq coll)
      (recur (rest coll))))
  ([n coll]
    (when (and (seq coll) (pos? n))
      (recur (dec n) (rest coll)))))

(defn
	#^{:doc "When lazy sequences are produced via functions that have side effects, any effects other than those
	needed to produce the first element in the seq do not occur until the seq is consumed. doall can be used to force
	any effects. Walks through the successive rests of the seq, retains the head and returns it,
	thus causing the entire seq to reside in memory at one time."}
doall
  ([coll]
   (dorun coll)
   coll)
  ([n coll]
   (dorun n coll)
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
  
(defmacro
	#^{:doc "Repeatedly executes body (presumably for side-effects) with name bound to integers from 0 through n-1."}
dotimes [i n & body]
  `(loop [~i 0 n# ~n]
     (when (< ~i n#)
       ~@body
       (recur (inc ~i) n#))))

(defn
	#^{:doc "import-list => (package-symbol class-name-symbols*)
	For each name in class-name-symbols, adds a mapping from name to the class named by package.name 
	to the current namespace."}
import [& import-lists]
 (when import-lists
   (let [#^clojure.lang.Namespace ns *ns*
         pkg (ffirst import-lists)
         classes (rfirst import-lists)]
       (doseq c classes
         (. ns (importClass c (. Class (forName (str pkg "." c)))))) )
   (apply import (rest import-lists))))

(defn into-array [aseq]
  (. clojure.lang.RT (seqToTypedArray (seq aseq))))

(defn
	#^{:doc "Returns a new coll consisting of to-coll with all of the items of from-coll conjoined."}
into [to from]
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

(defn
	#^{:doc "Prints the object(s) to the output stream that is the current value of *out*.
	Prints the object(s), separated by spaces if there is more than one.
	By default, pr and prn print in a way that objects can be read by the reader"}
pr
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

(defn
	#^{:doc "Same as pr followed by (newline)"}
prn [& more]
  (apply pr more)
  (newline))

(defn
	#^{:doc "Prints the object(s) to the output stream that is the current value of *out*.
	print and println produce output for human consumption."}
print [& more]
  (binding [*print-readably* nil]
    (apply pr more)))

(defn
	#^{:doc "Same as print followed by (newline)"}
println [& more]
  (binding [*print-readably* nil]
    (apply prn more)))


(defn
	#^{:doc "Reads the next object from stream, which must be an instance of java.io.PushbackReader or some derivee.
	stream defaults to the current value of *in* ."}
read
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

(defmacro
	#^{:doc "Evaluates expr and prints the time it took. Returns the value of expr."}
time [expr]
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
         consume (fn thisfn []
                     (if (sync nil (and (or @todo (pos? @out))
                                        (commute out dec)))
                       (fnseq (. q (take)) thisfn)
                       (do
                         (. exec (shutdown))
                         (doseq x tasks)
                         nil)))]
     (consume)))
  ([f coll & colls]
   (pmap (fn [items] (apply f items))
         (let [encl-fn (fn thisfn [collseq]
                           (when (every? seq collseq)
                             (lazy-cons (map first collseq)
                                        (thisfn (map rest collseq)))))]
           (encl-fn (cons coll colls))))))

(defn
	#^{:doc "If form represents a macro form, returns its expansion, else returns form."}
macroexpand-1 [form]
  (let [v (. clojure.lang.Compiler (isMacro (first form)))]
    (if v
      (apply @v (rest form))
      form)))

(defn
	#^{:doc "Repeatedly calls macroexpand-1 on form until it no longer represents a macro form, then returns it.
	Note neither macroexpand-1 nor macroexpand expand macros in subforms."}
macroexpand [form]
   (let [ex (macroexpand-1 form)
	 v  (. clojure.lang.Compiler (isMacro (first ex)))]
     (if v
       (macroexpand ex)
       ex)))

(defn
	#^{:doc "Returns a structure basis object."}
create-struct [& keys]
   (. clojure.lang.PersistentStructMap (createSlotMap keys)))

(defmacro
	#^{:doc "Same as (def name (create-struct keys...))"}
defstruct [name & keys]
  `(def ~name (create-struct ~@keys)))
  
(defn
	#^{:doc "Returns a new structmap instance with the keys of the structure-basis. keyvals may contain all,
	some or none of the basis keys - where values are not supplied they will default to nil.
	keyvals can also contain keys not in the basis."}
struct-map [s & inits]
  (. clojure.lang.PersistentStructMap (create s inits)))

(defn
	#^{:doc "Returns a new structmap instance with the keys of the structure-basis. vals must be supplied for
	basis keys in order - where values are not supplied they will default to nil."}
struct [s & vals]
  (. clojure.lang.PersistentStructMap (construct s vals)))

(defn
	#^{:doc "Returns a fn that, given an instance of a structmap with the basis, returns the value at the key.
	The key must be in the basis. The returned function should be (slightly) more efficient than using get,
	but such use of accessors should be limited to known performance-critical areas."}
accessor [s key]
   (. clojure.lang.PersistentStructMap (getAccessor s key)))

(defn subvec
  ([v start]
    (subvec v start (count v)))
  ([v start end]
    (. clojure.lang.RT (subvec v start end))))

(defn
	#^{:doc "sequentially read and evaluate the set of forms contained in the stream/file"}
load [rdr]
  (. clojure.lang.Compiler (load rdr)))

(defn resultset-seq [#^java.sql.ResultSet rs]
  (let [rsmeta (. rs (getMetaData))
	idxs (range 1 (inc (. rsmeta (getColumnCount))))
	keys (map (comp keyword (memfn toLowerCase))
	       (map (fn [i] (. rsmeta (getColumnName i))) idxs))
	row-struct (apply create-struct keys)
	row-values (fn [] (map (fn [#^Integer i] (. rs (getObject i))) idxs))
	rows (fn thisfn []
	       (when (. rs (next))
		     (fnseq (apply struct row-struct (row-values)) thisfn)))]
    (rows)))

(defn
	#^{:doc "Returns a map of the distinct elements of coll to true."}
to-set [coll]
  (loop [ret {} keys (seq coll)]
    (if keys
      (recur (if (contains? ret (first keys))
	       ret
	       (assoc ret (first keys) true))
	     (rest keys))
      ret)))

(defn
	#^{:doc "Returns a sequence of the elements of coll with duplicates removed"}
distinct [coll]
  (keys (to-set coll)))

(defn filter-key [keyfn pred amap]
  (loop [ret {} es (seq amap)]
    (if es
      (if (pred (keyfn (first es)))
	(recur (assoc ret (key (first es)) (val (first es))) (rest es))
	(recur ret (rest es)))
      ret)))

(defn
	#^{:doc "Returns the namespace named by the symbol or nil if it doesn't exist."}
find-ns [sym]
  (. clojure.lang.Namespace (find sym)))

(defn
	#^{:doc "Create a new namespace named by the symbol if one doesn't already exist,
	returns it or the already-existing namespace of the same name."}
create-ns [sym]
  (. clojure.lang.Namespace (findOrCreate sym)))

(defn
	#^{:doc "Removes the namespace named by the symbol. Use with caution.
	Cannot be used to remove the clojure namespace."}
remove-ns [sym]
  (. clojure.lang.Namespace (remove sym)))

(defn
	#^{:doc "Returns a sequence of all namespaces."}
all-ns []
  (. clojure.lang.Namespace (all)))

(defn
	#^{:doc "Returns the name of the namespace, a symbol."}
ns-name [#^clojure.lang.Namespace ns]
  (. ns (getName)))

(defn
	#^{:doc "Returns a map of all the mappings for the namespace."}
ns-map [#^clojure.lang.Namespace ns]
  (. ns (getMappings)))

(defn
	#^{:doc "Removes the mappings for the symbol from the namespace."}
ns-unmap [#^clojure.lang.Namespace ns sym]
  (. ns (unmap sym)))

;(defn export [syms]
;  (doseq sym syms
;   (.. *ns* (intern sym) (setExported true))))

(defn
	#^{:doc "Returns a map of the public intern mappings for the namespace."}
ns-publics [#^clojure.lang.Namespace ns]
  (filter-key val (fn [v] (and (instance? clojure.lang.Var v)
                               (= ns (. v ns))
                               (. v (isPublic))))
          (ns-map ns)))

(defn
	#^{:doc "Returns a map of the import mappings for the namespace."}
ns-imports [#^clojure.lang.Namespace ns]
  (filter-key val (partial instance? Class) (ns-map ns)))

(defn
	#^{:doc "refers to all public vars of ns, subject to filters
 filters can include at most one each of:
   :exclude list-of-symbols
   :only list-of-symbols
   :rename map-of-fromsymbol-tosymbol


  For each public interned var in the namespace named by the symbol, adds a mapping from the name of the var
  to the var to the current namespace.  Throws an exception if name is already mapped to something else in the
  current namespace. Filters can be used to select a subset, via inclusion or exclusion, or to provide a mapping
  to a symbol different from the var's name, in order to prevent clashes."}
refer [ns-sym & filters]
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

(defn
	#^{:doc "Returns a map of the refer mappings for the namespace."}
ns-refers [#^clojure.lang.Namespace ns]
  (filter-key val (fn [v] (and (instance? clojure.lang.Var v)
			                (not= ns (. v ns))))
          (ns-map ns)))

(defn
	#^{:doc "Returns a map of the intern mappings for the namespace."}
ns-interns [#^clojure.lang.Namespace ns]
  (filter-key val (fn [v] (and (instance? clojure.lang.Var v)
			                (= ns (. v ns))))
          (ns-map ns)))

(defn
	#^{:doc "Returns a lazy seq of every nth item in coll."}
take-nth [n coll]
  (when (seq coll)
    (lazy-cons (first coll) (take-nth n (drop n coll)))))

(defn
	#^{:doc "Returns a lazy seq of the first item in each coll, then the second etc."}
interleave [& colls]
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

(defn
	#^{:doc "Returns the var or Class to which a symbol will be resolved in the namespace.
	Note that if the symbol is fully qualified, the var/Class to which it resolves need not
	be present in the namespace."}
ns-resolve [ns sym]
  (. clojure.lang.Compiler (resolveIn ns sym)))

(defn
	#^{:doc "same as (ns-resolve *ns* symbol)"}
resolve [sym]
  (ns-resolve *ns* sym))

(defn
	#^{:doc "Constructs an array-map."}
array-map
	([] (. clojure.lang.PersistentArrayMap EMPTY))
	([& keyvals] (new clojure.lang.PersistentArrayMap (to-array keyvals))))

(defn
	#^{:doc "Returns the nth rest of coll, (seq coll) when n is 0."}
nthrest [coll n]
  (loop [n n xs (seq coll)]
    (if (and xs (pos? n))
      (recur (dec n) (rest xs))
      xs)))

(defn
	#^{:doc "Return true if x is a String"}
string? [x]
  (instance? String x))

(defn
	#^{:doc "Return true if x is a Symbol"}
symbol? [x]
  (instance? clojure.lang.Symbol x))

(defn
	#^{:doc "Return true if x is a Keyword"}
keyword? [x]
  (instance? clojure.lang.Keyword x))

(defn
	#^{:doc "Return true if x implements IPersistentMap"}
map? [x]
  (instance? clojure.lang.IPersistentMap x))


(defn
	#^{:doc "Return true if x implements IPersistentVector "}
vector? [x]
  (instance? clojure.lang.IPersistentVector x))

;redefine let with destructuring
(defmacro
	#^{:doc "Evaluates the exprs in a lexical context in which the symbols in the binding-forms are bound to their
	respective init-exprs or parts therein."}
let [bindings & body]
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

(defmacro
	#^{:doc "Same as (when (seq xs) (let [x (first xs)] body))"}
when-first [x xs & body]
  `(when ~xs
     (let [~x (first ~xs)]
       ~@body)))

(defmacro
	#^{:doc "Expands to code which yields a lazy sequence of the concatenation of the supplied colls.
	Each coll expr is not evaluated until it is needed."}
lazy-cat
  ([coll] `(seq ~coll))
  ([coll & colls]
   `(let [iter# (fn iter# [coll#]
		    (if (seq coll#)
		      (lazy-cons (first coll#) (iter# (rest coll#)))
		      (lazy-cat ~@colls)))]
      (iter# ~coll))))
      
(defmacro
	#^{:doc "List comprehension. Takes one or more binding-form/collection-expr pairs, an optional filtering (where)
	expression, and yields a lazy sequence of evaluations of expr. Collections are iterated in a nested fashion,
	rightmost fastest, and nested coll-exprs can refer to bindings created in prior binding-forms."}
for
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
      (seq [] ((fn thisfn [pseq]
		  (when pseq
		    (lazy-cons (new clojure.lang.MapEntry (first pseq) (v (first pseq)))
			       (thisfn (rest pseq))))) (keys pmap))))))

(defmacro comment [& body])

(defmacro
	#^{:doc "Evaluates exprs in a context in which *out* is bound to a fresh StringWriter.
	Returns the string created by any nested printing calls."}
with-out-str [& body]
  `(let [s# (new java.io.StringWriter)]
    (binding [*out* s#]
      ~@body
      (str s#))))

(defn
	#^{:doc "pr to a string, returning it"}
pr-str [& xs]
  (with-out-str
    (apply pr xs)))

(defn
	#^{:doc "prn to a string, returning it"}
prn-str [& xs]
  (with-out-str
    (apply prn xs)))

(defn
	#^{:doc "print to a string, returning it"}
print-str [& xs]
  (with-out-str
    (apply print xs)))

(defn
	#^{:doc "println to a string, returning it"}
println-str [& xs]
  (with-out-str
    (apply println xs)))

(defmacro
	#^{:doc "Evaluates expr and throws an exception if it does not evaluate to logical true."}
assert [x]
  `(when-not ~x
     (throw (new Exception (str "Assert failed: " (pr-str '~x))))))

(defn
	#^{:doc "test [v] finds fn at key :test in var metadata and calls it, presuming failure will throw exception"}
test [v]
  (let [f (:test ^v)]
    (if f
      (do (f) :ok)
      :no-test)))

(defn
	#^{:tag java.util.regex.Pattern
	   :doc "Returns an instance of java.util.regex.Pattern, for use, e.g. in re-matcher."}
re-pattern [s]
  (. java.util.regex.Pattern (compile s)))
  
(defn
	#^{:tag java.util.regex.Matcher
	   :doc "Returns an instance of java.util.regex.Matcher, for use, e.g. in re-find."}
re-matcher [#^java.util.regex.Pattern re s]
  (. re (matcher s)))

(defn
	#^{:doc "Returns the groups from the most recent match/find. If there are no nested groups,
	returns a string of the entire match. If there are nested groups, returns a vector of the groups,
	the first element being the entire match."}
re-groups [#^java.util.regex.Matcher m]
  (let [gc  (. m (groupCount))]
    (if (zero? gc)
      (. m (group))
      (loop [ret [] c 0]
	(if (<= c gc)
	  (recur (conj ret (. m (group c))) (inc c))
	  ret)))))

(defn
	#^{:doc "Returns a lazy sequence of successive matches of pattern in string, using java.util.regex.Matcher.find(),
	each such match processed with re-groups."}
re-seq [#^java.util.regex.Pattern re s]
  (let [m (re-matcher re s)]
    ((fn step []
	(when (. m (find))
	  (lazy-cons (re-groups m) (step)))))))

(defn
	#^{:doc "Returns the match, if any, of string to pattern, using java.util.regex.Matcher.matches().
	Uses re-groups to return the groups."}
re-matches [#^java.util.regex.Pattern re s]
  (let [m (re-matcher re s)]
    (when (. m (matches))
      (re-groups m))))


(defn
	#^{:doc "Returns the next regex match, if any, of string to pattern, using java.util.regex.Matcher.find().
	Uses re-groups to return the groups."}
re-find
  ([#^java.util.regex.Matcher m]
   (when (. m (find))
     (re-groups m)))
  ([#^java.util.regex.Pattern re s]
   (let [m (re-matcher re s)]
     (re-find m))))

(defn rand
  ([] (. Math (random)))
  ([n] (* n (rand))))

(defn rand-int [n]
	(int (rand n)))

(defmacro defn- [name & decls]
  (list* `defn (with-meta name (assoc (meta name) :private true)) decls))

(defn print-doc [v]
  (println "-------------------------")
  (println (str (ns-name (:ns ^v)) "/" (:name ^v)))
  (prn (:arglists ^v))
  (println "\t\t" (:doc ^v)))

(defn find-doc [re-string]
  (let [re  (re-pattern re-string)]
    (dorun (for [ns (all-ns) v (vals (ns-interns ns))]
               (and (:doc ^v)
                    (or (re-find (re-matcher re (:doc ^v)))
                        (re-find (re-matcher re (str (:name ^v))))))
              (print-doc v)))))

(defmacro doc [varname]
  `(print-doc (var ~varname)))
