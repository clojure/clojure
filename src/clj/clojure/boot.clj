;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure)

(def
 #^{:arglists '([& items])
    :doc "Creates a new list containing the items."}
  list (. clojure.lang.PersistentList creator))

(def
 #^{:arglists '([x seq])
    :doc "Returns a new seq where x is the first element and seq is
    the rest."}

 cons (fn* cons [x seq] (. clojure.lang.RT (cons x seq))))

;during bootstrap we don't have destructuring let, loop or fn, will redefine later
(def
 #^{:macro true}
  let (fn* let [& decl] (cons 'let* decl)))

(def
 #^{:macro true}
 loop (fn* loop [& decl] (cons 'loop* decl)))

(def
 #^{:macro true}
 fn (fn* fn [& decl] (cons 'fn* decl)))

(def
 #^{:arglists '([coll])
    :doc "Returns the first item in the collection. Calls seq on its
    argument. If coll is nil, returns nil."}
 first (fn first [coll] (. clojure.lang.RT (first coll))))

(def
 #^{:arglists '([coll])
    :doc "Returns a seq of the items after the first. Calls seq on its
  argument.  If there are no more items, returns nil."}  
 rest (fn rest [x] (. clojure.lang.RT (rest x))))

(def
 #^{:arglists '([coll x] [coll x & xs])
    :doc "conj[oin]. Returns a new collection with the xs
    'added'. (conj nil item) returns (item).  The 'addition' may
    happen at different 'places' depending on the concrete type."}
 conj (fn conj 
        ([coll x] (. clojure.lang.RT (conj coll x)))
        ([coll x & xs]
         (if xs
           (recur (conj coll x) (first xs) (rest xs))
           (conj coll x)))))

(def
 #^{:doc "Same as (first (rest x))"
    :arglists '([x])}
 second (fn second [x] (first (rest x))))

(def
 #^{:doc "Same as (first (first x))"
    :arglists '([x])}
 ffirst (fn ffirst [x] (first (first x))))

(def
 #^{:doc "Same as (rest (first x))"
    :arglists '([x])}
 rfirst (fn rfirst [x] (rest (first x))))

(def
 #^{:doc "Same as (first (rest x))"
    :arglists '([x])}
 frest (fn frest [x] (first (rest x))))

(def
 #^{:doc "Same as (rest (rest x))"
    :arglists '([x])}
 rrest (fn rrest [x] (rest (rest x))))

(def
 #^{:arglists '([coll])
    :doc "Sequence. Returns a new ISeq on the collection. If the
    collection is empty, returns nil.  (seq nil) returns nil. seq also
    works on Strings, native Java arrays (of reference types) and any
    objects that implement Iterable."
    :tag clojure.lang.ISeq}
 seq (fn seq [coll] (. clojure.lang.RT (seq coll))))

(def
 #^{:arglists '([#^Class c x])
    :doc "Evaluates x and tests if it is an instance of the class
    c. Returns true or false"}
 instance? (fn instance? [#^Class c x] (. c (isInstance x))))

(def
 #^{:arglists '([x])
    :doc "Return true if x implements ISeq"}
 seq? (fn seq? [x] (instance? clojure.lang.ISeq x)))

(def
 #^{:arglists '([x])
    :doc "Return true if x is a String"}
 string? (fn string? [x] (instance? String x)))

(def
 #^{:arglists '([x])
    :doc "Return true if x implements IPersistentMap"}
 map? (fn map? [x] (instance? clojure.lang.IPersistentMap x)))

(def
 #^{:arglists '([x])
    :doc "Return true if x implements IPersistentVector "}
 vector? (fn vector? [x] (instance? clojure.lang.IPersistentVector x)))

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
    :doc "assoc[iate]. When applied to a map, returns a new map of the
    same (hashed/sorted) type, that contains the mapping of key(s) to
    val(s). When applied to a vector, returns a new vector that
    contains val at index. Note - index must be <= (count vector)."}
 assoc
 (fn assoc
   ([map key val] (. clojure.lang.RT (assoc map key val)))
   ([map key val & kvs]
    (let [ret (assoc map key val)]
      (if kvs
        (recur ret (first kvs) (second kvs) (rrest kvs))
        ret)))))

;;;;;;;;;;;;;;;;; metadata ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def
 #^{:arglists '([obj])
    :doc "Returns the metadata of obj, returns nil if there is no metadata."}
 meta (fn meta [x]
        (if (instance? clojure.lang.IObj x)
          (. #^clojure.lang.IObj x (meta)))))

(def
 #^{:arglists '([#^clojure.lang.IObj obj m])
    :doc "Returns an object of the same type and value as obj, with
    map m as its metadata."}
 with-meta (fn with-meta [#^clojure.lang.IObj x m]
             (. x (withMeta m))))

(def 
 #^{:arglists '([coll])
    :doc "Return the last item in coll, in linear time"}
 last (fn last [s]
        (if (rest s)
          (recur (rest s))
          (first s))))

(def 
 #^{:arglists '([coll])
    :doc "Return a sequence of all but the last item in coll, in linear time"}
 butlast (fn butlast [s]
           (loop [ret [] s s]
             (if (rest s)
               (recur (conj ret (first s)) (rest s))
               (seq ret)))))

(def 

 #^{:doc "Same as (def name (fn [params* ] exprs*)) or (def
    name (fn ([params* ] exprs*)+)) with any doc-string or attrs added
    to the var metadata"
    :arglists '([name doc-string? attr-map? [params*] body]
                [name doc-string? attr-map? ([params*] body)+ attr-map?])}
 defn (fn defn [name & fdecl]
        (let [m (if (string? (first fdecl))
                  {:doc (first fdecl)}
                  {})
              fdecl (if (string? (first fdecl))
                      (rest fdecl)
                      fdecl)
              m (if (map? (first fdecl))
                  (conj m (first fdecl))
                  m)
              fdecl (if (map? (first fdecl))
                      (rest fdecl)
                      fdecl)
              fdecl (if (vector? (first fdecl))
                      (list fdecl)
                      fdecl)
              m (if (map? (last fdecl))
                  (conj m (last fdecl))
                  m)
              fdecl (if (map? (last fdecl))
                      (butlast fdecl)
                      fdecl)
              m (conj {:arglists (list 'quote (sigs fdecl))} m)]
          (list 'def (with-meta name (conj (if (meta name) (meta name) {}) m))
                (cons `fn fdecl)))))

(. (var defn) (setMacro))

(defn cast
  "Throws a ClassCastException if x is not a c, else returns x."
  [#^Class c x] 
  (. c (cast x)))

(defn to-array
  "Returns an array of Objects containing the contents of coll, which
  can be any Collection.  Maps to java.util.Collection.toArray()."
  [coll] (. clojure.lang.RT (toArray coll)))
 
(defn vector
  "Creates a new vector containing the args."
  ([] [])
  ([& args]
   (. clojure.lang.LazilyPersistentVector (create args))))

(defn vec
  "Creates a new vector containing the contents of coll."
  ([coll]
   (. clojure.lang.LazilyPersistentVector (createOwning (to-array coll)))))

(defn hash-map
  "keyval => key val
  Returns a new hash map with supplied mappings."
  ([] {})
  ([& keyvals]
   (. clojure.lang.PersistentHashMap (create keyvals))))

(defn hash-set
  "Returns a new hash set with supplied keys."
  ([] #{})
  ([& keys]
   (. clojure.lang.PersistentHashSet (create keys))))

(defn sorted-map
  "keyval => key val
  Returns a new sorted map with supplied mappings."
  ([& keyvals]
   (. clojure.lang.PersistentTreeMap (create keyvals))))

(defn sorted-set
  "Returns a new sorted set with supplied keys."
  ([& keys]
   (. clojure.lang.PersistentTreeSet (create keys))))

(defn sorted-map-by
  "keyval => key val
  Returns a new sorted map with supplied mappings, using the supplied comparator."
  ([comparator & keyvals]
   (. clojure.lang.PersistentTreeMap (create comparator keyvals))))
 
;;;;;;;;;;;;;;;;;;;;
(def

 #^{:doc "Like defn, but the resulting function name is declared as a
  macro and will be used as a macro by the compiler when it is
  called."
    :arglists '([name doc-string? attr-map? [params*] body]
                [name doc-string? attr-map? ([params*] body)+ attr-map?])}
 defmacro (fn [name & args]
            (list 'do
                  (cons `defn (cons name args))
                  (list '. (list 'var name) '(setMacro)))))

(. (var defmacro) (setMacro))

(defmacro when
  "Evaluates test. If logical true, evaluates body in an implicit do."
  [test & body]
  (list 'if test (cons 'do body)))

(defmacro when-not
  "Evaluates test. If logical false, evaluates body in an implicit do."
  [test & body]
    (list 'if test nil (cons 'do body)))

(defn nil?
  "Returns true if x is nil, false otherwise."
  {:tag Boolean}
  [x] (identical? x nil))

(defn false?
  "Returns true if x is the value false, false otherwise."
  {:tag Boolean}
  [x] (identical? x false))

(defn true?
  "Returns true if x is the value true, false otherwise."
  {:tag Boolean}
  [x] (identical? x true))

(defn not
  "Returns true if x is logical false, false otherwise."
  {:tag Boolean}
  [x] (if x false true))

(defn str
  "With no args, returns the empty string. With one arg x, returns
  x.toString().  (str nil) returns the empty string. With more than
  one arg, returns the concatenation of the str values of the args."
  {:tag String}
  ([] "")
  ([#^Object x]
   (if (nil? x) "" (. x (toString))))
  ([x & ys]
   (loop [sb (new StringBuilder #^String (str x)) more ys]
     (if more
       (recur (. sb  (append (str (first more)))) (rest more))
       (str sb)))))

(defn symbol
  "Returns a Symbol with the given namespace and name."
  ([name] (. clojure.lang.Symbol (intern name)))
  ([ns name] (. clojure.lang.Symbol (intern ns name))))

(defn keyword
  "Returns a Keyword with the given namespace and name.  Do not use :
  in the keyword strings, it will be added automatically."
  ([name] (. clojure.lang.Keyword (intern nil name)))
  ([ns name] (. clojure.lang.Keyword (intern ns name))))

(defn gensym
  "Returns a new symbol with a unique name. If a prefix string is
  supplied, the name is prefix# where # is some unique number. If
  prefix is not supplied, the prefix is 'G'."
  ([] (gensym "G__"))
  ([prefix-string] (. clojure.lang.Symbol (intern (str prefix-string (str (. clojure.lang.RT (nextID))))))))

(defmacro cond
  "Takes a set of test/expr pairs. It evaluates each test one at a
  time.  If a test returns logical true, cond evaluates and returns
  the value of the corresponding expr and doesn't evaluate any of the
  other tests or exprs. (cond) returns nil."
  [& clauses]
    (when clauses
      (list 'if (first clauses)
            (second clauses)
            (cons 'cond (rest (rest clauses))))))

(defn spread
  {:private true}
  [arglist]
  (cond
   (nil? arglist) nil
   (nil? (rest arglist)) (seq (first arglist))
   :else (cons (first arglist) (spread (rest arglist)))))

(defn apply
  "Applies fn f to the argument list formed by prepending args to argseq."
  {:arglists '([f args* argseq])}
  [#^clojure.lang.IFn f & args]
    (. f (applyTo (spread args))))

(defn list*
  "Creates a new list containing the item prepended to more."
  [item & more]
    (spread (cons item more)))

(defmacro delay
  "Takes a body of expressions and yields a Delay object than will
  invoke the body only the first time it is forced (with force), and
  will cache the result and return it on all subsequent force calls"
  [& body] 
    (list 'new 'clojure.lang.Delay (list* `fn [] body)))

(defn delay? 
  "returns true if x is a Delay created with delay"
  [x] (instance? clojure.lang.Delay x))

(defn force
  "If x is a Delay, returns the (possibly cached) value of its expression, else returns x"
  [x] (. clojure.lang.Delay (force x)))

(defn fnseq
  "Returns a seq object whose first is first and whose rest is the
  value produced by calling restfn with no arguments. restfn will be
  called at most once per step in the sequence, e.g. calling rest
  repeatedly on the head of the seq calls restfn once - the value it
  yields is cached."
  [first restfn]
    (new clojure.lang.FnSeq first restfn))

(defmacro lazy-cons
  "Expands to code which produces a seq object whose first is
  first-expr and whose rest is rest-expr, neither of which is
  evaluated until first/rest is called. Each expr will be evaluated at most
  once per step in the sequence, e.g. calling first/rest repeatedly on the
  same node of the seq evaluates first/rest-expr once - the values they yield are
  cached."
 [first-expr & rest-expr]
 (list 'new 'clojure.lang.LazyCons (list `fn (list [] first-expr) (list* [(gensym)] rest-expr))))

;(defmacro lazy-seq
;  "Expands to code which produces a seq object whose first is the
;  value of first-expr and whose rest is the value of rest-expr,
;  neither of which is evaluated until first/rest is called. Each expr
;  will be evaluated every step in the sequence, e.g. calling
;  first/rest repeatedly on the same node of the seq evaluates
;  first/rest-expr repeatedly - the values they yield are not cached."
; [first-expr rest-expr]
;  (list 'new 'clojure.lang.LazySeq (list `fn (list [] first-expr) (list [(gensym)] rest-expr))))
 
(defn cache-seq
  "Given a seq s, returns a lazy seq that will touch each element of s
  at most once, caching the results."
  [s] (when s (clojure.lang.CachedSeq. s)))

(defn concat
  "Returns a lazy seq representing the concatenation of	the elements in the supplied colls."
  ([] nil)
  ([x] (seq x))
  ([x y] 
     (if (seq x) 
       (lazy-cons (first x) (concat (rest x) y))
       (seq y)))
  ([x y & zs]
     (let [cat (fn cat [xys zs]
                   (if (seq xys)
                     (lazy-cons (first xys) (cat (rest xys) zs))
                     (when zs
                       (recur (first zs) (rest zs)))))]
       (cat (concat x y) zs))))
 
;;;;;;;;;;;;;;;;at this point all the support for syntax-quote exists;;;;;;;;;;;;;;;;;;;;;;
(defn =
  "Equality. Returns true if x equals y, false if not. Same as
  Java x.equals(y) except it also works for nil, and compares
  numbers in a type-independent manner.  Clojure's immutable data
  structures define equals() (and thus =) as a value, not an identity,
  comparison."
  {:tag Boolean
   :inline (fn [x y] `(. clojure.lang.Util equal ~x ~y))
   :inline-arities #{2}} 
  ([x] true)
  ([x y] (. clojure.lang.Util (equal x y)))
  ([x y & more]
   (if (= x y)
     (if (rest more)
       (recur y (first more) (rest more))
       (= y (first more)))
     false)))

(defn not=
  "Same as (not (= obj1 obj2))"
  {:tag Boolean}
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more]
   (not (apply = x y more))))



(defn compare
  "Comparator. Returns 0 if x equals y, -1 if x is logically 'less
  than' y, else 1. Same as Java x.compareTo(y) except it also works
  for nil, and compares numbers in a type-independent manner. x must
  implement Comparable"
  {:tag Integer
   :inline (fn [x y] `(. clojure.lang.Util compare ~x ~y))} 
  [x y] (. clojure.lang.Util (compare x y)))

(defmacro and
  "Evaluates exprs one at a time, from left to right. If a form
  returns logical false (nil or false), and returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expr. (and) returns true."
  ([] true)
  ([x] x)
  ([x & rest]
   `(let [and# ~x]
      (if and# (and ~@rest) and#))))

(defmacro or
  "Evaluates exprs one at a time, from left to right. If a form
  returns a logical true value, or returns that value and doesn't
  evaluate any of the other expressions, otherwise it returns the
  value of the last expression. (or) returns nil."
  ([] nil)
  ([x] x)
  ([x & rest]
      `(let [or# ~x]
         (if or# or# (or ~@rest)))))

;;;;;;;;;;;;;;;;;;; sequence fns  ;;;;;;;;;;;;;;;;;;;;;;;
(defn reduce
  "f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to the first 2 items in coll, then
  applying f to that result and the 3rd item, etc. If coll contains no
  items, f must accept no arguments as well, and reduce returns the
  result of calling f with no arguments.  If coll has only 1 item, it
  is returned and f is not called.  If val is supplied, returns the
  result of applying f to val and the first item in coll, then
  applying f to that result and the 2nd item, etc. If coll contains no
  items, returns val and f is not called."
  ([f coll]
   (let [s (seq coll)]
     (if s
       (if (instance? clojure.lang.IReduce s)
         (. #^clojure.lang.IReduce s (reduce f))
         (reduce f (first s) (rest s)))
       (f))))
  ([f val coll]
     (let [s (seq coll)]
       (if (instance? clojure.lang.IReduce s)
         (. #^clojure.lang.IReduce s (reduce f val))
         ((fn [f val s]
            (if s
              (recur f (f val (first s)) (rest s))
              val)) 
          f val s)))))

(defn reverse
  "Returns a seq of the items in coll in reverse order. Not lazy."
  [coll]
    (reduce conj nil coll))
  
;;math stuff
(defn +
  "Returns the sum of nums. (+) returns 0."
  {:inline (fn [x y] `(. clojure.lang.Numbers (add ~x ~y)))
   :inline-arities #{2}}
  ([] 0)
  ([x] (cast Number x))
  ([x y] (. clojure.lang.Numbers (add x y)))
  ([x y & more]
   (reduce + (+ x y) more)))

(defn *
  "Returns the product of nums. (*) returns 1."
  {:inline (fn [x y] `(. clojure.lang.Numbers (multiply ~x ~y)))
   :inline-arities #{2}}
  ([] 1)
  ([x] (cast Number x))
  ([x y] (. clojure.lang.Numbers (multiply x y)))
  ([x y & more]
   (reduce * (* x y) more)))

(defn /
  "If no denominators are supplied, returns 1/numerator,
  else returns numerator divided by all of the denominators."
  {:inline (fn [x y] `(. clojure.lang.Numbers (divide ~x ~y)))
   :inline-arities #{2}}
  ([x] (/ 1 x))
  ([x y] (. clojure.lang.Numbers (divide x y)))
  ([x y & more]
   (reduce / (/ x y) more)))

(defn -
  "If no ys are supplied, returns the negation of x, else subtracts
  the ys from x and returns the result."
  {:inline (fn [& args] `(. clojure.lang.Numbers (minus ~@args)))
   :inline-arities #{1 2}}
  ([x] (. clojure.lang.Numbers (minus x)))
  ([x y] (. clojure.lang.Numbers (minus x y)))
  ([x y & more]
   (reduce - (- x y) more)))

(defn <
  "Returns non-nil if nums are in monotonically increasing order,
  otherwise false."
  {:inline (fn [x y] `(. clojure.lang.Numbers (lt ~x ~y)))
   :inline-arities #{2}}
  ([x] true)
  ([x y] (. clojure.lang.Numbers (lt x y)))
  ([x y & more]
   (if (< x y)
     (if (rest more)
       (recur y (first more) (rest more))
       (< y (first more)))
     false)))

(defn <=
  "Returns non-nil if nums are in monotonically non-decreasing order,
  otherwise false."
  {:inline (fn [x y] `(. clojure.lang.Numbers (lte ~x ~y)))
   :inline-arities #{2}}
  ([x] true)
  ([x y] (. clojure.lang.Numbers (lte x y)))
  ([x y & more]
   (if (<= x y)
     (if (rest more)
       (recur y (first more) (rest more))
       (<= y (first more)))
     false)))

(defn >
  "Returns non-nil if nums are in monotonically decreasing order,
  otherwise false."
  {:inline (fn [x y] `(. clojure.lang.Numbers (gt ~x ~y)))
   :inline-arities #{2}}
  ([x] true)
  ([x y] (. clojure.lang.Numbers (gt x y)))
  ([x y & more]
   (if (> x y)
     (if (rest more)
       (recur y (first more) (rest more))
       (> y (first more)))
     false)))

(defn >=
  "Returns non-nil if nums are in monotonically non-increasing order,
  otherwise false."
  {:inline (fn [x y] `(. clojure.lang.Numbers (gte ~x ~y)))
   :inline-arities #{2}}
  ([x] true)
  ([x y] (. clojure.lang.Numbers (gte x y)))
  ([x y & more]
   (if (>= x y)
     (if (rest more)
       (recur y (first more) (rest more))
       (>= y (first more)))
     false)))

(defn ==
  "Returns non-nil if nums all have the same value, otherwise false"
  {:inline (fn [x y] `(. clojure.lang.Numbers (equiv ~x ~y)))
   :inline-arities #{2}}
  ([x] true)
  ([x y] (. clojure.lang.Numbers (equiv x y)))
  ([x y & more]
   (if (== x y)
     (if (rest more)
       (recur y (first more) (rest more))
       (== y (first more)))
     false)))

(defn max
  "Returns the greatest of the nums."
  ([x] x)
  ([x y] (if (> x y) x y))
  ([x y & more]
   (reduce max (max x y) more)))

(defn min
  "Returns the least of the nums."
  ([x] x)
  ([x y] (if (< x y) x y))
  ([x y & more]
   (reduce min (min x y) more)))

(defn inc
  "Returns a number one greater than num."
  {:inline (fn [x] `(. clojure.lang.Numbers (inc ~x)))}
  [x] (. clojure.lang.Numbers (inc x)))

(defn dec
  "Returns a number one less than num."
  {:inline (fn [x] `(. clojure.lang.Numbers (dec ~x)))}
  [x] (. clojure.lang.Numbers (dec x)))

(defn unchecked-inc
  "Returns a number one greater than x, an int or long. 
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x] `(. clojure.lang.Numbers (unchecked_inc ~x)))}
  [x] (. clojure.lang.Numbers (unchecked_inc x)))

(defn unchecked-dec
  "Returns a number one less than x, an int or long. 
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x] `(. clojure.lang.Numbers (unchecked_dec ~x)))}
  [x] (. clojure.lang.Numbers (unchecked_dec x)))

(defn unchecked-negate
  "Returns the negation of x, an int or long. 
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x] `(. clojure.lang.Numbers (unchecked_negate ~x)))}
  [x] (. clojure.lang.Numbers (unchecked_negate x)))

(defn unchecked-add
  "Returns the sum of x and y, both int or long. 
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x y] `(. clojure.lang.Numbers (unchecked_add ~x ~y)))}
  [x y] (. clojure.lang.Numbers (unchecked_add x y)))

(defn unchecked-subtract
  "Returns the difference of x and y, both int or long. 
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x y] `(. clojure.lang.Numbers (unchecked_subtract ~x ~y)))}
  [x y] (. clojure.lang.Numbers (unchecked_subtract x y)))

(defn unchecked-multiply
  "Returns the product of x and y, both int or long. 
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x y] `(. clojure.lang.Numbers (unchecked_multiply ~x ~y)))}
  [x y] (. clojure.lang.Numbers (unchecked_multiply x y)))

(defn unchecked-divide
  "Returns the division of x by y, both int or long. 
  Note - uses a primitive operator subject to truncation."
  {:inline (fn [x y] `(. clojure.lang.Numbers (unchecked_divide ~x ~y)))}
  [x y] (. clojure.lang.Numbers (unchecked_divide x y)))

(defn pos?
  "Returns true if num is greater than zero, else false"
  {:tag Boolean
   :inline (fn [x] `(. clojure.lang.Numbers (isPos ~x)))}
  [x] (. clojure.lang.Numbers (isPos x)))

(defn neg?
  "Returns true if num is less than zero, else false"
  {:tag Boolean
   :inline (fn [x] `(. clojure.lang.Numbers (isNeg ~x)))}
  [x] (. clojure.lang.Numbers (isNeg x)))

(defn zero?
  "Returns true if num is zero, else false"
  {:tag Boolean
   :inline (fn [x] `(. clojure.lang.Numbers (isZero ~x)))}
  [x] (. clojure.lang.Numbers (isZero x)))

(defn quot
  "quot[ient] of dividing numerator by denominator."
  [num div]
    (. clojure.lang.Numbers (quotient num div)))

(defn rem
  "rem[ainder] of dividing numerator by denominator."
  [num div]
    (. clojure.lang.Numbers (remainder num div)))

(defn rationalize
  "returns the rational value of num"
  [num]
  (. clojure.lang.Numbers (rationalize num)))

;;Bit ops

(defn bit-not
  "Bitwise complement"
  [x] (. clojure.lang.Numbers not x))


(defn bit-and
  "Bitwise and"
  [x y] (. clojure.lang.Numbers and x y))

(defn bit-or
  "Bitwise or"
  [x y] (. clojure.lang.Numbers or x y))

(defn bit-xor
  "Bitwise exclusive or"
  [x y] (. clojure.lang.Numbers xor x y))

(defn bit-and-not
  "Bitwise and with complement"
  [x y] (. clojure.lang.Numbers andNot x y))


(defn bit-clear
  "Clear bit at index n"
  [x n] (. clojure.lang.Numbers clearBit x n))

(defn bit-set
  "Set bit at index n"
  [x n] (. clojure.lang.Numbers setBit x n))

(defn bit-flip
  "Flip bit at index n"
  [x n] (. clojure.lang.Numbers flipBit x n))

(defn bit-test
  "Test bit at index n"
  [x n] (. clojure.lang.Numbers testBit x n))


(defn bit-shift-left
  "Bitwise shift left"
  [x n] (. clojure.lang.Numbers shiftLeft x n))

(defn bit-shift-right
  "Bitwise shift right"
  [x n] (. clojure.lang.Numbers shiftRight x n))

;;

(defn complement
  "Takes a fn f and returns a fn that takes the same arguments as f,
  has the same effects, if any, and returns the opposite truth value."  
  [f] (fn [& args]
        (not (apply f args))))

(defn constantly
  "Returns a function that takes any number of arguments and returns x."
  [x] (fn [& args] x))

(defn identity
  "Returns its argument."
  [x] x)

;;Collection stuff



(defn count
  "Returns the number of items in the collection. (count nil) returns
  0.  Also works on strings, arrays, and Java Collections and Maps"
  [coll] (. clojure.lang.RT (count coll)))

;;list stuff
(defn peek
  "For a list or queue, same as first, for a vector, same as, but much
  more efficient than, last. If the collection is empty, returns nil."
  [coll] (. clojure.lang.RT (peek coll)))

(defn pop
  "For a list or queue, returns a new list/queue without the first
  item, for a vector, returns a new vector without the last item. If
  the collection is empty, throws an exception.  Note - not the same
  as rest/butlast."
  [coll] (. clojure.lang.RT (pop coll)))

(defn nth
  "Returns the value at the index. get returns nil if index out of
  bounds, nth throws an exception unless not-found is supplied.  nth
  also works for strings, Java arrays, regex Matchers and Lists, and,
  in O(n) time, for sequences."
  ([coll index] (. clojure.lang.RT (nth coll index)))
  ([coll index not-found] (. clojure.lang.RT (nth coll index not-found))))

;;map stuff

(defn contains?
  "Returns true if key is present, else false."
  [map key] (. clojure.lang.RT (contains map key)))

(defn get
  "Returns the value mapped to key, not-found or nil if key not present."
  ([map key]
   (. clojure.lang.RT (get map key)))
  ([map key not-found]
   (. clojure.lang.RT (get map key not-found))))

(defn dissoc
  "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,
  that does not contain a mapping for key(s)."
  ([map] map)
  ([map key]
   (. clojure.lang.RT (dissoc map key)))
  ([map key & ks]
   (let [ret (dissoc map key)]
     (if ks
       (recur ret (first ks) (rest ks))
       ret))))

(defn disj
  "disj[oin]. Returns a new set of the same (hashed/sorted) type, that
  does not contain key(s)."
  ([set] set)
  ([#^clojure.lang.IPersistentSet set key]
   (. set (disjoin key)))
  ([set key & ks]
   (let [ret (disj set key)]
     (if ks
       (recur ret (first ks) (rest ks))
       ret))))

(defn find
  "Returns the map entry for key, or nil if key not present."
  [map key] (. clojure.lang.RT (find map key)))

(defn select-keys
  "Returns a map containing only those entries in map whose key is in keys"
  [map keyseq]
    (loop [ret {} keys (seq keyseq)]
      (if keys
        (let [entry (. clojure.lang.RT (find map (first keys)))]
          (recur
           (if entry
             (conj ret entry)
             ret)
           (rest keys)))
        ret)))

(defn keys
  "Returns a sequence of the map's keys."
  [map] (. clojure.lang.RT (keys map)))

(defn vals
  "Returns a sequence of the map's values."
  [map] (. clojure.lang.RT (vals map)))

(defn key
  "Returns the key of the map entry."
  [#^java.util.Map$Entry e]
    (. e (getKey)))

(defn val
  "Returns the value in the map entry."
  [#^java.util.Map$Entry e]
    (. e (getValue)))

(defn rseq
  "Returns, in constant time, a sequence of the items in rev (which
  can be a vector or sorted-map), in reverse order."
  [#^clojure.lang.Reversible rev]
    (. rev (rseq)))

(defn name
  "Returns the name String of a symbol or keyword."
  [#^clojure.lang.Named x]
    (. x (getName)))

(defn namespace
  "Returns the namespace String of a symbol or keyword, or nil if not present."
  [#^clojure.lang.Named x]
    (. x (getNamespace)))

(defmacro locking
  "Executes exprs in an implicit do, while holding the monitor of x.
  Will release the monitor of x in all circumstances."
  [x & body]
  `(let [lockee# ~x]
     (try
      (monitor-enter lockee#)
      ~@body
      (finally
       (monitor-exit lockee#)))))

(defmacro ..
  "form => fieldName-symbol or (instanceMethodName-symbol args*)

  Expands into a member access (.) of the first member on the first
  argument, followed by the next member on the result, etc. For
  instance:

  (.. System (getProperties) (get \"os.name\"))

  expands to:

  (. (. System (getProperties)) (get \"os.name\"))

  but is easier to write, read, and understand."
  ([x form] `(. ~x ~form))
  ([x form & more] `(.. (. ~x ~form) ~@more)))

(defmacro ->
  "Macro. Threads the expr through the forms. Inserts x as the
  second item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  second item in second form, etc."
  ([x form] (if (seq? form)
              `(~(first form) ~x ~@(rest form))
              (list form x)))
  ([x form & more] `(-> (-> ~x ~form) ~@more)))

;;multimethods
(defmacro defmulti
  "Creates a new multimethod with the associated dispatch function. If
  default-dispatch-val is supplied it becomes the default dispatch
  value of the multimethod, otherwise the default dispatch value
  is :default."
  ([name dispatch-fn] `(defmulti ~name ~dispatch-fn :default))
  ([name dispatch-fn default-val]
   `(def ~(with-meta name {:tag 'clojure.lang.MultiFn}) (new clojure.lang.MultiFn ~dispatch-fn ~default-val))))

(defmacro defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value. "
  [multifn dispatch-val & fn-tail]
  `(. ~multifn addMethod ~dispatch-val (fn ~@fn-tail)))

(defmacro remove-method
  "Removes the method of multimethod associated	with dispatch-value."
 [multifn dispatch-val]
 `(. ~multifn removeMethod ~dispatch-val))

(defmacro prefer-method
  "Causes the multimethod to prefer matches of dispatch-val-x over dispatch-val-y when there is a conflict"
  [multifn dispatch-val-x dispatch-val-y]
  `(. ~multifn preferMethod ~dispatch-val-x ~dispatch-val-y))

;;;;;;;;; var stuff      

(defmacro binding
  "binding => var-symbol init-expr 

  Creates new bindings for the (already-existing) vars, with the
  supplied initial values, executes the exprs in an implicit do, then
  re-establishes the bindings that existed before."  
  [bindings & body]
    (let [var-ize (fn [var-vals]
                    (loop [ret [] vvs (seq var-vals)]
                      (if vvs
                        (recur  (conj (conj ret `(var ~(first vvs))) (second vvs))
                                (rest (rest vvs)))
                        (seq ret))))]
      `(do
         (. clojure.lang.Var (pushThreadBindings (hash-map ~@(var-ize bindings))))
         (try
          ~@body
          (finally
           (. clojure.lang.Var (popThreadBindings)))))))

(defn find-var
  "Returns the global var named by the namespace-qualified symbol, or
  nil if no var with that name."
 [sym] (. clojure.lang.Var (find sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Refs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn agent
  "Creates and returns an agent with an initial value of state and an
  optional validate fn. validate-fn must be nil or a side-effect-free fn of
  one argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  throw an exception."
  ([state] (new clojure.lang.Agent state))
  ([state validate-fn] (new clojure.lang.Agent state validate-fn)))

(defn ! [& args] (throw (new Exception "! is now send. See also send-off")))

(defn send
  "Dispatch an action to an agent. Returns the agent immediately.
  Subsequently, in a thread from a thread pool, the state of the agent
  will be set to the value of:

  (apply action-fn state-of-agent args)"
  [#^clojure.lang.Agent a f & args]
    (. a (dispatch f args false)))

(defn send-off
  "Dispatch a potentially blocking action to an agent. Returns the
  agent immediately. Subsequently, in a separate thread, the state of
  the agent will be set to the value of:

  (apply action-fn state-of-agent args)"
  [#^clojure.lang.Agent a f & args]
    (. a (dispatch f args true)))

(defn agent-errors
  "Returns a sequence of the exceptions thrown during asynchronous
  actions of the agent."  
  [#^clojure.lang.Agent a] (. a (getErrors)))

(defn clear-agent-errors
  "Clears any exceptions thrown during asynchronous actions of the
  agent, allowing subsequent actions to occur."  
  [#^clojure.lang.Agent a] (. a (clearErrors)))

(defn shutdown-agents
  "Initiates a shutdown of the thread pools that back the agent
  system. Running actions will complete, but no new actions will be
  accepted"
  [] (. clojure.lang.Agent shutdown))

(defn ref
  "Creates and returns a Ref with an initial value of x and an optional validate fn.
  validate-fn must be nil or a side-effect-free fn of one argument, which will
  be passed the intended new state on any state change. If the new
  state is unacceptable, the validate-fn should throw an
  exception. validate-fn will be called on transaction commit, when
  all refs have their final values."  
  ([x] (new clojure.lang.Ref x))
  ([x validate-fn] (new clojure.lang.Ref x validate-fn)))

(defn deref
  "Also reader macro: @ref/@agent Within a transaction, returns the
  in-transaction-value of ref, else returns the
  most-recently-committed value of ref. When applied to an agent,
  returns its current state."
 [#^clojure.lang.IRef ref] (. ref (get)))

(defn set-validator
  "Sets the validator-fn for a var/ref/agent. validator-fn must be nil or a
  side-effect-free fn of one argument, which will be passed the intended
  new state on any state change. If the new state is unacceptable, the
  validator-fn should throw an exception. If the current state (root
  value if var) is not acceptable to the new validator, an exception
  will be thrown and the validator will not be changed." 
  [#^clojure.lang.IRef iref validator-fn] (. iref (setValidator validator-fn)))

(defn get-validator
  "Gets the validator-fn for a var/ref/agent."
 [#^clojure.lang.IRef iref] (. iref (getValidator)))

(defn commute
  "Must be called in a transaction. Sets the in-transaction-value of
  ref to:

  (apply fun in-transaction-value-of-ref args)

  and returns the in-transaction-value of ref.

  At the commit point of the transaction, sets the value of ref to be:

  (apply fun most-recently-committed-value-of-ref args)

  Thus fun should be commutative, or, failing that, you must accept
  last-one-in-wins behavior.  commute allows for more concurrency than
  ref-set."  

  [#^clojure.lang.Ref ref fun & args]
    (. ref (commute fun args)))

(defn alter
  "Must be called in a transaction. Sets the in-transaction-value of
  ref to:

  (apply fun in-transaction-value-of-ref args)

  and returns the in-transaction-value of ref."
  [#^clojure.lang.Ref ref fun & args]
    (. ref (alter fun args)))

(defn ref-set
  "Must be called in a transaction. Sets the value of ref.
  Returns val."  
  [#^clojure.lang.Ref ref val]
    (. ref (set val)))

(defn ensure
  "Must be called in a transaction. Protects the ref from modification
  by other transactions.  Returns the in-transaction-value of
  ref. Allows for more concurrency than (ref-set ref @ref)"
  [#^clojure.lang.Ref ref]
    (. ref (touch))
    (. ref (get)))

(defmacro sync
  "transaction-flags => TBD, pass nil for now

  Runs the exprs (in an implicit do) in a transaction that encompasses
  exprs and any nested calls.  Starts a transaction if none is already
  running on this thread. Any uncaught exception will abort the
  transaction and flow out of sync. The exprs may be run more than
  once, but any effects on Refs will be atomic."
  [flags-ignored-for-now & body]
  `(. clojure.lang.LockingTransaction
      (runInTransaction (fn [] ~@body))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fn stuff ;;;;;;;;;;;;;;;;


(defn comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc."
  [& fs]
    (let [fs (reverse fs)]
      (fn [& args]
        (loop [ret (apply (first fs) args) fs (rest fs)]
          (if fs
            (recur ((first fs) ret) (rest fs))
            ret)))))

(defn partial
  "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args. When
  called, the returned function calls f with args + additional args."
  ([f arg1]
   (fn [& args] (apply f arg1 args)))
  ([f arg1 arg2]
   (fn [& args] (apply f arg1 arg2 args)))
  ([f arg1 arg2 arg3]
   (fn [& args] (apply f arg1 arg2 arg3 args)))
  ([f arg1 arg2 arg3 & more]
   (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))

;;;;;;;;;;;;;;;;;;; sequence fns  ;;;;;;;;;;;;;;;;;;;;;;;
  
(defn every?
  "Returns true if (pred x) is logical true for every x in coll, else
  false."  
  {:tag Boolean}
  [pred coll]
    (if (seq coll)
      (and (pred (first coll))
           (recur pred (rest coll)))
      true))

(def
 #^{:tag Boolean
    :doc "Returns false if (pred x) is logical true for every x in 
  coll, else true."
    :arglists '([pred coll])}
 not-every? (comp not every?))

(defn some
  "Returns the first logical true value of (pred x) for any x in coll,
  else nil."
  [pred coll]
    (when (seq coll)
      (or (pred (first coll)) (recur pred (rest coll)))))

(def 
 #^{:tag Boolean
    :doc "Returns false if (pred x) is logical true for any x in coll,
  else true."
    :arglists '([pred coll])}  
 not-any? (comp not some))

(defn map
  "Returns a lazy seq consisting of the result of applying f to the
  set of first items of each coll, followed by applying f to the set
  of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments."
  ([f coll]
   (when (seq coll)
     (lazy-cons (f (first coll)) (map f (rest coll)))))
  ([f coll & colls]
   (when (and (seq coll) (every? seq colls))
     (lazy-cons (apply f (first coll) (map first colls))
                (apply map f (rest coll) (map rest colls))))))

(defn mapcat
  "Returns the result of applying concat to the result of applying map
  to f and colls.  Thus function f should return a collection."
  [f & colls]
    (apply concat (apply map f colls)))

(defn filter
  "Returns a lazy seq of the items in coll for which
  (pred item) returns true. pred must be free of side-effects."
  [pred coll]
    (when (seq coll)
      (if (pred (first coll))
        (lazy-cons (first coll) (filter pred (rest coll)))
        (recur pred (rest coll)))))

(defn take
  "Returns a lazy seq of the first n items in coll, or all items if
  there are fewer than n."  
  [n coll]
    (when (and (pos? n) (seq coll))
      (lazy-cons (first coll) (take (dec n) (rest coll)))))

(defn take-while
  "Returns a lazy seq of successive items from coll while
  (pred item) returns true. pred must be free of side-effects."
  [pred coll]
    (when (and (seq coll) (pred (first coll)))
      (lazy-cons (first coll) (take-while pred (rest coll)))))

(defn drop
  "Returns a lazy seq of all but the first n items in coll."
  [n coll]
    (if (and (pos? n) (seq coll))
      (recur (dec n) (rest coll))
      (seq coll)))

(defn drop-last
  "Return a lazy seq of all but the last n (default 1) items in coll"
  ([s] (drop-last 1 s))
  ([n s] (map (fn [x _] x) (seq s) (drop n s))))

(defn drop-while
  "Returns a lazy seq of the items in coll starting from the first
  item for which (pred item) returns nil."
  [pred coll]
    (if (and (seq coll) (pred (first coll)))
      (recur pred (rest coll))
      (seq coll)))

(defn cycle
  "Returns a lazy (infinite!) seq of repetitions of the items in
  coll."  
  [coll]
    (when (seq coll)
      (let [rep (fn thisfn [xs]
                    (if xs
                      (lazy-cons (first xs) (thisfn (rest xs)))
                      (recur (seq coll))))]
        (rep (seq coll)))))

(defn split-at
  "Returns a vector of [(take n coll) (drop n coll)]"
  [n coll]
    [(take n coll) (drop n coll)])

(defn split-with
  "Returns a vector of [(take-while pred coll) (drop-while pred coll)]"
  [pred coll]
    [(take-while pred coll) (drop-while pred coll)])

(defn repeat
  "Returns a lazy (infinite!) seq of xs."
  [x] (lazy-cons x (repeat x)))

(defn replicate
  "Returns a lazy seq of n xs."
  [n x] (take n (repeat x)))
  
(defn iterate
  "Returns a lazy seq of x, (f x), (f (f x)) etc. f must be free of side-effects"
  [f x] (lazy-cons x (iterate f (f x))))

(defn range
  "Returns a lazy seq of nums from start (inclusive) to end
  (exclusive), by step, where start defaults to 0 and step to 1."
  ([end] (if (and (> end 0) (< end (. Integer MAX_VALUE)))
           (new clojure.lang.Range 0 end)
           (take end (iterate inc 0))))
  ([start end] (if (and (< start end) (< end (. Integer MAX_VALUE)))
                 (new clojure.lang.Range start end)
                 (take (- end start) (iterate inc start))))
  ([start end step]
   (take-while (partial (if (pos? step) > <) end) (iterate (partial + step) start))))

(defn merge
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping from
  the latter (left-to-right) will be the mapping in the result."
  [& maps] (reduce conj maps))

(defn merge-with
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f val-in-result val-in-latter)."
  [f & maps]
    (let [merge-entry (fn [m e]
			(let [k (key e) v (val e)]
			  (if (contains? m k)
			    (assoc m k (f (m k) v))
			    (assoc m k v))))
          merge2 (fn [m1 m2]
		   (reduce merge-entry m1 (seq m2)))]
      (reduce merge2 maps)))



(defn zipmap
  "Returns a map with the keys mapped to the corresponding vals."
  [keys vals]
    (loop [map {}
           ks (seq keys)
           vs (seq vals)]
      (if (and ks vs)
        (recur (assoc map (first ks) (first vs))
               (rest ks)
               (rest vs))
        map)))

(defn line-seq
  "Returns the lines of text from rdr as a lazy sequence of strings.
  rdr must implement java.io.BufferedReader."
  [#^java.io.BufferedReader rdr]
    (let [line  (. rdr (readLine))]
      (when line
        (lazy-cons line (line-seq rdr)))))

(defn comparator
  "Returns an implementation of java.util.Comparator based upon pred."
  [pred]
    (fn [x y] 
      (cond (pred x y) -1 (pred y x) 1 :else 0)))
  
(defn sort
  "Returns a sorted sequence of the items in coll. If no comparator is
  supplied, uses compare. comparator must
  implement java.util.Comparator."
  ([#^java.util.Collection coll]
   (sort compare coll))
  ([#^java.util.Comparator comp #^java.util.Collection coll]
   (when (and coll (not (. coll (isEmpty))))
     (let [a (. coll (toArray))]
       (. java.util.Arrays (sort a comp))
       (seq a)))))

(defn sort-by
  "Returns a sorted sequence of the items in coll, where the sort
  order is determined by comparing (keyfn item).  If no comparator is
  supplied, uses compare. comparator must
  implement java.util.Comparator."
  ([keyfn coll]
   (sort-by keyfn compare coll))
  ([keyfn #^java.util.Comparator comp coll]
   (sort (fn [x y] (. comp (compare (keyfn x) (keyfn y)))) coll)))

;; evaluation

(defn eval
  "Evaluates the form data structure (not text!) and returns the result."
  [form] (. clojure.lang.Compiler (eval form)))

;(defn defimports [& imports-maps]
;  (def *imports* (apply merge imports-maps)))

(defmacro doseq
  "Repeatedly executes body (presumably for side-effects) with
  binding-form bound to successive items from coll.  Does not retain
  the head of the sequence. Returns nil."
  [item list & body]
  `(loop [list# (seq ~list)]
     (when list#
       (let [~item (first list#)]
         ~@body)
       (recur (rest list#)))))

(defn scan [& args] (throw (new Exception "scan is now called dorun")))
(defn touch [& args] (throw (new Exception "touch is now called doall")))

(defn dorun
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. dorun can
  be used to force any effects. Walks through the successive rests of
  the seq, does not retain the head and returns nil."
  ([coll]
   (when (and (seq coll) (or (first coll) true))
     (recur (rest coll))))
  ([n coll]
   (when (and (seq coll) (pos? n) (or (first coll) true))
     (recur (dec n) (rest coll)))))

(defn doall
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. doall can
  be used to force any effects. Walks through the successive rests of
  the seq, retains the head and returns it, thus causing the entire
  seq to reside in memory at one time."
  ([coll]
   (dorun coll)
   coll)
  ([n coll]
   (dorun n coll)
   coll))

(defn await
  "Blocks the current thread (indefinitely!) until all actions
  dispatched thus far, from this thread or agent, to the agent(s) have
  occurred."
  [& agents]
    (when *agent*
      (throw (new Exception "Can't await in agent action")))
    (let [latch (new java.util.concurrent.CountDownLatch (count agents))
          count-down (fn [agent] (. latch (countDown)) agent)]
      (doseq agent agents
        (send agent count-down))
      (. latch (await))))

(defn await1 [#^clojure.lang.Agent a]
  (when (pos? (.getQueueCount a))
    (await a))
    a)

(defn await-for
  "Blocks the current thread until all actions dispatched thus
  far (from this thread or agent) to the agents have occurred, or the
  timeout (in milliseconds) has elapsed. Returns nil if returning due
  to timeout, non-nil otherwise."
  [timeout-ms & agents]
    (when *agent*
      (throw (new Exception "Can't await in agent action")))
    (let [latch (new java.util.concurrent.CountDownLatch (count agents))
          count-down (fn [agent] (. latch (countDown)) agent)]
      (doseq agent agents
        (send agent count-down))
      (. latch (await  timeout-ms (. java.util.concurrent.TimeUnit MILLISECONDS)))))
  
(defmacro dotimes
  "Repeatedly executes body (presumably for side-effects) with name
  bound to integers from 0 through n-1."
  [i n & body]
  `(let [n# (int ~n)]
     (loop [~i (int 0)]
       (when (< ~i n#)
         ~@body
         (recur (unchecked-inc ~i))))))

(defn import
  "import-list => (package-symbol class-name-symbols*)

  For each name in class-name-symbols, adds a mapping from name to the
  class named by package.name to the current namespace."  
  [& import-lists]
    (when import-lists
      (let [#^clojure.lang.Namespace ns *ns*
            pkg (ffirst import-lists)
            classes (rfirst import-lists)]
        (doseq c classes
          (. ns (importClass c (. Class (forName (str pkg "." c)))))) )
      (apply import (rest import-lists))))

(defmacro imports
  "import-list => (package-name class-names*)

  For each (unevaluated) name in class-names, adds a mapping from name to the
  class named by package-name.name to the current namespace."
  [& import-lists] `(import ~@(map #(list 'quote %) import-lists)))

(defn into-array
  "Returns an array of the type of the first element in coll,
  containing the contents of coll, which must be of a compatible
  type."
  [aseq]
    (. clojure.lang.RT (seqToTypedArray (seq aseq))))

(defn into
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined."
  [to from]
    (let [ret to items (seq from)]
      (if items
        (recur (conj ret (first items)) (rest items))
        ret)))

(defn #^{:private true}
  array [& items]
    (into-array items))

(defn pr
  "Prints the object(s) to the output stream that is the current value
  of *out*.  Prints the object(s), separated by spaces if there is
  more than one.  By default, pr and prn print in a way that objects
  can be read by the reader"
  ([] nil)
  ([x]
   (. clojure.lang.RT (print x *out*))
   nil)
  ([x & more]
   (pr x)
   (. *out* (append \space))
   (apply pr more)))

(defn newline 
  "Writes a newline to the output stream that is the current value of
  *out*" 
  []
    (. *out* (append \newline))
    nil)

(defn flush 
  "Flushes the output stream that is the current value of
  *out*" 
  []
    (. *out* (flush))
    nil)

(defn prn
  "Same as pr followed by (newline). Observes *flush-on-newline*"
  [& more]
    (apply pr more)
    (newline)
    (when *flush-on-newline*
      (flush)))

(defn print
  "Prints the object(s) to the output stream that is the current value
  of *out*.  print and println produce output for human consumption."
  [& more]
    (binding [*print-readably* nil]
      (apply pr more)))

(defn println
  "Same as print followed by (newline)"
  [& more]
    (binding [*print-readably* nil]
      (apply prn more)))


(defn read
  "Reads the next object from stream, which must be an instance of
  java.io.PushbackReader or some derivee.  stream defaults to the
  current value of *in* ."
  ([]
   (read *in*))
  ([stream]
   (read stream true nil))
  ([stream eof-error? eof-value]
   (read stream eof-error? eof-value false))
  ([stream eof-error? eof-value recursive?]
   (. clojure.lang.LispReader (read stream eof-error? eof-value recursive?))))

(defn read-line
  "Reads the next line from stream that is the current value of *in* ."
  [] (. *in* (readLine)))

(defmacro with-open
  "Evaluates body in a try expression with name bound to the value of
  init, and a finally clause that calls (. name (close))."
  [name init & body]
  `(let [~name ~init]
     (try
      ~@body
      (finally
       (. ~name (close))))))

(defmacro doto
  "Evaluates x then calls all of the methods with the supplied
  arguments in succession on the resulting object, returning it.

  (doto (new java.util.HashMap) (put \"a\" 1) (put \"b\" 2))"
  [x & members]
    (let [gx (gensym)]
      `(let [~gx ~x]
         (do
           ~@(map (fn [m] (list '. gx m))
                  members))
         ~gx)))

(defmacro memfn
  "Expands into code that creates a fn that expects to be passed an
  object and any args and calls the named instance method on the
  object passing the args. Use when you want to treat a Java method as
  a first-class fn."
  [name & args]
  `(fn [target# ~@args]
     (. target# (~name ~@args))))

(defmacro time
  "Evaluates expr and prints the time it took.  Returns the value of
 expr."  
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (prn (str "Elapsed time: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
     ret#))

(defn num
  "Coerce to Number"
  {:tag Number
   :inline (fn  [x] `(. clojure.lang.Numbers (num ~x)))}
  [x] (. clojure.lang.Numbers (num x)))

(defn int
  "Coerce to int"
  {:tag Integer
   :inline (fn  [x] `(. clojure.lang.RT (intCast ~x)))}
  [x] (. clojure.lang.RT (intCast x)))

(defn long
  "Coerce to long"
  {:tag Long
   :inline (fn  [x] `(. clojure.lang.RT (longCast ~x)))}
  [#^Number x] (. x (longValue)))

(defn float
  "Coerce to float"
  {:tag Float
   :inline (fn  [x] `(. clojure.lang.RT (floatCast ~x)))}
  [#^Number x] (. x (floatValue)))

(defn double
  "Coerce to double"
  {:tag Double
   :inline (fn  [x] `(. clojure.lang.RT (doubleCast ~x)))}
  [#^Number x] (. x (doubleValue)))

(defn short
  "Coerce to short"
  {:tag Short}
  [#^Number x] (. x (shortValue)))

(defn byte
  "Coerce to byte"
  {:tag Byte}
  [#^Number x] (. x (byteValue)))

(defn char
  "Coerce to char"
  {:tag Character}
  [x] (. clojure.lang.RT (charCast x)))

(defn boolean
  "Coerce to boolean"
  {:tag Boolean} 
  [x] (if x true false))

(defn bigint 
  "Coerce to BigInteger"
  {:tag BigInteger} 
  [x] (. BigInteger valueOf x))

(defn bigdec
  "Coerce to BigDecimal"
  {:tag BigDecimal}
  [x] (. BigDecimal valueOf x))

(import '(java.lang.reflect Array))

(defn alength
  "Returns the length of the Java array. Works on arrays of all
  types."
  {:inline (fn [a] `(. clojure.lang.RT (alength ~a)))}
  [array] (. clojure.lang.RT (alength array)))

(defn aclone
  "Returns a clone of the Java array. Works on arrays of known
  types."
  {:inline (fn [a] `(. clojure.lang.RT (aclone ~a)))}
  [array] (. clojure.lang.RT (aclone array)))

(defn aget
  "Returns the value at the index/indices. Works on Java arrays of all
  types."
  {:inline (fn [a i] `(. clojure.lang.RT (aget ~a ~i)))
   :inline-arities #{2}}
  ([array idx]
   (. Array (get array idx)))
  ([array idx & idxs]
   (apply aget (aget array idx) idxs)))

(defn aset
  "Sets the value at the index/indices. Works on Java arrays of
  reference types. Returns val."
  {:inline (fn [a i v] `(. clojure.lang.RT (aset ~a ~i ~v)))
   :inline-arities #{3}}
  ([array idx val]
   (. Array (set array idx val))
   val)
  ([array idx idx2 & idxv]
   (apply aset (aget array idx) idx2 idxv)))

(defmacro
  #^{:private true}
  def-aset [name method coerce]
    `(defn ~name
       {:arglists '([~'array ~'idx ~'val] [~'array ~'idx ~'idx2 & ~'idxv])}
       ([array# idx# val#]
        (. Array (~method array# idx# (~coerce val#)))
        val#)
       ([array# idx# idx2# & idxv#]
        (apply ~name (aget array# idx#) idx2# idxv#))))

(def-aset
  #^{:doc "Sets the value at the index/indices. Works on arrays of int. Returns val."}
  aset-int setInt int)

(def-aset
  #^{:doc "Sets the value at the index/indices. Works on arrays of long. Returns val."}
  aset-long setLong long)

(def-aset
  #^{:doc "Sets the value at the index/indices. Works on arrays of boolean. Returns val."}
  aset-boolean setBoolean boolean)

(def-aset
  #^{:doc "Sets the value at the index/indices. Works on arrays of float. Returns val."}
  aset-float setFloat float)

(def-aset
  #^{:doc "Sets the value at the index/indices. Works on arrays of double. Returns val."}
  aset-double setDouble double)

(def-aset
  #^{:doc "Sets the value at the index/indices. Works on arrays of short. Returns val."}
  aset-short setShort short)

(def-aset
  #^{:doc "Sets the value at the index/indices. Works on arrays of byte. Returns val."}
  aset-byte setByte byte)

(def-aset
  #^{:doc "Sets the value at the index/indices. Works on arrays of char. Returns val."}
  aset-char setChar char)

(defn make-array
  "Creates and returns an array of instances of the specified class of
  the specified dimension(s).  Note that a class object is required.
  Class objects can be obtained by using their imported or
  fully-qualified name.  Class objects for the primitive types can be
  obtained using, e.g., (. Integer TYPE)."
  ([#^Class type len]
   (. Array (newInstance type (int len))))
  ([#^Class type dim & more-dims]
   (let [dims (cons dim more-dims)
         #^"[I" dimarray (make-array (. Integer TYPE)  (count dims))]
     (dotimes i (alength dimarray)
       (aset-int dimarray i (nth dims i)))
     (. Array (newInstance type dimarray)))))

(defn to-array-2d
  "Returns a (potentially-ragged) 2-dimensional array of Objects
  containing the contents of coll, which can be any Collection of any
  Collection."
  [#^java.util.Collection coll]
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

(defn macroexpand-1
  "If form represents a macro form, returns its expansion,
  else returns form."
  [form]
    (. clojure.lang.Compiler (macroexpand1 form)))

(defn macroexpand
  "Repeatedly calls macroexpand-1 on form until it no longer
  represents a macro form, then returns it.  Note neither
  macroexpand-1 nor macroexpand expand macros in subforms."
  [form]
    (let [ex (macroexpand-1 form)]
      (if (identical? ex form)
        form
        (macroexpand ex))))

(defn create-struct
  "Returns a structure basis object."
  [& keys]
    (. clojure.lang.PersistentStructMap (createSlotMap keys)))

(defmacro defstruct
  "Same as (def name (create-struct keys...))"
  [name & keys]
  `(def ~name (create-struct ~@keys)))
  
(defn struct-map
  "Returns a new structmap instance with the keys of the
  structure-basis. keyvals may contain all, some or none of the basis
  keys - where values are not supplied they will default to nil.
  keyvals can also contain keys not in the basis."
  [s & inits]
    (. clojure.lang.PersistentStructMap (create s inits)))

(defn struct
  "Returns a new structmap instance with the keys of the
  structure-basis. vals must be supplied for basis keys in order -
  where values are not supplied they will default to nil."
  [s & vals]
    (. clojure.lang.PersistentStructMap (construct s vals)))

(defn accessor
  "Returns a fn that, given an instance of a structmap with the basis,
  returns the value at the key.  The key must be in the basis. The
  returned function should be (slightly) more efficient than using
  get, but such use of accessors should be limited to known
  performance-critical areas."
  [s key]
    (. clojure.lang.PersistentStructMap (getAccessor s key)))

(defn subvec
  "Returns a persistent vector of the items in vector from
  start (inclusive) to end (exclusive).  If end is not supplied,
  defaults to (count vector). This operation is O(1) and very fast, as
  the resulting vector shares structure with the original and no
  trimming is done."
  ([v start]
   (subvec v start (count v)))
  ([v start end]
   (. clojure.lang.RT (subvec v start end))))

(defn load
  "Sequentially read and evaluate the set of forms contained in the
  stream/file" 
  [rdr] (. clojure.lang.Compiler (load rdr)))

(defn resultset-seq
  "Creates and returns a lazy sequence of structmaps corresponding to
  the rows in the java.sql.ResultSet rs" 
  [#^java.sql.ResultSet rs]
    (let [rsmeta (. rs (getMetaData))
          idxs (range 1 (inc (. rsmeta (getColumnCount))))
          keys (map (comp keyword (memfn toLowerCase))
                    (map (fn [i] (. rsmeta (getColumnName i))) idxs))
          row-struct (apply create-struct keys)
          row-values (fn [] (map (fn [#^Integer i] (. rs (getObject i))) idxs))
          rows (fn thisfn []
                   (when (. rs (next))
		     (lazy-cons (apply struct row-struct (row-values)) (thisfn))))]
      (rows)))

(defn set 
  "Returns a set of the distinct elements of coll."
  [coll] (apply hash-set coll))

(defn #^{:private true}
  filter-key [keyfn pred amap]
    (loop [ret {} es (seq amap)]
      (if es
        (if (pred (keyfn (first es)))
          (recur (assoc ret (key (first es)) (val (first es))) (rest es))
          (recur ret (rest es)))
        ret)))

(defn find-ns
  "Returns the namespace named by the symbol or nil if it doesn't exist."
  [sym] (. clojure.lang.Namespace (find sym)))

(defn create-ns
  "Create a new namespace named by the symbol if one doesn't already
  exist, returns it or the already-existing namespace of the same
  name."
  [sym] (. clojure.lang.Namespace (findOrCreate sym)))

(defn remove-ns
  "Removes the namespace named by the symbol. Use with caution.
  Cannot be used to remove the clojure namespace."
  [sym] (. clojure.lang.Namespace (remove sym)))

(defn all-ns
  "Returns a sequence of all namespaces."
  [] (. clojure.lang.Namespace (all)))

(defn ns-name
  "Returns the name of the namespace, a symbol."
  [#^clojure.lang.Namespace ns]
    (. ns (getName)))

(defn ns-map
  "Returns a map of all the mappings for the namespace."
  [#^clojure.lang.Namespace ns]
    (. ns (getMappings)))

(defn ns-unmap
  "Removes the mappings for the symbol from the namespace."
  [#^clojure.lang.Namespace ns sym]
    (. ns (unmap sym)))

;(defn export [syms]
;  (doseq sym syms
;   (.. *ns* (intern sym) (setExported true))))

(defn ns-publics
  "Returns a map of the public intern mappings for the namespace."
  [#^clojure.lang.Namespace ns]
    (filter-key val (fn [#^clojure.lang.Var v] (and (instance? clojure.lang.Var v)
                                 (= ns (. v ns))
                                 (. v (isPublic))))
                (ns-map ns)))

(defn ns-imports
  "Returns a map of the import mappings for the namespace."
  [#^clojure.lang.Namespace ns]
    (filter-key val (partial instance? Class) (ns-map ns)))

(defn refer
  "refers to all public vars of ns, subject to filters.
  filters can include at most one each of:

  :exclude list-of-symbols
  :only list-of-symbols
  :rename map-of-fromsymbol-tosymbol

  For each public interned var in the namespace named by the symbol,
  adds a mapping from the name of the var to the var to the current
  namespace.  Throws an exception if name is already mapped to
  something else in the current namespace. Filters can be used to
  select a subset, via inclusion or exclusion, or to provide a mapping
  to a symbol different from the var's name, in order to prevent
  clashes."
  [ns-sym & filters]
    (let [ns (or (find-ns ns-sym) (throw (new Exception (str "No namespace: " ns-sym))))
          fs (apply hash-map filters)
          nspublics (ns-publics ns)
          rename (or (:rename fs) {})
          exclude (set (:exclude fs))
          to-do (or (:only fs) (keys nspublics))]
      (doseq sym to-do
        (when-not (exclude sym)
          (let [v (nspublics sym)]
            (when-not v
              (throw (new java.lang.IllegalAccessError (str sym " is not public"))))
            (. *ns* (refer (or (rename sym) sym) v)))))))

(defmacro refers 
  "refers to all public vars of ns, subject to filters.
  filters can include at most one each of:

  :exclude list-of-names
  :only list-of-names
  :rename map-of-fromname-toname

  For each public interned var in the namespace named by the symbol,
  adds a mapping from the name of the var to the var to the current
  namespace.  Throws an exception if name is already mapped to
  something else in the current namespace. Filters can be used to
  select a subset, via inclusion or exclusion, or to provide a mapping
  to a symbol different from the var's name, in order to prevent
  clashes."
  [ns-name & filters] `(refer '~ns-name ~@(map #(list 'quote %) filters)))

(defn ns-refers
  "Returns a map of the refer mappings for the namespace."
  [#^clojure.lang.Namespace ns]
    (filter-key val (fn [#^clojure.lang.Var v] (and (instance? clojure.lang.Var v)
                                 (not= ns (. v ns))))
                (ns-map ns)))

(defn ns-interns
  "Returns a map of the intern mappings for the namespace."
  [#^clojure.lang.Namespace ns]
    (filter-key val (fn [#^clojure.lang.Var v] (and (instance? clojure.lang.Var v)
                                 (= ns (. v ns))))
                (ns-map ns)))

(defn alias
  "Add an alias in the current namespace to another
  namespace. Arguments are two symbols: the alias to be used, and
  the symbolic name of the target namespace."
  [alias namespace-sym]
  (.addAlias *ns* alias (find-ns namespace-sym)))

(defn ns-aliases
  "Returns a map of the aliases for the namespace."
  [#^clojure.lang.Namespace ns]  (.getAliases ns))

(defn ns-unalias
  "Removes the alias for the symbol from the namespace."
  [#^clojure.lang.Namespace ns sym]
    (. ns (removeAlias sym)))

(defn take-nth
  "Returns a lazy seq of every nth item in coll."
  [n coll]
    (when (seq coll)
      (lazy-cons (first coll) (take-nth n (drop n coll)))))

(defn interleave
  "Returns a lazy seq of the first item in each coll, then the second
  etc."
  [& colls]
    (apply concat (apply map list colls)))

(defn var-get
  "Gets the value in the var object"
  [#^clojure.lang.Var x] (. x (get)))

(defn var-set
  "Sets the value in the var object to val. The var must be
 thread-locally bound."  
  [#^clojure.lang.Var x val] (. x (set val)))

(defmacro with-local-vars
  "varbinding=> symbol init-expr

  Executes the exprs in a context in which the symbols are bound to
  vars with per-thread bindings to the init-exprs.  The symbols refer
  to the var objects themselves, and must be accessed with var-get and
  var-set"
  [name-vals-vec & body]
  `(let [~@(interleave (take-nth 2 name-vals-vec)
                       (repeat '(. clojure.lang.Var (create))))]
     (. clojure.lang.Var (pushThreadBindings (hash-map ~@name-vals-vec)))
     (try
      ~@body
      (finally (. clojure.lang.Var (popThreadBindings))))))

(defn ns-resolve
  "Returns the var or Class to which a symbol will be resolved in the
  namespace, else nil.  Note that if the symbol is fully qualified,
  the var/Class to which it resolves need not be present in the
  namespace."
  [ns sym]
    (. clojure.lang.Compiler (maybeResolveIn ns sym)))

(defn resolve
  "same as (ns-resolve *ns* symbol)"
  [sym] (ns-resolve *ns* sym))

(defn array-map
  "Constructs an array-map."
  ([] (. clojure.lang.PersistentArrayMap EMPTY))
  ([& keyvals] (new clojure.lang.PersistentArrayMap (to-array keyvals))))

(defn nthrest
  "Returns the nth rest of coll, (seq coll) when n is 0."
  [coll n]
    (loop [n n xs (seq coll)]
      (if (and xs (pos? n))
        (recur (dec n) (rest xs))
        xs)))

(defn symbol?
  "Return true if x is a Symbol"
  [x] (instance? clojure.lang.Symbol x))

(defn keyword?
  "Return true if x is a Keyword"
  [x] (instance? clojure.lang.Keyword x))

;redefine let and loop  with destructuring
(defn destructure [bindings]
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
                                        (recur (pb ret firstb  (list `nth gvec n nil))
                                               (inc n)
                                               (rest bs)
                                               seen-rest?))))
                             ret))))
                     pmap
                     (fn [bvec b v]
                       (let [gmap (or (:as b) (gensym "map__"))
                             defaults (:or b)]
                         (loop [ret (-> bvec (conj gmap) (conj v))
                                bes (reduce
                                     (fn [bes entry]
                                       (reduce #(assoc %1 %2 ((val entry) %2))
                                               (dissoc bes (key entry))
                                               ((key entry) bes)))
                                     (dissoc b :as :or)
                                     {:keys #(keyword (str %)), :strs str, :syms #(list `quote %)})]
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
                  :else (throw (new Exception (str "Unsupported binding form: " b))))))
        process-entry (fn [bvec b] (pb bvec (key b) (val b)))]
    (if (every? symbol? (keys bmap))
      bindings
      (reduce process-entry [] bmap))))

(defmacro let
  "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein."
  [bindings & body]
  `(let* ~(destructure bindings) ~@body))

;redefine fn with destructuring
(defmacro fn
  "(fn name? [params* ] exprs*)
  (fn name? ([params* ] exprs*)+)

  params => positional-params* , or positional-params* & rest-param
  positional-param => binding-form
  rest-param => binding-form
  name => symbol

  Defines a function"
  [& sigs]
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
                             (recur (rest params) (conj new-params gparam) 
                                    (-> lets (conj (first params)) (conj gparam)))))
                         `(~new-params
                           (let ~lets
                             ~@body)))))))
          new-sigs (map psig sigs)]
      (with-meta
        (if name
          (list* 'fn* name new-sigs)
          (cons 'fn* new-sigs))
        *macro-meta*)))

(defmacro loop
  "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Acts as a recur target."
  [bindings & body]
    (let [db (destructure bindings)]
      (if (= db bindings)
        `(loop* ~bindings ~@body)
        (let [vs (take-nth 2 (drop 1 bindings))
              bs (take-nth 2 bindings)
              gs (map (fn [b] (if (symbol? b) b (gensym))) bs)
              bfs (reduce (fn [ret [b v g]]
                            (if (symbol? b)
                              (conj ret g v)
                              (conj ret g v b g)))
                          [] (map vector bs vs gs))]
          `(let ~bfs
             (loop* ~(vec (interleave gs gs))
               (let ~(vec (interleave bs gs))
                 ~@body)))))))
  
(defmacro when-first
  "Same as (when (seq xs) (let [x (first xs)] body))"
  [x xs & body]
  `(when (seq ~xs)
     (let [~x (first ~xs)]
       ~@body)))

(defmacro lazy-cat
  "Expands to code which yields a lazy sequence of the concatenation
  of the supplied colls.  Each coll expr is not evaluated until it is
  needed."
  ([coll] `(seq ~coll))
  ([coll & colls]
   `(let [iter# (fn iter# [coll#]
		    (if (seq coll#)
		      (lazy-cons (first coll#) (iter# (rest coll#)))
		      (lazy-cat ~@colls)))]
      (iter# ~coll))))
      


(defmacro for
 "List comprehension. Takes a vector of one or more
 binding-form/collection-expr pairs, each followed by an optional filtering
 :when/:while expression (:when test or :while test), and yields a
 lazy sequence of evaluations of expr. Collections are iterated in a
 nested fashion, rightmost fastest, and nested coll-exprs can refer to
 bindings created in prior binding-forms.

 (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)]  [x y]))"
 ([seq-exprs expr]
  (let [pargs (fn [xs]
                (loop [ret []
                       [b e & [w f & wr :as r] :as xs] (seq xs)]
                  (if xs
                    (cond 
                     (= w :when) (recur (conj ret {:b b :e e :f f :w :when}) wr)
                     (= w :while) (recur (conj ret {:b b :e e :f f :w :while}) wr)
                     :else (recur (conj ret {:b b :e e :f true :w :while}) r))
                    (seq ret))))
        emit (fn emit [[{b :b f :f w :w} & [{ys :e} :as rses]]]
		  (let [giter (gensym "iter__") gxs (gensym "s__")]
		    `(fn ~giter [~gxs]
			 (when-first ~b ~gxs
                           (if ~f
			    ~(if rses
			       `(let [iterys# ~(emit rses)]
				  (lazy-cat (iterys# ~ys)
					    (~giter (rest ~gxs))))
			       `(lazy-cons ~expr (~giter (rest ~gxs))))
                            ~(if (= w :when)
                               `(recur (rest ~gxs))
                               nil))))))]
    `(let [iter# ~(emit (pargs seq-exprs))]
	(iter# ~(second seq-exprs))))))

(defmacro comment
  "Ignores body, yields nil"
  [& body])

(defmacro with-out-str
  "Evaluates exprs in a context in which *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       ~@body
       (str s#))))

(defmacro with-in-str
  "Evaluates body in a context in which *in* is bound to a fresh
  StringReader initialized with the string s."
  [s & body]
  `(with-open s# (-> (java.io.StringReader. ~s) clojure.lang.LineNumberingPushbackReader.)
     (binding [*in* s#]
       ~@body)))

(defn pr-str
  "pr to a string, returning it"
  {:tag String}
  [& xs]
    (with-out-str
     (apply pr xs)))

(defn prn-str
  "prn to a string, returning it"
  {:tag String}
  [& xs]
  (with-out-str
   (apply prn xs)))

(defn print-str
  "print to a string, returning it"
  {:tag String}
  [& xs]
    (with-out-str
     (apply print xs)))

(defn println-str
  "println to a string, returning it"
  {:tag String}
  [& xs]
    (with-out-str
     (apply println xs)))

(defmacro assert
  "Evaluates expr and throws an exception if it does not evaluate to
 logical true."
  [x]
  `(when-not ~x
     (throw (new Exception (str "Assert failed: " (pr-str '~x))))))

(defn test
  "test [v] finds fn at key :test in var metadata and calls it,
  presuming failure will throw exception"
  [v]
    (let [f (:test ^v)]
      (if f
        (do (f) :ok)
        :no-test)))

(defn re-pattern
  "Returns an instance of java.util.regex.Pattern, for use, e.g. in
  re-matcher."  
  {:tag java.util.regex.Pattern}
  [s] (. java.util.regex.Pattern (compile s)))
  
(defn re-matcher
  "Returns an instance of java.util.regex.Matcher, for use, e.g. in
  re-find."  
  {:tag java.util.regex.Matcher}
  [#^java.util.regex.Pattern re s]
    (. re (matcher s)))

(defn re-groups
  "Returns the groups from the most recent match/find. If there are no
  nested groups, returns a string of the entire match. If there are
  nested groups, returns a vector of the groups, the first element
  being the entire match."
  [#^java.util.regex.Matcher m]
    (let [gc  (. m (groupCount))]
      (if (zero? gc)
        (. m (group))
        (loop [ret [] c 0]
          (if (<= c gc)
            (recur (conj ret (. m (group c))) (inc c))
            ret)))))

(defn re-seq
  "Returns a lazy sequence of successive matches of pattern in string,
  using java.util.regex.Matcher.find(), each such match processed with
  re-groups."
  [#^java.util.regex.Pattern re s]
    (let [m (re-matcher re s)]
      ((fn step []
           (when (. m (find))
             (lazy-cons (re-groups m) (step)))))))

(defn re-matches
  "Returns the match, if any, of string to pattern, using
  java.util.regex.Matcher.matches().  Uses re-groups to return the
  groups."
  [#^java.util.regex.Pattern re s]
    (let [m (re-matcher re s)]
      (when (. m (matches))
        (re-groups m))))


(defn re-find
  "Returns the next regex match, if any, of string to pattern, using
  java.util.regex.Matcher.find().  Uses re-groups to return the
  groups."
  ([#^java.util.regex.Matcher m]
   (when (. m (find))
     (re-groups m)))
  ([#^java.util.regex.Pattern re s]
   (let [m (re-matcher re s)]
     (re-find m))))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  1 (exclusive)."
  ([] (. Math (random)))
  ([n] (* n (rand))))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive)."
  [n] (int (rand n)))

(defmacro defn-
  "same as defn, yielding non-public def"
  [name & decls]
    (list* `defn (with-meta name (assoc (meta name) :private true)) decls))

(defn print-doc [v]
  (println "-------------------------")
  (println (str (ns-name (:ns ^v)) "/" (:name ^v)))
  (prn (:arglists ^v))
  (when (:macro ^v)
    (println "Macro"))
  (println " " (:doc ^v)))

(defn find-doc
  "Prints documentation for any var whose documentation or name
 contains a match for re-string"
  [re-string]
    (let [re  (re-pattern re-string)]
      (dorun (for [ns (all-ns) 
                   v (sort-by (comp :name meta) (vals (ns-interns ns)))
                   :when (and (:doc ^v)
                          (or (re-find (re-matcher re (:doc ^v)))
                              (re-find (re-matcher re (str (:name ^v))))))]
               (print-doc v)))))

(defmacro doc
  "Prints documentation for the var named by varname"
  [varname]
  `(print-doc (var ~varname)))

(defn tree-seq
  "returns a lazy sequence of the nodes in a tree, via a depth-first walk.
  branch? must be a fn of one arg that returns true if passed a node
  that can have children (but may not).  children must be a fn of one
  arg that returns a sequence of the children. Will only be called on
  nodes for which branch? returns true. Root is the root node of the
  tree, must be a branch."  
  [branch? children root]
    (let [walk (fn walk [nodes]
                   (when-first node nodes
                     (lazy-cons
                      node
                      (if (branch? node)
                        (lazy-cat (walk (children node))
                                  (walk (rest nodes)))
                        (walk (rest nodes))))))]
      (lazy-cons root (walk (children root)))))

(defn file-seq
  "A tree seq on java.io.Files"
  [dir]
    (tree-seq
     (fn [#^java.io.File f] (. f (isDirectory)))
     (fn [#^java.io.File d] (seq (. d (listFiles))))
     dir))

(defn xml-seq
  "A tree seq on the xml elements as per xml/parse"
  [root]
    (tree-seq
     (complement string?)
     (comp seq :content)
     root))

(defn special-symbol?
  "Returns true if s names a special form"
  [s]
    (contains? (. clojure.lang.Compiler specials) s))

(defn var?
  "Returns true if v is of type clojure.lang.Var"
  [v] (instance? clojure.lang.Var v))

(defn class
  "Returns the Class of x"
  [#^Object x] (if (nil? x) x (. x (getClass))))

(defn slurp
  "Reads the file named by f into a string and returns it."
  [#^String f]
  (with-open r (new java.io.BufferedReader (new java.io.FileReader f))
    (let [sb (new StringBuilder)]
      (loop [c (. r (read))]
        (if (neg? c)
          (str sb)
          (do 
            (. sb (append (char c)))
            (recur (. r (read)))))))))

(defn subs
  "Returns the substring of s beginning at start inclusive, and ending
  at end (defaults to length of string), exclusive."
  ([#^String s start] (. s (substring start)))
  ([#^String s start end] (. s (substring start end))))

(defn max-key
  "Returns the x for which (k x), a number, is greatest."
  ([k x] x)
  ([k x y] (if (> (k x) (k y)) x y))
  ([k x y & more]
   (reduce #(max-key k %1 %2) (max-key k x y) more)))

(defn min-key
  "Returns the x for which (k x), a number, is least."
  ([k x] x)
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
   (reduce #(min-key k %1 %2) (min-key k x y) more)))

(defn distinct
  "Returns a lazy sequence of the elements of coll with duplicates removed"
  [coll] 
    (let [step (fn step [[f & r :as xs] seen]
                   (when xs
                     (if (seen f) (recur r seen)
                         (lazy-cons f (step r (conj seen f))))))]
      (step (seq coll) #{})))

(defmacro if-let 
  "if test is true, evaluates then with binding-form bound to the value of test, if not, yields else"
  ([binding-form test then]
   `(if-let ~binding-form ~test ~then nil))
  ([binding-form test then else]
   `(let [temp# ~test]
      (if temp# 
        (let [~binding-form temp#]
          ~then)
        ~else))))

(defmacro when-let 
  "when test is true, evaluates body with binding-form bound to the value of test"
  [binding-form test & body]
  `(let [temp# ~test]
     (when temp#
       (let [~binding-form temp#]
         ~@body))))

(defn replace
  "Given a map of replacement pairs and a vector/collection, returns a
  vector/seq with any elements = a key in smap replaced with the
  corresponding val in smap" 
  [smap coll]
    (if (vector? coll)
      (reduce (fn [v i]
                (if-let e (find smap (nth v i))
                        (assoc v i (val e))
                        v))
              coll (range (count coll)))
      (map #(if-let e (find smap %) (val e) %) coll)))

(defmacro dosync 
  "Runs the exprs (in an implicit do) in a transaction that encompasses
  exprs and any nested calls.  Starts a transaction if none is already
  running on this thread. Any uncaught exception will abort the
  transaction and flow out of dosync. The exprs may be run more than
  once, but any effects on Refs will be atomic."
  [& exprs]
  `(sync nil ~@exprs))

(defmacro with-precision
  "Sets the precision and rounding mode to be used for BigDecimal operations.

  Usage: (with-precision 10 (/ 1M 3))
  or:    (with-precision 10 :rounding HALF_DOWN (/ 1M 3))
  
  The rounding mode is one of CEILING, FLOOR, HALF_UP, HALF_DOWN,
  HALF_EVEN, UP, DOWN and UNNECESSARY; it defaults to HALF_UP."
  [precision & exprs]
    (let [[body rm] (if (= (first exprs) :rounding)
                      [(rest (rest exprs))
                       `((. java.math.RoundingMode ~(second exprs)))]
                      [exprs nil])]
      `(binding [*math-context* (java.math.MathContext. ~precision ~@rm)]
         ~@body))) 

(defn bound-fn
  {:private true}
  [#^clojure.lang.Sorted sc test key]
  (fn [e]
    (test (.. sc comparator (compare (. sc entryKey e) key)) 0)))
  
(defn subseq
  "sc must be a sorted collection, test(s) one of <, <=, > or
  >=. Returns a seq of those entries with keys ek for
  which (test (.. sc comparator (compare ek key)) 0) is true"  
  ([#^clojure.lang.Sorted sc test key]
   (let [include (bound-fn sc test key)]
     (if (#{> >=} test)
       (when-let [e :as s] (. sc seqFrom key true)
         (if (include e) s (rest s)))
       (take-while include (. sc seq true)))))     
  ([#^clojure.lang.Sorted sc start-test start-key end-test end-key]
   (when-let [e :as s] (. sc seqFrom start-key true)
     (take-while (bound-fn sc end-test end-key)
                 (if ((bound-fn sc start-test start-key) e) s (rest s))))))

(defn rsubseq
  "sc must be a sorted collection, test(s) one of <, <=, > or
  >=. Returns a reverse seq of those entries with keys ek for
  which (test (.. sc comparator (compare ek key)) 0) is true"
  ([#^clojure.lang.Sorted sc test key]
   (let [include (bound-fn sc test key)]
     (if (#{< <=} test)
       (when-let [e :as s] (. sc seqFrom key false)
         (if (include e) s (rest s)))
       (take-while include (. sc seq false)))))     
  ([#^clojure.lang.Sorted sc start-test start-key end-test end-key]
   (when-let [e :as s] (. sc seqFrom end-key false)
     (take-while (bound-fn sc start-test start-key)
                 (if ((bound-fn sc end-test end-key) e) s (rest s))))))

(defn repeatedly 
  "Takes a function of no args, presumably with side effects, and returns an infinite
  lazy sequence of calls to it"
  [f] (lazy-cons (f) (repeatedly f)))

(defn add-classpath
  "Adds the url (String or URL object) to the classpath per URLClassLoader.addURL"
  [url] (. clojure.lang.RT addURL url))



(defn hash
  "Returns the hash code of its argument"
  [x] (. clojure.lang.Util (hash x)))

(defn interpose
  "Returns a lazy seq of the elements of coll separated by sep"
  [sep coll] (drop 1 (interleave (repeat sep) coll)))

(defn partition
  "Returns a lazy sequence of lists of n items each, at offsets step
  apart. If step is not supplied, defaults to n, i.e. the partitions
  do not overlap."
  ([n coll]
     (partition n n coll))
  ([n step coll]
   (when (seq coll)
     (let [p (take n coll)]
       (when (= n (count p))
         (lazy-cons  p (partition n step (drop step coll))))))))

(defmacro definline 
  "Experimental - like defmacro, except defines a named function whose
  body is the expansion, calls to which may be expanded inline as if
  it were a macro. Cannot be used with variadic (&) args." 
  [name & decl]
  (let [[args expr] (drop-while (comp not vector?) decl)
        inline (eval (list 'fn args expr))]
    `(do
       (defn ~name ~args ~(apply inline args))
       (let [v# (var ~name)]
         (.setMeta v# (assoc ^v# :inline ~inline))))))

(defn empty 
  "Returns an empty collection of the same category as coll, or nil"
  [#^clojure.lang.IPersistentCollection coll]
  (.empty coll))

(defmacro amap
  "Maps an expression across an array a, using an index named idx, and
  return value named ret, initialized to a clone of a, then setting each element of
  ret to the evaluation of expr, returning the new array ret."
  [a idx ret expr]
  `(let [a# ~a
         ~ret (aclone a#)]
     (loop  [~idx (int 0)]
       (if (< ~idx  (alength a#))
         (do 
           (aset ~ret ~idx ~expr)
           (recur (unchecked-inc ~idx)))
         ~ret))))

(defmacro areduce 
  "Reduces an expression across an array a, using an index named idx,
  and return value named ret, initialized to init, setting ret to the evaluation of expr at
  each step, returning ret."
  [a idx ret init expr]
  `(let [a# ~a]
     (loop  [~idx (int 0) ~ret ~init]
       (if (< ~idx  (alength a#)) 
         (recur (unchecked-inc ~idx) ~expr)
         ~ret))))

(defn float-array 
  "Creates an array of floats"
  {:inline (fn [& args] `(. clojure.lang.Numbers float_array ~@args))
   :inline-arities #{1 2}}
  ([size-or-seq] (. clojure.lang.Numbers float_array size-or-seq))
  ([size init-val-or-seq] (. clojure.lang.Numbers float_array size init-val-or-seq)))

(defn double-array 
  "Creates an array of doubles"
  {:inline (fn [& args] `(. clojure.lang.Numbers double_array ~@args))
   :inline-arities #{1 2}}
  ([size-or-seq] (. clojure.lang.Numbers double_array size-or-seq))
  ([size init-val-or-seq] (. clojure.lang.Numbers double_array size init-val-or-seq)))

(defn int-array 
  "Creates an array of ints"
  {:inline (fn [& args] `(. clojure.lang.Numbers int_array ~@args))
   :inline-arities #{1 2}}
  ([size-or-seq] (. clojure.lang.Numbers int_array size-or-seq))
  ([size init-val-or-seq] (. clojure.lang.Numbers int_array size init-val-or-seq)))

(defn long-array 
  "Creates an array of ints"
  {:inline (fn [& args] `(. clojure.lang.Numbers long_array ~@args))
   :inline-arities #{1 2}}
  ([size-or-seq] (. clojure.lang.Numbers long_array size-or-seq))
  ([size init-val-or-seq] (. clojure.lang.Numbers long_array size init-val-or-seq)))

(definline floats
  "Casts to float[]"
  [xs] `(. clojure.lang.Numbers floats ~xs))

(definline ints
  "Casts to int[]"
  [xs] `(. clojure.lang.Numbers ints ~xs))

(definline doubles
  "Casts to double[]"
  [xs] `(. clojure.lang.Numbers doubles ~xs))

(definline longs
  "Casts to long[]"
  [xs] `(. clojure.lang.Numbers longs ~xs))

(import '(java.util.concurrent BlockingQueue LinkedBlockingQueue))

(defn seque
  "Creates a queued seq on another (presumably lazy) seq s. The queued
  seq will produce a concrete seq in the background, and can get up to
  n items ahead of the consumer. n-or-q can be an integer n buffer
  size, or an instance of java.util.concurrent BlockingQueue. Note
  that reading from a seque can block if the reader gets ahead of the
  producer."
  ([s] (seque 100 s))
  ([n-or-q s]
   (let [#^BlockingQueue q (if (instance? BlockingQueue n-or-q)
                             n-or-q
                             (LinkedBlockingQueue. (int n-or-q)))
         NIL (Object.) ;nil sentinel since LBQ doesn't support nils
         agt (agent (seq s))
         fill (fn [s]
                (try
                  (loop [[x & xs :as s] s]
                    (if s
                      (if (.offer q (if (nil? x) NIL x))
                        (recur xs)
                        s)
                      (.put q q))) ; q itself is eos sentinel
                  (catch Exception e
                    (.put q q)
                    (throw e)))) 
         drain (fn drain []
                 (let [x (.take q)]
                   (if (identical? x q) ;q itself is eos sentinel
                     @agt  ;will be nil - touch agent just to propagate errors
                     (do
                       (send-off agt fill)
                       (lazy-cons (if (identical? x NIL) nil x) (drain))))))]
     (send-off agt fill)
     (drain))))

(defn class?
  "Returns true if x is an instance of Class"
  [x] (instance? Class x))

(defn alter-var-root
  "Atomically alters the root binding of var v by applying f to its
  current value plus any args"
  [#^clojure.lang.Var v f & args] (.alterRoot v f args))

(defn make-hierarchy 
  "Creates a hierarchy object for use with derive, isa? etc."
  [] {:parents {} :descendants {} :ancestors {}})

(def #^{:private true}
     global-hierarchy (make-hierarchy))

(defn not-empty 
  "If coll is empty, returns nil, else coll"
  [coll] (when (seq coll) coll))

(defn bases
  "Returns the immediate superclass and direct interfaces of c, if any"
  [#^Class c]
  (let [i (.getInterfaces c)
        s (.getSuperclass c)]
    (not-empty
     (if s (cons s i) i))))

(defn supers 
  "Returns the immediate and indirect superclasses and interfaces of c, if any"
  [#^Class class]
  (loop [ret #{} c class]
    (if c
      (recur (into ret (bases c)) (.getSuperclass c))
      (not-empty ret))))

(defn isa?
  "Returns true if (= child parent), or child is directly or indirectly derived from
  parent, either via a Java type inheritance relationship or a
  relationship established via derive. h must be a hierarchy obtained
  from make-hierarchy, if not supplied defaults to the global
  hierarchy"
  ([child parent] (isa? global-hierarchy child parent))
  ([h child parent]
   (or (= child parent)
       (and (class? parent) (class? child) 
            (. #^Class parent isAssignableFrom child))
       (contains? ((:ancestors h) child) parent)
       (and (class? child) (some #(contains? ((:ancestors h) %) parent) (supers child)))
       (and (vector? parent) (vector? child)
            (= (count parent) (count child))
            (loop [ret true i 0]
              (if (= i (count parent))
                ret
                (recur (and (isa? (child i) (parent i)) ret) (inc i))))))))

(defn parents
  "Returns the immediate parents of tag, either via a Java type
  inheritance relationship or a relationship established via derive. h
  must be a hierarchy obtained from make-hierarchy, if not supplied
  defaults to the global hierarchy"
  ([tag] (parents global-hierarchy tag))
  ([h tag] (not-empty
            (let [tp (get (:parents h) tag)]
              (if (class? tag)
                (into (set (bases tag)) tp) 
                tp)))))

(defn ancestors 
  "Returns the immediate and indirect parents of tag, either via a Java type
  inheritance relationship or a relationship established via derive. h
  must be a hierarchy obtained from make-hierarchy, if not supplied
  defaults to the global hierarchy"
  ([tag] (ancestors global-hierarchy tag))
  ([h tag] (not-empty
            (let [ta (get (:ancestors h) tag)]
              (if (class? tag)
                (into (set (supers tag)) ta) 
                ta)))))

(defn descendants
  "Returns the immediate and indirect children of tag, either via a Java type
  inheritance relationship or a relationship established via derive. h
  must be a hierarchy obtained from make-hierarchy, if not supplied
  defaults to the global hierarchy" 
  ([tag] (descendants global-hierarchy tag))
  ([h tag] (if (class? tag)
             (throw (java.lang.UnsupportedOperationException. "Can't get descendants of classes"))
             (not-empty (get (:descendants h) tag)))))

(defn derive
  "Establishes a parent/child relationship between parent and
  tag. Parent must be a namespace-qualified symbol or keyword and
  child can be either a namespace-qualified symbol or keyword or a
  class. h must be a hierarchy obtained from make-hierarchy, if not
  supplied defaults to, and modifies, the global hierarchy."
  ([tag parent] (alter-var-root #'global-hierarchy derive tag parent) nil)
  ([h tag parent]
   (assert (not= tag parent))
   (assert (or (class? tag) (and (instance? clojure.lang.Named tag) (namespace tag))))
   (assert (instance? clojure.lang.Named parent))
   (assert (namespace parent))

   (let [tp (:parents h)
         td (:descendants h)
         ta (:ancestors h)
         tf (fn [m source sources target targets]
              (reduce (fn [ret k]
                        (assoc ret k 
                               (reduce conj (get targets k #{}) (cons target (targets target)))))
                      m (cons source (sources source))))]
     (or 
      (when-not (contains? (tp tag) parent)
        (when (contains? (ta tag) parent)
          (throw (Exception. (print-str tag "already has" parent "as ancestor"))))
        (when (contains? (ta parent) tag)
          (throw (Exception. (print-str "Cyclic derivation:" parent "has" tag "as ancestor"))))        
        {:parents (assoc (:parents h) tag (conj (get tp tag #{}) parent))     
         :ancestors (tf (:ancestors h) tag td parent ta)
         :descendants (tf (:descendants h) parent ta tag td)})
      h))))

(defn underive 
  "Removes a parent/child relationship between parent and
  tag. h must be a hierarchy obtained from make-hierarchy, if not
  supplied defaults to, and modifies, the global hierarchy."
  ([tag parent] (alter-var-root #'global-hierarchy underive tag parent) nil)
  ([h tag parent]
   (let [tp (:parents h)
         td (:descendants h)
         ta (:ancestors h)
         tf (fn [m source sources target targets]
              (reduce
               (fn [ret k]
                 (assoc ret k 
                        (reduce disj (get targets k) (cons target (targets target)))))
               m (cons source (sources source))))]
     (if (contains? (tp tag) parent)
       {:parent (assoc (:parents h) tag (disj (get tp tag) parent))    
        :ancestors (tf (:ancestors h) tag td parent ta)
        :descendants (tf (:descendants h) parent ta tag td)}
       h))))


(defn distinct?
  "Returns true if no two of the arguments are equal"
  {:tag Boolean}
  ([x] true)
  ([x y] (not (= x y)))
  ([x y & more]
   (if (not= x y)
     (loop [s #{x y} [x & etc :as xs] more]
       (if xs
         (if (contains? s x)
           false
           (recur (conj s x) etc))
         true))
     false)))

(defn iterator-seq 
  "Returns a seq on a java.util.Iterator. Note that most collections
  providing iterators implement Iterable and thus support seq directly."
  [iter]
  (clojure.lang.IteratorSeq/create iter))

(defn enumeration-seq 
  "Returns a seq on a java.lang.Enumeration"
  [e]
  (clojure.lang.EnumerationSeq/create e))

(defn format
  "Formats a string using java.lang.String.format, see java.util.Formatter for format
  string syntax"
  [fmt & args]
  (String/format fmt (to-array args)))

(defn printf
  "Prints formatted output, as per format"
  [fmt & args]
  (print (apply format fmt args)))

(defmacro ns
  "Sets *ns* to the namespace named by name (unevaluated), creating it if needed. 
  If the ns didn't already exist, refers the clojure namespace"
  [name]
  `(let [existed# (clojure.lang.Namespace/find '~name)]
     (in-ns '~name)
     (when-not existed#
       (clojure/refer '~'clojure))))

(defmacro defonce 
  "defs name to have the root value of the expr iff the named var has no root value, 
  else expr is unevaluated"
  [name expr]
  `(let [v# (def ~name)]
     (when-not (.hasRoot v#)
       (def ~name ~expr))))
