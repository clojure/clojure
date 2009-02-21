;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure.core)

(def unquote)
(def unquote-splicing)

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

(def   ;;;  Had do change Class to Type and isInstance to IsInstanceOfType
 #^{:arglists '([#^Class c x])
    :doc "Evaluates x and tests if it is an instance of the class
    c. Returns true or false"}
 instance? (fn instance? [#^Type c x] (. c (IsInstanceOfType x))))
 
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
        (if (instance? clojure.lang.IMeta x)
          (. #^clojure.lang.IMeta x (meta)))))

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
;;; The following didn't work, I've handled the few uses below as special cases.
;;;(defn cast
;;;  "Throws a ClassCastException if x is not a c, else returns x."
;;;  [#^Type c x]   ;;; changed Class to Type
;;;   (. clojure.lang.RT (Cast c x)))               ;;;  original (. c (cast x)))     

(defn to-array
  "Returns an array of Objects containing the contents of coll, which
  can be any Collection.  Maps to java.util.Collection.toArray()."
  {:tag "Object[]" }                                                  ;;;{:tag "[Ljava.lang.Object;"}  
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
  ([] (. clojure.lang.PersistentTreeMap EMPTY))  ;;; I HAD TO ADD THIS EXTRA CASE TO AVOID AMBIGUOUS CALL TO CREATE WITH NULL
  ([& keyvals]    (. clojure.lang.PersistentTreeMap (create keyvals))))

(defn sorted-set  
  "Returns a new sorted set with supplied keys."
  ([] (. clojure.lang.PersistentTreeSet EMPTY))  ;;; I HAD TO ADD THIS EXTRA CASE TO AVOID AMBIGUOUS CALL TO CREATE WITH NULL
  ([& keys]   (. clojure.lang.PersistentTreeSet (create keys))))

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
                  (list '. (list 'var name) '(setMacro))
                  (list 'var name))))

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
   (if (nil? x) "" (. x (ToString))))   ;; java: toString
  ([x & ys]
     ((fn [#^StringBuilder sb more]
          (if more
            (recur (. sb  (Append (str (first more)))) (rest more))  ;; java: append
            (str sb)))
      (new StringBuilder #^String (str x)) ys)))


(defn symbol?
  "Return true if x is a Symbol"
  [x] (instance? clojure.lang.Symbol x))

(defn keyword?
  "Return true if x is a Keyword"
  [x] (instance? clojure.lang.Keyword x))

(defn symbol
  "Returns a Symbol with the given namespace and name."
  ([name] (if (symbol? name) name (. clojure.lang.Symbol (intern name))))
  ([ns name] (. clojure.lang.Symbol (intern ns name))))

(defn keyword
  "Returns a Keyword with the given namespace and name.  Do not use :
  in the keyword strings, it will be added automatically."
  ([name] (if (keyword? name) name (. clojure.lang.Keyword (intern nil name))))
  ([ns name] (. clojure.lang.Keyword (intern ns name))))

(defn gensym
  "Returns a new symbol with a unique name. If a prefix string is
  supplied, the name is prefix# where # is some unique number. If
  prefix is not supplied, the prefix is 'G__'."
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
            (if (rest clauses)
                (second clauses)
                (throw (ArgumentException.                           ;;;IllegalArgumentException.
                         "cond requires an even number of forms")))
            (cons 'clojure.core/cond (rest (rest clauses))))))           

(defn spread
  {:private true}
  [arglist]
  (cond
   (nil? arglist) nil
   (nil? (rest arglist)) (seq (first arglist))
   :else (cons (first arglist) (spread (rest arglist)))))

(defn apply
  "Applies fn f to the argument list formed by prepending args to argseq."
  { :arglists '([f args* argseq])}    
  [#^clojure.lang.IFn f & args]
    (. f (applyTo (spread args))))
    
(defn vary-meta
 "Returns an object of the same type and value as obj, with
  (apply f (meta obj) args) as its metadata."
 [obj f & args]
  (with-meta obj (apply f (meta obj) args)))

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
  [ first restfn]
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

;(defmacro lazy-seq  ;;; THIS IS COMMENTED OUT IN THE JAVA VERSION
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
(defmacro if-not
  "Evaluates test. If logical false, evaluates and returns then expr, otherwise else expr, if supplied, else nil."
  ([test then] `(if-not ~test ~then nil))
  ([test then else]
   `(if (not ~test) ~then ~else)))
   
(defn =
  "Equality. Returns true if x equals y, false if not. Same as
  Java x.equals(y) except it also works for nil, and compares
  numbers and collections in a type-independent manner.  Clojure's immutable data
  structures define equals() (and thus =) as a value, not an identity,
  comparison."
  {:tag Boolean
   :inline (fn [x y] `(. clojure.lang.Util equiv ~x ~y))  
   :inline-arities #{2}}
  ([x] true)
  ([x y] (clojure.lang.Util/equiv x y))
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
  for nil, and compares numbers and collections in a type-independent 
  manner. x must implement Comparable"
  {:tag Int32  ; was Integer
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
  ([x] (. clojure.lang.RT (NumberCast x)))         ;; (cast Number x))
  ([x y] (. clojure.lang.Numbers (add x y)))
  ([x y & more]
   (reduce + (+ x y) more)))

(defn *
  "Returns the product of nums. (*) returns 1."
  {:inline (fn [x y] `(. clojure.lang.Numbers (multiply ~x ~y)))
   :inline-arities #{2}}
  ([] 1)
  ([x] (. clojure.lang.RT (NumberCast x)))         ;; (cast Number x))
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

(defn unchecked-remainder
  "Returns the remainder of division of x by y, both int or long.
  Note - uses a primitive operator subject to truncation."
  {:inline (fn [x y] `(. clojure.lang.Numbers (unchecked_remainder ~x ~y)))}
  [x y] (. clojure.lang.Numbers (unchecked_remainder x y)))

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
  "remainder of dividing numerator by denominator."
  [num div]
    (. clojure.lang.Numbers (remainder num div)))

(defn rationalize
  "returns the rational value of num"
  [num]
  (. clojure.lang.Numbers (rationalize num)))

;;Bit ops

(defn bit-not
  "Bitwise complement"
  {:inline (fn [x] `(. clojure.lang.Numbers (not ~x)))}
  [x] (. clojure.lang.Numbers not x))


(defn bit-and
  "Bitwise and"
   {:inline (fn [x y] `(. clojure.lang.Numbers (and ~x ~y)))}
  [x y] (. clojure.lang.Numbers and x y))

(defn bit-or
  "Bitwise or"
  {:inline (fn [x y] `(. clojure.lang.Numbers (or ~x ~y)))}
  [x y] (. clojure.lang.Numbers or x y))

(defn bit-xor
  "Bitwise exclusive or"
  {:inline (fn [x y] `(. clojure.lang.Numbers (xor ~x ~y)))}
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

(defn even?
  "Returns true if n is even, throws an exception if n is not an integer"
  [n] (zero? (bit-and n 1)))

(defn odd?
  "Returns true if n is odd, throws an exception if n is not an integer"
  [n] (not (even? n)))


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
  "Returns true if key is present in the given collection, otherwise
  returns false.  Note that for numerically indexed collections like
  vectors and Java arrays, this tests if the numeric key is within the
  range of indexes. 'contains?' operates constant or logarithmic time;
  it will not perform a linear search for a value.  See also 'some'."
  [coll key] (. clojure.lang.RT (contains coll key)))

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
  [#^clojure.lang.IMapEntry e]  ;;  [#^java.util.Map$Entry e]
    (. e (key)))                ;; (. e (getKey)))

(defn val
  "Returns the value in the map entry."
  [#^clojure.lang.IMapEntry e]  ;;  [#^java.util.Map$Entry e]
    (. e (val)))                ;; (. e (getValue)))

(defn rseq
  "Returns, in constant time, a sequence of the items in rev (which
  can be a vector or sorted-map), in reverse order."
  [#^clojure.lang.Reversible rev]
    (. rev (rseq)))

(defn name
  "Returns the name String of a symbol or keyword."
  {:tag String}
  [#^clojure.lang.Named x]
    (. x (getName)))

(defn namespace
  "Returns the namespace String of a symbol or keyword, or nil if not present."
  {:tag String}
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
  "Threads the expr through the forms. Inserts x as the
  second item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  second item in second form, etc."
  ([x form] (if (seq? form)
              `(~(first form) ~x ~@(rest form))
              (list form x)))
  ([x form & more] `(-> (-> ~x ~form) ~@more)))

;;multimethods
(def global-hierarchy)

(defmacro defmulti
  "Creates a new multimethod with the associated dispatch function. 
  The docstring and attribute-map are optional.  
  
  Options are key-value pairs and may be one of:
    :default    the default dispatch value, defaults to :default
    :hierarchy  the isa? hierarchy to use for dispatching
                defaults to the global hierarchy"
  {:arglists '([name docstring? attr-map? dispatch-fn & options])}
  [mm-name & options]
  (let [docstring   (if (string? (first options))
                      (first options)
                      nil)
        options     (if (string? (first options))
                      (rest options)
                      options)
        m           (if (map? (first options))
                      (first options)
                      {})
        options     (if (map? (first options))
                      (rest options)
                      options)
        dispatch-fn (first options)
        options     (rest options)
        m           (assoc m :tag 'clojure.lang.MultiFn)
        m           (if docstring
                      (assoc m :doc docstring)
                      m)
        m           (if (meta mm-name)
                      (conj (meta mm-name) m)
                      m)]
    (when (= (count options) 1)
      (throw (Exception. "The syntax for defmulti has changed. Example: (defmulti name dispatch-fn :default dispatch-value)")))
    (let [options   (apply hash-map options)
          default   (get options :default :default)
          hierarchy (get options :hierarchy #'global-hierarchy)]
      `(def ~(with-meta mm-name m)
         (new clojure.lang.MultiFn ~dispatch-fn ~default ~hierarchy)))))

(defmacro defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value. "
  [multifn dispatch-val & fn-tail]
  `(. ~multifn addMethod ~dispatch-val (fn ~@fn-tail)))

(defn remove-method
  "Removes the method of multimethod associated	with dispatch-value."
 [multifn dispatch-val]
 (. multifn removeMethod dispatch-val))

(defn prefer-method
  "Causes the multimethod to prefer matches of dispatch-val-x over dispatch-val-y when there is a conflict"
  [multifn dispatch-val-x dispatch-val-y]
  (. multifn preferMethod dispatch-val-x dispatch-val-y))

(defn methods
  "Given a multimethod, returns a map of dispatch values -> dispatch fns"
  [#^clojure.lang.MultiFn multifn] (.getMethodTable multifn))

(defn prefers
  "Given a multimethod, returns a map of preferred value -> set of other values"
  [#^clojure.lang.MultiFn multifn] (.getMethodTable multifn))
  
;;;;;;;;; var stuff

(defmacro #^{:private true} assert-args [fnname & pairs]
  `(do (when-not ~(first pairs)
         (throw (ArgumentException.                            ;;;IllegalArgumentException.
                  ~(str fnname " requires " (second pairs)))))
     ~(let [more (rrest pairs)]
        (when more
          (list* `assert-args fnname more)))))

(defmacro binding
  "binding => var-symbol init-expr

  Creates new bindings for the (already-existing) vars, with the
  supplied initial values, executes the exprs in an implicit do, then
  re-establishes the bindings that existed before."
  [bindings & body]
    (assert-args binding
      (vector? bindings) "a vector for its binding"
      (even? (count bindings)) "an even number of forms in binding vector")
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
(defn #^{:private true}
  setup-reference [#^clojure.lang.ARef r options]
  (let [opts (apply hash-map options)]
    (when (:meta opts)
      (.resetMeta r (:meta opts)))
    (when (:validator opts)
      (.setValidator r (:validator opts)))
    r))
    
(defn agent
  "Creates and returns an agent with an initial value of state and 
  zero or more options (in any order):

  :meta metadata-map

  :validator validate-fn

  If metadata-map is supplied, it will be come the metadata on the
  agent. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an exception."
  ([state] (new clojure.lang.Agent state))
  ([state & options] 
      (setup-reference (agent state) options)))

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

(defn release-pending-sends
  "Normally, actions sent directly or indirectly during another action
  are held until the action completes (changes the agent's
  state). This function can be used to dispatch any pending sent
  actions immediately. This has no impact on actions sent during a
  transaction, which are still held until commit. If no action is
  occurring, does nothing. Returns the number of actions dispatched."
  [] (clojure.lang.Agent/releasePendingSends))

(defn add-watcher
  "Experimental.
  Adds a watcher to an agent/atom/var/ref reference. The watcher must
  be an Agent, and the action a function of the agent's state and one
  additional arg, the reference. Whenever the reference's state
  changes, any registered watchers will have their actions
  sent. send-type must be one of :send or :send-off. The actions will
  be sent after the reference's state is changed. Var watchers are
  triggered only by root binding changes, not thread-local set!s"
  [#^clojure.lang.IRef reference send-type watcher-agent action-fn]
  (.addWatch reference watcher-agent action-fn (= send-type :send-off)))

(defn remove-watcher
  "Experimental.
  Removes a watcher (set by add-watcher) from a reference"
  [#^clojure.lang.IRef reference watcher-agent]
  (.removeWatch reference watcher-agent))

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
  "Creates and returns a Ref with an initial value of x and zero or
  more options (in any order):

  :meta metadata-map

  :validator validate-fn

  If metadata-map is supplied, it will be come the metadata on the
  ref. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an exception. validate-fn will be called on
  transaction commit, when all refs have their final values."
  ([x] (new clojure.lang.Ref x))
  ([x & options] (setup-reference (ref x) options)))

(defn deref
  "Also reader macro: @ref/@agent/@var/@atom/@delay/@future. Within a transaction,
  returns the in-transaction-value of ref, else returns the
  most-recently-committed value of ref. When applied to a var, agent
  or atom, returns its current state. When applied to a delay, forces
  it if not already forced. When applied to a future, will block if
  computation not complete" 
  [#^clojure.lang.IDeref ref] (.deref ref))
 
(defn atom
  "Creates and returns an Atom with an initial value of x and zero or
  more options (in any order):

  :meta metadata-map

  :validator validate-fn

  If metadata-map is supplied, it will be come the metadata on the
  atom. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an exception."
  ([x] (new clojure.lang.Atom x))
  ([x & options] (setup-reference (atom x) options)))

(defn swap!
  "Atomically swaps the value of atom to be:
  (apply f current-value-of-atom args). Note that f may be called
  multiple times, and thus should be free of side effects.  Returns
  the value that was swapped in."
  ([#^clojure.lang.Atom atom f] (.swap atom f))
  ([#^clojure.lang.Atom atom f x] (.swap atom f x))
  ([#^clojure.lang.Atom atom f x y] (.swap atom f x y))
  ([#^clojure.lang.Atom atom f x y & args] (.swap atom f x y args)))
  
(defn compare-and-set!
  "Atomically sets the value of atom to newval if and only if the
  current value of the atom is identical to oldval. Returns true if
  set happened, else false"
  [#^clojure.lang.Atom atom oldval newval] (.compareAndSet atom oldval newval))

(defn reset!
  "Sets the value of atom to newval without regard for the
  current value. Returns newval."
  [#^clojure.lang.Atom atom newval] (.reset atom newval))

(defn set-validator
  "Sets the validator-fn for a var/ref/agent/atom. validator-fn must be nil or a
  side-effect-free fn of one argument, which will be passed the intended
  new state on any state change. If the new state is unacceptable, the
  validator-fn should return false or throw an exception. If the current state (root
  value if var) is not acceptable to the new validator, an exception
  will be thrown and the validator will not be changed."
  [#^clojure.lang.IRef iref validator-fn] (. iref (setValidator validator-fn)))

(defn get-validator
  "Gets the validator-fn for a var/ref/agent/atom."
 [#^clojure.lang.IRef iref] (. iref (getValidator)))

(defn alter-meta!
  "Atomically sets the metadata for a namespace/var/ref/agent/atom to be: 

  (apply f its-current-meta args) 

  f must be free of side-effects"
 [#^clojure.lang.IReference iref f & args] (.alterMeta iref f args))

(defn reset-meta!
  "Atomically resets the metadata for a namespace/var/ref/agent/atom"
 [#^clojure.lang.IReference iref metadata-map] (.resetMeta iref metadata-map))
 
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
    (. ref (deref)))

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


(defmacro io!
  "If an io! block occurs in a transaction, throws an
  IllegalStateException, else runs body in an implicit do. If the
  first expression in body is a literal string, will use that as the
  exception message."
  [& body]
  (let [message (when (string? (first body)) (first body))
        body (if message (rest body) body)]
    `(if (clojure.lang.LockingTransaction/isRunning)
       (throw (new InvalidOperationException ~(or message "I/O in transaction")))   ;;; IllegalStateException
       (do ~@body))))
       
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
  else nil.  One common idiom is to use a set as pred, for example
  this will return true if :fred is in the sequence, otherwise nil:
  (some #{:fred} coll)"
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
  ([f c1 c2]
   (when (and (seq c1) (seq c2))
     (lazy-cons (f (first c1) (first c2))
                (map f (rest c1) (rest c2)))))
  ([f c1 c2 c3]
   (when (and (seq c1) (seq c2) (seq c3))
     (lazy-cons (f (first c1) (first c2) (first c3))
                (map f (rest c1) (rest c2) (rest c3)))))
  ([f c1 c2 c3 & colls]
   (let [step (fn step [cs]
                  (when (every? seq cs)
                    (lazy-cons (map first cs) (step (map rest cs)))))]
     (map #(apply f %) (step (conj colls c3 c2 c1))))))

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

(defn remove
  "Returns a lazy seq of the items in coll for which
  (pred item) returns false. pred must be free of side-effects."
  [pred coll]
    (when (seq coll)
      (if (pred (first coll))
        (recur pred (rest coll))
        (lazy-cons (first coll) (remove pred (rest coll))))))

(defn take
  "Returns a lazy seq of the first n items in coll, or all items if
  there are fewer than n."
  [n coll]
    (when (and (pos? n) (seq coll))
      (lazy-cons (first coll) (when (> n 1) (take (dec n) (rest coll))))))

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
  ([end] (if (and (> end 0) (< end  (. Int32 MaxValue)))                  ;;;(. Integer MAX_VALUE))) 
           (new clojure.lang.Range 0 end)
           (take end (iterate inc 0))))
  ([start end] (if (and (< start end)
                        (>= start (. Int32 MinValue))       ;;;(. Integer MIN_VALUE 
                        (<= end (. Int32 MaxValue)))         ;;;(. Integer MAX_VALUE)))
                 (new clojure.lang.Range start end)
                 (take (- end start) (iterate inc start))))
  ([start end step]
   (take-while (partial (if (pos? step) > <) end) (iterate (partial + step) start))))

(defn merge
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping from
  the latter (left-to-right) will be the mapping in the result."
  [& maps]
  (when (some identity maps)
    (reduce #(conj (or %1 {}) %2) maps)))

(defn merge-with
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f val-in-result val-in-latter)."
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
			(let [k (key e) v (val e)]
			  (if (contains? m k)
			    (assoc m k (f (m k) v)) 
			    (assoc m k v))))
          merge2 (fn [m1 m2]
		   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))



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
  [#^System.IO.TextReader rdr ] ;;;  [#^java.io.BufferedReader rdr]
    (let [line  (. rdr (ReadLine))]   ;;; was (readLine)
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
  ([coll]
   (sort compare coll))
  ([comp coll]     ;;;   We can't pass in a Comparator directly at this point, only a ClojureRuntimeDelegate :  [#^java.util.Comparator comp coll]
   (when (and coll (not (zero? (count coll))))
     (let [a (to-array coll)]
       (. clojure.lang.RT (SortArray a comp))   ;;; see above: (. java.util.Arrays (sort a comp))
       (seq a)))))

(defn sort-by
  "Returns a sorted sequence of the items in coll, where the sort
  order is determined by comparing (keyfn item).  If no comparator is
  supplied, uses compare. comparator must
  implement java.util.Comparator."
  ([keyfn coll]
   (sort-by keyfn compare coll))
  ([keyfn comp coll]   ;;; --- Can't pass a Comparator directly: [keyfn #^java.util.Comparator comp coll]
   (sort (fn [x y] (comp (keyfn x) (keyfn y))) coll)))  ;;;(sort (fn [x y] (. comp (compare (keyfn x) (keyfn y)))) coll)))

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

;; evaluation

(defn eval
  "Evaluates the form data structure (not text!) and returns the result."
  [form] (. clojure.lang.Compiler (eval form)))

(defmacro doseq
  "Repeatedly executes body (presumably for side-effects) with
  bindings and filtering as provided by \"for\".  Does not retain
  the head of the sequence. Returns nil."
  [seq-exprs & body]
  (assert-args doseq
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [groups (reduce (fn [groups p]
                        (if (keyword? (first p))
                          (conj (pop groups) (apply assoc (peek groups) p))
                          (conj groups {:bind (first p) :seq (second p)})))
                      [] (partition 2 seq-exprs))
        emit (fn emit [group & more-groups]
               `(loop [sq# (seq ~(:seq group))]
                  (when sq#
                    (let [~(:bind group) (first sq#)]
                      (when ~(or (:while group) true)
                        (when ~(or (:when group) true)
                          ~(if more-groups
                             (apply emit more-groups)
                             `(do ~@body)))
                        (recur (rest sq#)))))))]
    (apply emit groups)))

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
; Need to figure out how to do this in CLR. Should be some kind of event handle
;(defn await
;  "Blocks the current thread (indefinitely!) until all actions
;  dispatched thus far, from this thread or agent, to the agent(s) have
;  occurred."
;  [& agents]
;  (io! "await in transaction"
;    (when *agent*
;      (throw (new Exception "Can't await in agent action")))
;    (let [latch (new java.util.concurrent.CountDownLatch (count agents))
;          count-down (fn [agent] (. latch (countDown)) agent)]
;      (doseq [agent agents]
;        (send agent count-down))
;      (. latch (await)))))
;
;(defn await1 [#^clojure.lang.Agent a]
;  (when (pos? (.getQueueCount a))
;    (await a))
;    a)
;
;(defn await-for
;  "Blocks the current thread until all actions dispatched thus
;  far (from this thread or agent) to the agents have occurred, or the
;  timeout (in milliseconds) has elapsed. Returns nil if returning due
;  to timeout, non-nil otherwise."
;  [timeout-ms & agents]
;    (io! "await-for in transaction"
;     (when *agent*
;       (throw (new Exception "Can't await in agent action")))
;     (let [latch (new java.util.concurrent.CountDownLatch (count agents))
;           count-down (fn [agent] (. latch (countDown)) agent)]
;       (doseq [agent agents]
;           (send agent count-down))
;       (. latch (await  timeout-ms (. java.util.concurrent.TimeUnit MILLISECONDS))))))

(defmacro dotimes
  "bindings => name n

  Repeatedly executes body (presumably for side-effects) with name
  bound to integers from 0 through n-1."
  [bindings & body]
  (assert-args dotimes
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [i (first bindings)
        n (second bindings)]
    `(let [n# (int ~n)]
       (loop [~i (int 0)]
         (when (< ~i n#)
           ~@body
           (recur (unchecked-inc ~i)))))))
  
(defn import
  "import-list => (package-symbol class-name-symbols*)

  For each name in class-name-symbols, adds a mapping from name to the
  class named by package.name to the current namespace. Use :import in the ns
  macro in preference to calling this directly."
  [& import-symbols-or-lists]
    (let [#^clojure.lang.Namespace ns *ns*]
      (doseq [spec import-symbols-or-lists]
        (if (symbol? spec)
          (let [n (name spec)
                dot (.lastIndexOf n (. clojure.lang.RT (intCast \.)))
                c (symbol (.substring n (inc dot)))]
            (. ns (importClass c (. clojure.lang.RT (classForName (name spec))))))              
          (let [pkg (first spec)
                classes (rest spec)]
            (doseq [c classes]
              (. ns (importClass c (. clojure.lang.RT (classForName (str pkg "." c)))))))))))    


(defn into-array
  "Returns an array with components set to the values in aseq. The array's
  component type is type if provided, or the type of the first value in
  aseq if present, or Object. All values in aseq must be compatible with
  the component type. Class objects for the primitive types can be obtained
  using, e.g., Integer/TYPE."
  ([aseq]
     (clojure.lang.RT/seqToTypedArray (seq aseq)))
  ([type aseq]
     (clojure.lang.RT/seqToTypedArray type (seq aseq))))

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

(defn #^Type class               ;;;#^Class class
  "Returns the Class of x"
  [#^Object x] (if (nil? x) x (. x (GetType))))  ;;; getClass => GetType
;;; Don't know what to do with this.  No equivalent to Number in CLR.
;(defn num
;  "Coerce to Number"
;  {:tag Number
;   :inline (fn  [x] `(. clojure.lang.Numbers (num ~x)))}
;  [x] (. clojure.lang.Numbers (num x)))

(defn int                                    ;;; Need to make this handle args out of range
  "Coerce to int"
  {:tag Int32   ;;; Integer
   :inline (fn  [x] `(. clojure.lang.RT (intCast ~x)))}
  [x] (. clojure.lang.RT (intCast x)))
  
  (defn long
  "Coerce to long"
  {:tag Int64    ;;; Long
   :inline (fn  [x] `(. clojure.lang.RT (longCast ~x)))}
  [x] (. clojure.lang.RT (longCast x)))  ;;;    [#^Number x] (. x (longValue)))
  
(defn float
  "Coerce to float"
  {:tag Single   ;;; Float
   :inline (fn  [x] `(. clojure.lang.RT (floatCast ~x)))}
  [x] (. clojure.lang.RT (floatCast x)))  ;;; [#^Number x] (. x (floatValue)))

(defn double
  "Coerce to double"
  {:tag Double
   :inline (fn  [x] `(. clojure.lang.RT (doubleCast ~x)))}
  [x] (. clojure.lang.RT (doubleCast x)))  ;;;   [#^Number x] (. x (doubleValue)))

(defn short
  "Coerce to short"
  {:tag Int16 
   :inline (fn  [x] `(. clojure.lang.RT (shortCast ~x)))}
  [x] (. clojure.lang.RT (shortCast x)))  ;;;   [#^Number x] (. x (shortValue)))

(defn byte
  "Coerce to byte"
  {:tag Byte
   :inline (fn  [x] `(. clojure.lang.RT (byteCast ~x)))}
  [x] (. clojure.lang.RT (byteCast x)))  ;;;   [#^Number x] (. x (byteValue)))

(defn char
  "Coerce to char"
  {:tag Char    ;;; Character
   :inline (fn  [x] `(. clojure.lang.RT (charCast ~x)))}
  [x] (. clojure.lang.RT (charCast x)))  

(defn boolean
  "Coerce to boolean"
  {:tag Boolean
   :inline (fn  [x] `(. clojure.lang.RT (booleanCast ~x)))}
  [x] (if x true false))

(defn number?
  "Returns true if x is a Number"
  [x]
  (. clojure.lang.Util (IsNumeric x)))    ;;; (instance? Number x))

(defn integer?
  "Returns true if n is an integer"
  [n]
  (or (instance? Int32 n)      ;;; Integer
      (instance? Int64 n)         ;;; Long
      (instance? BigInteger n)   (instance? Char n)  ;;; added Char test
      (instance? int 16 n)        ;;; Short
      (instance? Byte n)))

(defn mod
  "modulus of num and div."
  [num div]
  (cond
   (or (not (integer? num)) (not (integer? div)))
     (throw (ArgumentException.                                 ;;; IllegalArgumentException.
           "mod requires two integers"))
   (or (< num 0 div) (< div 0 num)) (+ (rem num div) div)
   :else (rem num div)))

(defn ratio?
  "Returns true if n is a Ratio"
  [n] (instance? clojure.lang.Ratio n))

(defn decimal?
  "Returns true if n is a BigDecimal"
  [n] (instance? BigDecimal n))

(defn float?
  "Returns true if n is a floating point number"
  [n]   
  (or (instance? Double n)
      (instance? Single n)))     ;;; Float

(defn rational? [n]
  "Returns true if n is a rational number"
  (or (integer? n) (ratio? n) (decimal? n)))

(defn bigint
  "Coerce to BigInteger"
  {:tag BigInteger}
  [x] (cond
       (instance? BigInteger x) x
       (decimal? x) (.toBigInteger #^BigDecimal x)
       (number? x) (BigInteger/valueOf (long x))
       :else (BigInteger. x)))

(defn bigdec
  "Coerce to BigDecimal"
  {:tag BigDecimal}
  [x] (cond
       (decimal? x) x
       (float? x) (. BigDecimal valueOf (double x))
       (ratio? x) (/ (BigDecimal. (.numerator x)) (.denominator x))
       (instance? BigInteger x) (BigDecimal. #^BigInteger x)
       (number? x) (BigDecimal/valueOf (long x))
       :else (BigDecimal. x)))

(def #^{:private true} print-initialized false)

(defmulti print-method (fn [x writer] (class x)))
(defmulti print-dup (fn [x writer] (class x)))

(defn pr-on
  {:private true}
  [x w]
  (if *print-dup*
    (print-dup x w)
    (print-method x w))
  nil)

(defn pr
  "Prints the object(s) to the output stream that is the current value
  of *out*.  Prints the object(s), separated by spaces if there is
  more than one.  By default, pr and prn print in a way that objects
  can be read by the reader"
  ([] nil)
  ([x]
     (pr-on x *out*))
  ([x & more]
   (pr x)
   (. *out* (Write \space))  ;; append -> Write
   (apply pr more)))
   
(defn newline
  "Writes a newline to the output stream that is the current value of
  *out*"
  []
    (. *out* (Write \newline))  ;; append -> Write
    nil)

(defn flush 
  "Flushes the output stream that is the current value of
  *out*"
  []
    (. *out* (Flush))             ;; flush => Flush
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


(defn read   ;;; still have an error here, probably from leftover newline causing interference with REPL
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
   (. clojure.lang.LispReader (read stream (boolean eof-error?) eof-value recursive?))))

(defn read-line  ;; ALSO HAS A PROBLEM -- interference from REPL?
  "Reads the next line from stream that is the current value of *in* ."
  [] (. #^System.IO.Reader *in* (ReadLine)))   ;;; readLine => ReadLine     #^java.io.BufferedReader 

(defn read-string
  "Reads one object from the string s"
  [s] (clojure.lang.RT/readString s))

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
   
   (defmacro with-open
  "bindings => name init

  Evaluates body in a try expression with names bound to the values
  of the inits, and a finally clause that calls (.close name) on each
  name in reverse order."
  [bindings & body]
  (assert-args with-open
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
  (cond
    (= (count bindings) 0) `(do ~@body)
    (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                              (try
                                (with-open ~(subvec bindings 2) ~@body)
                                (finally
                                  (. ~(bindings 0) close))))
    :else (throw (ArgumentException.                              ;;;IllegalArgumentException.
                   "with-open only allows Symbols in bindings"))))

(defmacro doto
  "Evaluates x then calls all of the methods and functions with the
  value of x supplied at the from of the given arguments.  The forms
  are evaluated in order.  Returns x.

  (doto (new java.util.HashMap) (.put \"a\" 1) (.put \"b\" 2))"
  [x & forms]
    (let [gx (gensym)]
      `(let [~gx ~x]
         ~@(map (fn [f]
                  (if (seq? f)
                    `(~(first f) ~gx ~@(rest f))
                    `(~f ~gx)))
                forms)
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
  `(let [start# (. clojure.lang.RT (StartStopwatch))   ;;; (. System (nanoTime))       
         ret# ~expr]
     (prn (str "Elapsed time: " (. clojure.lang.RT StopStopwatch) " msecs"))     ;;;(/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
     ret#))



;;; Java version has: (import '(java.lang.reflect Array))

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
;;; We have a real problem with aget/aset -- Java has only single dim arrays, CLR has true multidim.  How to distinguish true multidim from ragged?  For now, treat all as ragged. 
(defn aget
  "Returns the value at the index/indices. Works on Java arrays of all
  types."
  {:inline (fn [a i] `(. clojure.lang.RT (aget ~a ~i)))
   :inline-arities #{2}}
  ([array idx]
   (clojure.lang.Compiler/prepRet (. array (GetValue idx))))  ;;; was  (. Array (get array idx)))  also replaced clojure.lang.Reflector/prepRet
  ([array idx & idxs]
   (apply aget (aget array idx) idxs)))

(defn aset
  "Sets the value at the index/indices. Works on Java arrays of
  reference types. Returns val."
  {:inline (fn [a i v] `(. clojure.lang.RT (aset ~a ~i ~v)))
   :inline-arities #{3}}
  ([array idx val]
   (. array (SetValue val idx))  ;;; was     (. Array (set array idx val))
   val)
  ([array idx idx2 & idxv]
   (apply aset (aget array idx) idx2 idxv)))
;;; Do we really need to do this in CLR?
;(defmacro
;  #^{:private true}
;  def-aset [name method coerce]
;    `(defn ~name
;       {:arglists '([~'array ~'idx ~'val] [~'array ~'idx ~'idx2 & ~'idxv])}
;       ([array# idx# val#]
;        (. Array (~method array# idx# (~coerce val#)))
;        val#)
;       ([array# idx# idx2# & idxv#]
;        (apply ~name (aget array# idx#) idx2# idxv#))))
;
;(def-aset
;  #^{:doc "Sets the value at the index/indices. Works on arrays of int. Returns val."}
;  aset-int setInt int)
;
;(def-aset
;  #^{:doc "Sets the value at the index/indices. Works on arrays of long. Returns val."}
;  aset-long setLong long)
;
;(def-aset
;  #^{:doc "Sets the value at the index/indices. Works on arrays of boolean. Returns val."}
;  aset-boolean setBoolean boolean)
;
;(def-aset
;  #^{:doc "Sets the value at the index/indices. Works on arrays of float. Returns val."}
;  aset-float setFloat float)
;
;(def-aset
;  #^{:doc "Sets the value at the index/indices. Works on arrays of double. Returns val."}
;  aset-double setDouble double)
;
;(def-aset
;  #^{:doc "Sets the value at the index/indices. Works on arrays of short. Returns val."}
;  aset-short setShort short)
;
;(def-aset
;  #^{:doc "Sets the value at the index/indices. Works on arrays of byte. Returns val."}
;  aset-byte setByte byte)
;
;(def-aset
;  #^{:doc "Sets the value at the index/indices. Works on arrays of char. Returns val."}
;  aset-char setChar char)
;;; Another  ragged versus true multidimensional array problem
;(defn make-array
;  "Creates and returns an array of instances of the specified class of
;  the specified dimension(s).  Note that a class object is required.
;  Class objects can be obtained by using their imported or
;  fully-qualified name.  Class objects for the primitive types can be
;  obtained using, e.g., Integer/TYPE."
;  ([#^Class type len]
;   (. Array (newInstance type (int len))))
;  ([#^Class type dim & more-dims]
;   (let [dims (cons dim more-dims)
;         #^"[I" dimarray (make-array (. Integer TYPE)  (count dims))]
;     (dotimes [i (alength dimarray)]
;       (aset-int dimarray i (nth dims i)))
;     (. Array (newInstance type dimarray)))))
;
;(defn to-array-2d
;  "Returns a (potentially-ragged) 2-dimensional array of Objects
;  containing the contents of coll, which can be any Collection of any
;  Collection."
;  {:tag "Object[][]" }   ;;; "[[Ljava.lang.Object;"
;  [#^java.util.Collection coll]
;    (let [ret (make-array (. Class (forName "[Ljava.lang.Object;")) (. coll (size)))]
;      (loop [i 0 xs (seq coll)]
;        (when xs
;          (aset ret i (to-array (first xs)))
;          (recur (inc i) (rest xs))))
;      ret))

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
;;; In current java version, missing basis keys cause an error  
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
 
(defn load-reader
  "Sequentially read and evaluate the set of forms contained in the
  stream/file"
  [rdr] (. clojure.lang.Compiler (load rdr)))

(defn load-string
  "Sequentially read and evaluate the set of forms contained in the
  string"
  [s]
  (let [rdr (-> (System.IO.StringReader. s)     ;;; was (java.io.StringReader. s)
                (clojure.lang.Readers.LineNumberingReader.))]   ;;; was (clojure.lang.LineNumberingPushbackReader.))]
    (load-reader rdr)))
;;; NOT CLEAR WHAT TO WORK AGAINST HERE.  MAYBE THIS SHOULD NOT BE IN THE CORE?
;(defn resultset-seq
;  "Creates and returns a lazy sequence of structmaps corresponding to
;  the rows in the java.sql.ResultSet rs"
;  [#^java.sql.ResultSet rs]
;    (let [rsmeta (. rs (getMetaData))
;          idxs (range 1 (inc (. rsmeta (getColumnCount))))
;          keys (map (comp keyword #(.ToLowerCase #^String %))				;;; .toLowerCase
;                    (map (fn [i] (. rsmeta (getColumnName i))) idxs))
;          row-struct (apply create-struct keys)
;          row-values (fn [] (map (fn [#^Integer i] (. rs (getObject i))) idxs))
;          rows (fn thisfn []
;                   (when (. rs (next))
;		     (lazy-cons (apply struct row-struct (row-values)) (thisfn))))]
;      (rows)))

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
  [sym] (clojure.lang.Namespace/find sym))

(defn create-ns
  "Create a new namespace named by the symbol if one doesn't already
  exist, returns it or the already-existing namespace of the same
  name."
  [sym] (clojure.lang.Namespace/findOrCreate sym))

(defn remove-ns
  "Removes the namespace named by the symbol. Use with caution.
  Cannot be used to remove the clojure namespace."
  [sym] (clojure.lang.Namespace/remove sym))

(defn all-ns
  "Returns a sequence of all namespaces."
  [] (clojure.lang.Namespace/all))

(defn #^clojure.lang.Namespace the-ns 
  "If passed a namespace, returns it. Else, when passed a symbol,
  returns the namespace named by it, throwing an exception if not
  found."
  [x]
  (if (instance? clojure.lang.Namespace x)
    x
    (or (find-ns x) (throw (Exception. (str "No namespace: " x " found"))))))

(defn ns-name
  "Returns the name of the namespace, a symbol."
  [ns]
  (.getName (the-ns ns)))

(defn ns-map
  "Returns a map of all the mappings for the namespace."
  [ns]
  (.getMappings (the-ns ns)))

(defn ns-unmap
  "Removes the mappings for the symbol from the namespace."
  [ns sym]
  (.unmap (the-ns ns) sym))
; commented out in Java original
;(defn export [syms]
;  (doseq [sym syms]
;   (.. *ns* (intern sym) (setExported true))))

(defn ns-publics
  "Returns a map of the public intern mappings for the namespace."
  [ns]
  (let [ns (the-ns ns)]
    (filter-key val (fn [#^clojure.lang.Var v] (and (instance? clojure.lang.Var v)
                                 (= ns (.ns v))
                                 (.isPublic v)))
                (ns-map ns))))

(defn ns-imports
  "Returns a map of the import mappings for the namespace."
  [ns]
  (filter-key val (partial instance? Type) (ns-map ns)))   ;;; Class => Type

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
  clashes. Use :use in the ns macro in preference to calling this directly."
  [ns-sym & filters]
    (let [ns (or (find-ns ns-sym) (throw (new Exception (str "No namespace: " ns-sym))))
          fs (apply hash-map filters)
          nspublics (ns-publics ns)
          rename (or (:rename fs) {})
          exclude (set (:exclude fs))
          to-do (or (:only fs) (keys nspublics))]
      (doseq [sym to-do]
        (when-not (exclude sym)
          (let [v (nspublics sym)]
            (when-not v
              (throw (new InvalidOperationException (str sym " is not public"))))    ;;; java.lang.IllegalAccessError  ==> InvalidOperationException
            (. *ns* (refer (or (rename sym) sym) v)))))))

(defn ns-refers
  "Returns a map of the refer mappings for the namespace."
  [ns]
  (let [ns (the-ns ns)]
    (filter-key val (fn [#^clojure.lang.Var v] (and (instance? clojure.lang.Var v)
                                 (not= ns (.ns v))))
                (ns-map ns))))

(defn ns-interns
  "Returns a map of the intern mappings for the namespace."
  [ns]
  (let [ns (the-ns ns)]
    (filter-key val (fn [#^clojure.lang.Var v] (and (instance? clojure.lang.Var v)
                                 (= ns (.ns v))))
                (ns-map ns))))

(defn alias
  "Add an alias in the current namespace to another
  namespace. Arguments are two symbols: the alias to be used, and
  the symbolic name of the target namespace. Use :as in the ns macro in preference
  to calling this directly."
  [alias namespace-sym]
  (.addAlias *ns* alias (find-ns namespace-sym)))

(defn ns-aliases
  "Returns a map of the aliases for the namespace."
  [ns]
  (.getAliases (the-ns ns)))

(defn ns-unalias
  "Removes the alias for the symbol from the namespace."
  [ns sym]
  (.removeAlias (the-ns ns) sym))

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
  (assert-args with-local-vars
     (vector? name-vals-vec) "a vector for its binding"
     (even? (count name-vals-vec)) "an even number of forms in binding vector")
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
  (clojure.lang.Compiler/maybeResolveIn (the-ns ns) sym))

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
                           (if (seq bs)
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
                           (if (seq bes)
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
  (assert-args let
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
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
    (assert-args loop
      (vector? bindings) "a vector for its binding"
      (even? (count bindings)) "an even number of forms in binding vector")
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
  "bindings => x xs

  Same as (when (seq xs) (let [x (first xs)] body))"
  [bindings & body]
  (assert-args when-first
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [[x xs] bindings]
    `(when (seq ~xs)
       (let [~x (first ~xs)]
         ~@body))))

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
  (assert-args for
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [to-groups (fn [seq-exprs]
                    (reduce (fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (assoc (peek groups) k v))
                                (conj groups {:bind k :seq v})))
                            [] (partition 2 seq-exprs)))
        emit (fn emit [[group & [{next-seq :seq} :as more-groups]]]
		  (let [giter (gensym "iter__") gxs (gensym "s__")]
		    `(fn ~giter [~gxs]
			 (when-first [~(:bind group) ~gxs]
                           (when ~(or (:while group) true)
                             (if ~(or (:when group) true)
                               ~(if more-groups
                                  `(let [iterys# ~(emit more-groups)
                                         fs# (iterys# ~next-seq)]
                                     (if fs#
                                       (lazy-cat fs# (~giter (rest ~gxs)))
                                       (recur (rest ~gxs))))
                                  `(lazy-cons ~expr (~giter (rest ~gxs))))
                              (recur (rest ~gxs))))))))]
    `(let [iter# ~(emit (to-groups seq-exprs))]
	(iter# ~(second seq-exprs))))))

(defmacro comment
  "Ignores body, yields nil"
  [& body])

(defmacro with-out-str
  "Evaluates exprs in a context in which *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
  [& body]
  `(let [s# (new System.IO.StringWriter)]   ;;; Was java.io.StringWriter
     (binding [*out* s#]
       ~@body
       (str s#))))

(defmacro with-in-str
  "Evaluates body in a context in which *in* is bound to a fresh
  StringReader initialized with the string s."
  [s & body]
  `(with-open s# (-> (System.IO.StringReader. ~s) clojure.lang.Readers.LineNumberingReader.)  ;;; were java.io.StringReader & clojure.lang.LineNumberingPushbackReader
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
;;;Need to take a closer look at CLR Regex vs Java Regex
;(defn re-pattern
;  "Returns an instance of java.util.regex.Pattern, for use, e.g. in
;  re-matcher."
;  {:tag java.util.regex.Pattern}
;  [s] (if (instance? java.util.regex.Pattern s)
;        s
;        (. java.util.regex.Pattern (compile s))))
;
;(defn re-matcher
;  "Returns an instance of java.util.regex.Matcher, for use, e.g. in
;  re-find."
;  {:tag java.util.regex.Matcher}
;  [#^java.util.regex.Pattern re s]
;    (. re (matcher s)))
;
;(defn re-groups
;  "Returns the groups from the most recent match/find. If there are no
;  nested groups, returns a string of the entire match. If there are
;  nested groups, returns a vector of the groups, the first element
;  being the entire match."
;  [#^java.util.regex.Matcher m]
;    (let [gc  (. m (groupCount))]
;      (if (zero? gc)
;        (. m (group))
;        (loop [ret [] c 0]
;          (if (<= c gc)
;            (recur (conj ret (. m (group c))) (inc c))
;            ret)))))
;
;(defn re-seq
;  "Returns a lazy sequence of successive matches of pattern in string,
;  using java.util.regex.Matcher.find(), each such match processed with
;  re-groups."
;  [#^java.util.regex.Pattern re s]
;    (let [m (re-matcher re s)]
;      ((fn step []
;           (when (. m (find))
;             (lazy-cons (re-groups m) (step)))))))
;
;(defn re-matches
;  "Returns the match, if any, of string to pattern, using
;  java.util.regex.Matcher.matches().  Uses re-groups to return the
;  groups."
;  [#^java.util.regex.Pattern re s]
;    (let [m (re-matcher re s)]
;      (when (. m (matches))
;        (re-groups m))))
;
;
;(defn re-find
;  "Returns the next regex match, if any, of string to pattern, using
;  java.util.regex.Matcher.find().  Uses re-groups to return the
;  groups."
;  ([#^java.util.regex.Matcher m]
;   (when (. m (find))
;     (re-groups m)))
;  ([#^java.util.regex.Pattern re s]
;   (let [m (re-matcher re s)]
;     (re-find m))))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  1 (exclusive)."
  ([] (. clojure.lang.RT (random)))  ;;; Math ==> RT.  No Math.random in CLR.
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
;;; Needs re-pattern
;(defn find-doc
;  "Prints documentation for any var whose documentation or name
; contains a match for re-string"
;  [re-string]
;    (let [re  (re-pattern re-string)]
;      (dorun (for [ns (all-ns)
;                   v (sort-by (comp :name meta) (vals (ns-interns ns)))
;                   :when (and (:doc ^v)
;                          (or (re-find (re-matcher re (:doc ^v)))
;                              (re-find (re-matcher re (str (:name ^v))))))]
;               (print-doc v)))))

(defn special-form-anchor
  "Returns the anchor tag on http://clojure.org/special_forms for the
  special form x, or nil"
  [x]
  (#{'. 'def 'do 'fn 'if 'let 'loop 'monitor-enter 'monitor-exit 'new
  'quote 'recur 'set! 'throw 'try 'var} x))

(defn syntax-symbol-anchor
  "Returns the anchor tag on http://clojure.org/special_forms for the
  special form that uses syntax symbol x, or nil"
  [x]
  ({'& 'fn 'catch 'try 'finally 'try} x))

(defn print-special-doc
  [name type anchor]
  (println "-------------------------")
  (println name)
  (println type)
  (println (str "  Please see http://clojure.org/special_forms#" anchor)))
;;; None of the doc stuff tested yet -- need printing and RE.
(defn print-namespace-doc
  "Print the documentation string of a Namespace."
  [nspace]
  (println "-------------------------")
  (println (str (ns-name nspace)))
  (println " " (:doc ^nspace)))

(defmacro doc
  "Prints documentation for a var or special form given its name"
  [name]
  (cond
   (special-form-anchor `~name)
   `(print-special-doc '~name "Special Form" (special-form-anchor '~name))
   (syntax-symbol-anchor `~name)
   `(print-special-doc '~name "Syntax Symbol" (syntax-symbol-anchor '~name))
   :else
    (let [nspace (find-ns name)]
      (if nspace
        `(print-namespace-doc ~nspace)
        `(print-doc (var ~name))))))
;;; Not tested yet.  
 (defn tree-seq
  "Returns a lazy sequence of the nodes in a tree, via a depth-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
  tree."  
   [branch? children root]
   (let [walk (fn walk [node]
                (lazy-cons node
                  (when (branch? node)
                    (mapcat walk (children node)))))]
     (walk root)))
;;; This will be harder in the CLR
;(defn file-seq
;  "A tree seq on java.io.Files"
;  [dir]
;    (tree-seq
;     (fn [#^java.io.File f] (. f (isDirectory)))
;     (fn [#^java.io.File d] (seq (. d (listFiles))))
;     dir))
;;; not tested
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
    (contains? (. clojure.lang.Compiler _specials) s))   ;;; specials => _specials, because I'm stubborn

(defn var?
  "Returns true if v is of type clojure.lang.Var"
  [v] (instance? clojure.lang.Var v))

(defn slurp
  "Reads the file named by f into a string and returns it."
  [#^String f]
  (with-open [r (new System.IO.StreamReader f)]   ;;; (new java.io.BufferedReader (new java.io.FileReader f))]
    (let [sb (new StringBuilder)]
      (loop [c (. r (Read))]                      ;;; read -> Read
        (if (neg? c)
          (str sb)
          (do
            (. sb (Append (char c)))              ;;; append -> Append
            (recur (. r (Read)))))))))            ;;; read -> Read

(defn subs
  "Returns the substring of s beginning at start inclusive, and ending
  at end (defaults to length of string), exclusive."
  ([#^String s start] (. s (Substring start)))             ;; substring => Substring
  ([#^String s start end] (. s (Substring start (- end start)))))    ;; was (substring start end) -- different interpretation of second arg

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
                     (if (contains? seen f) (recur r seen)
                         (lazy-cons f (step r (conj seen f))))))]
      (step (seq coll) #{})))
;;; NOT TESTED YET
(defmacro if-let
  "bindings => binding-form test

  If test is true, evaluates then with binding-form bound to the value of test, if not, yields else"
  ([bindings then]
   `(if-let ~bindings ~then nil))
  ([bindings then else & oldform]
   (assert-args if-let
     (and (vector? bindings) (nil? oldform)) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [[form tst] bindings]
     `(let [temp# ~tst]
        (if temp#
          (let [~form temp#]
            ~then)
          ~else)))))
;;; NOT TESTED YET
(defmacro when-let
  "bindings => binding-form test

  When test is true, evaluates body with binding-form bound to the value of test"
  [bindings & body]
  (assert-args when-let
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [[form tst] bindings]
    `(let [temp# ~tst]
       (when temp#
         (let [~form temp#]
           ~@body)))))

(defn replace
  "Given a map of replacement pairs and a vector/collection, returns a
  vector/seq with any elements = a key in smap replaced with the
  corresponding val in smap"
  [smap coll]
    (if (vector? coll)
      (reduce (fn [v i]
                (if-let [e (find smap (nth v i))]
                        (assoc v i (val e))
                        v))
              coll (range (count coll)))
      (map #(if-let [e (find smap %)] (val e) %) coll)))

(defmacro dosync
  "Runs the exprs (in an implicit do) in a transaction that encompasses
  exprs and any nested calls.  Starts a transaction if none is already
  running on this thread. Any uncaught exception will abort the
  transaction and flow out of dosync. The exprs may be run more than
  once, but any effects on Refs will be atomic."
  [& exprs]
  `(sync nil ~@exprs))
;;; Figure out equivalent for CLR
;(defmacro with-precision
;  "Sets the precision and rounding mode to be used for BigDecimal operations.
;
;  Usage: (with-precision 10 (/ 1M 3))
;  or:    (with-precision 10 :rounding HALF_DOWN (/ 1M 3))
;
;  The rounding mode is one of CEILING, FLOOR, HALF_UP, HALF_DOWN,
;  HALF_EVEN, UP, DOWN and UNNECESSARY; it defaults to HALF_UP."
;  [precision & exprs]
;    (let [[body rm] (if (= (first exprs) :rounding)
;                      [(rest (rest exprs))
;                       `((. java.math.RoundingMode ~(second exprs)))]
;                      [exprs nil])]
;      `(binding [*math-context* (java.math.MathContext. ~precision ~@rm)]
;         ~@body))) 

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
       (when-let [[e :as s] (. sc seqFrom key true)]
         (if (include e) s (rest s)))
       (take-while include (. sc seq true)))))
  ([#^clojure.lang.Sorted sc start-test start-key end-test end-key]
   (when-let [[e :as s] (. sc seqFrom start-key true)]
     (take-while (bound-fn sc end-test end-key)
                 (if ((bound-fn sc start-test start-key) e) s (rest s))))))

(defn rsubseq
  "sc must be a sorted collection, test(s) one of <, <=, > or
  >=. Returns a reverse seq of those entries with keys ek for
  which (test (.. sc comparator (compare ek key)) 0) is true"
  ([#^clojure.lang.Sorted sc test key]
   (let [include (bound-fn sc test key)]
     (if (#{< <=} test)
       (when-let [[e :as s] (. sc seqFrom key false)]
         (if (include e) s (rest s)))
       (take-while include (. sc seq false)))))
  ([#^clojure.lang.Sorted sc start-test start-key end-test end-key]
   (when-let [[e :as s] (. sc seqFrom end-key false)]
     (take-while (bound-fn sc start-test start-key)
                 (if ((bound-fn sc end-test end-key) e) s (rest s))))))

(defn repeatedly
  "Takes a function of no args, presumably with side effects, and returns an infinite
  lazy sequence of calls to it"
  [f] (lazy-cons (f) (repeatedly f)))
;;; What is CLR equivalent -- should this just be a no-op?
;(defn add-classpath
;  "Adds the url (String or URL object) to the classpath per URLClassLoader.addURL"
;  [url] (. clojure.lang.RT addURL url))



(defn hash
  "Returns the hash code of its argument"
  [x] (. clojure.lang.Util (hash x)))

(defn interpose
  "Returns a lazy seq of the elements of coll separated by sep"
  [sep coll] (drop 1 (interleave (repeat sep) coll)))

(defmacro definline
  "Experimental - like defmacro, except defines a named function whose
  body is the expansion, calls to which may be expanded inline as if
  it were a macro. Cannot be used with variadic (&) args."
  [name & decl]
  (let [[pre-args [args expr]] (split-with (comp not vector?) decl)]
    `(do
       (defn ~name ~@pre-args ~args ~(apply (eval (list `fn args expr)) args))
       (alter-meta! (var ~name) assoc :inline (fn ~args ~expr))
       (var ~name))))
       
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
;;; How do you use this?  How can you get the value of the array at the current index?
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
;;; NOT WORTH THE EFFORT AT THE MOMENT
;(defn float-array
;  "Creates an array of floats"
;  {:inline (fn [& args] `(. clojure.lang.Numbers float_array ~@args))
;   :inline-arities #{1 2}}
;  ([size-or-seq] (. clojure.lang.Numbers float_array size-or-seq))
;  ([size init-val-or-seq] (. clojure.lang.Numbers float_array size init-val-or-seq)))
;
;(defn double-array
;  "Creates an array of doubles"
;  {:inline (fn [& args] `(. clojure.lang.Numbers double_array ~@args))
;   :inline-arities #{1 2}}
;  ([size-or-seq] (. clojure.lang.Numbers double_array size-or-seq))
;  ([size init-val-or-seq] (. clojure.lang.Numbers double_array size init-val-or-seq)))
;
;(defn int-array
;  "Creates an array of ints"
;  {:inline (fn [& args] `(. clojure.lang.Numbers int_array ~@args))
;   :inline-arities #{1 2}}
;  ([size-or-seq] (. clojure.lang.Numbers int_array size-or-seq))
;  ([size init-val-or-seq] (. clojure.lang.Numbers int_array size init-val-or-seq)))
;
;(defn long-array
;  "Creates an array of ints"
;  {:inline (fn [& args] `(. clojure.lang.Numbers long_array ~@args))
;   :inline-arities #{1 2}}
;  ([size-or-seq] (. clojure.lang.Numbers long_array size-or-seq))
;  ([size init-val-or-seq] (. clojure.lang.Numbers long_array size init-val-or-seq)))
;
;(definline floats
;  "Casts to float[]"
;  [xs] `(. clojure.lang.Numbers floats ~xs))
;
;(definline ints
;  "Casts to int[]"
;  [xs] `(. clojure.lang.Numbers ints ~xs))
;
;(definline doubles
;  "Casts to double[]"
;  [xs] `(. clojure.lang.Numbers doubles ~xs))
;
;(definline longs
;  "Casts to long[]"
;  [xs] `(. clojure.lang.Numbers longs ~xs))
;
;(import '(java.util.concurrent BlockingQueue LinkedBlockingQueue))
;;;NOT WORTH THE EFFORT AT THE MOMENT
;(defn seque
;  "Creates a queued seq on another (presumably lazy) seq s. The queued
;  seq will produce a concrete seq in the background, and can get up to
;  n items ahead of the consumer. n-or-q can be an integer n buffer
;  size, or an instance of java.util.concurrent BlockingQueue. Note
;  that reading from a seque can block if the reader gets ahead of the
;  producer."
;  ([s] (seque 100 s))
;  ([n-or-q s]
;   (let [#^BlockingQueue q (if (instance? BlockingQueue n-or-q)
;                             n-or-q
;                             (LinkedBlockingQueue. (int n-or-q)))
;         NIL (Object.) ;nil sentinel since LBQ doesn't support nils
;         agt (agent (seq s))
;         fill (fn [s]
;                (try
;                  (loop [[x & xs :as s] s]
;                    (if s
;                      (if (.offer q (if (nil? x) NIL x))
;                        (recur xs)
;                        s)
;                      (.put q q))) ; q itself is eos sentinel
;                  (catch Exception e
;                    (.put q q)
;                    (throw e))))
;         drain (fn drain []
;                 (let [x (.take q)]
;                   (if (identical? x q) ;q itself is eos sentinel
;                     @agt  ;will be nil - touch agent just to propagate errors
;                     (do
;                       (send-off agt fill)
;                       (lazy-cons (if (identical? x NIL) nil x) (drain))))))]
;     (send-off agt fill)
;     (drain))))

(defn class?
  "Returns true if x is an instance of Class"
  [x] (instance? Type x))               ;;  Class ==> Type

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
  [#^Type c]                             ;;;  Class ==> Type
  (let [i (.GetInterfaces c)             ;;;  .getInterfaces ==> .GetInterfaces
        s (.BaseType c)]                 ;;;  .getSuperclass ==> BaseType
    (not-empty
     (if s (cons s i) i))))

(defn supers
  "Returns the immediate and indirect superclasses and interfaces of c, if any"
  [#^Type class]                                ;;;  Class ==> Type
  (loop [ret (set (bases class)) cs ret]
    (if (seq cs)
      (let [c (first cs) bs (bases c)]
        (recur (into ret bs) (into (disj cs c) bs)))
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
            (. #^Type parent IsAssignableFrom child))  ;;; Class ==> Type, isAssignableFrom 
       (contains? ((:ancestors h) child) parent)
       (and (class? child) (some #(contains? ((:ancestors h) %) parent) (supers child)))
       (and (vector? parent) (vector? child)
            (= (count parent) (count child))
            (loop [ret true i 0]
              (if (or (not ret) (= i (count parent)))
                ret
                (recur (isa? h (child i) (parent i)) (inc i))))))))

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
;;; NOT TESTED YET
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
;;; NOT TESTED YET
(defn descendants
  "Returns the immediate and indirect children of tag, through a
  relationship established via derive. h must be a hierarchy obtained
  from make-hierarchy, if not supplied defaults to the global
  hierarchy. Note: does not work on Java type inheritance
  relationships."
  ([tag] (descendants global-hierarchy tag))
  ([h tag] (if (class? tag)
             (throw (NotImplementedException. "Can't get descendants of classes"))    ;;; java.lang.UnsupportedOperationException --> NotImplementedException
             (not-empty (get (:descendants h) tag)))))
;;; NOT TESTED YET
(defn derive
  "Establishes a parent/child relationship between parent and
  tag. Parent must be a namespace-qualified symbol or keyword and
  child can be either a namespace-qualified symbol or keyword or a
  class. h must be a hierarchy obtained from make-hierarchy, if not
  supplied defaults to, and modifies, the global hierarchy."
  ([tag parent]
   (assert (namespace parent))
   (assert (or (class? tag) (and (instance? clojure.lang.Named tag) (namespace tag))))

   (alter-var-root #'global-hierarchy derive tag parent) nil)
  ([h tag parent]
   (assert (not= tag parent))
   (assert (or (class? tag) (instance? clojure.lang.Named tag)))
   (assert (instance? clojure.lang.Named parent))

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
;;; NOT TESTED YET
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
  "Returns true if no two of the arguments are ="
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
;;; later (boring)
;(defn iterator-seq
;  "Returns a seq on a java.util.Iterator. Note that most collections
;  providing iterators implement Iterable and thus support seq directly."
;  [iter]
;  (clojure.lang.IteratorSeq/create iter))
;
;(defn enumeration-seq
;  "Returns a seq on a java.lang.Enumeration"
;  [e]
;  (clojure.lang.EnumerationSeq/create e))
;;; Should we make compatible with Java?
(defn format
  "Formats a string using java.lang.String.format, see java.util.Formatter for format
  string syntax"
  {:tag String}
  [fmt & args]
  (String/Format fmt (to-array args)))    ;; format => Format

(defn printf
  "Prints formatted output, as per format"
  [fmt & args]
  (print (apply format fmt args)))

(def gen-class)

(defmacro ns
  "Sets *ns* to the namespace named by name (unevaluated), creating it
  if needed.  references can be zero or more of: (:refer-clojure ...)
  (:require ...) (:use ...) (:import ...) (:load ...) (:gen-class)
  with the syntax of refer-clojure/require/use/import/load/gen-class
  respectively, except the arguments are unevaluated and need not be
  quoted. (:gen-class ...), when supplied, defaults to :name
  corresponding to the ns name, :main true, :impl-ns same as ns, and
  :init-impl-ns true. All options of gen-class are
  supported. The :gen-class directive is ignored when not
  compiling. If :gen-class is not supplied, when compiled only an
  nsname__init.class will be generated. If :refer-clojure is not used, a
  default (refer 'clojure) is used.  Use of ns is preferred to
  individual calls to in-ns/require/use/import:

  (ns foo.bar
    (:refer-clojure :exclude [ancestors printf])
    (:require (clojure.contrib sql sql.tests))
    (:use (my.lib this that))
    (:import (java.util Date Timer Random)
              (java.sql Connection Statement)))"

  [name & references]
  (let [process-reference
        (fn [[kname & args]]
          `(~(symbol "clojure.core" (clojure.core/name kname))
             ~@(map #(list 'quote %) args)))
        docstring  (when (string? (first references)) (first references))
        references (if docstring (rest references) references)
        name (if docstring
               (with-meta name (assoc (meta name)
                                      :doc docstring))
               name)
        gen-class-clause (first (filter #(= :gen-class (first %)) references))
        gen-class-call
          (when gen-class-clause
            (list* `gen-class :name (.replace (str name) \- \_) :impl-ns name :main true (rest gen-class-clause)))
        references (remove #(= :gen-class (first %)) references)]
    `(do
       (clojure.core/in-ns '~name)
       ~@(when gen-class-call (list gen-class-call))
       ~@(when (and (not= name 'clojure.core) (not-any? #(= :refer-clojure (first %)) references))
           `((clojure.core/refer '~'clojure.core)))
       ~@(map process-reference references))))

(defmacro refer-clojure
  "Same as (refer 'clojure.core <filters>)"
  [& filters]
  `(clojure.core/refer '~'clojure.core ~@filters))
 
(defmacro defonce
  "defs name to have the root value of the expr iff the named var has no root value,
  else expr is unevaluated"
  [name expr]
  `(let [v# (def ~name)]
     (when-not (.hasRoot v#)
       (def ~name ~expr))))

;;;;;;;;;;; require/use/load, contributed by Stephen C. Gilardi ;;;;;;;;;;;;;;;;;;

(defonce
  #^{:private true
     :doc "A ref to a sorted set of symbols representing loaded libs"}
  *loaded-libs* (ref (sorted-set)))

(defonce
  #^{:private true
     :doc "the set of paths currently being loaded by this thread"}
  *pending-paths* #{})

(defonce
  #^{:private true :doc
     "True while a verbose load is pending"}
  *loading-verbosely* false)

(defn- throw-if
  "Throws an exception with a message if pred is true"
  [pred fmt & args]
  (when pred
    (let [ #^String message (apply format fmt args)
          exception (Exception. message)     
          ;; can't set the stacktrace ---- raw-trace (.getStackTrace exception)   
          ;;                          ---- boring? #(not= (.getMethodName #^StackTraceElement %) "doInvoke")
         ];;                          ---- trace (into-array (drop 2 (drop-while boring? raw-trace)))]
      ;;;                           ---- (.setStackTrace exception trace)
      (throw exception))))

(defn- libspec?
  "Returns true if x is a libspec"
  [x]
  (or (symbol? x)
      (and (vector? x)
           (or
            (nil? (second x))
            (keyword? (second x))))))

(defn- prependss
  "Prepends a symbol or a seq to coll"
  [x coll]
  (if (symbol? x)
    (cons x coll)
    (concat x coll)))

(defn- root-resource
  "Returns the root directory path for a lib"
  {:tag String}
  [lib]
  (str \/
       (.. (name lib)
           (replace \- \_)
           (replace \. \/))))

(defn- root-directory
  "Returns the root resource path for a lib"
  [lib]
  (let [d (root-resource lib)]
    (subs d 0 (.lastIndexOf d "/"))))

(def load)

(defn- load-one
  "Loads a lib given its name. If need-ns, ensures that the associated
  namespace exists after loading. If require, records the load so any
  duplicate loads can be skipped."
  [lib need-ns require]
  (load (root-resource lib))
  (throw-if (and need-ns (not (find-ns lib)))
            "namespace '%s' not found after loading '%s'"
            lib (root-resource lib))
  (when require
    (dosync
     (commute *loaded-libs* conj lib))))

(defn- load-all
  "Loads a lib given its name and forces a load of any libs it directly or
  indirectly loads. If need-ns, ensures that the associated namespace
  exists after loading. If require, records the load so any duplicate loads
  can be skipped."
  [lib need-ns require]
  (dosync
   (commute *loaded-libs* #(reduce conj %1 %2)
            (binding [*loaded-libs* (ref (sorted-set))]
              (load-one lib need-ns require)
              @*loaded-libs*))))

(defn- load-lib
  "Loads a lib with options"
  [prefix lib & options]
  (throw-if (and prefix (pos? (.indexOf (name lib) (int \.))))
            "lib names inside prefix lists must not contain periods")
  (let [lib (if prefix (symbol (str prefix \. lib)) lib)
        opts (apply hash-map options)
        {:keys [as reload reload-all require use verbose]} opts
        loaded (contains? @*loaded-libs* lib)
        load (cond reload-all
                   load-all
                   (or reload (not require) (not loaded))
                   load-one)
        need-ns (or as use)
        filter-opts (select-keys opts '(:exclude :only :rename))]
    (binding [*loading-verbosely* (or *loading-verbosely* verbose)]
      (if load
        (load lib need-ns require)
        (throw-if (and need-ns (not (find-ns lib)))
                  "namespace '%s' not found" lib))
      (when (and need-ns *loading-verbosely*)
        (printf "(clojure.core/in-ns '%s)\n" (ns-name *ns*)))
      (when as
        (when *loading-verbosely*
          (printf "(clojure.core/alias '%s '%s)\n" as lib))
        (alias as lib))
      (when use
        (when *loading-verbosely*
          (printf "(clojure.core/refer '%s" lib)
          (doseq [opt filter-opts]
            (printf " %s '%s" (key opt) (print-str (val opt))))
          (printf ")\n"))
        (apply refer lib (mapcat seq filter-opts))))))

(defn- load-libs
  "Loads libs, interpreting libspecs, prefix lists, and flags for
  forwarding to load-lib"
  [& args]
  (let [flags (filter keyword? args)
        opts (interleave flags (repeat true))
        args (filter (complement keyword?) args)]
    (doseq [arg args]
      (if (libspec? arg)
        (apply load-lib nil (prependss arg opts))
        (let [[prefix & args] arg]
          (throw-if (nil? prefix) "prefix cannot be nil")
          (doseq [arg args]
            (apply load-lib prefix (prependss arg opts))))))))

;; Public

(defn require
  "Loads libs, skipping any that are already loaded. Each argument is
  either a libspec that identifies a lib, a prefix list that identifies
  multiple libs whose names share a common prefix, or a flag that modifies
  how all the identified libs are loaded. Use :require in the ns macro
  in preference to calling this directly.

  Libs

  A 'lib' is a named set of resources in classpath whose contents define a
  library of Clojure code. Lib names are symbols and each lib is associated
  with a Clojure namespace and a Java package that share its name. A lib's
  name also locates its root directory within classpath using Java's
  package name to classpath-relative path mapping. All resources in a lib
  should be contained in the directory structure under its root directory.
  All definitions a lib makes should be in its associated namespace.

  'require loads a lib by loading its root resource. The root resource path
  is derived from the root directory path by repeating its last component
  and appending '.clj'. For example, the lib 'x.y.z has root directory
  <classpath>/x/y/z; root resource <classpath>/x/y/z/z.clj. The root
  resource should contain code to create the lib's namespace and load any
  additional lib resources.

  Libspecs

  A libspec is a lib name or a vector containing a lib name followed by
  options expressed as sequential keywords and arguments.

  Recognized options: :as
  :as takes a symbol as its argument and makes that symbol an alias to the
    lib's namespace in the current namespace.

  Prefix Lists

  It's common for Clojure code to depend on several libs whose names have
  the same prefix. When specifying libs, prefix lists can be used to reduce
  repetition. A prefix list contains the shared prefix followed by libspecs
  with the shared prefix removed from the lib names. After removing the
  prefix, the names that remain must not contain any periods.

  Flags

  A flag is a keyword.
  Recognized flags: :reload, :reload-all, :verbose
  :reload forces loading of all the identified libs even if they are
    already loaded
  :reload-all implies :reload and also forces loading of all libs that the
    identified libs directly or indirectly load via require or use
  :verbose triggers printing information about each load, alias, and refer"

  [& args]
  (apply load-libs :require args))

(defn use
  "Like 'require, but also refers to each lib's namespace using
  clojure.core/refer. Use :use in the ns macro in preference to calling
  this directly.

  'use accepts additional options in libspecs: :exclude, :only, :rename.
  The arguments and semantics for :exclude, :only, and :rename are the same
  as those documented for clojure.core/refer."
  [& args] (apply load-libs :require :use args))

(defn loaded-libs
  "Returns a sorted set of symbols naming the currently loaded libs"
  [] @*loaded-libs*)

(defn load
  "Loads Clojure code from resources in classpath. A path is interpreted as
  classpath-relative if it begins with a slash or relative to the root
  directory for the current namespace otherwise."
  [& paths]
  (doseq [#^String path paths]
    (let [#^String path (if (.startsWith path "/")
                          path
                         (str (root-directory (ns-name *ns*)) \/ path))]
      (when *loading-verbosely*
        (printf "(clojure.core/load \"%s\")\n" path)
        (flush))
;      (throw-if (*pending-paths* path)
;                "cannot load '%s' again while it is loading"
;                path)
      (when-not (*pending-paths* path)
        (binding [*pending-paths* (conj *pending-paths* path)]
          (clojure.lang.RT/load  (.substring path 1)))))))

(defn compile
  "Compiles the namespace named by the symbol lib into a set of
  classfiles. The source for the lib must be in a proper
  classpath-relative directory. The output files will go into the
  directory specified by *compile-path*, and that directory too must
  be in the classpath."
  [lib]
  (binding [*compile-files* true]
    (load-one lib true true))
  lib)

;;;;;;;;;;;;; nested associative ops ;;;;;;;;;;;

(defn get-in
  "returns the value in a nested associative structure, where ks is a sequence of keys"
  [m ks]
  (reduce get m ks))

(defn assoc-in
  "Associates a value in a nested associative structure, where ks is a
  sequence of keys and v is the new value and returns a new nested structure.
  If any levels do not exist, hash-maps will be created."
  [m [k & ks] v]
  (if ks
    (assoc m k (assoc-in (get m k) ks v))
    (assoc m k v)))

(defn update-in
  "'Updates' a value in a nested associative structure, where ks is a
  sequence of keys and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  nested structure.  If any levels do not exist, hash-maps will be
  created."
  ([m [k & ks] f & args]
   (if ks
     (assoc m k (apply update-in (get m k) ks f args))
     (assoc m k (apply f (get m k) args)))))


(defn empty?
  "Returns true if coll has no items - same as (not (seq coll)).
  Please use the idiom (seq x) rather than (not (empty? x))"
  [coll] (not (seq coll)))

(defn coll?
  "Returns true if x implements IPersistentCollection"
  [x] (instance? clojure.lang.IPersistentCollection x))

(defn list?
  "Returns true if x implements IPersistentList"
  [x] (instance? clojure.lang.IPersistentList x))

(defn set?
  "Returns true if x implements IPersistentSet"
  [x] (instance? clojure.lang.IPersistentSet x))
  
(defn ifn?
  "Returns true if x implements IFn. Note that many data structures
  (e.g. sets and maps) implement IFn"
  [x] (instance? clojure.lang.IFn x))
  
(defn fn?
  "Returns true if x implements Fn, i.e. is an object created via fn."
  [x] (instance? clojure.lang.Fn x))     
  

(defn associative?
 "Returns true if coll implements Associative"
  [coll] (instance? clojure.lang.Associative coll))

(defn sequential?
 "Returns true if coll implements Sequential"
  [coll] (instance? clojure.lang.Sequential coll))

(defn sorted?
 "Returns true if coll implements Sorted"
  [coll] (instance? clojure.lang.Sorted coll))

(defn counted?
 "Returns true if coll implements count in constant time"
  [coll] (instance? clojure.lang.Counted coll))

(defn reversible?
 "Returns true if coll implements Reversible"
  [coll] (instance? clojure.lang.Reversible coll))

(def
 #^{:doc "bound in a repl thread to the most recent value printed"}
 *1)

(def
 #^{:doc "bound in a repl thread to the second most recent value printed"}
 *2)

(def
 #^{:doc "bound in a repl thread to the third most recent value printed"}
 *3)

(def
 #^{:doc "bound in a repl thread to the most recent exception caught by the repl"}
 *e)

(defmacro declare
  "defs the supplied var names with no bindings, useful for making forward declarations."
  [& names] `(do ~@(map #(list 'def %) names)))

(defn trampoline
  "trampoline can be used to convert algorithms requiring mutual
  recursion without stack consumption. Calls f with supplied args, if
  any. If f returns a fn, calls that fn with no arguments, and
  continues to repeat, until the return value is not a fn, then
  returns that non-fn value. Note that if you want to return a fn as a
  final value, you must wrap it in some data structure and unpack it
  after trampoline returns."
  ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
  ([f & args]
     (trampoline #(apply f args))))

(defn intern
  "Finds or creates a var named by the symbol name in the namespace
  ns (which can be a symbol or a namespace), setting its root binding
  to val if supplied. The namespace must exist. The var will adopt any
  metadata from the name symbol.  Returns the var."
  ([ns #^clojure.lang.Symbol name] 
     (let [v (clojure.lang.Var/intern (the-ns ns) name)]
       (when ^name (.setMeta v ^name))
       v))
  ([ns name val] 
     (let [v (clojure.lang.Var/intern (the-ns ns) name val)]
       (when ^name (.setMeta v ^name))
       v)))

(defmacro while
  "Repeatedly executes body while test expression is true. Presumes
  some side-effect will cause test to become false/nil. Returns nil"
  [test & body]
  `(loop []
     (when ~test
       ~@body
       (recur))))

(defn memoize
  "Returns a memoized version of a referentially transparent function. The
  memoized version of the function keeps a cache of the mapping from arguments
  to results and, when calls with the same arguments are repeated often, has
  higher performance at the expense of higher memory use."
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(defmacro condp
  "Takes a binary predicate, an expression, and a set of clauses.
  Each clause can take the form of either:

  test-expr result-expr

  test-expr :>> result-fn

  Note :>> is an ordinary keyword.

  For each clause, (pred test-expr expr) is evaluated. If it returns
  logical true, the clause is a match. If a binary clause matches, the
  result-expr is returned, if a ternary clause matches, its result-fn,
  which must be a unary function, is called with the result of the
  predicate as its argument, the result of that call being the return
  value of condp. A single default expression can follow the clauses,
  and its value will be returned if no clause matches. If no default
  expression is provided and no clause matches, an
  IllegalArgumentException is thrown."

  [pred expr & clauses]
  (let [gpred (gensym "pred__")
        gexpr (gensym "expr__")
        emit (fn emit [pred expr args]
               (let [[[a b c :as clause] more] 
                       (split-at (if (= :>> (second args)) 3 2) args)
                       n (count clause)]
                 (cond
                  (= 0 n) `(throw (IllegalArgumentException. "No matching clause"))
                  (= 1 n) a
                  (= 2 n) `(if (~pred ~a ~expr)
                             ~b
                             ~(emit pred expr more))
                  :else `(if-let [p# (~pred ~a ~expr)]
                           (~c p#)
                           ~(emit pred expr more)))))
        gres (gensym "res__")]
    `(let [~gpred ~pred
           ~gexpr ~expr]
       ~(emit gpred gexpr clauses))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; var documentation ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro add-doc {:private true} [name docstring]
  `(alter-meta! (var ~name)  assoc :doc ~docstring))

(add-doc *file*
  "The path of the file being evaluated, as a String.

  Evaluates to nil when there is no file, eg. in the REPL.")

(add-doc *command-line-args*
  "A sequence of the supplied command line arguments, or nil if
  none were supplied")

(add-doc *warn-on-reflection*
  "When set to true, the compiler will emit warnings when reflection is
  needed to resolve Java method calls or field accesses.

  Defaults to false.")

(add-doc *compile-path*
  "Specifies the directory where 'compile' will write out .class
  files. This directory must be in the classpath for 'compile' to
  work.

  Defaults to \"classes\"")

(add-doc *compile-files*
  "Set to true when compiling files, false otherwise.")

(add-doc *ns*
  "A clojure.lang.Namespace object representing the current namespace.")

(add-doc *in*
  "A java.io.Reader object representing standard input for read operations.

  Defaults to System/in, wrapped in a LineNumberingPushbackReader")

(add-doc *out*
  "A java.io.Writer object representing standard output for print operations.

  Defaults to System/out")

(add-doc *err*
  "A java.io.Writer object representing standard error for print operations.

  Defaults to System/err, wrapped in a PrintWriter")

(add-doc *flush-on-newline*
  "When set to true, output will be flushed whenever a newline is printed.

  Defaults to true.")

(add-doc *print-meta*
  "If set to logical true, when printing an object, its metadata will also
  be printed in a form that can be read back by the reader.

  Defaults to false.")

(add-doc *print-dup*
  "When set to logical true, objects will be printed in a way that preserves
  their type when read in later.

  Defaults to false.")

(add-doc *print-readably*
  "When set to logical false, strings and characters will be printed with
  non-alphanumeric characters converted to the appropriate escape sequences.

  Defaults to true")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; helper files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(alter-meta! (find-ns 'clojure.core) assoc :doc "Fundamental library of the Clojure language")
;(load "core_proxy")
;(load "core_print")
;(load "genclass")

;;; Need to figure out equivalents for pooledExecutor, java.util.concurrent.Future + we need proxies.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; futures (needs proxy);;;;;;;;;;;;;;;;;;
;(defn future-call 
;  "Takes a function of no args and yields a future object that will
;  invoke the function in another thread, and will cache the result and
;  return it on all subsequent calls to deref/@. If the computation has
;  not yet finished, calls to deref/@ will block."
;  [#^Callable f]
;  (let [fut (.submit clojure.lang.Agent/pooledExecutor f)]
;    (proxy [clojure.lang.IDeref java.util.concurrent.Future] []
;      (deref [] (.get fut))
;      (get ([] (.get fut))
;           ([timeout unit] (.get fut timeout unit)))
;      (isCancelled [] (.isCancelled fut))
;      (isDone [] (.isDone fut))
;      (cancel [interrupt?] (.cancel fut interrupt?)))))
;  
;(defmacro future
;  "Takes a body of expressions and yields a future object that will
;  invoke the body in another thread, and will cache the result and
;  return it on all subsequent calls to deref/@. If the computation has
;  not yet finished, calls to deref/@ will block."  
;  [& body] `(future-call (fn [] ~@body)))
;
;(defn pmap
;  "Like map, except f is applied in parallel. Semi-lazy in that the
;  parallel computation stays ahead of the consumption, but doesn't
;  realize the entire result unless required. Only useful for
;  computationally intensive functions where the time of f dominates
;  the coordination overhead."
;  ([f coll]
;   (let [n (+ 2 (.. Runtime getRuntime availableProcessors))
;         rets (map #(future (f %)) coll)
;         step (fn step [[x & xs :as vs] fs]
;                  (if fs
;                    (lazy-cons (deref x) (step xs (rest fs)))
;                    (map deref vs)))]
;     (step rets (drop n rets))))
;  ([f coll & colls]
;   (let [step (fn step [cs]
;                  (when (every? seq cs)
;                    (lazy-cons (map first cs) (step (map rest cs)))))]
;     (pmap #(apply f %) (step (cons coll colls))))))
;
;(defn pcalls
;  "Executes the no-arg fns in parallel, returning a lazy sequence of
;  their values" 
;  [& fns] (pmap #(%) fns))
;
;(defmacro pvalues
;  "Returns a lazy sequence of the values of the exprs, which are
;  evaluated in parallel" 
;  [& exprs]
;  `(pcalls ~@(map #(list `fn [] %) exprs)))
;