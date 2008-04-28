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
 #^{:arglists '([coll x])
    :doc "conj[oin]. Returns a new collection with the x
    'added'. (conj nil item) returns (item).  The 'addition' may
    happen at different 'places' depending on the concrete type."}
 conj (fn conj [coll x] (. clojure.lang.RT (conj coll x))))

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
 
(defn vector
  "Creates a new vector containing the args."
  ([] [])
  ([& args]
   (. clojure.lang.PersistentVector (create args))))

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
  ([] {})
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


(defn =
  "Equality. Returns true if obj1 equals obj2, false if not. Same as
  Java obj1.equals(obj2) except it also works for nil, and compares
  numbers in a type-independent manner.  Clojure's immutable data
  structures define equals() (and thus =) as a value, not an identity,
  comparison."
  {:tag Boolean} 
  [x y] (. clojure.lang.Util (equal x y)))

(defn not=
  "Same as (not (= obj1 obj2))"
  {:tag Boolean}
  [x y] (not (= x y)))

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
  {:private true}
  [& body] 
    (list 'new 'clojure.lang.Delay (list* `fn [] body)))

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
  (list 'new 'clojure.lang.LazySeq (list `fn [] first-expr) (list* `fn [] rest-expr)))


  
(defn concat
  "Returns a lazy seq representing the concatenation of	the elements in x + xs."
  ([] nil)
  ([x & xs]
   (cond
    (nil? xs) (seq x)
    (nil? (seq x)) (recur (first xs) (rest xs))
    :else (lazy-cons (first x) (apply concat (rest x) xs)))))

;;;;;;;;;;;;;;;;at this point all the support for syntax-quote exists;;;;;;;;;;;;;;;;;;;;;;

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
       (. s (reduce f))
       (f))))
  ([f val coll]
   (let [s (seq coll)]
     (if s
       (. s (reduce f val))
       val))))

(defn reverse
  "Returns a seq of the items in coll in reverse order. Not lazy."
  [coll]
    (reduce conj nil coll))
  
;;math stuff
(defn +
  "Returns the sum of nums. (+) returns 0."
  ([] 0)
  ([x] (cast Number x))
  ([x y] (. clojure.lang.Numbers (add x y)))
  ([x y & more]
   (reduce + (+ x y) more)))

(defn *
  "Returns the product of nums. (*) returns 1."
  ([] 1)
  ([x] (cast Number x))
  ([x y] (. clojure.lang.Numbers (multiply x y)))
  ([x y & more]
   (reduce * (* x y) more)))

(defn /
  "If no denominators are supplied, returns 1/numerator,
  else returns numerator divided by all of the denominators."
  ([x] (/ 1 x))
  ([x y] (. clojure.lang.Numbers (divide x y)))
  ([x y & more]
   (reduce / (/ x y) more)))

(defn -
  "If no ys are supplied, returns the negation of x, else subtracts
  the ys from x and returns the result."
  ([x] (. clojure.lang.Numbers (negate x)))
  ([x y] (. clojure.lang.Numbers (subtract x y)))
  ([x y & more]
   (reduce - (- x y) more)))

(defn <
  "Returns non-nil if nums are in monotonically increasing order,
  otherwise false."
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
  [x] (. clojure.lang.Numbers (inc x)))

(defn dec
  "Returns a number one less than num."
  [x] (. clojure.lang.Numbers (dec x)))

(defn pos?
  "Returns true if num is greater than zero, else false"
  {:tag Boolean}
  [x] (. clojure.lang.Numbers (isPos x)))

(defn neg?
  "Returns true if num is less than zero, else false"
  {:tag Boolean}
  [x] (. clojure.lang.Numbers (isNeg x)))

(defn zero?
  "Returns true if num is zero, else false"
  {:tag Boolean}
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
  bounds, nth throws an exception.  nth also works for strings, Java
  arrays and Lists, and, in O(n) time, for sequences."
  [coll index] (. clojure.lang.RT (nth coll index)))

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
   `(def ~name (new clojure.lang.MultiFn ~dispatch-fn ~default-val))))

(defmacro defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value. "
  [multifn dispatch-val & fn-tail]
  `(let [pvar# (var ~multifn)]
     (. pvar# (commuteRoot (fn [#^clojure.lang.MultiFn mf#] 
                             (. mf# (assoc ~dispatch-val (fn ~@fn-tail))))))))

(defmacro remove-method
  "Removes the method of multimethod associated	with dispatch-value."
 [multifn dispatch-val]
  `(let [pvar# (var ~multifn)]
      (. pvar# (commuteRoot (fn [#^clojure.lang.MultiFn mf#] 
                              (. mf# (dissoc ~dispatch-val)))))))

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
  "Creates and returns an agent with an initial value of state."
  [state] (new clojure.lang.Agent state))

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

(defn ref
  "Creates and returns a Ref with an initial value of x."
  [x] (new clojure.lang.Ref x))

(defn deref
  "Also reader macro: @ref/@agent Within a transaction, returns the
  in-transaction-value of ref, else returns the
  most-recently-committed value of ref. When applied to an agent,
  returns its current state."
 [#^clojure.lang.IRef ref] (. ref (get)))


(defn commute
  "Must be called in a transaction. Sets the in-transaction-value of
  ref to:

  (apply fun in-transaction-value-of-ref args)

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

  (apply fun in-transaction-value-of-ref args)"
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
  (pred item) returns true."
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
  (pred item) returns true."
  [pred coll]
    (when (and (seq coll) (pred (first coll)))
      (lazy-cons (first coll) (take-while pred (rest coll)))))

(defn drop
  "Returns a lazy seq of all but the first n items in coll."
  [n coll]
    (if (and (pos? n) (seq coll))
      (recur (dec n) (rest coll))
      coll))

(defn drop-while
  "Returns a lazy seq of the items in coll starting from the first
  item for which (pred item) returns nil."
  [pred coll]
    (if (and (seq coll) (pred (first coll)))
      (recur pred (rest coll))
      coll))

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
  "Returns a lazy seq of x, (f x), (f (f x)) etc."
  [f x] (lazy-cons x (iterate f (f x))))

(defn range
  "Returns a lazy seq of nums from start (inclusive) to end
  (exclusive), by step, where start defaults to 0 and step to 1."
  ([end] (if (< end (. Integer MAX_VALUE))
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
  supplied, the items must implement Comparable. comparator must
  implement java.util.Comparator."
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
  "Returns a sorted sequence of the items in coll, where the sort
  order is determined by comparing (keyfn item).  If no comparator is
  supplied, the keys must implement Comparable. comparator must
  implement java.util.Comparator."
  ([keyfn coll]
   (sort (fn [x y] (. #^Comparable (keyfn x) (compareTo (keyfn y)))) coll))
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
   (when (seq coll)
     (recur (rest coll))))
  ([n coll]
   (when (and (seq coll) (pos? n))
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
    (let [latch (new java.util.concurrent.CountDownLatch (count agents))
          count-down (fn [agent] (. latch (countDown)) agent)]
      (doseq agent agents
        (send agent count-down))
      (. latch (await))))

(defn await-for
  "Blocks the current thread until all actions dispatched thus
  far (from this thread or agent) to the agents have occurred, or the
  timeout (in milliseconds) has elapsed. Returns nil if returning due
  to timeout, non-nil otherwise."
  [timeout-ms & agents]
    (let [latch (new java.util.concurrent.CountDownLatch (count agents))
          count-down (fn [agent] (. latch (countDown)) agent)]
      (doseq agent agents
        (send agent count-down))
      (. latch (await  timeout-ms (. java.util.concurrent.TimeUnit MILLISECONDS)))))
  
(defmacro dotimes
  "Repeatedly executes body (presumably for side-effects) with name
  bound to integers from 0 through n-1."
  [i n & body]
  `(loop [~i 0 n# ~n]
     (when (< ~i n#)
       ~@body
       (recur (inc ~i) n#))))

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
  "Same as pr followed by (newline)"
  [& more]
    (apply pr more)
    (newline))

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
     (prn (str "Elapsed time: " (/ (- (. System (nanoTime)) start#) 1000000.0) " msecs"))
     ret#))

(defn int
  "Coerce to int"
  {:tag Integer}
  [x] (. clojure.lang.RT (intCast x)))

(defn long
  "Coerce to long"
  {:tag Long}
  [#^Number x] (. x (longValue)))

(defn float
  "Coerce to float"
  {:tag Float}
  [#^Number x] (. x (floatValue)))

(defn double
  "Coerce to double"
  {:tag Double}
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
  [x] (BigInteger.valueOf x))

(defn bigdec
  "Coerce to BigDecimal"
  {:tag BigDecimal}
  [x] (BigDecimal.valueOf x))

(import '(java.lang.reflect Array))

(defn alength
  "Returns the length of the Java array. Works on arrays of all
  types."
  [array] (. Array (getLength array)))

(defn aget
  "Returns the value at the index/indices. Works on Java arrays of all
  types."
  ([array idx]
   (. Array (get array idx)))
  ([array idx & idxs]
   (apply aget (aget array idx) idxs)))

(defn aset
  "Sets the value at the index/indices. Works on Java arrays of
  reference types. Returns val."
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

(defn to-array
  "Returns an array of Objects containing the contents of coll, which
  can be any Collection.  Maps to java.util.Collection.toArray()."
  [#^java.util.Collection coll]
    (if (zero? (count coll))
      (. clojure.lang.RT EMPTY_ARRAY)
      (. coll (toArray))))

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
                                        (recur (pb ret firstb  (list `nth gvec n))
                                               (inc n)
                                               (rest bs)
                                               seen-rest?))))
                             ret))))
                     pmap
                     (fn [bvec b v]
                       (let [gmap (or (:as b) (gensym "map__"))
                             defaults (:or b)]
                         (loop [ret (-> bvec (conj gmap) (conj (list `or v {})))
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
                                                   (list gmap bk)))
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

(defmacro loop
  "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Acts as a recur target."
  [bindings & body]
    (let [db (destructure bindings)]
      (if (= db bindings)
        `(loop* ~bindings ~@body)
        (let [vs (take-nth 2 (drop 1 bindings))
              gs (map (fn [x] (gensym)) vs)
              ds (take-nth 2 bindings)]
          `(loop* ~(apply vector (interleave gs vs))
              (let ~(apply vector (interleave ds gs))
                ~@body))))))
  
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

(defn pr-str
  "pr to a string, returning it"
  [& xs]
    (with-out-str
     (apply pr xs)))

(defn prn-str
  "prn to a string, returning it"
  [& xs]
  (with-out-str
   (apply prn xs)))

(defn print-str
  "print to a string, returning it"
  [& xs]
    (with-out-str
     (apply print xs)))

(defn println-str
  "println to a string, returning it"
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
  [#^Object x] (. x (getClass)))

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


