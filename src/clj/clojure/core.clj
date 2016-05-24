;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "The core Clojure language."
       :author "Rich Hickey"}
  clojure.core)

(def unquote)
(def unquote-splicing)

(def
 ^{:arglists '([& items])
   :doc "Creates a new list containing the items."
   :added "1.0"}
  list (. clojure.lang.PersistentList creator))

(def
 ^{:arglists '([x seq])
    :doc "Returns a new seq where x is the first element and seq is
    the rest."
   :added "1.0"
   :static true}

 cons (fn* ^:static cons [x seq] (. clojure.lang.RT (cons x seq))))

;during bootstrap we don't have destructuring let, loop or fn, will redefine later
(def
  ^{:macro true
    :added "1.0"}
  let (fn* let [&form &env & decl] (cons 'let* decl)))

(def
 ^{:macro true
   :added "1.0"}
 loop (fn* loop [&form &env & decl] (cons 'loop* decl)))

(def
 ^{:macro true
   :added "1.0"}
 fn (fn* fn [&form &env & decl] 
         (.withMeta ^clojure.lang.IObj (cons 'fn* decl) 
                    (.meta ^clojure.lang.IMeta &form))))

(def
 ^{:arglists '([coll])
   :doc "Returns the first item in the collection. Calls seq on its
    argument. If coll is nil, returns nil."
   :added "1.0"
   :static true}
 first (fn ^:static first [coll] (. clojure.lang.RT (first coll))))

(def
 ^{:arglists '([coll])
   :tag clojure.lang.ISeq
   :doc "Returns a seq of the items after the first. Calls seq on its
  argument.  If there are no more items, returns nil."
   :added "1.0"
   :static true}  
 next (fn ^:static next [x] (. clojure.lang.RT (next x))))

(def
 ^{:arglists '([coll])
   :tag clojure.lang.ISeq
   :doc "Returns a possibly empty seq of the items after the first. Calls seq on its
  argument."
   :added "1.0"
   :static true}  
 rest (fn ^:static rest [x] (. clojure.lang.RT (more x))))

(def
 ^{:arglists '([coll x] [coll x & xs])
   :doc "conj[oin]. Returns a new collection with the xs
    'added'. (conj nil item) returns (item).  The 'addition' may
    happen at different 'places' depending on the concrete type."
   :added "1.0"
   :static true}
 conj (fn ^:static conj
        ([] [])
        ([coll] coll)
        ([coll x] (clojure.lang.RT/conj coll x))
        ([coll x & xs]
         (if xs
           (recur (clojure.lang.RT/conj coll x) (first xs) (next xs))
           (clojure.lang.RT/conj coll x)))))

(def
 ^{:doc "Same as (first (next x))"
   :arglists '([x])
   :added "1.0"
   :static true}
 second (fn ^:static second [x] (first (next x))))

(def
 ^{:doc "Same as (first (first x))"
   :arglists '([x])
   :added "1.0"
   :static true}
 ffirst (fn ^:static ffirst [x] (first (first x))))

(def
 ^{:doc "Same as (next (first x))"
   :arglists '([x])
   :added "1.0"
   :static true}
 nfirst (fn ^:static nfirst [x] (next (first x))))

(def
 ^{:doc "Same as (first (next x))"
   :arglists '([x])
   :added "1.0"
   :static true}
 fnext (fn ^:static fnext [x] (first (next x))))

(def
 ^{:doc "Same as (next (next x))"
   :arglists '([x])
   :added "1.0"
   :static true}
 nnext (fn ^:static nnext [x] (next (next x))))

(def
 ^{:arglists '(^clojure.lang.ISeq [coll])
   :doc "Returns a seq on the collection. If the collection is
    empty, returns nil.  (seq nil) returns nil. seq also works on
    Strings, native Java arrays (of reference types) and any objects
    that implement Iterable. Note that seqs cache values, thus seq
    should not be used on any Iterable whose iterator repeatedly
    returns the same mutable object."
   :tag clojure.lang.ISeq
   :added "1.0"
   :static true}
 seq (fn ^:static seq ^clojure.lang.ISeq [coll] (. clojure.lang.RT (seq coll))))

(def
 ^{:arglists '([^Class c x])
   :doc "Evaluates x and tests if it is an instance of the class
    c. Returns true or false"
   :added "1.0"}
 instance? (fn instance? [^Class c x] (. c (isInstance x))))

(def
 ^{:arglists '([x])
   :doc "Return true if x implements ISeq"
   :added "1.0"
   :static true}
 seq? (fn ^:static seq? [x] (instance? clojure.lang.ISeq x)))

(def
 ^{:arglists '([x])
   :doc "Return true if x is a Character"
   :added "1.0"
   :static true}
 char? (fn ^:static char? [x] (instance? Character x)))

(def
 ^{:arglists '([x])
   :doc "Return true if x is a String"
   :added "1.0"
   :static true}
 string? (fn ^:static string? [x] (instance? String x)))

(def
 ^{:arglists '([x])
   :doc "Return true if x implements IPersistentMap"
   :added "1.0"
   :static true}
 map? (fn ^:static map? [x] (instance? clojure.lang.IPersistentMap x)))

(def
 ^{:arglists '([x])
   :doc "Return true if x implements IPersistentVector"
   :added "1.0"
   :static true}
 vector? (fn ^:static vector? [x] (instance? clojure.lang.IPersistentVector x)))

(def
 ^{:arglists '([map key val] [map key val & kvs])
   :doc "assoc[iate]. When applied to a map, returns a new map of the
    same (hashed/sorted) type, that contains the mapping of key(s) to
    val(s). When applied to a vector, returns a new vector that
    contains val at index. Note - index must be <= (count vector)."
   :added "1.0"
   :static true}
 assoc
 (fn ^:static assoc
   ([map key val] (clojure.lang.RT/assoc map key val))
   ([map key val & kvs]
    (let [ret (clojure.lang.RT/assoc map key val)]
      (if kvs
        (if (next kvs)
          (recur ret (first kvs) (second kvs) (nnext kvs))
          (throw (IllegalArgumentException.
                  "assoc expects even number of arguments after map/vector, found odd number")))
        ret)))))

;;;;;;;;;;;;;;;;; metadata ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def
 ^{:arglists '([obj])
   :doc "Returns the metadata of obj, returns nil if there is no metadata."
   :added "1.0"
   :static true}
 meta (fn ^:static meta [x]
        (if (instance? clojure.lang.IMeta x)
          (. ^clojure.lang.IMeta x (meta)))))

(def
 ^{:arglists '([^clojure.lang.IObj obj m])
   :doc "Returns an object of the same type and value as obj, with
    map m as its metadata."
   :added "1.0"
   :static true}
 with-meta (fn ^:static with-meta [^clojure.lang.IObj x m]
             (. x (withMeta m))))

(def ^{:private true :dynamic true}
  assert-valid-fdecl (fn [fdecl]))

(def
 ^{:private true}
 sigs
 (fn [fdecl]
   (assert-valid-fdecl fdecl)
   (let [asig 
         (fn [fdecl]
           (let [arglist (first fdecl)
                 ;elide implicit macro args
                 arglist (if (clojure.lang.Util/equals '&form (first arglist)) 
                           (clojure.lang.RT/subvec arglist 2 (clojure.lang.RT/count arglist))
                           arglist)
                 body (next fdecl)]
             (if (map? (first body))
               (if (next body)
                 (with-meta arglist (conj (if (meta arglist) (meta arglist) {}) (first body)))
                 arglist)
               arglist)))
         resolve-tag (fn [argvec]
                        (let [m (meta argvec)
                              ^clojure.lang.Symbol tag (:tag m)]
                          (if (instance? clojure.lang.Symbol tag)
                            (if (clojure.lang.Util/equiv (.indexOf (.getName tag) ".") -1)
                              (if (clojure.lang.Util/equals nil (clojure.lang.Compiler$HostExpr/maybeSpecialTag tag))
                                (let [c (clojure.lang.Compiler$HostExpr/maybeClass tag false)]
                                  (if c
                                    (with-meta argvec (assoc m :tag (clojure.lang.Symbol/intern (.getName c))))
                                    argvec))
                                argvec)
                              argvec)
                            argvec)))]
     (if (seq? (first fdecl))
       (loop [ret [] fdecls fdecl]
         (if fdecls
           (recur (conj ret (resolve-tag (asig (first fdecls)))) (next fdecls))
           (seq ret)))
       (list (resolve-tag (asig fdecl)))))))


(def 
 ^{:arglists '([coll])
   :doc "Return the last item in coll, in linear time"
   :added "1.0"
   :static true}
 last (fn ^:static last [s]
        (if (next s)
          (recur (next s))
          (first s))))

(def 
 ^{:arglists '([coll])
   :doc "Return a seq of all but the last item in coll, in linear time"
   :added "1.0"
   :static true}
 butlast (fn ^:static butlast [s]
           (loop [ret [] s s]
             (if (next s)
               (recur (conj ret (first s)) (next s))
               (seq ret)))))

(def 

 ^{:doc "Same as (def name (fn [params* ] exprs*)) or (def
    name (fn ([params* ] exprs*)+)) with any doc-string or attrs added
    to the var metadata. prepost-map defines a map with optional keys
    :pre and :post that contain collections of pre or post conditions."
   :arglists '([name doc-string? attr-map? [params*] prepost-map? body]
                [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])
   :added "1.0"}
 defn (fn defn [&form &env name & fdecl]
        ;; Note: Cannot delegate this check to def because of the call to (with-meta name ..)
        (if (instance? clojure.lang.Symbol name)
          nil
          (throw (IllegalArgumentException. "First argument to defn must be a symbol")))
        (let [m (if (string? (first fdecl))
                  {:doc (first fdecl)}
                  {})
              fdecl (if (string? (first fdecl))
                      (next fdecl)
                      fdecl)
              m (if (map? (first fdecl))
                  (conj m (first fdecl))
                  m)
              fdecl (if (map? (first fdecl))
                      (next fdecl)
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
              m (conj {:arglists (list 'quote (sigs fdecl))} m)
              m (let [inline (:inline m)
                      ifn (first inline)
                      iname (second inline)]
                  ;; same as: (if (and (= 'fn ifn) (not (symbol? iname))) ...)
                  (if (if (clojure.lang.Util/equiv 'fn ifn)
                        (if (instance? clojure.lang.Symbol iname) false true))
                    ;; inserts the same fn name to the inline fn if it does not have one
                    (assoc m :inline (cons ifn (cons (clojure.lang.Symbol/intern (.concat (.getName ^clojure.lang.Symbol name) "__inliner"))
                                                     (next inline))))
                    m))
              m (conj (if (meta name) (meta name) {}) m)]
          (list 'def (with-meta name m)
                ;;todo - restore propagation of fn name
                ;;must figure out how to convey primitive hints to self calls first
								;;(cons `fn fdecl)
								(with-meta (cons `fn fdecl) {:rettag (:tag m)})))))

(. (var defn) (setMacro))

(defn to-array
  "Returns an array of Objects containing the contents of coll, which
  can be any Collection.  Maps to java.util.Collection.toArray()."
  {:tag "[Ljava.lang.Object;"
   :added "1.0"
   :static true}
  [coll] (. clojure.lang.RT (toArray coll)))

(defn cast
  "Throws a ClassCastException if x is not a c, else returns x."
  {:added "1.0"
   :static true}
  [^Class c x] 
  (. c (cast x)))
 
(defn vector
  "Creates a new vector containing the args."
  {:added "1.0"
   :static true}
  ([] [])
  ([a] [a])
  ([a b] [a b])
  ([a b c] [a b c])
  ([a b c d] [a b c d])
	([a b c d e] [a b c d e])
	([a b c d e f] [a b c d e f])
  ([a b c d e f & args]
     (. clojure.lang.LazilyPersistentVector (create (cons a (cons b (cons c (cons d (cons e (cons f args))))))))))

(defn vec
  "Creates a new vector containing the contents of coll. Java arrays
  will be aliased and should not be modified."
  {:added "1.0"
   :static true}
  ([coll]
   (if (vector? coll)
     (if (instance? clojure.lang.IObj coll)
       (with-meta coll nil)
       (clojure.lang.LazilyPersistentVector/create coll))
     (clojure.lang.LazilyPersistentVector/create coll))))

(defn hash-map
  "keyval => key val
  Returns a new hash map with supplied mappings.  If any keys are
  equal, they are handled as if by repeated uses of assoc."
  {:added "1.0"
   :static true}
  ([] {})
  ([& keyvals]
   (. clojure.lang.PersistentHashMap (create keyvals))))

(defn hash-set
  "Returns a new hash set with supplied keys.  Any equal keys are
  handled as if by repeated uses of conj."
  {:added "1.0"
   :static true}
  ([] #{})
  ([& keys]
   (clojure.lang.PersistentHashSet/create keys)))

(defn sorted-map
  "keyval => key val
  Returns a new sorted map with supplied mappings.  If any keys are
  equal, they are handled as if by repeated uses of assoc."
  {:added "1.0"
   :static true}
  ([& keyvals]
   (clojure.lang.PersistentTreeMap/create keyvals)))

(defn sorted-map-by
  "keyval => key val
  Returns a new sorted map with supplied mappings, using the supplied
  comparator.  If any keys are equal, they are handled as if by
  repeated uses of assoc."
  {:added "1.0"
   :static true}
  ([comparator & keyvals]
   (clojure.lang.PersistentTreeMap/create comparator keyvals)))

(defn sorted-set
  "Returns a new sorted set with supplied keys.  Any equal keys are
  handled as if by repeated uses of conj."
  {:added "1.0"
   :static true}
  ([& keys]
   (clojure.lang.PersistentTreeSet/create keys)))

(defn sorted-set-by
  "Returns a new sorted set with supplied keys, using the supplied
  comparator.  Any equal keys are handled as if by repeated uses of
  conj."
  {:added "1.1"
   :static true} 
  ([comparator & keys]
   (clojure.lang.PersistentTreeSet/create comparator keys)))

 
;;;;;;;;;;;;;;;;;;;;
(defn nil?
  "Returns true if x is nil, false otherwise."
  {:tag Boolean
   :added "1.0"
   :static true
   :inline (fn [x] (list 'clojure.lang.Util/identical x nil))}
  [x] (clojure.lang.Util/identical x nil))

(def

 ^{:doc "Like defn, but the resulting function name is declared as a
  macro and will be used as a macro by the compiler when it is
  called."
   :arglists '([name doc-string? attr-map? [params*] body]
                 [name doc-string? attr-map? ([params*] body)+ attr-map?])
   :added "1.0"}
 defmacro (fn [&form &env 
                name & args]
             (let [prefix (loop [p (list name) args args]
                            (let [f (first args)]
                              (if (string? f)
                                (recur (cons f p) (next args))
                                (if (map? f)
                                  (recur (cons f p) (next args))
                                  p))))
                   fdecl (loop [fd args]
                           (if (string? (first fd))
                             (recur (next fd))
                             (if (map? (first fd))
                               (recur (next fd))
                               fd)))
                   fdecl (if (vector? (first fdecl))
                           (list fdecl)
                           fdecl)
                   add-implicit-args (fn [fd]
                             (let [args (first fd)]
                               (cons (vec (cons '&form (cons '&env args))) (next fd))))
                   add-args (fn [acc ds]
                              (if (nil? ds)
                                acc
                                (let [d (first ds)]
                                  (if (map? d)
                                    (conj acc d)
                                    (recur (conj acc (add-implicit-args d)) (next ds))))))
                   fdecl (seq (add-args [] fdecl))
                   decl (loop [p prefix d fdecl]
                          (if p
                            (recur (next p) (cons (first p) d))
                            d))]
               (list 'do
                     (cons `defn decl)
                     (list '. (list 'var name) '(setMacro))
                     (list 'var name)))))


(. (var defmacro) (setMacro))

(defmacro when
  "Evaluates test. If logical true, evaluates body in an implicit do."
  {:added "1.0"}
  [test & body]
  (list 'if test (cons 'do body)))

(defmacro when-not
  "Evaluates test. If logical false, evaluates body in an implicit do."
  {:added "1.0"}
  [test & body]
    (list 'if test nil (cons 'do body)))

(defn false?
  "Returns true if x is the value false, false otherwise."
  {:tag Boolean,
   :added "1.0"
   :static true}
  [x] (clojure.lang.Util/identical x false))

(defn true?
  "Returns true if x is the value true, false otherwise."
  {:tag Boolean,
   :added "1.0"
   :static true}
  [x] (clojure.lang.Util/identical x true))

(defn not
  "Returns true if x is logical false, false otherwise."
  {:tag Boolean
   :added "1.0"
   :static true}
  [x] (if x false true))

(defn some?
  "Returns true if x is not nil, false otherwise."
  {:tag Boolean
   :added "1.6"
   :static true}
  [x] (not (nil? x)))

(defn str
  "With no args, returns the empty string. With one arg x, returns
  x.toString().  (str nil) returns the empty string. With more than
  one arg, returns the concatenation of the str values of the args."
  {:tag String
   :added "1.0"
   :static true}
  (^String [] "")
  (^String [^Object x]
   (if (nil? x) "" (. x (toString))))
  (^String [x & ys]
     ((fn [^StringBuilder sb more]
          (if more
            (recur (. sb  (append (str (first more)))) (next more))
            (str sb)))
      (new StringBuilder (str x)) ys)))


(defn symbol?
  "Return true if x is a Symbol"
  {:added "1.0"
   :static true}
  [x] (instance? clojure.lang.Symbol x))

(defn keyword?
  "Return true if x is a Keyword"
  {:added "1.0"
   :static true}
  [x] (instance? clojure.lang.Keyword x))

(defn symbol
  "Returns a Symbol with the given namespace and name."
  {:tag clojure.lang.Symbol
   :added "1.0"
   :static true}
  ([name] (if (symbol? name) name (clojure.lang.Symbol/intern name)))
  ([ns name] (clojure.lang.Symbol/intern ns name)))

(defn gensym
  "Returns a new symbol with a unique name. If a prefix string is
  supplied, the name is prefix# where # is some unique number. If
  prefix is not supplied, the prefix is 'G__'."
  {:added "1.0"
   :static true}
  ([] (gensym "G__"))
  ([prefix-string] (. clojure.lang.Symbol (intern (str prefix-string (str (. clojure.lang.RT (nextID))))))))

(defmacro cond
  "Takes a set of test/expr pairs. It evaluates each test one at a
  time.  If a test returns logical true, cond evaluates and returns
  the value of the corresponding expr and doesn't evaluate any of the
  other tests or exprs. (cond) returns nil."
  {:added "1.0"}
  [& clauses]
    (when clauses
      (list 'if (first clauses)
            (if (next clauses)
                (second clauses)
                (throw (IllegalArgumentException.
                         "cond requires an even number of forms")))
            (cons 'clojure.core/cond (next (next clauses))))))

(defn keyword
  "Returns a Keyword with the given namespace and name.  Do not use :
  in the keyword strings, it will be added automatically."
  {:tag clojure.lang.Keyword
   :added "1.0"
   :static true}
  ([name] (cond (keyword? name) name
                (symbol? name) (clojure.lang.Keyword/intern ^clojure.lang.Symbol name)
                (string? name) (clojure.lang.Keyword/intern ^String name)))
  ([ns name] (clojure.lang.Keyword/intern ns name)))

(defn find-keyword
  "Returns a Keyword with the given namespace and name if one already
  exists.  This function will not intern a new keyword. If the keyword
  has not already been interned, it will return nil.  Do not use :
  in the keyword strings, it will be added automatically."
  {:tag clojure.lang.Keyword
   :added "1.3"
   :static true}
  ([name] (cond (keyword? name) name
                (symbol? name) (clojure.lang.Keyword/find ^clojure.lang.Symbol name)
                (string? name) (clojure.lang.Keyword/find ^String name)))
  ([ns name] (clojure.lang.Keyword/find ns name)))


(defn spread
  {:private true
   :static true}
  [arglist]
  (cond
   (nil? arglist) nil
   (nil? (next arglist)) (seq (first arglist))
   :else (cons (first arglist) (spread (next arglist)))))

(defn list*
  "Creates a new seq containing the items prepended to the rest, the
  last of which will be treated as a sequence."
  {:added "1.0"
   :static true}
  ([args] (seq args))
  ([a args] (cons a args))
  ([a b args] (cons a (cons b args)))
  ([a b c args] (cons a (cons b (cons c args))))
  ([a b c d & more]
     (cons a (cons b (cons c (cons d (spread more)))))))

(defn apply
  "Applies fn f to the argument list formed by prepending intervening arguments to args."
  {:added "1.0"
   :static true}
  ([^clojure.lang.IFn f args]
     (. f (applyTo (seq args))))
  ([^clojure.lang.IFn f x args]
     (. f (applyTo (list* x args))))
  ([^clojure.lang.IFn f x y args]
     (. f (applyTo (list* x y args))))
  ([^clojure.lang.IFn f x y z args]
     (. f (applyTo (list* x y z args))))
  ([^clojure.lang.IFn f a b c d & args]
     (. f (applyTo (cons a (cons b (cons c (cons d (spread args)))))))))

(defn vary-meta
 "Returns an object of the same type and value as obj, with
  (apply f (meta obj) args) as its metadata."
 {:added "1.0"
   :static true}
 [obj f & args]
  (with-meta obj (apply f (meta obj) args)))

(defmacro lazy-seq
  "Takes a body of expressions that returns an ISeq or nil, and yields
  a Seqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls. See also - realized?"
  {:added "1.0"}
  [& body]
  (list 'new 'clojure.lang.LazySeq (list* '^{:once true} fn* [] body)))    

(defn ^:static ^clojure.lang.ChunkBuffer chunk-buffer ^clojure.lang.ChunkBuffer [capacity]
  (clojure.lang.ChunkBuffer. capacity))

(defn ^:static chunk-append [^clojure.lang.ChunkBuffer b x]
  (.add b x))

(defn ^:static ^clojure.lang.IChunk chunk [^clojure.lang.ChunkBuffer b]
  (.chunk b))

(defn ^:static  ^clojure.lang.IChunk chunk-first ^clojure.lang.IChunk [^clojure.lang.IChunkedSeq s]
  (.chunkedFirst s))

(defn ^:static ^clojure.lang.ISeq chunk-rest ^clojure.lang.ISeq [^clojure.lang.IChunkedSeq s]
  (.chunkedMore s))

(defn ^:static ^clojure.lang.ISeq chunk-next ^clojure.lang.ISeq [^clojure.lang.IChunkedSeq s]
  (.chunkedNext s))

(defn ^:static chunk-cons [chunk rest]
  (if (clojure.lang.Numbers/isZero (clojure.lang.RT/count chunk))
    rest
    (clojure.lang.ChunkedCons. chunk rest)))
  
(defn ^:static chunked-seq? [s]
  (instance? clojure.lang.IChunkedSeq s))

(defn concat
  "Returns a lazy seq representing the concatenation of the elements in the supplied colls."
  {:added "1.0"
   :static true}
  ([] (lazy-seq nil))
  ([x] (lazy-seq x))
  ([x y]
    (lazy-seq
      (let [s (seq x)]
        (if s
          (if (chunked-seq? s)
            (chunk-cons (chunk-first s) (concat (chunk-rest s) y))
            (cons (first s) (concat (rest s) y)))
          y))))
  ([x y & zs]
     (let [cat (fn cat [xys zs]
                 (lazy-seq
                   (let [xys (seq xys)]
                     (if xys
                       (if (chunked-seq? xys)
                         (chunk-cons (chunk-first xys)
                                     (cat (chunk-rest xys) zs))
                         (cons (first xys) (cat (rest xys) zs)))
                       (when zs
                         (cat (first zs) (next zs)))))))]
       (cat (concat x y) zs))))

;;;;;;;;;;;;;;;;at this point all the support for syntax-quote exists;;;;;;;;;;;;;;;;;;;;;;
(defmacro delay
  "Takes a body of expressions and yields a Delay object that will
  invoke the body only the first time it is forced (with force or deref/@), and
  will cache the result and return it on all subsequent force
  calls. See also - realized?"
  {:added "1.0"}
  [& body]
    (list 'new 'clojure.lang.Delay (list* `^{:once true} fn* [] body)))

(defn delay?
  "returns true if x is a Delay created with delay"
  {:added "1.0"
   :static true}
  [x] (instance? clojure.lang.Delay x))

(defn force
  "If x is a Delay, returns the (possibly cached) value of its expression, else returns x"
  {:added "1.0"
   :static true}
  [x] (. clojure.lang.Delay (force x)))

(defmacro if-not
  "Evaluates test. If logical false, evaluates and returns then expr, 
  otherwise else expr, if supplied, else nil."
  {:added "1.0"}
  ([test then] `(if-not ~test ~then nil))
  ([test then else]
   `(if (not ~test) ~then ~else)))

(defn identical?
  "Tests if 2 arguments are the same object"
  {:inline (fn [x y] `(. clojure.lang.Util identical ~x ~y))
   :inline-arities #{2}
   :added "1.0"}
  ([x y] (clojure.lang.Util/identical x y)))

;equiv-based
(defn =
  "Equality. Returns true if x equals y, false if not. Same as
  Java x.equals(y) except it also works for nil, and compares
  numbers and collections in a type-independent manner.  Clojure's immutable data
  structures define equals() (and thus =) as a value, not an identity,
  comparison."
  {:inline (fn [x y] `(. clojure.lang.Util equiv ~x ~y))
   :inline-arities #{2}
   :added "1.0"}
  ([x] true)
  ([x y] (clojure.lang.Util/equiv x y))
  ([x y & more]
   (if (clojure.lang.Util/equiv x y)
     (if (next more)
       (recur y (first more) (next more))
       (clojure.lang.Util/equiv y (first more)))
     false)))

;equals-based
#_(defn =
  "Equality. Returns true if x equals y, false if not. Same as Java
  x.equals(y) except it also works for nil. Boxed numbers must have
  same type. Clojure's immutable data structures define equals() (and
  thus =) as a value, not an identity, comparison."
  {:inline (fn [x y] `(. clojure.lang.Util equals ~x ~y))
   :inline-arities #{2}
   :added "1.0"}
  ([x] true)
  ([x y] (clojure.lang.Util/equals x y))
  ([x y & more]
   (if (= x y)
     (if (next more)
       (recur y (first more) (next more))
       (= y (first more)))
     false)))

(defn not=
  "Same as (not (= obj1 obj2))"
  {:tag Boolean
   :added "1.0"
   :static true}
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more]
   (not (apply = x y more))))



(defn compare
  "Comparator. Returns a negative number, zero, or a positive number
  when x is logically 'less than', 'equal to', or 'greater than'
  y. Same as Java x.compareTo(y) except it also works for nil, and
  compares numbers and collections in a type-independent manner. x
  must implement Comparable"
  {
   :inline (fn [x y] `(. clojure.lang.Util compare ~x ~y))
   :added "1.0"}
  [x y] (. clojure.lang.Util (compare x y)))

(defmacro and
  "Evaluates exprs one at a time, from left to right. If a form
  returns logical false (nil or false), and returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expr. (and) returns true."
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(let [and# ~x]
      (if and# (and ~@next) and#))))

(defmacro or
  "Evaluates exprs one at a time, from left to right. If a form
  returns a logical true value, or returns that value and doesn't
  evaluate any of the other expressions, otherwise it returns the
  value of the last expression. (or) returns nil."
  {:added "1.0"}
  ([] nil)
  ([x] x)
  ([x & next]
      `(let [or# ~x]
         (if or# or# (or ~@next)))))

;;;;;;;;;;;;;;;;;;; sequence fns  ;;;;;;;;;;;;;;;;;;;;;;;
(defn zero?
  "Returns true if num is zero, else false"
  {
   :inline (fn [x] `(. clojure.lang.Numbers (isZero ~x)))
   :added "1.0"}
  [x] (. clojure.lang.Numbers (isZero x)))

(defn count
  "Returns the number of items in the collection. (count nil) returns
  0.  Also works on strings, arrays, and Java Collections and Maps"
  {
   :inline (fn  [x] `(. clojure.lang.RT (count ~x)))
   :added "1.0"}
  [coll] (clojure.lang.RT/count coll))

(defn int
  "Coerce to int"
  {
   :inline (fn  [x] `(. clojure.lang.RT (~(if *unchecked-math* 'uncheckedIntCast 'intCast) ~x)))
   :added "1.0"}
  [x] (. clojure.lang.RT (intCast x)))

(defn nth
  "Returns the value at the index. get returns nil if index out of
  bounds, nth throws an exception unless not-found is supplied.  nth
  also works for strings, Java arrays, regex Matchers and Lists, and,
  in O(n) time, for sequences."
  {:inline (fn  [c i & nf] `(. clojure.lang.RT (nth ~c ~i ~@nf)))
   :inline-arities #{2 3}
   :added "1.0"}
  ([coll index] (. clojure.lang.RT (nth coll index)))
  ([coll index not-found] (. clojure.lang.RT (nth coll index not-found))))

(defn <
  "Returns non-nil if nums are in monotonically increasing order,
  otherwise false."
  {:inline (fn [x y] `(. clojure.lang.Numbers (lt ~x ~y)))
   :inline-arities #{2}
   :added "1.0"}
  ([x] true)
  ([x y] (. clojure.lang.Numbers (lt x y)))
  ([x y & more]
   (if (< x y)
     (if (next more)
       (recur y (first more) (next more))
       (< y (first more)))
     false)))

(defn inc'
  "Returns a number one greater than num. Supports arbitrary precision.
  See also: inc"
  {:inline (fn [x] `(. clojure.lang.Numbers (incP ~x)))
   :added "1.0"}
  [x] (. clojure.lang.Numbers (incP x)))

(defn inc
  "Returns a number one greater than num. Does not auto-promote
  longs, will throw on overflow. See also: inc'"
  {:inline (fn [x] `(. clojure.lang.Numbers (~(if *unchecked-math* 'unchecked_inc 'inc) ~x)))
   :added "1.2"}
  [x] (. clojure.lang.Numbers (inc x)))

;; reduce is defined again later after InternalReduce loads
(defn ^:private ^:static
  reduce1
       ([f coll]
             (let [s (seq coll)]
               (if s
         (reduce1 f (first s) (next s))
                 (f))))
       ([f val coll]
          (let [s (seq coll)]
            (if s
              (if (chunked-seq? s)
                (recur f 
                       (.reduce (chunk-first s) f val)
                       (chunk-next s))
                (recur f (f val (first s)) (next s)))
         val))))

(defn reverse
  "Returns a seq of the items in coll in reverse order. Not lazy."
  {:added "1.0"
   :static true}
  [coll]
    (reduce1 conj () coll))

;;math stuff
(defn ^:private nary-inline
  ([op] (nary-inline op op))
  ([op unchecked-op]
     (fn
       ([x] (let [op (if *unchecked-math* unchecked-op op)]
              `(. clojure.lang.Numbers (~op ~x))))
       ([x y] (let [op (if *unchecked-math* unchecked-op op)]
                `(. clojure.lang.Numbers (~op ~x ~y))))
       ([x y & more]
          (let [op (if *unchecked-math* unchecked-op op)]
            (reduce1
             (fn [a b] `(. clojure.lang.Numbers (~op ~a ~b)))
             `(. clojure.lang.Numbers (~op ~x ~y)) more))))))

(defn ^:private >1? [n] (clojure.lang.Numbers/gt n 1))
(defn ^:private >0? [n] (clojure.lang.Numbers/gt n 0))

(defn +'
  "Returns the sum of nums. (+') returns 0. Supports arbitrary precision.
  See also: +"
  {:inline (nary-inline 'addP)
   :inline-arities >1?
   :added "1.0"}
  ([] 0)
  ([x] (cast Number x))
  ([x y] (. clojure.lang.Numbers (addP x y)))
  ([x y & more]
   (reduce1 +' (+' x y) more)))

(defn +
  "Returns the sum of nums. (+) returns 0. Does not auto-promote
  longs, will throw on overflow. See also: +'"
  {:inline (nary-inline 'add 'unchecked_add)
   :inline-arities >1?
   :added "1.2"}
  ([] 0)
  ([x] (cast Number x))
  ([x y] (. clojure.lang.Numbers (add x y)))
  ([x y & more]
     (reduce1 + (+ x y) more)))

(defn *'
  "Returns the product of nums. (*') returns 1. Supports arbitrary precision.
  See also: *"
  {:inline (nary-inline 'multiplyP)
   :inline-arities >1?
   :added "1.0"}
  ([] 1)
  ([x] (cast Number x))
  ([x y] (. clojure.lang.Numbers (multiplyP x y)))
  ([x y & more]
   (reduce1 *' (*' x y) more)))

(defn *
  "Returns the product of nums. (*) returns 1. Does not auto-promote
  longs, will throw on overflow. See also: *'"
  {:inline (nary-inline 'multiply 'unchecked_multiply)
   :inline-arities >1?
   :added "1.2"}
  ([] 1)
  ([x] (cast Number x))
  ([x y] (. clojure.lang.Numbers (multiply x y)))
  ([x y & more]
     (reduce1 * (* x y) more)))

(defn /
  "If no denominators are supplied, returns 1/numerator,
  else returns numerator divided by all of the denominators."
  {:inline (nary-inline 'divide)
   :inline-arities >1?
   :added "1.0"}
  ([x] (/ 1 x))
  ([x y] (. clojure.lang.Numbers (divide x y)))
  ([x y & more]
   (reduce1 / (/ x y) more)))

(defn -'
  "If no ys are supplied, returns the negation of x, else subtracts
  the ys from x and returns the result. Supports arbitrary precision.
  See also: -"
  {:inline (nary-inline 'minusP)
   :inline-arities >0?
   :added "1.0"}
  ([x] (. clojure.lang.Numbers (minusP x)))
  ([x y] (. clojure.lang.Numbers (minusP x y)))
  ([x y & more]
   (reduce1 -' (-' x y) more)))

(defn -
  "If no ys are supplied, returns the negation of x, else subtracts
  the ys from x and returns the result. Does not auto-promote
  longs, will throw on overflow. See also: -'"
  {:inline (nary-inline 'minus 'unchecked_minus)
   :inline-arities >0?
   :added "1.2"}
  ([x] (. clojure.lang.Numbers (minus x)))
  ([x y] (. clojure.lang.Numbers (minus x y)))
  ([x y & more]
     (reduce1 - (- x y) more)))

(defn <=
  "Returns non-nil if nums are in monotonically non-decreasing order,
  otherwise false."
  {:inline (fn [x y] `(. clojure.lang.Numbers (lte ~x ~y)))
   :inline-arities #{2}
   :added "1.0"}
  ([x] true)
  ([x y] (. clojure.lang.Numbers (lte x y)))
  ([x y & more]
   (if (<= x y)
     (if (next more)
       (recur y (first more) (next more))
       (<= y (first more)))
     false)))

(defn >
  "Returns non-nil if nums are in monotonically decreasing order,
  otherwise false."
  {:inline (fn [x y] `(. clojure.lang.Numbers (gt ~x ~y)))
   :inline-arities #{2}
   :added "1.0"}
  ([x] true)
  ([x y] (. clojure.lang.Numbers (gt x y)))
  ([x y & more]
   (if (> x y)
     (if (next more)
       (recur y (first more) (next more))
       (> y (first more)))
     false)))

(defn >=
  "Returns non-nil if nums are in monotonically non-increasing order,
  otherwise false."
  {:inline (fn [x y] `(. clojure.lang.Numbers (gte ~x ~y)))
   :inline-arities #{2}
   :added "1.0"}
  ([x] true)
  ([x y] (. clojure.lang.Numbers (gte x y)))
  ([x y & more]
   (if (>= x y)
     (if (next more)
       (recur y (first more) (next more))
       (>= y (first more)))
     false)))

(defn ==
  "Returns non-nil if nums all have the equivalent
  value (type-independent), otherwise false"
  {:inline (fn [x y] `(. clojure.lang.Numbers (equiv ~x ~y)))
   :inline-arities #{2}
   :added "1.0"}
  ([x] true)
  ([x y] (. clojure.lang.Numbers (equiv x y)))
  ([x y & more]
   (if (== x y)
     (if (next more)
       (recur y (first more) (next more))
       (== y (first more)))
     false)))

(defn max
  "Returns the greatest of the nums."
  {:added "1.0"
   :inline-arities >1?
   :inline (nary-inline 'max)}
  ([x] x)
  ([x y] (. clojure.lang.Numbers (max x y)))
  ([x y & more]
   (reduce1 max (max x y) more)))

(defn min
  "Returns the least of the nums."
  {:added "1.0"
   :inline-arities >1?
   :inline (nary-inline 'min)}
  ([x] x)
  ([x y] (. clojure.lang.Numbers (min x y)))
  ([x y & more]
   (reduce1 min (min x y) more)))

(defn dec'
  "Returns a number one less than num. Supports arbitrary precision.
  See also: dec"
  {:inline (fn [x] `(. clojure.lang.Numbers (decP ~x)))
   :added "1.0"}
  [x] (. clojure.lang.Numbers (decP x)))

(defn dec
  "Returns a number one less than num. Does not auto-promote
  longs, will throw on overflow. See also: dec'"
  {:inline (fn [x] `(. clojure.lang.Numbers (~(if *unchecked-math* 'unchecked_dec 'dec) ~x)))
   :added "1.2"}
  [x] (. clojure.lang.Numbers (dec x)))

(defn unchecked-inc-int
  "Returns a number one greater than x, an int.
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x] `(. clojure.lang.Numbers (unchecked_int_inc ~x)))
   :added "1.0"}
  [x] (. clojure.lang.Numbers (unchecked_int_inc x)))

(defn unchecked-inc
  "Returns a number one greater than x, a long.
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x] `(. clojure.lang.Numbers (unchecked_inc ~x)))
   :added "1.0"}
  [x] (. clojure.lang.Numbers (unchecked_inc x)))

(defn unchecked-dec-int
  "Returns a number one less than x, an int.
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x] `(. clojure.lang.Numbers (unchecked_int_dec ~x)))
   :added "1.0"}
  [x] (. clojure.lang.Numbers (unchecked_int_dec x)))

(defn unchecked-dec
  "Returns a number one less than x, a long.
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x] `(. clojure.lang.Numbers (unchecked_dec ~x)))
   :added "1.0"}
  [x] (. clojure.lang.Numbers (unchecked_dec x)))

(defn unchecked-negate-int
  "Returns the negation of x, an int.
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x] `(. clojure.lang.Numbers (unchecked_int_negate ~x)))
   :added "1.0"}
  [x] (. clojure.lang.Numbers (unchecked_int_negate x)))

(defn unchecked-negate
  "Returns the negation of x, a long.
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x] `(. clojure.lang.Numbers (unchecked_minus ~x)))
   :added "1.0"}
  [x] (. clojure.lang.Numbers (unchecked_minus x)))

(defn unchecked-add-int
  "Returns the sum of x and y, both int.
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x y] `(. clojure.lang.Numbers (unchecked_int_add ~x ~y)))
   :added "1.0"}
  [x y] (. clojure.lang.Numbers (unchecked_int_add x y)))

(defn unchecked-add
  "Returns the sum of x and y, both long.
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x y] `(. clojure.lang.Numbers (unchecked_add ~x ~y)))
   :added "1.0"}
  [x y] (. clojure.lang.Numbers (unchecked_add x y)))

(defn unchecked-subtract-int
  "Returns the difference of x and y, both int.
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x y] `(. clojure.lang.Numbers (unchecked_int_subtract ~x ~y)))
   :added "1.0"}
  [x y] (. clojure.lang.Numbers (unchecked_int_subtract x y)))

(defn unchecked-subtract
  "Returns the difference of x and y, both long.
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x y] `(. clojure.lang.Numbers (unchecked_minus ~x ~y)))
   :added "1.0"}
  [x y] (. clojure.lang.Numbers (unchecked_minus x y)))

(defn unchecked-multiply-int
  "Returns the product of x and y, both int.
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x y] `(. clojure.lang.Numbers (unchecked_int_multiply ~x ~y)))
   :added "1.0"}
  [x y] (. clojure.lang.Numbers (unchecked_int_multiply x y)))

(defn unchecked-multiply
  "Returns the product of x and y, both long.
  Note - uses a primitive operator subject to overflow."
  {:inline (fn [x y] `(. clojure.lang.Numbers (unchecked_multiply ~x ~y)))
   :added "1.0"}
  [x y] (. clojure.lang.Numbers (unchecked_multiply x y)))

(defn unchecked-divide-int
  "Returns the division of x by y, both int.
  Note - uses a primitive operator subject to truncation."
  {:inline (fn [x y] `(. clojure.lang.Numbers (unchecked_int_divide ~x ~y)))
   :added "1.0"}
  [x y] (. clojure.lang.Numbers (unchecked_int_divide x y)))

(defn unchecked-remainder-int
  "Returns the remainder of division of x by y, both int.
  Note - uses a primitive operator subject to truncation."
  {:inline (fn [x y] `(. clojure.lang.Numbers (unchecked_int_remainder ~x ~y)))
   :added "1.0"}
  [x y] (. clojure.lang.Numbers (unchecked_int_remainder x y)))

(defn pos?
  "Returns true if num is greater than zero, else false"
  {
   :inline (fn [x] `(. clojure.lang.Numbers (isPos ~x)))
   :added "1.0"}
  [x] (. clojure.lang.Numbers (isPos x)))

(defn neg?
  "Returns true if num is less than zero, else false"
  {
   :inline (fn [x] `(. clojure.lang.Numbers (isNeg ~x)))
   :added "1.0"}
  [x] (. clojure.lang.Numbers (isNeg x)))

(defn quot
  "quot[ient] of dividing numerator by denominator."
  {:added "1.0"
   :static true
   :inline (fn [x y] `(. clojure.lang.Numbers (quotient ~x ~y)))}
  [num div]
    (. clojure.lang.Numbers (quotient num div)))

(defn rem
  "remainder of dividing numerator by denominator."
  {:added "1.0"
   :static true
   :inline (fn [x y] `(. clojure.lang.Numbers (remainder ~x ~y)))}
  [num div]
    (. clojure.lang.Numbers (remainder num div)))

(defn rationalize
  "returns the rational value of num"
  {:added "1.0"
   :static true}
  [num]
  (. clojure.lang.Numbers (rationalize num)))

;;Bit ops

(defn bit-not
  "Bitwise complement"
  {:inline (fn [x] `(. clojure.lang.Numbers (not ~x)))
   :added "1.0"}
  [x] (. clojure.lang.Numbers not x))


(defn bit-and
  "Bitwise and"
   {:inline (nary-inline 'and)
    :inline-arities >1?
    :added "1.0"}
   ([x y] (. clojure.lang.Numbers and x y))
   ([x y & more]
      (reduce1 bit-and (bit-and x y) more)))

(defn bit-or
  "Bitwise or"
  {:inline (nary-inline 'or)
   :inline-arities >1?
   :added "1.0"}
  ([x y] (. clojure.lang.Numbers or x y))
  ([x y & more]
    (reduce1 bit-or (bit-or x y) more)))

(defn bit-xor
  "Bitwise exclusive or"
  {:inline (nary-inline 'xor)
   :inline-arities >1?
   :added "1.0"}
  ([x y] (. clojure.lang.Numbers xor x y))
  ([x y & more]
    (reduce1 bit-xor (bit-xor x y) more)))

(defn bit-and-not
  "Bitwise and with complement"
  {:inline (nary-inline 'andNot)
   :inline-arities >1?
   :added "1.0"
   :static true}
  ([x y] (. clojure.lang.Numbers andNot x y))
  ([x y & more]
    (reduce1 bit-and-not (bit-and-not x y) more)))


(defn bit-clear
  "Clear bit at index n"
  {:added "1.0"
   :static true}
  [x n] (. clojure.lang.Numbers clearBit x n))

(defn bit-set
  "Set bit at index n"
  {:added "1.0"
   :static true}
  [x n] (. clojure.lang.Numbers setBit x n))

(defn bit-flip
  "Flip bit at index n"
  {:added "1.0"
   :static true}
  [x n] (. clojure.lang.Numbers flipBit x n))

(defn bit-test
  "Test bit at index n"
  {:added "1.0"
   :static true}
  [x n] (. clojure.lang.Numbers testBit x n))


(defn bit-shift-left
  "Bitwise shift left"
  {:inline (fn [x n] `(. clojure.lang.Numbers (shiftLeft ~x ~n)))
   :added "1.0"}
  [x n] (. clojure.lang.Numbers shiftLeft x n))

(defn bit-shift-right
  "Bitwise shift right"
  {:inline (fn [x n] `(. clojure.lang.Numbers (shiftRight ~x ~n)))
   :added "1.0"}
  [x n] (. clojure.lang.Numbers shiftRight x n))

(defn unsigned-bit-shift-right
  "Bitwise shift right, without sign-extension."
  {:inline (fn [x n] `(. clojure.lang.Numbers (unsignedShiftRight ~x ~n)))
   :added "1.6"}
  [x n] (. clojure.lang.Numbers unsignedShiftRight x n))

(defn integer?
  "Returns true if n is an integer"
  {:added "1.0"
   :static true}
  [n]
  (or (instance? Integer n)
      (instance? Long n)
      (instance? clojure.lang.BigInt n)
      (instance? BigInteger n)
      (instance? Short n)
      (instance? Byte n)))

(defn even?
  "Returns true if n is even, throws an exception if n is not an integer"
  {:added "1.0"
   :static true}
   [n] (if (integer? n)
        (zero? (bit-and (clojure.lang.RT/uncheckedLongCast n) 1))
        (throw (IllegalArgumentException. (str "Argument must be an integer: " n)))))

(defn odd?
  "Returns true if n is odd, throws an exception if n is not an integer"
  {:added "1.0"
   :static true}
  [n] (not (even? n)))


;;

(defn complement
  "Takes a fn f and returns a fn that takes the same arguments as f,
  has the same effects, if any, and returns the opposite truth value."
  {:added "1.0"
   :static true}
  [f] 
  (fn 
    ([] (not (f)))
    ([x] (not (f x)))
    ([x y] (not (f x y)))
    ([x y & zs] (not (apply f x y zs)))))

(defn constantly
  "Returns a function that takes any number of arguments and returns x."
  {:added "1.0"
   :static true}
  [x] (fn [& args] x))

(defn identity
  "Returns its argument."
  {:added "1.0"
   :static true}
  [x] x)

;;Collection stuff

;;list stuff
(defn peek
  "For a list or queue, same as first, for a vector, same as, but much
  more efficient than, last. If the collection is empty, returns nil."
  {:added "1.0"
   :static true}
  [coll] (. clojure.lang.RT (peek coll)))

(defn pop
  "For a list or queue, returns a new list/queue without the first
  item, for a vector, returns a new vector without the last item. If
  the collection is empty, throws an exception.  Note - not the same
  as next/butlast."
  {:added "1.0"
   :static true}
  [coll] (. clojure.lang.RT (pop coll)))

;;map stuff

(defn map-entry?
  "Return true if x is a map entry"
  {:added "1.8"}
  [x]
	(instance? java.util.Map$Entry x))

(defn contains?
  "Returns true if key is present in the given collection, otherwise
  returns false.  Note that for numerically indexed collections like
  vectors and Java arrays, this tests if the numeric key is within the
  range of indexes. 'contains?' operates constant or logarithmic time;
  it will not perform a linear search for a value.  See also 'some'."
  {:added "1.0"
   :static true}
  [coll key] (. clojure.lang.RT (contains coll key)))

(defn get
  "Returns the value mapped to key, not-found or nil if key not present."
  {:inline (fn  [m k & nf] `(. clojure.lang.RT (get ~m ~k ~@nf)))
   :inline-arities #{2 3}
   :added "1.0"}
  ([map key]
   (. clojure.lang.RT (get map key)))
  ([map key not-found]
   (. clojure.lang.RT (get map key not-found))))

(defn dissoc
  "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,
  that does not contain a mapping for key(s)."
  {:added "1.0"
   :static true}
  ([map] map)
  ([map key]
   (. clojure.lang.RT (dissoc map key)))
  ([map key & ks]
   (let [ret (dissoc map key)]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))

(defn disj
  "disj[oin]. Returns a new set of the same (hashed/sorted) type, that
  does not contain key(s)."
  {:added "1.0"
   :static true}
  ([set] set)
  ([^clojure.lang.IPersistentSet set key]
   (when set
     (. set (disjoin key))))
  ([set key & ks]
   (when set
     (let [ret (disj set key)]
       (if ks
         (recur ret (first ks) (next ks))
         ret)))))

(defn find
  "Returns the map entry for key, or nil if key not present."
  {:added "1.0"
   :static true}
  [map key] (. clojure.lang.RT (find map key)))

(defn select-keys
  "Returns a map containing only those entries in map whose key is in keys"
  {:added "1.0"
   :static true}
  [map keyseq]
    (loop [ret {} keys (seq keyseq)]
      (if keys
        (let [entry (. clojure.lang.RT (find map (first keys)))]
          (recur
           (if entry
             (conj ret entry)
             ret)
           (next keys)))
        (with-meta ret (meta map)))))

(defn keys
  "Returns a sequence of the map's keys, in the same order as (seq map)."
  {:added "1.0"
   :static true}
  [map] (. clojure.lang.RT (keys map)))

(defn vals
  "Returns a sequence of the map's values, in the same order as (seq map)."
  {:added "1.0"
   :static true}
  [map] (. clojure.lang.RT (vals map)))

(defn key
  "Returns the key of the map entry."
  {:added "1.0"
   :static true}
  [^java.util.Map$Entry e]
    (. e (getKey)))

(defn val
  "Returns the value in the map entry."
  {:added "1.0"
   :static true}
  [^java.util.Map$Entry e]
    (. e (getValue)))

(defn rseq
  "Returns, in constant time, a seq of the items in rev (which
  can be a vector or sorted-map), in reverse order. If rev is empty returns nil"
  {:added "1.0"
   :static true}
  [^clojure.lang.Reversible rev]
    (. rev (rseq)))

(defn name
  "Returns the name String of a string, symbol or keyword."
  {:tag String
   :added "1.0"
   :static true}
  [x]
  (if (string? x) x (. ^clojure.lang.Named x (getName))))

(defn namespace
  "Returns the namespace String of a symbol or keyword, or nil if not present."
  {:tag String
   :added "1.0"
   :static true}
  [^clojure.lang.Named x]
    (. x (getNamespace)))

(defmacro locking
  "Executes exprs in an implicit do, while holding the monitor of x.
  Will release the monitor of x in all circumstances."
  {:added "1.0"}
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
  {:added "1.0"}
  ([x form] `(. ~x ~form))
  ([x form & more] `(.. (. ~x ~form) ~@more)))

(defmacro ->
  "Threads the expr through the forms. Inserts x as the
  second item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  second item in second form, etc."
  {:added "1.0"}
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(defmacro ->>
  "Threads the expr through the forms. Inserts x as the
  last item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  last item in second form, etc."
  {:added "1.1"}
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
              (with-meta `(~(first form) ~@(next form)  ~x) (meta form))
              (list form x))]
        (recur threaded (next forms)))
      x)))

(def map)

(defn ^:private check-valid-options
  "Throws an exception if the given option map contains keys not listed
  as valid, else returns nil."
  [options & valid-keys]
  (when (seq (apply disj (apply hash-set (keys options)) valid-keys))
    (throw
      (IllegalArgumentException.
        (apply str "Only these options are valid: "
          (first valid-keys)
          (map #(str ", " %) (rest valid-keys)))))))

;;multimethods
(def global-hierarchy)

(defmacro defmulti
  "Creates a new multimethod with the associated dispatch function.
  The docstring and attr-map are optional.

  Options are key-value pairs and may be one of:

  :default

  The default dispatch value, defaults to :default

  :hierarchy

  The value used for hierarchical dispatch (e.g. ::square is-a ::shape)

  Hierarchies are type-like relationships that do not depend upon type
  inheritance. By default Clojure's multimethods dispatch off of a
  global hierarchy map.  However, a hierarchy relationship can be
  created with the derive function used to augment the root ancestor
  created with make-hierarchy.

  Multimethods expect the value of the hierarchy option to be supplied as
  a reference type e.g. a var (i.e. via the Var-quote dispatch macro #'
  or the var special form)."
  {:arglists '([name docstring? attr-map? dispatch-fn & options])
   :added "1.0"}
  [mm-name & options]
  (let [docstring   (if (string? (first options))
                      (first options)
                      nil)
        options     (if (string? (first options))
                      (next options)
                      options)
        m           (if (map? (first options))
                      (first options)
                      {})
        options     (if (map? (first options))
                      (next options)
                      options)
        dispatch-fn (first options)
        options     (next options)
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
      (check-valid-options options :default :hierarchy)
      `(let [v# (def ~mm-name)]
         (when-not (and (.hasRoot v#) (instance? clojure.lang.MultiFn (deref v#)))
           (def ~(with-meta mm-name m)
                (new clojure.lang.MultiFn ~(name mm-name) ~dispatch-fn ~default ~hierarchy)))))))

(defmacro defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value. "
  {:added "1.0"}
  [multifn dispatch-val & fn-tail]
  `(. ~(with-meta multifn {:tag 'clojure.lang.MultiFn}) addMethod ~dispatch-val (fn ~@fn-tail)))

(defn remove-all-methods
  "Removes all of the methods of multimethod."
  {:added "1.2"
   :static true} 
 [^clojure.lang.MultiFn multifn]
 (.reset multifn))

(defn remove-method
  "Removes the method of multimethod associated with dispatch-value."
  {:added "1.0"
   :static true}
 [^clojure.lang.MultiFn multifn dispatch-val]
 (. multifn removeMethod dispatch-val))

(defn prefer-method
  "Causes the multimethod to prefer matches of dispatch-val-x over dispatch-val-y 
   when there is a conflict"
  {:added "1.0"
   :static true}
  [^clojure.lang.MultiFn multifn dispatch-val-x dispatch-val-y]
  (. multifn preferMethod dispatch-val-x dispatch-val-y))

(defn methods
  "Given a multimethod, returns a map of dispatch values -> dispatch fns"
  {:added "1.0"
   :static true}
  [^clojure.lang.MultiFn multifn] (.getMethodTable multifn))

(defn get-method
  "Given a multimethod and a dispatch value, returns the dispatch fn
  that would apply to that value, or nil if none apply and no default"
  {:added "1.0"
   :static true}
  [^clojure.lang.MultiFn multifn dispatch-val] (.getMethod multifn dispatch-val))

(defn prefers
  "Given a multimethod, returns a map of preferred value -> set of other values"
  {:added "1.0"
   :static true}
  [^clojure.lang.MultiFn multifn] (.getPreferTable multifn))

;;;;;;;;; var stuff

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args more)))))

(defmacro if-let
  "bindings => binding-form test

  If test is true, evaluates then with binding-form bound to the value of 
  test, if not, yields else"
  {:added "1.0"}
  ([bindings then]
   `(if-let ~bindings ~then nil))
  ([bindings then else & oldform]
   (assert-args
     (vector? bindings) "a vector for its binding"
     (nil? oldform) "1 or 2 forms after binding vector"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if temp#
          (let [~form temp#]
            ~then)
          ~else)))))

(defmacro when-let
  "bindings => binding-form test

  When test is true, evaluates body with binding-form bound to the value of test"
  {:added "1.0"}
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (when temp#
         (let [~form temp#]
           ~@body)))))

(defmacro if-some
  "bindings => binding-form test

   If test is not nil, evaluates then with binding-form bound to the
   value of test, if not, yields else"
  {:added "1.6"}
  ([bindings then]
   `(if-some ~bindings ~then nil))
  ([bindings then else & oldform]
   (assert-args
     (vector? bindings) "a vector for its binding"
     (nil? oldform) "1 or 2 forms after binding vector"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if (nil? temp#)
          ~else
          (let [~form temp#]
            ~then))))))

(defmacro when-some
  "bindings => binding-form test

   When test is not nil, evaluates body with binding-form bound to the
   value of test"
  {:added "1.6"}
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if (nil? temp#)
         nil
         (let [~form temp#]
           ~@body)))))

(defn push-thread-bindings
  "WARNING: This is a low-level function. Prefer high-level macros like
  binding where ever possible.

  Takes a map of Var/value pairs. Binds each Var to the associated value for
  the current thread. Each call *MUST* be accompanied by a matching call to
  pop-thread-bindings wrapped in a try-finally!
  
      (push-thread-bindings bindings)
      (try
        ...
        (finally
          (pop-thread-bindings)))"
  {:added "1.1"
   :static true} 
  [bindings]
  (clojure.lang.Var/pushThreadBindings bindings))

(defn pop-thread-bindings
  "Pop one set of bindings pushed with push-binding before. It is an error to
  pop bindings without pushing before."
  {:added "1.1"
   :static true}
  []
  (clojure.lang.Var/popThreadBindings))

(defn get-thread-bindings
  "Get a map with the Var/value pairs which is currently in effect for the
  current thread."
  {:added "1.1"
   :static true}
  []
  (clojure.lang.Var/getThreadBindings))

(defmacro binding
  "binding => var-symbol init-expr

  Creates new bindings for the (already-existing) vars, with the
  supplied initial values, executes the exprs in an implicit do, then
  re-establishes the bindings that existed before.  The new bindings
  are made in parallel (unlike let); all init-exprs are evaluated
  before the vars are bound to their new values."
  {:added "1.0"}
  [bindings & body]
  (assert-args
    (vector? bindings) "a vector for its binding"
    (even? (count bindings)) "an even number of forms in binding vector")
  (let [var-ize (fn [var-vals]
                  (loop [ret [] vvs (seq var-vals)]
                    (if vvs
                      (recur  (conj (conj ret `(var ~(first vvs))) (second vvs))
                             (next (next vvs)))
                      (seq ret))))]
    `(let []
       (push-thread-bindings (hash-map ~@(var-ize bindings)))
       (try
         ~@body
         (finally
           (pop-thread-bindings))))))

(defn with-bindings*
  "Takes a map of Var/value pairs. Installs for the given Vars the associated
  values as thread-local bindings. Then calls f with the supplied arguments.
  Pops the installed bindings after f returned. Returns whatever f returns."
  {:added "1.1"
   :static true}
  [binding-map f & args]
  (push-thread-bindings binding-map)
  (try
    (apply f args)
    (finally
      (pop-thread-bindings))))

(defmacro with-bindings
  "Takes a map of Var/value pairs. Installs for the given Vars the associated
  values as thread-local bindings. Then executes body. Pops the installed
  bindings after body was evaluated. Returns the value of body."
  {:added "1.1"}
  [binding-map & body]
  `(with-bindings* ~binding-map (fn [] ~@body)))

(defn bound-fn*
  "Returns a function, which will install the same bindings in effect as in
  the thread at the time bound-fn* was called and then call f with any given
  arguments. This may be used to define a helper function which runs on a
  different thread, but needs the same bindings in place."
  {:added "1.1"
   :static true}
  [f]
  (let [bindings (get-thread-bindings)]
    (fn [& args]
      (apply with-bindings* bindings f args))))

(defmacro bound-fn
  "Returns a function defined by the given fntail, which will install the
  same bindings in effect as in the thread at the time bound-fn was called.
  This may be used to define a helper function which runs on a different
  thread, but needs the same bindings in place."
  {:added "1.1"}
  [& fntail]
  `(bound-fn* (fn ~@fntail)))

(defn find-var
  "Returns the global var named by the namespace-qualified symbol, or
  nil if no var with that name."
  {:added "1.0"
   :static true}
  [sym] (. clojure.lang.Var (find sym)))

(defn binding-conveyor-fn
  {:private true
   :added "1.3"}
  [f]
  (let [frame (clojure.lang.Var/cloneThreadBindingFrame)]
    (fn 
      ([]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f))
      ([x]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x))
      ([x y]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x y))
      ([x y z]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x y z))
      ([x y z & args] 
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (apply f x y z args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Refs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ^{:private true}
  setup-reference [^clojure.lang.ARef r options]
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

  :error-handler handler-fn

  :error-mode mode-keyword

  If metadata-map is supplied, it will become the metadata on the
  agent. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an exception.  handler-fn is called if an
  action throws an exception or if validate-fn rejects a new state --
  see set-error-handler! for details.  The mode-keyword may be either
  :continue (the default if an error-handler is given) or :fail (the
  default if no error-handler is given) -- see set-error-mode! for
  details."
  {:added "1.0"
   :static true
   }
  ([state & options]
     (let [a (new clojure.lang.Agent state)
           opts (apply hash-map options)]
       (setup-reference a options)
       (when (:error-handler opts)
         (.setErrorHandler a (:error-handler opts)))
       (.setErrorMode a (or (:error-mode opts)
                            (if (:error-handler opts) :continue :fail)))
       a)))

(defn set-agent-send-executor!
  "Sets the ExecutorService to be used by send"
  {:added "1.5"}
  [executor]
  (set! clojure.lang.Agent/pooledExecutor executor))

(defn set-agent-send-off-executor!
  "Sets the ExecutorService to be used by send-off"
  {:added "1.5"}
  [executor]
  (set! clojure.lang.Agent/soloExecutor executor))

(defn send-via
  "Dispatch an action to an agent. Returns the agent immediately.
  Subsequently, in a thread supplied by executor, the state of the agent
  will be set to the value of:

  (apply action-fn state-of-agent args)"
  {:added "1.5"}
  [executor ^clojure.lang.Agent a f & args]
  (.dispatch a (binding [*agent* a] (binding-conveyor-fn f)) args executor))

(defn send
  "Dispatch an action to an agent. Returns the agent immediately.
  Subsequently, in a thread from a thread pool, the state of the agent
  will be set to the value of:

  (apply action-fn state-of-agent args)"
  {:added "1.0"
   :static true}
  [^clojure.lang.Agent a f & args]
  (apply send-via clojure.lang.Agent/pooledExecutor a f args))

(defn send-off
  "Dispatch a potentially blocking action to an agent. Returns the
  agent immediately. Subsequently, in a separate thread, the state of
  the agent will be set to the value of:

  (apply action-fn state-of-agent args)"
  {:added "1.0"
   :static true}
  [^clojure.lang.Agent a f & args]
  (apply send-via clojure.lang.Agent/soloExecutor a f args))

(defn release-pending-sends
  "Normally, actions sent directly or indirectly during another action
  are held until the action completes (changes the agent's
  state). This function can be used to dispatch any pending sent
  actions immediately. This has no impact on actions sent during a
  transaction, which are still held until commit. If no action is
  occurring, does nothing. Returns the number of actions dispatched."
  {:added "1.0"
   :static true}
  [] (clojure.lang.Agent/releasePendingSends))

(defn add-watch
  "Adds a watch function to an agent/atom/var/ref reference. The watch
  fn must be a fn of 4 args: a key, the reference, its old-state, its
  new-state. Whenever the reference's state might have been changed,
  any registered watches will have their functions called. The watch fn
  will be called synchronously, on the agent's thread if an agent,
  before any pending sends if agent or ref. Note that an atom's or
  ref's state may have changed again prior to the fn call, so use
  old/new-state rather than derefing the reference. Note also that watch
  fns may be called from multiple threads simultaneously. Var watchers
  are triggered only by root binding changes, not thread-local
  set!s. Keys must be unique per reference, and can be used to remove
  the watch with remove-watch, but are otherwise considered opaque by
  the watch mechanism."
  {:added "1.0"
   :static true}
  [^clojure.lang.IRef reference key fn] (.addWatch reference key fn))

(defn remove-watch
  "Removes a watch (set by add-watch) from a reference"
  {:added "1.0"
   :static true}
  [^clojure.lang.IRef reference key]
  (.removeWatch reference key))

(defn agent-error
  "Returns the exception thrown during an asynchronous action of the
  agent if the agent is failed.  Returns nil if the agent is not
  failed."
  {:added "1.2"
   :static true}
  [^clojure.lang.Agent a] (.getError a))

(defn restart-agent
  "When an agent is failed, changes the agent state to new-state and
  then un-fails the agent so that sends are allowed again.  If
  a :clear-actions true option is given, any actions queued on the
  agent that were being held while it was failed will be discarded,
  otherwise those held actions will proceed.  The new-state must pass
  the validator if any, or restart will throw an exception and the
  agent will remain failed with its old state and error.  Watchers, if
  any, will NOT be notified of the new state.  Throws an exception if
  the agent is not failed."
  {:added "1.2"
   :static true
   }
  [^clojure.lang.Agent a, new-state & options]
  (let [opts (apply hash-map options)]
    (.restart a new-state (if (:clear-actions opts) true false))))

(defn set-error-handler!
  "Sets the error-handler of agent a to handler-fn.  If an action
  being run by the agent throws an exception or doesn't pass the
  validator fn, handler-fn will be called with two arguments: the
  agent and the exception."
  {:added "1.2"
   :static true}
  [^clojure.lang.Agent a, handler-fn]
  (.setErrorHandler a handler-fn))

(defn error-handler
  "Returns the error-handler of agent a, or nil if there is none.
  See set-error-handler!"
  {:added "1.2"
   :static true}
  [^clojure.lang.Agent a]
  (.getErrorHandler a))

(defn set-error-mode!
  "Sets the error-mode of agent a to mode-keyword, which must be
  either :fail or :continue.  If an action being run by the agent
  throws an exception or doesn't pass the validator fn, an
  error-handler may be called (see set-error-handler!), after which,
  if the mode is :continue, the agent will continue as if neither the
  action that caused the error nor the error itself ever happened.
  
  If the mode is :fail, the agent will become failed and will stop
  accepting new 'send' and 'send-off' actions, and any previously
  queued actions will be held until a 'restart-agent'.  Deref will
  still work, returning the state of the agent before the error."
  {:added "1.2"
   :static true}
  [^clojure.lang.Agent a, mode-keyword]
  (.setErrorMode a mode-keyword))

(defn error-mode
  "Returns the error-mode of agent a.  See set-error-mode!"
  {:added "1.2"
   :static true}
  [^clojure.lang.Agent a]
  (.getErrorMode a))

(defn agent-errors
  "DEPRECATED: Use 'agent-error' instead.
  Returns a sequence of the exceptions thrown during asynchronous
  actions of the agent."
  {:added "1.0"
   :deprecated "1.2"}
  [a]
  (when-let [e (agent-error a)]
    (list e)))

(defn clear-agent-errors
  "DEPRECATED: Use 'restart-agent' instead.
  Clears any exceptions thrown during asynchronous actions of the
  agent, allowing subsequent actions to occur."
  {:added "1.0"
   :deprecated "1.2"}
  [^clojure.lang.Agent a] (restart-agent a (.deref a)))

(defn shutdown-agents
  "Initiates a shutdown of the thread pools that back the agent
  system. Running actions will complete, but no new actions will be
  accepted"
  {:added "1.0"
   :static true}
  [] (. clojure.lang.Agent shutdown))

(defn ref
  "Creates and returns a Ref with an initial value of x and zero or
  more options (in any order):

  :meta metadata-map

  :validator validate-fn

  :min-history (default 0)
  :max-history (default 10)

  If metadata-map is supplied, it will become the metadata on the
  ref. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an exception. validate-fn will be called on
  transaction commit, when all refs have their final values.

  Normally refs accumulate history dynamically as needed to deal with
  read demands. If you know in advance you will need history you can
  set :min-history to ensure it will be available when first needed (instead
  of after a read fault). History is limited, and the limit can be set
  with :max-history."
  {:added "1.0"
   :static true
   }
  ([x] (new clojure.lang.Ref x))
  ([x & options] 
   (let [r  ^clojure.lang.Ref (setup-reference (ref x) options)
         opts (apply hash-map options)]
    (when (:max-history opts)
      (.setMaxHistory r (:max-history opts)))
    (when (:min-history opts)
      (.setMinHistory r (:min-history opts)))
    r)))

(defn ^:private deref-future
  ([^java.util.concurrent.Future fut]
     (.get fut))
  ([^java.util.concurrent.Future fut timeout-ms timeout-val]
     (try (.get fut timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
          (catch java.util.concurrent.TimeoutException e
            timeout-val))))
     
(defn deref
  "Also reader macro: @ref/@agent/@var/@atom/@delay/@future/@promise. Within a transaction,
  returns the in-transaction-value of ref, else returns the
  most-recently-committed value of ref. When applied to a var, agent
  or atom, returns its current state. When applied to a delay, forces
  it if not already forced. When applied to a future, will block if
  computation not complete. When applied to a promise, will block
  until a value is delivered.  The variant taking a timeout can be
  used for blocking references (futures and promises), and will return
  timeout-val if the timeout (in milliseconds) is reached before a
  value is available. See also - realized?."
  {:added "1.0"
   :static true}
  ([ref] (if (instance? clojure.lang.IDeref ref)
           (.deref ^clojure.lang.IDeref ref)
           (deref-future ref)))
  ([ref timeout-ms timeout-val]
     (if (instance? clojure.lang.IBlockingDeref ref)
       (.deref ^clojure.lang.IBlockingDeref ref timeout-ms timeout-val)
       (deref-future ref timeout-ms timeout-val))))

(defn atom
  "Creates and returns an Atom with an initial value of x and zero or
  more options (in any order):

  :meta metadata-map

  :validator validate-fn

  If metadata-map is supplied, it will become the metadata on the
  atom. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an exception."
  {:added "1.0"
   :static true}
  ([x] (new clojure.lang.Atom x))
  ([x & options] (setup-reference (atom x) options)))

(defn swap!
  "Atomically swaps the value of atom to be:
  (apply f current-value-of-atom args). Note that f may be called
  multiple times, and thus should be free of side effects.  Returns
  the value that was swapped in."
  {:added "1.0"
   :static true}
  ([^clojure.lang.IAtom atom f] (.swap atom f))
  ([^clojure.lang.IAtom atom f x] (.swap atom f x))
  ([^clojure.lang.IAtom atom f x y] (.swap atom f x y))
  ([^clojure.lang.IAtom atom f x y & args] (.swap atom f x y args)))

(defn compare-and-set!
  "Atomically sets the value of atom to newval if and only if the
  current value of the atom is identical to oldval. Returns true if
  set happened, else false"
  {:added "1.0"
   :static true}
  [^clojure.lang.IAtom atom oldval newval] (.compareAndSet atom oldval newval))

(defn reset!
  "Sets the value of atom to newval without regard for the
  current value. Returns newval."
  {:added "1.0"
   :static true}
  [^clojure.lang.IAtom atom newval] (.reset atom newval))

(defn set-validator!
  "Sets the validator-fn for a var/ref/agent/atom. validator-fn must be nil or a
  side-effect-free fn of one argument, which will be passed the intended
  new state on any state change. If the new state is unacceptable, the
  validator-fn should return false or throw an exception. If the current state (root
  value if var) is not acceptable to the new validator, an exception
  will be thrown and the validator will not be changed."
  {:added "1.0"
   :static true}
  [^clojure.lang.IRef iref validator-fn] (. iref (setValidator validator-fn)))

(defn get-validator
  "Gets the validator-fn for a var/ref/agent/atom."
  {:added "1.0"
   :static true}
 [^clojure.lang.IRef iref] (. iref (getValidator)))

(defn alter-meta!
  "Atomically sets the metadata for a namespace/var/ref/agent/atom to be:

  (apply f its-current-meta args)

  f must be free of side-effects"
  {:added "1.0"
   :static true}
 [^clojure.lang.IReference iref f & args] (.alterMeta iref f args))

(defn reset-meta!
  "Atomically resets the metadata for a namespace/var/ref/agent/atom"
  {:added "1.0"
   :static true}
 [^clojure.lang.IReference iref metadata-map] (.resetMeta iref metadata-map))

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
  {:added "1.0"
   :static true}

  [^clojure.lang.Ref ref fun & args]
    (. ref (commute fun args)))

(defn alter
  "Must be called in a transaction. Sets the in-transaction-value of
  ref to:

  (apply fun in-transaction-value-of-ref args)

  and returns the in-transaction-value of ref."
  {:added "1.0"
   :static true}
  [^clojure.lang.Ref ref fun & args]
    (. ref (alter fun args)))

(defn ref-set
  "Must be called in a transaction. Sets the value of ref.
  Returns val."
  {:added "1.0"
   :static true}
  [^clojure.lang.Ref ref val]
    (. ref (set val)))

(defn ref-history-count
  "Returns the history count of a ref"
  {:added "1.1"
   :static true}
  [^clojure.lang.Ref ref]
    (.getHistoryCount ref))

(defn ref-min-history
  "Gets the min-history of a ref, or sets it and returns the ref"
  {:added "1.1"
   :static true}
  ([^clojure.lang.Ref ref]
    (.getMinHistory ref))
  ([^clojure.lang.Ref ref n]
    (.setMinHistory ref n)))

(defn ref-max-history
  "Gets the max-history of a ref, or sets it and returns the ref"
  {:added "1.1"
   :static true}
  ([^clojure.lang.Ref ref]
    (.getMaxHistory ref))
  ([^clojure.lang.Ref ref n]
    (.setMaxHistory ref n)))

(defn ensure
  "Must be called in a transaction. Protects the ref from modification
  by other transactions.  Returns the in-transaction-value of
  ref. Allows for more concurrency than (ref-set ref @ref)"
  {:added "1.0"
   :static true}
  [^clojure.lang.Ref ref]
    (. ref (touch))
    (. ref (deref)))

(defmacro sync
  "transaction-flags => TBD, pass nil for now

  Runs the exprs (in an implicit do) in a transaction that encompasses
  exprs and any nested calls.  Starts a transaction if none is already
  running on this thread. Any uncaught exception will abort the
  transaction and flow out of sync. The exprs may be run more than
  once, but any effects on Refs will be atomic."
  {:added "1.0"}
  [flags-ignored-for-now & body]
  `(. clojure.lang.LockingTransaction
      (runInTransaction (fn [] ~@body))))


(defmacro io!
  "If an io! block occurs in a transaction, throws an
  IllegalStateException, else runs body in an implicit do. If the
  first expression in body is a literal string, will use that as the
  exception message."
  {:added "1.0"}
  [& body]
  (let [message (when (string? (first body)) (first body))
        body (if message (next body) body)]
    `(if (clojure.lang.LockingTransaction/isRunning)
       (throw (new IllegalStateException ~(or message "I/O in transaction")))
       (do ~@body))))

(defn volatile!
  "Creates and returns a Volatile with an initial value of val."
  {:added "1.7"
   :tag clojure.lang.Volatile}
  [val]
  (clojure.lang.Volatile. val))

(defn vreset!
  "Sets the value of volatile to newval without regard for the
   current value. Returns newval."
  {:added "1.7"}
  [^clojure.lang.Volatile vol newval]
  (.reset vol newval))

(defmacro vswap!
  "Non-atomically swaps the value of the volatile as if:
   (apply f current-value-of-vol args). Returns the value that
   was swapped in."
  {:added "1.7"}
  [vol f & args]
  (let [v (with-meta vol {:tag 'clojure.lang.Volatile})]
    `(.reset ~v (~f (.deref ~v) ~@args))))

(defn volatile?
  "Returns true if x is a volatile."
  {:added "1.7"}
  [x]
  (instance? clojure.lang.Volatile x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fn stuff ;;;;;;;;;;;;;;;;


(defn comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc."
  {:added "1.0"
   :static true}
  ([] identity)
  ([f] f)
  ([f g] 
     (fn 
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
  ([f g & fs]
     (reduce1 comp (list* f g fs))))

(defn juxt 
  "Takes a set of functions and returns a fn that is the juxtaposition
  of those fns.  The returned fn takes a variable number of args, and
  returns a vector containing the result of applying each fn to the
  args (left-to-right).
  ((juxt a b c) x) => [(a x) (b x) (c x)]"
  {:added "1.1"
   :static true}
  ([f] 
     (fn
       ([] [(f)])
       ([x] [(f x)])
       ([x y] [(f x y)])
       ([x y z] [(f x y z)])
       ([x y z & args] [(apply f x y z args)])))
  ([f g] 
     (fn
       ([] [(f) (g)])
       ([x] [(f x) (g x)])
       ([x y] [(f x y) (g x y)])
       ([x y z] [(f x y z) (g x y z)])
       ([x y z & args] [(apply f x y z args) (apply g x y z args)])))
  ([f g h] 
     (fn
       ([] [(f) (g) (h)])
       ([x] [(f x) (g x) (h x)])
       ([x y] [(f x y) (g x y) (h x y)])
       ([x y z] [(f x y z) (g x y z) (h x y z)])
       ([x y z & args] [(apply f x y z args) (apply g x y z args) (apply h x y z args)])))
  ([f g h & fs]
     (let [fs (list* f g h fs)]
       (fn
         ([] (reduce1 #(conj %1 (%2)) [] fs))
         ([x] (reduce1 #(conj %1 (%2 x)) [] fs))
         ([x y] (reduce1 #(conj %1 (%2 x y)) [] fs))
         ([x y z] (reduce1 #(conj %1 (%2 x y z)) [] fs))
         ([x y z & args] (reduce1 #(conj %1 (apply %2 x y z args)) [] fs))))))

(defn partial
  "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args. When
  called, the returned function calls f with args + additional args."
  {:added "1.0"
   :static true}
  ([f] f)
  ([f arg1]
   (fn
     ([] (f arg1))
     ([x] (f arg1 x))
     ([x y] (f arg1 x y))
     ([x y z] (f arg1 x y z))
     ([x y z & args] (apply f arg1 x y z args))))
  ([f arg1 arg2]
   (fn
     ([] (f arg1 arg2))
     ([x] (f arg1 arg2 x))
     ([x y] (f arg1 arg2 x y))
     ([x y z] (f arg1 arg2 x y z))
     ([x y z & args] (apply f arg1 arg2 x y z args))))
  ([f arg1 arg2 arg3]
   (fn
     ([] (f arg1 arg2 arg3))
     ([x] (f arg1 arg2 arg3 x))
     ([x y] (f arg1 arg2 arg3 x y))
     ([x y z] (f arg1 arg2 arg3 x y z))
     ([x y z & args] (apply f arg1 arg2 arg3 x y z args))))
  ([f arg1 arg2 arg3 & more]
   (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))

;;;;;;;;;;;;;;;;;;; sequence fns  ;;;;;;;;;;;;;;;;;;;;;;;

(defn sequence
  "Coerces coll to a (possibly empty) sequence, if it is not already
  one. Will not force a lazy seq. (sequence nil) yields (), When a
  transducer is supplied, returns a lazy sequence of applications of
  the transform to the items in coll(s), i.e. to the set of first
  items of each coll, followed by the set of second
  items in each coll, until any one of the colls is exhausted.  Any
  remaining items in other colls are ignored. The transform should accept
  number-of-colls arguments"
  {:added "1.0"
   :static true}
  ([coll]
     (if (seq? coll) coll
         (or (seq coll) ())))
  ([xform coll]
     (or (clojure.lang.RT/chunkIteratorSeq
         (clojure.lang.TransformerIterator/create xform (clojure.lang.RT/iter coll)))
       ()))
  ([xform coll & colls]
     (or (clojure.lang.RT/chunkIteratorSeq
         (clojure.lang.TransformerIterator/createMulti
           xform
           (map #(clojure.lang.RT/iter %) (cons coll colls))))
       ())))

(defn every?
  "Returns true if (pred x) is logical true for every x in coll, else
  false."
  {:tag Boolean
   :added "1.0"
   :static true}
  [pred coll]
  (cond
   (nil? (seq coll)) true
   (pred (first coll)) (recur pred (next coll))
   :else false))

(def
 ^{:tag Boolean
   :doc "Returns false if (pred x) is logical true for every x in
  coll, else true."
   :arglists '([pred coll])
   :added "1.0"}
 not-every? (comp not every?))

(defn some
  "Returns the first logical true value of (pred x) for any x in coll,
  else nil.  One common idiom is to use a set as pred, for example
  this will return :fred if :fred is in the sequence, otherwise nil:
  (some #{:fred} coll)"
  {:added "1.0"
   :static true}
  [pred coll]
    (when (seq coll)
      (or (pred (first coll)) (recur pred (next coll)))))

(def
 ^{:tag Boolean
   :doc "Returns false if (pred x) is logical true for any x in coll,
  else true."
   :arglists '([pred coll])
   :added "1.0"}
 not-any? (comp not some))

;will be redefed later with arg checks
(defmacro dotimes
  "bindings => name n

  Repeatedly executes body (presumably for side-effects) with name
  bound to integers from 0 through n-1."
  {:added "1.0"}
  [bindings & body]
  (let [i (first bindings)
        n (second bindings)]
    `(let [n# (clojure.lang.RT/longCast ~n)]
       (loop [~i 0]
         (when (< ~i n#)
           ~@body
           (recur (unchecked-inc ~i)))))))

(defn map
  "Returns a lazy sequence consisting of the result of applying f to
  the set of first items of each coll, followed by applying f to the
  set of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments. Returns a transducer when
  no collection is provided."
  {:added "1.0"
   :static true}
  ([f]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
           (rf result (f input)))
        ([result input & inputs]
           (rf result (apply f input inputs))))))
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if (chunked-seq? s)
        (let [c (chunk-first s)
              size (int (count c))
              b (chunk-buffer size)]
          (dotimes [i size]
              (chunk-append b (f (.nth c i))))
          (chunk-cons (chunk b) (map f (chunk-rest s))))
        (cons (f (first s)) (map f (rest s)))))))
  ([f c1 c2]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (when (and s1 s2)
        (cons (f (first s1) (first s2))
              (map f (rest s1) (rest s2)))))))
  ([f c1 c2 c3]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
      (when (and  s1 s2 s3)
        (cons (f (first s1) (first s2) (first s3))
              (map f (rest s1) (rest s2) (rest s3)))))))
  ([f c1 c2 c3 & colls]
   (let [step (fn step [cs]
                 (lazy-seq
                  (let [ss (map seq cs)]
                    (when (every? identity ss)
                      (cons (map first ss) (step (map rest ss)))))))]
     (map #(apply f %) (step (conj colls c3 c2 c1))))))

(defmacro declare
  "defs the supplied var names with no bindings, useful for making forward declarations."
  {:added "1.0"}
  [& names] `(do ~@(map #(list 'def (vary-meta % assoc :declared true)) names)))

(declare cat)

(defn mapcat
  "Returns the result of applying concat to the result of applying map
  to f and colls.  Thus function f should return a collection. Returns
  a transducer when no collections are provided"
  {:added "1.0"
   :static true}
  ([f] (comp (map f) cat))
  ([f & colls]
     (apply concat (apply map f colls))))

(defn filter
  "Returns a lazy sequence of the items in coll for which
  (pred item) returns true. pred must be free of side-effects.
  Returns a transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([pred]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
           (if (pred input)
             (rf result input)
             result)))))
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if (chunked-seq? s)
        (let [c (chunk-first s)
              size (count c)
              b (chunk-buffer size)]
          (dotimes [i size]
              (let [v (.nth c i)]
                (when (pred v)
                  (chunk-append b v))))
          (chunk-cons (chunk b) (filter pred (chunk-rest s))))
        (let [f (first s) r (rest s)]
          (if (pred f)
            (cons f (filter pred r))
            (filter pred r))))))))


(defn remove
  "Returns a lazy sequence of the items in coll for which
  (pred item) returns false. pred must be free of side-effects.
  Returns a transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([pred] (filter (complement pred)))
  ([pred coll]
     (filter (complement pred) coll)))

(defn reduced
  "Wraps x in a way such that a reduce will terminate with the value x"
  {:added "1.5"}
  [x]
  (clojure.lang.Reduced. x))

(defn reduced?
  "Returns true if x is the result of a call to reduced"
  {:inline (fn [x] `(clojure.lang.RT/isReduced ~x ))
   :inline-arities #{1}
   :added "1.5"}
  ([x] (clojure.lang.RT/isReduced x)))

(defn ensure-reduced
  "If x is already reduced?, returns it, else returns (reduced x)"
  {:added "1.7"}
  [x]
  (if (reduced? x) x (reduced x)))

(defn unreduced
  "If x is reduced?, returns (deref x), else returns x"
  {:added "1.7"}
  [x]
  (if (reduced? x) (deref x) x))

(defn take
  "Returns a lazy sequence of the first n items in coll, or all items if
  there are fewer than n.  Returns a stateful transducer when
  no collection is provided."
  {:added "1.0"
   :static true}
  ([n]
     (fn [rf]
       (let [nv (volatile! n)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [n @nv
                    nn (vswap! nv dec)
                    result (if (pos? n)
                             (rf result input)
                             result)]
                (if (not (pos? nn))
                  (ensure-reduced result)
                  result)))))))
  ([n coll]
     (lazy-seq
      (when (pos? n) 
        (when-let [s (seq coll)]
          (cons (first s) (take (dec n) (rest s))))))))

(defn take-while
  "Returns a lazy sequence of successive items from coll while
  (pred item) returns true. pred must be free of side-effects.
  Returns a transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([pred]
     (fn [rf]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
            (if (pred input)
              (rf result input)
              (reduced result))))))
  ([pred coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (when (pred (first s))
          (cons (first s) (take-while pred (rest s))))))))

(defn drop
  "Returns a lazy sequence of all but the first n items in coll.
  Returns a stateful transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([n]
     (fn [rf]
       (let [nv (volatile! n)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [n @nv]
                (vswap! nv dec)
                (if (pos? n)
                  result
                  (rf result input))))))))
  ([n coll]
     (let [step (fn [n coll]
                  (let [s (seq coll)]
                    (if (and (pos? n) s)
                      (recur (dec n) (rest s))
                      s)))]
       (lazy-seq (step n coll)))))

(defn drop-last
  "Return a lazy sequence of all but the last n (default 1) items in coll"
  {:added "1.0"
   :static true}
  ([s] (drop-last 1 s))
  ([n s] (map (fn [x _] x) s (drop n s))))

(defn take-last
  "Returns a seq of the last n items in coll.  Depending on the type
  of coll may be no better than linear time.  For vectors, see also subvec."
  {:added "1.1"
   :static true}
  [n coll]
  (loop [s (seq coll), lead (seq (drop n coll))]
    (if lead
      (recur (next s) (next lead))
      s)))

(defn drop-while
  "Returns a lazy sequence of the items in coll starting from the
  first item for which (pred item) returns logical false.  Returns a
  stateful transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([pred]
     (fn [rf]
       (let [dv (volatile! true)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [drop? @dv]
                (if (and drop? (pred input))
                  result
                  (do
                    (vreset! dv nil)
                    (rf result input)))))))))
  ([pred coll]
     (let [step (fn [pred coll]
                  (let [s (seq coll)]
                    (if (and s (pred (first s)))
                      (recur pred (rest s))
                      s)))]
       (lazy-seq (step pred coll)))))

(defn cycle
  "Returns a lazy (infinite!) sequence of repetitions of the items in coll."
  {:added "1.0"
   :static true}
  [coll] (clojure.lang.Cycle/create (seq coll)))

(defn split-at
  "Returns a vector of [(take n coll) (drop n coll)]"
  {:added "1.0"
   :static true}
  [n coll]
    [(take n coll) (drop n coll)])

(defn split-with
  "Returns a vector of [(take-while pred coll) (drop-while pred coll)]"
  {:added "1.0"
   :static true}
  [pred coll]
    [(take-while pred coll) (drop-while pred coll)])

(defn repeat
  "Returns a lazy (infinite!, or length n if supplied) sequence of xs."
  {:added "1.0"
   :static true}
  ([x] (clojure.lang.Repeat/create x))
  ([n x] (clojure.lang.Repeat/create n x)))

(defn replicate
  "DEPRECATED: Use 'repeat' instead.
   Returns a lazy seq of n xs."
  {:added "1.0"
   :deprecated "1.3"}
  [n x] (take n (repeat x)))

(defn iterate
  "Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects"
  {:added "1.0"
   :static true}
  [f x] (clojure.lang.Iterate/create f x) )

(defn range
  "Returns a lazy seq of nums from start (inclusive) to end
  (exclusive), by step, where start defaults to 0, step to 1, and end to
  infinity. When step is equal to 0, returns an infinite sequence of
  start. When start is equal to end, returns empty list."
  {:added "1.0"
   :static true}
  ([]
   (iterate inc' 0))
  ([end]
   (if (instance? Long end)
     (clojure.lang.LongRange/create end)
     (clojure.lang.Range/create end)))
  ([start end]
   (if (and (instance? Long start) (instance? Long end))
     (clojure.lang.LongRange/create start end)
     (clojure.lang.Range/create start end)))
  ([start end step]
   (if (and (instance? Long start) (instance? Long end) (instance? Long step))
     (clojure.lang.LongRange/create start end step)
     (clojure.lang.Range/create start end step))))

(defn merge
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping from
  the latter (left-to-right) will be the mapping in the result."
  {:added "1.0"
   :static true}
  [& maps]
  (when (some identity maps)
    (reduce1 #(conj (or %1 {}) %2) maps)))

(defn merge-with
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f val-in-result val-in-latter)."
  {:added "1.0"
   :static true}
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
			(let [k (key e) v (val e)]
			  (if (contains? m k)
			    (assoc m k (f (get m k) v))
			    (assoc m k v))))
          merge2 (fn [m1 m2]
		   (reduce1 merge-entry (or m1 {}) (seq m2)))]
      (reduce1 merge2 maps))))



(defn zipmap
  "Returns a map with the keys mapped to the corresponding vals."
  {:added "1.0"
   :static true}
  [keys vals]
    (loop [map {}
           ks (seq keys)
           vs (seq vals)]
      (if (and ks vs)
        (recur (assoc map (first ks) (first vs))
               (next ks)
               (next vs))
        map)))

(defn line-seq
  "Returns the lines of text from rdr as a lazy sequence of strings.
  rdr must implement java.io.BufferedReader."
  {:added "1.0"
   :static true}
  [^java.io.BufferedReader rdr]
  (when-let [line (.readLine rdr)]
    (cons line (lazy-seq (line-seq rdr)))))

(defn comparator
  "Returns an implementation of java.util.Comparator based upon pred."
  {:added "1.0"
   :static true}
  [pred]
    (fn [x y]
      (cond (pred x y) -1 (pred y x) 1 :else 0)))

(defn sort
  "Returns a sorted sequence of the items in coll. If no comparator is
  supplied, uses compare.  comparator must implement
  java.util.Comparator.  Guaranteed to be stable: equal elements will
  not be reordered.  If coll is a Java array, it will be modified.  To
  avoid this, sort a copy of the array."
  {:added "1.0"
   :static true}
  ([coll]
   (sort compare coll))
  ([^java.util.Comparator comp coll]
   (if (seq coll)
     (let [a (to-array coll)]
       (. java.util.Arrays (sort a comp))
       (seq a))
     ())))

(defn sort-by
  "Returns a sorted sequence of the items in coll, where the sort
  order is determined by comparing (keyfn item).  If no comparator is
  supplied, uses compare.  comparator must implement
  java.util.Comparator.  Guaranteed to be stable: equal elements will
  not be reordered.  If coll is a Java array, it will be modified.  To
  avoid this, sort a copy of the array."
  {:added "1.0"
   :static true}
  ([keyfn coll]
   (sort-by keyfn compare coll))
  ([keyfn ^java.util.Comparator comp coll]
   (sort (fn [x y] (. comp (compare (keyfn x) (keyfn y)))) coll)))

(defn dorun
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. dorun can
  be used to force any effects. Walks through the successive nexts of
  the seq, does not retain the head and returns nil."
  {:added "1.0"
   :static true}
  ([coll]
   (when-let [s (seq coll)]
     (recur (next s))))
  ([n coll]
   (when (and (seq coll) (pos? n))
     (recur (dec n) (next coll)))))

(defn doall
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. doall can
  be used to force any effects. Walks through the successive nexts of
  the seq, retains the head and returns it, thus causing the entire
  seq to reside in memory at one time."
  {:added "1.0"
   :static true}
  ([coll]
   (dorun coll)
   coll)
  ([n coll]
   (dorun n coll)
   coll))

(defn nthnext
  "Returns the nth next of coll, (seq coll) when n is 0."
  {:added "1.0"
   :static true}
  [coll n]
    (loop [n n xs (seq coll)]
      (if (and xs (pos? n))
        (recur (dec n) (next xs))
        xs)))

(defn nthrest
  "Returns the nth rest of coll, coll when n is 0."
  {:added "1.3"
   :static true}
  [coll n]
    (loop [n n xs coll]
      (if-let [xs (and (pos? n) (seq xs))]
        (recur (dec n) (rest xs))
        xs)))

(defn partition
  "Returns a lazy sequence of lists of n items each, at offsets step
  apart. If step is not supplied, defaults to n, i.e. the partitions
  do not overlap. If a pad collection is supplied, use its elements as
  necessary to complete last partition upto n items. In case there are
  not enough padding elements, return a partition with less than n items."
  {:added "1.0"
   :static true}
  ([n coll]
     (partition n n coll))
  ([n step coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (let [p (doall (take n s))]
           (when (= n (count p))
             (cons p (partition n step (nthrest s step))))))))
  ([n step pad coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (let [p (doall (take n s))]
           (if (= n (count p))
             (cons p (partition n step pad (nthrest s step)))
             (list (take n (concat p pad)))))))))

;; evaluation

(defn eval
  "Evaluates the form data structure (not text!) and returns the result."
  {:added "1.0"
   :static true}
  [form] (. clojure.lang.Compiler (eval form)))

(defmacro doseq
  "Repeatedly executes body (presumably for side-effects) with
  bindings and filtering as provided by \"for\".  Does not retain
  the head of the sequence. Returns nil."
  {:added "1.0"}
  [seq-exprs & body]
  (assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [step (fn step [recform exprs]
               (if-not exprs
                 [true `(do ~@body)]
                 (let [k (first exprs)
                       v (second exprs)]
                   (if (keyword? k)
                     (let [steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)]
                       (cond
                         (= k :let) [needrec `(let ~v ~subform)]
                         (= k :while) [false `(when ~v
                                                ~subform
                                                ~@(when needrec [recform]))]
                         (= k :when) [false `(if ~v
                                               (do
                                                 ~subform
                                                 ~@(when needrec [recform]))
                                               ~recform)]))
                     (let [seq- (gensym "seq_")
                           chunk- (with-meta (gensym "chunk_")
                                             {:tag 'clojure.lang.IChunk})
                           count- (gensym "count_")
                           i- (gensym "i_")
                           recform `(recur (next ~seq-) nil 0 0)
                           steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)
                           recform-chunk 
                             `(recur ~seq- ~chunk- ~count- (unchecked-inc ~i-))
                           steppair-chunk (step recform-chunk (nnext exprs))
                           subform-chunk (steppair-chunk 1)]
                       [true
                        `(loop [~seq- (seq ~v), ~chunk- nil,
                                ~count- 0, ~i- 0]
                           (if (< ~i- ~count-)
                             (let [~k (.nth ~chunk- ~i-)]
                               ~subform-chunk
                               ~@(when needrec [recform-chunk]))
                             (when-let [~seq- (seq ~seq-)]
                               (if (chunked-seq? ~seq-)
                                 (let [c# (chunk-first ~seq-)]
                                   (recur (chunk-rest ~seq-) c#
                                          (int (count c#)) (int 0)))
                                 (let [~k (first ~seq-)]
                                   ~subform
                                   ~@(when needrec [recform]))))))])))))]
    (nth (step nil (seq seq-exprs)) 1)))

(defn await
  "Blocks the current thread (indefinitely!) until all actions
  dispatched thus far, from this thread or agent, to the agent(s) have
  occurred.  Will block on failed agents.  Will never return if
  a failed agent is restarted with :clear-actions true."
  {:added "1.0"
   :static true}
  [& agents]
  (io! "await in transaction"
    (when *agent*
      (throw (new Exception "Can't await in agent action")))
    (let [latch (new java.util.concurrent.CountDownLatch (count agents))
          count-down (fn [agent] (. latch (countDown)) agent)]
      (doseq [agent agents]
        (send agent count-down))
      (. latch (await)))))

(defn ^:static await1 [^clojure.lang.Agent a]
  (when (pos? (.getQueueCount a))
    (await a))
    a)

(defn await-for
  "Blocks the current thread until all actions dispatched thus
  far (from this thread or agent) to the agents have occurred, or the
  timeout (in milliseconds) has elapsed. Returns logical false if
  returning due to timeout, logical true otherwise."
  {:added "1.0"
   :static true}
  [timeout-ms & agents]
    (io! "await-for in transaction"
     (when *agent*
       (throw (new Exception "Can't await in agent action")))
     (let [latch (new java.util.concurrent.CountDownLatch (count agents))
           count-down (fn [agent] (. latch (countDown)) agent)]
       (doseq [agent agents]
           (send agent count-down))
       (. latch (await  timeout-ms (. java.util.concurrent.TimeUnit MILLISECONDS))))))

(defmacro dotimes
  "bindings => name n

  Repeatedly executes body (presumably for side-effects) with name
  bound to integers from 0 through n-1."
  {:added "1.0"}
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [i (first bindings)
        n (second bindings)]
    `(let [n# (long ~n)]
       (loop [~i 0]
         (when (< ~i n#)
           ~@body
           (recur (unchecked-inc ~i)))))))

#_(defn into
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined."
  {:added "1.0"}
  [to from]
    (let [ret to items (seq from)]
      (if items
        (recur (conj ret (first items)) (next items))
        ret)))

;;;;;;;;;;;;;;;;;;;;; editable collections ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn transient 
  "Returns a new, transient version of the collection, in constant time."
  {:added "1.1"
   :static true}
  [^clojure.lang.IEditableCollection coll] 
  (.asTransient coll))

(defn persistent! 
  "Returns a new, persistent version of the transient collection, in
  constant time. The transient collection cannot be used after this
  call, any such use will throw an exception."
  {:added "1.1"
   :static true}
  [^clojure.lang.ITransientCollection coll]
  (.persistent coll))

(defn conj!
  "Adds x to the transient collection, and return coll. The 'addition'
  may happen at different 'places' depending on the concrete type."
  {:added "1.1"
   :static true}
  ([] (transient []))
  ([coll] coll)
  ([^clojure.lang.ITransientCollection coll x]
     (.conj coll x)))

(defn assoc!
  "When applied to a transient map, adds mapping of key(s) to
  val(s). When applied to a transient vector, sets the val at index.
  Note - index must be <= (count vector). Returns coll."
  {:added "1.1"
   :static true}
  ([^clojure.lang.ITransientAssociative coll key val] (.assoc coll key val))
  ([^clojure.lang.ITransientAssociative coll key val & kvs]
   (let [ret (.assoc coll key val)]
     (if kvs
       (recur ret (first kvs) (second kvs) (nnext kvs))
       ret))))

(defn dissoc!
  "Returns a transient map that doesn't contain a mapping for key(s)."
  {:added "1.1"
   :static true}
  ([^clojure.lang.ITransientMap map key] (.without map key))
  ([^clojure.lang.ITransientMap map key & ks]
   (let [ret (.without map key)]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))

(defn pop!
  "Removes the last item from a transient vector. If
  the collection is empty, throws an exception. Returns coll"
  {:added "1.1"
   :static true}
  [^clojure.lang.ITransientVector coll] 
  (.pop coll)) 

(defn disj!
  "disj[oin]. Returns a transient set of the same (hashed/sorted) type, that
  does not contain key(s)."
  {:added "1.1"
   :static true}
  ([set] set)
  ([^clojure.lang.ITransientSet set key]
   (. set (disjoin key)))
  ([^clojure.lang.ITransientSet set key & ks]
   (let [ret (. set (disjoin key))]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))

;redef into with batch support
(defn ^:private into1
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined."
  {:added "1.0"
   :static true}
  [to from]
  (if (instance? clojure.lang.IEditableCollection to)
    (persistent! (reduce1 conj! (transient to) from))
    (reduce1 conj to from)))

(defmacro import 
  "import-list => (package-symbol class-name-symbols*)

  For each name in class-name-symbols, adds a mapping from name to the
  class named by package.name to the current namespace. Use :import in the ns
  macro in preference to calling this directly."
  {:added "1.0"}
  [& import-symbols-or-lists]
  (let [specs (map #(if (and (seq? %) (= 'quote (first %))) (second %) %) 
                   import-symbols-or-lists)]
    `(do ~@(map #(list 'clojure.core/import* %)
                (reduce1 (fn [v spec] 
                          (if (symbol? spec)
                            (conj v (name spec))
                            (let [p (first spec) cs (rest spec)]
                              (into1 v (map #(str p "." %) cs)))))
                        [] specs)))))

(defn into-array
  "Returns an array with components set to the values in aseq. The array's
  component type is type if provided, or the type of the first value in
  aseq if present, or Object. All values in aseq must be compatible with
  the component type. Class objects for the primitive types can be obtained
  using, e.g., Integer/TYPE."
  {:added "1.0"
   :static true}
  ([aseq]
     (clojure.lang.RT/seqToTypedArray (seq aseq)))
  ([type aseq]
     (clojure.lang.RT/seqToTypedArray type (seq aseq))))

(defn ^{:private true}
  array [& items]
    (into-array items))

(defn class
  "Returns the Class of x"
  {:added "1.0"
   :static true}
  ^Class [^Object x] (if (nil? x) x (. x (getClass))))

(defn type 
  "Returns the :type metadata of x, or its Class if none"
  {:added "1.0"
   :static true}
  [x]
  (or (get (meta x) :type) (class x)))

(defn num
  "Coerce to Number"
  {:tag Number
   :inline (fn  [x] `(. clojure.lang.Numbers (num ~x)))
   :added "1.0"}
  [x] (. clojure.lang.Numbers (num x)))

(defn long
  "Coerce to long"
  {:inline (fn  [x] `(. clojure.lang.RT (longCast ~x)))
   :added "1.0"}
  [^Number x] (clojure.lang.RT/longCast x))

(defn float
  "Coerce to float"
  {:inline (fn  [x] `(. clojure.lang.RT (~(if *unchecked-math* 'uncheckedFloatCast 'floatCast) ~x)))
   :added "1.0"}
  [^Number x] (clojure.lang.RT/floatCast x))

(defn double
  "Coerce to double"
  {:inline (fn  [x] `(. clojure.lang.RT (doubleCast ~x)))
   :added "1.0"}
  [^Number x] (clojure.lang.RT/doubleCast x))

(defn short
  "Coerce to short"
  {:inline (fn  [x] `(. clojure.lang.RT (~(if *unchecked-math* 'uncheckedShortCast 'shortCast) ~x)))
   :added "1.0"}
  [^Number x] (clojure.lang.RT/shortCast x))

(defn byte
  "Coerce to byte"
  {:inline (fn  [x] `(. clojure.lang.RT (~(if *unchecked-math* 'uncheckedByteCast 'byteCast) ~x)))
   :added "1.0"}
  [^Number x] (clojure.lang.RT/byteCast x))

(defn char
  "Coerce to char"
  {:inline (fn  [x] `(. clojure.lang.RT (~(if *unchecked-math* 'uncheckedCharCast 'charCast) ~x)))
   :added "1.1"}
  [x] (. clojure.lang.RT (charCast x)))

(defn boolean
  "Coerce to boolean"
  {
   :inline (fn  [x] `(. clojure.lang.RT (booleanCast ~x)))
   :added "1.0"}
  [x] (clojure.lang.RT/booleanCast x))

(defn unchecked-byte
  "Coerce to byte. Subject to rounding or truncation."
  {:inline (fn  [x] `(. clojure.lang.RT (uncheckedByteCast ~x)))
   :added "1.3"}
  [^Number x] (clojure.lang.RT/uncheckedByteCast x))

(defn unchecked-short
  "Coerce to short. Subject to rounding or truncation."
  {:inline (fn  [x] `(. clojure.lang.RT (uncheckedShortCast ~x)))
   :added "1.3"}
  [^Number x] (clojure.lang.RT/uncheckedShortCast x))

(defn unchecked-char
  "Coerce to char. Subject to rounding or truncation."
  {:inline (fn  [x] `(. clojure.lang.RT (uncheckedCharCast ~x)))
   :added "1.3"}
  [x] (. clojure.lang.RT (uncheckedCharCast x)))

(defn unchecked-int
  "Coerce to int. Subject to rounding or truncation."
  {:inline (fn  [x] `(. clojure.lang.RT (uncheckedIntCast ~x)))
   :added "1.3"}
  [^Number x] (clojure.lang.RT/uncheckedIntCast x))

(defn unchecked-long
  "Coerce to long. Subject to rounding or truncation."
  {:inline (fn  [x] `(. clojure.lang.RT (uncheckedLongCast ~x)))
   :added "1.3"}
  [^Number x] (clojure.lang.RT/uncheckedLongCast x))

(defn unchecked-float
  "Coerce to float. Subject to rounding."
  {:inline (fn  [x] `(. clojure.lang.RT (uncheckedFloatCast ~x)))
   :added "1.3"}
  [^Number x] (clojure.lang.RT/uncheckedFloatCast x))

(defn unchecked-double
  "Coerce to double. Subject to rounding."
  {:inline (fn  [x] `(. clojure.lang.RT (uncheckedDoubleCast ~x)))
   :added "1.3"}
  [^Number x] (clojure.lang.RT/uncheckedDoubleCast x))


(defn number?
  "Returns true if x is a Number"
  {:added "1.0"
   :static true}
  [x]
  (instance? Number x))

(defn mod
  "Modulus of num and div. Truncates toward negative infinity."
  {:added "1.0"
   :static true}
  [num div] 
  (let [m (rem num div)] 
    (if (or (zero? m) (= (pos? num) (pos? div)))
      m 
      (+ m div))))

(defn ratio?
  "Returns true if n is a Ratio"
  {:added "1.0"
   :static true}
  [n] (instance? clojure.lang.Ratio n))

(defn numerator
  "Returns the numerator part of a Ratio."
  {:tag BigInteger
   :added "1.2"
   :static true}
  [r]
  (.numerator ^clojure.lang.Ratio r))

(defn denominator
  "Returns the denominator part of a Ratio."
  {:tag BigInteger
   :added "1.2"
   :static true}
  [r]
  (.denominator ^clojure.lang.Ratio r))

(defn decimal?
  "Returns true if n is a BigDecimal"
  {:added "1.0"
   :static true}
  [n] (instance? BigDecimal n))

(defn float?
  "Returns true if n is a floating point number"
  {:added "1.0"
   :static true}
  [n]
  (or (instance? Double n)
      (instance? Float n)))

(defn rational? 
  "Returns true if n is a rational number"
  {:added "1.0"
   :static true}
  [n]
  (or (integer? n) (ratio? n) (decimal? n)))

(defn bigint
  "Coerce to BigInt"
  {:tag clojure.lang.BigInt
   :static true
   :added "1.3"}
  [x] (cond
       (instance? clojure.lang.BigInt x) x
       (instance? BigInteger x) (clojure.lang.BigInt/fromBigInteger x)
       (decimal? x) (bigint (.toBigInteger ^BigDecimal x))
       (float? x)  (bigint (. BigDecimal valueOf (double x)))
       (ratio? x) (bigint (.bigIntegerValue ^clojure.lang.Ratio x))
       (number? x) (clojure.lang.BigInt/valueOf (long x))
       :else (bigint (BigInteger. x))))

(defn biginteger
  "Coerce to BigInteger"
  {:tag BigInteger
   :added "1.0"
   :static true}
  [x] (cond
       (instance? BigInteger x) x
       (instance? clojure.lang.BigInt x) (.toBigInteger ^clojure.lang.BigInt x)
       (decimal? x) (.toBigInteger ^BigDecimal x)
       (float? x) (.toBigInteger (. BigDecimal valueOf (double x)))
       (ratio? x) (.bigIntegerValue ^clojure.lang.Ratio x)
       (number? x) (BigInteger/valueOf (long x))
       :else (BigInteger. x)))

(defn bigdec
  "Coerce to BigDecimal"
  {:tag BigDecimal
   :added "1.0"
   :static true}
  [x] (cond
       (decimal? x) x
       (float? x) (. BigDecimal valueOf (double x))
       (ratio? x) (/ (BigDecimal. (.numerator ^clojure.lang.Ratio x)) (.denominator ^clojure.lang.Ratio x))
       (instance? clojure.lang.BigInt x) (.toBigDecimal ^clojure.lang.BigInt x)
       (instance? BigInteger x) (BigDecimal. ^BigInteger x)
       (number? x) (BigDecimal/valueOf (long x))
       :else (BigDecimal. x)))

(def ^:dynamic ^{:private true} print-initialized false)

(defmulti print-method (fn [x writer]
                         (let [t (get (meta x) :type)]
                           (if (keyword? t) t (class x)))))
(defmulti print-dup (fn [x writer] (class x)))

(defn pr-on
  {:private true
   :static true}
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
  {:dynamic true
   :added "1.0"}
  ([] nil)
  ([x]
     (pr-on x *out*))
  ([x & more]
   (pr x)
   (. *out* (append \space))
   (if-let [nmore (next more)]
     (recur (first more) nmore)
     (apply pr more))))

(def ^:private ^String system-newline
     (System/getProperty "line.separator"))

(defn newline
  "Writes a platform-specific newline to *out*"
  {:added "1.0"
   :static true}
  []
    (. *out* (append system-newline))
    nil)

(defn flush
  "Flushes the output stream that is the current value of
  *out*"
  {:added "1.0"
   :static true}
  []
    (. *out* (flush))
    nil)

(defn prn
  "Same as pr followed by (newline). Observes *flush-on-newline*"
  {:added "1.0"
   :static true}
  [& more]
    (apply pr more)
    (newline)
    (when *flush-on-newline*
      (flush)))

(defn print
  "Prints the object(s) to the output stream that is the current value
  of *out*.  print and println produce output for human consumption."
  {:added "1.0"
   :static true}
  [& more]
    (binding [*print-readably* nil]
      (apply pr more)))

(defn println
  "Same as print followed by (newline)"
  {:added "1.0"
   :static true}
  [& more]
    (binding [*print-readably* nil]
      (apply prn more)))

(defn read
  "Reads the next object from stream, which must be an instance of
  java.io.PushbackReader or some derivee.  stream defaults to the
  current value of *in*.

  Opts is a persistent map with valid keys:
    :read-cond - :allow to process reader conditionals, or
                 :preserve to keep all branches
    :features - persistent set of feature keywords for reader conditionals
    :eof - on eof, return value unless :eofthrow, then throw.
           if not specified, will throw

  Note that read can execute code (controlled by *read-eval*),
  and as such should be used only with trusted sources.

  For data structure interop use clojure.edn/read"
  {:added "1.0"
   :static true}
  ([]
   (read *in*))
  ([stream]
   (read stream true nil))
  ([stream eof-error? eof-value]
   (read stream eof-error? eof-value false))
  ([stream eof-error? eof-value recursive?]
   (. clojure.lang.LispReader (read stream (boolean eof-error?) eof-value recursive?)))
  ([opts stream]
   (. clojure.lang.LispReader (read stream opts))))

(defn read-line
  "Reads the next line from stream that is the current value of *in* ."
  {:added "1.0"
   :static true}
  []
  (if (instance? clojure.lang.LineNumberingPushbackReader *in*)
    (.readLine ^clojure.lang.LineNumberingPushbackReader *in*)
    (.readLine ^java.io.BufferedReader *in*)))

(defn read-string
  "Reads one object from the string s. Optionally include reader
  options, as specified in read.

  Note that read-string can execute code (controlled by *read-eval*),
  and as such should be used only with trusted sources.

  For data structure interop use clojure.edn/read-string"
  {:added "1.0"
   :static true}
  ([s] (clojure.lang.RT/readString s))
  ([opts s] (clojure.lang.RT/readString s opts)))

(defn subvec
  "Returns a persistent vector of the items in vector from
  start (inclusive) to end (exclusive).  If end is not supplied,
  defaults to (count vector). This operation is O(1) and very fast, as
  the resulting vector shares structure with the original and no
  trimming is done."
  {:added "1.0"
   :static true}
  ([v start]
   (subvec v start (count v)))
  ([v start end]
   (. clojure.lang.RT (subvec v start end))))

(defmacro with-open
  "bindings => [name init ...]

  Evaluates body in a try expression with names bound to the values
  of the inits, and a finally clause that calls (.close name) on each
  name in reverse order."
  {:added "1.0"}
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
  (cond
    (= (count bindings) 0) `(do ~@body)
    (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                              (try
                                (with-open ~(subvec bindings 2) ~@body)
                                (finally
                                  (. ~(bindings 0) close))))
    :else (throw (IllegalArgumentException.
                   "with-open only allows Symbols in bindings"))))

(defmacro doto
  "Evaluates x then calls all of the methods and functions with the
  value of x supplied at the front of the given arguments.  The forms
  are evaluated in order.  Returns x.

  (doto (new java.util.HashMap) (.put \"a\" 1) (.put \"b\" 2))"
  {:added "1.0"}
  [x & forms]
    (let [gx (gensym)]
      `(let [~gx ~x]
         ~@(map (fn [f]
                  (if (seq? f)
                    `(~(first f) ~gx ~@(next f))
                    `(~f ~gx)))
                forms)
         ~gx)))

(defmacro memfn
  "Expands into code that creates a fn that expects to be passed an
  object and any args and calls the named instance method on the
  object passing the args. Use when you want to treat a Java method as
  a first-class fn. name may be type-hinted with the method receiver's
  type in order to avoid reflective calls."
  {:added "1.0"}
  [name & args]
  (let [t (with-meta (gensym "target")
            (meta name))]
    `(fn [~t ~@args]
       (. ~t (~name ~@args)))))

(defmacro time
  "Evaluates expr and prints the time it took.  Returns the value of
 expr."
  {:added "1.0"}
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (prn (str "Elapsed time: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
     ret#))



(import '(java.lang.reflect Array))

(defn alength
  "Returns the length of the Java array. Works on arrays of all
  types."
  {:inline (fn [a] `(. clojure.lang.RT (alength ~a)))
   :added "1.0"}
  [array] (. clojure.lang.RT (alength array)))

(defn aclone
  "Returns a clone of the Java array. Works on arrays of known
  types."
  {:inline (fn [a] `(. clojure.lang.RT (aclone ~a)))
   :added "1.0"}
  [array] (. clojure.lang.RT (aclone array)))

(defn aget
  "Returns the value at the index/indices. Works on Java arrays of all
  types."
  {:inline (fn [a i] `(. clojure.lang.RT (aget ~a (int ~i))))
   :inline-arities #{2}
   :added "1.0"}
  ([array idx]
   (clojure.lang.Reflector/prepRet (.getComponentType (class array)) (. Array (get array idx))))
  ([array idx & idxs]
   (apply aget (aget array idx) idxs)))

(defn aset
  "Sets the value at the index/indices. Works on Java arrays of
  reference types. Returns val."
  {:inline (fn [a i v] `(. clojure.lang.RT (aset ~a (int ~i) ~v)))
   :inline-arities #{3}
   :added "1.0"}
  ([array idx val]
   (. Array (set array idx val))
   val)
  ([array idx idx2 & idxv]
   (apply aset (aget array idx) idx2 idxv)))

(defmacro
  ^{:private true}
  def-aset [name method coerce]
    `(defn ~name
       {:arglists '([~'array ~'idx ~'val] [~'array ~'idx ~'idx2 & ~'idxv])}
       ([array# idx# val#]
        (. Array (~method array# idx# (~coerce val#)))
        val#)
       ([array# idx# idx2# & idxv#]
        (apply ~name (aget array# idx#) idx2# idxv#))))

(def-aset
  ^{:doc "Sets the value at the index/indices. Works on arrays of int. Returns val."
    :added "1.0"}
  aset-int setInt int)

(def-aset
  ^{:doc "Sets the value at the index/indices. Works on arrays of long. Returns val."
    :added "1.0"}
  aset-long setLong long)

(def-aset
  ^{:doc "Sets the value at the index/indices. Works on arrays of boolean. Returns val."
    :added "1.0"}
  aset-boolean setBoolean boolean)

(def-aset
  ^{:doc "Sets the value at the index/indices. Works on arrays of float. Returns val."
    :added "1.0"}
  aset-float setFloat float)

(def-aset
  ^{:doc "Sets the value at the index/indices. Works on arrays of double. Returns val."
    :added "1.0"}
  aset-double setDouble double)

(def-aset
  ^{:doc "Sets the value at the index/indices. Works on arrays of short. Returns val."
    :added "1.0"}
  aset-short setShort short)

(def-aset
  ^{:doc "Sets the value at the index/indices. Works on arrays of byte. Returns val."
    :added "1.0"}
  aset-byte setByte byte)

(def-aset
  ^{:doc "Sets the value at the index/indices. Works on arrays of char. Returns val."
    :added "1.0"}
  aset-char setChar char)

(defn make-array
  "Creates and returns an array of instances of the specified class of
  the specified dimension(s).  Note that a class object is required.
  Class objects can be obtained by using their imported or
  fully-qualified name.  Class objects for the primitive types can be
  obtained using, e.g., Integer/TYPE."
  {:added "1.0"
   :static true}
  ([^Class type len]
   (. Array (newInstance type (int len))))
  ([^Class type dim & more-dims]
   (let [dims (cons dim more-dims)
         ^"[I" dimarray (make-array (. Integer TYPE)  (count dims))]
     (dotimes [i (alength dimarray)]
       (aset-int dimarray i (nth dims i)))
     (. Array (newInstance type dimarray)))))

(defn to-array-2d
  "Returns a (potentially-ragged) 2-dimensional array of Objects
  containing the contents of coll, which can be any Collection of any
  Collection."
  {:tag "[[Ljava.lang.Object;"
   :added "1.0"
   :static true}
  [^java.util.Collection coll]
    (let [ret (make-array (. Class (forName "[Ljava.lang.Object;")) (. coll (size)))]
      (loop [i 0 xs (seq coll)]
        (when xs
          (aset ret i (to-array (first xs)))
          (recur (inc i) (next xs))))
      ret))

(defn macroexpand-1
  "If form represents a macro form, returns its expansion,
  else returns form."
  {:added "1.0"
   :static true}
  [form]
    (. clojure.lang.Compiler (macroexpand1 form)))

(defn macroexpand
  "Repeatedly calls macroexpand-1 on form until it no longer
  represents a macro form, then returns it.  Note neither
  macroexpand-1 nor macroexpand expand macros in subforms."
  {:added "1.0"
   :static true}
  [form]
    (let [ex (macroexpand-1 form)]
      (if (identical? ex form)
        form
        (macroexpand ex))))

(defn create-struct
  "Returns a structure basis object."
  {:added "1.0"
   :static true}
  [& keys]
    (. clojure.lang.PersistentStructMap (createSlotMap keys)))

(defmacro defstruct
  "Same as (def name (create-struct keys...))"
  {:added "1.0"
   :static true}
  [name & keys]
  `(def ~name (create-struct ~@keys)))

(defn struct-map
  "Returns a new structmap instance with the keys of the
  structure-basis. keyvals may contain all, some or none of the basis
  keys - where values are not supplied they will default to nil.
  keyvals can also contain keys not in the basis."
  {:added "1.0"
   :static true}
  [s & inits]
    (. clojure.lang.PersistentStructMap (create s inits)))

(defn struct
  "Returns a new structmap instance with the keys of the
  structure-basis. vals must be supplied for basis keys in order -
  where values are not supplied they will default to nil."
  {:added "1.0"
   :static true}
  [s & vals]
    (. clojure.lang.PersistentStructMap (construct s vals)))

(defn accessor
  "Returns a fn that, given an instance of a structmap with the basis,
  returns the value at the key.  The key must be in the basis. The
  returned function should be (slightly) more efficient than using
  get, but such use of accessors should be limited to known
  performance-critical areas."
  {:added "1.0"
   :static true}
  [s key]
    (. clojure.lang.PersistentStructMap (getAccessor s key)))

(defn load-reader
  "Sequentially read and evaluate the set of forms contained in the
  stream/file"
  {:added "1.0"
   :static true}
  [rdr] (. clojure.lang.Compiler (load rdr)))

(defn load-string
  "Sequentially read and evaluate the set of forms contained in the
  string"
  {:added "1.0"
   :static true}
  [s]
  (let [rdr (-> (java.io.StringReader. s)
                (clojure.lang.LineNumberingPushbackReader.))]
    (load-reader rdr)))

(defn set?
  "Returns true if x implements IPersistentSet"
  {:added "1.0"
   :static true}
  [x] (instance? clojure.lang.IPersistentSet x))

(defn set
  "Returns a set of the distinct elements of coll."
  {:added "1.0"
   :static true}
  [coll]
  (if (set? coll)
    (with-meta coll nil)
    (if (instance? clojure.lang.IReduceInit coll)
      (persistent! (.reduce ^clojure.lang.IReduceInit coll conj! (transient #{})))
      (persistent! (reduce1 conj! (transient #{}) coll)))))

(defn ^{:private true
   :static true}
  filter-key [keyfn pred amap]
    (loop [ret {} es (seq amap)]
      (if es
        (if (pred (keyfn (first es)))
          (recur (assoc ret (key (first es)) (val (first es))) (next es))
          (recur ret (next es)))
        ret)))

(defn find-ns
  "Returns the namespace named by the symbol or nil if it doesn't exist."
  {:added "1.0"
   :static true}
  [sym] (clojure.lang.Namespace/find sym))

(defn create-ns
  "Create a new namespace named by the symbol if one doesn't already
  exist, returns it or the already-existing namespace of the same
  name."
  {:added "1.0"
   :static true}
  [sym] (clojure.lang.Namespace/findOrCreate sym))

(defn remove-ns
  "Removes the namespace named by the symbol. Use with caution.
  Cannot be used to remove the clojure namespace."
  {:added "1.0"
   :static true}
  [sym] (clojure.lang.Namespace/remove sym))

(defn all-ns
  "Returns a sequence of all namespaces."
  {:added "1.0"
   :static true}
  [] (clojure.lang.Namespace/all))

(defn the-ns
  "If passed a namespace, returns it. Else, when passed a symbol,
  returns the namespace named by it, throwing an exception if not
  found."
  {:added "1.0"
   :static true}
  ^clojure.lang.Namespace [x]
  (if (instance? clojure.lang.Namespace x)
    x
    (or (find-ns x) (throw (Exception. (str "No namespace: " x " found"))))))

(defn ns-name
  "Returns the name of the namespace, a symbol."
  {:added "1.0"
   :static true}
  [ns]
  (.getName (the-ns ns)))

(defn ns-map
  "Returns a map of all the mappings for the namespace."
  {:added "1.0"
   :static true}
  [ns]
  (.getMappings (the-ns ns)))

(defn ns-unmap
  "Removes the mappings for the symbol from the namespace."
  {:added "1.0"
   :static true}
  [ns sym]
  (.unmap (the-ns ns) sym))

;(defn export [syms]
;  (doseq [sym syms]
;   (.. *ns* (intern sym) (setExported true))))

(defn ns-publics
  "Returns a map of the public intern mappings for the namespace."
  {:added "1.0"
   :static true}
  [ns]
  (let [ns (the-ns ns)]
    (filter-key val (fn [^clojure.lang.Var v] (and (instance? clojure.lang.Var v)
                                 (= ns (.ns v))
                                 (.isPublic v)))
                (ns-map ns))))

(defn ns-imports
  "Returns a map of the import mappings for the namespace."
  {:added "1.0"
   :static true}
  [ns]
  (filter-key val (partial instance? Class) (ns-map ns)))

(defn ns-interns
  "Returns a map of the intern mappings for the namespace."
  {:added "1.0"
   :static true}
  [ns]
  (let [ns (the-ns ns)]
    (filter-key val (fn [^clojure.lang.Var v] (and (instance? clojure.lang.Var v)
                                 (= ns (.ns v))))
                (ns-map ns))))

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
  {:added "1.0"}
  [ns-sym & filters]
    (let [ns (or (find-ns ns-sym) (throw (new Exception (str "No namespace: " ns-sym))))
          fs (apply hash-map filters)
          nspublics (ns-publics ns)
          rename (or (:rename fs) {})
          exclude (set (:exclude fs))
          to-do (if (= :all (:refer fs))
                  (keys nspublics)
                  (or (:refer fs) (:only fs) (keys nspublics)))]
      (when (and to-do (not (instance? clojure.lang.Sequential to-do)))
        (throw (new Exception ":only/:refer value must be a sequential collection of symbols")))
      (doseq [sym to-do]
        (when-not (exclude sym)
          (let [v (nspublics sym)]
            (when-not v
              (throw (new java.lang.IllegalAccessError
                          (if (get (ns-interns ns) sym)
                            (str sym " is not public")
                            (str sym " does not exist")))))
            (. *ns* (refer (or (rename sym) sym) v)))))))

(defn ns-refers
  "Returns a map of the refer mappings for the namespace."
  {:added "1.0"
   :static true}
  [ns]
  (let [ns (the-ns ns)]
    (filter-key val (fn [^clojure.lang.Var v] (and (instance? clojure.lang.Var v)
                                 (not= ns (.ns v))))
                (ns-map ns))))

(defn alias
  "Add an alias in the current namespace to another
  namespace. Arguments are two symbols: the alias to be used, and
  the symbolic name of the target namespace. Use :as in the ns macro in preference
  to calling this directly."
  {:added "1.0"
   :static true}
  [alias namespace-sym]
  (.addAlias *ns* alias (the-ns namespace-sym)))

(defn ns-aliases
  "Returns a map of the aliases for the namespace."
  {:added "1.0"
   :static true}
  [ns]
  (.getAliases (the-ns ns)))

(defn ns-unalias
  "Removes the alias for the symbol from the namespace."
  {:added "1.0"
   :static true}
  [ns sym]
  (.removeAlias (the-ns ns) sym))

(defn take-nth
  "Returns a lazy seq of every nth item in coll.  Returns a stateful
  transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([n]
     (fn [rf]
       (let [iv (volatile! -1)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [i (vswap! iv inc)]
                (if (zero? (rem i n))
                  (rf result input)
                  result)))))))
  ([n coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (cons (first s) (take-nth n (drop n s)))))))

(defn interleave
  "Returns a lazy seq of the first item in each coll, then the second etc."
  {:added "1.0"
   :static true}
  ([] ())
  ([c1] (lazy-seq c1))
  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (when (and s1 s2)
          (cons (first s1) (cons (first s2) 
                                 (interleave (rest s1) (rest s2))))))))
  ([c1 c2 & colls] 
     (lazy-seq 
      (let [ss (map seq (conj colls c2 c1))]
        (when (every? identity ss)
          (concat (map first ss) (apply interleave (map rest ss))))))))

(defn var-get
  "Gets the value in the var object"
  {:added "1.0"
   :static true}
  [^clojure.lang.Var x] (. x (get)))

(defn var-set
  "Sets the value in the var object to val. The var must be
 thread-locally bound."
  {:added "1.0"
   :static true}
  [^clojure.lang.Var x val] (. x (set val)))

(defmacro with-local-vars
  "varbinding=> symbol init-expr

  Executes the exprs in a context in which the symbols are bound to
  vars with per-thread bindings to the init-exprs.  The symbols refer
  to the var objects themselves, and must be accessed with var-get and
  var-set"
  {:added "1.0"}
  [name-vals-vec & body]
  (assert-args
     (vector? name-vals-vec) "a vector for its binding"
     (even? (count name-vals-vec)) "an even number of forms in binding vector")
  `(let [~@(interleave (take-nth 2 name-vals-vec)
                       (repeat '(.. clojure.lang.Var create setDynamic)))]
     (. clojure.lang.Var (pushThreadBindings (hash-map ~@name-vals-vec)))
     (try
      ~@body
      (finally (. clojure.lang.Var (popThreadBindings))))))

(defn ns-resolve
  "Returns the var or Class to which a symbol will be resolved in the
  namespace (unless found in the environment), else nil.  Note that
  if the symbol is fully qualified, the var/Class to which it resolves
  need not be present in the namespace."
  {:added "1.0"
   :static true}
  ([ns sym]
    (ns-resolve ns nil sym))
  ([ns env sym]
    (when-not (contains? env sym)
      (clojure.lang.Compiler/maybeResolveIn (the-ns ns) sym))))

(defn resolve
  "same as (ns-resolve *ns* symbol) or (ns-resolve *ns* &env symbol)"
  {:added "1.0"
   :static true}
  ([sym] (ns-resolve *ns* sym))
  ([env sym] (ns-resolve *ns* env sym)))

(defn array-map
  "Constructs an array-map. If any keys are equal, they are handled as
  if by repeated uses of assoc."
  {:added "1.0"
   :static true}
  ([] (. clojure.lang.PersistentArrayMap EMPTY))
  ([& keyvals]
     (clojure.lang.PersistentArrayMap/createAsIfByAssoc (to-array keyvals))))

;redefine let and loop  with destructuring
(defn destructure [bindings]
  (let [bents (partition 2 bindings)
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
                                (= firstb '&) (recur (pb ret (second bs) (list `nthnext gvec n))
                                                     n
                                                     (nnext bs)
                                                     true)
                                (= firstb :as) (pb ret (second bs) gvec)
                                :else (if seen-rest?
                                        (throw (new Exception "Unsupported binding form, only :as can follow & parameter"))
                                        (recur (pb ret firstb  (list `nth gvec n nil))
                                               (inc n)
                                               (next bs)
                                               seen-rest?))))
                             ret))))
                     pmap
                     (fn [bvec b v]
                       (let [gmap (gensym "map__")
                             gmapseq (with-meta gmap {:tag 'clojure.lang.ISeq})
                             defaults (:or b)]
                         (loop [ret (-> bvec (conj gmap) (conj v)
                                        (conj gmap) (conj `(if (seq? ~gmap) (clojure.lang.PersistentHashMap/create (seq ~gmapseq)) ~gmap))
                                        ((fn [ret]
                                           (if (:as b)
                                             (conj ret (:as b) gmap)
                                             ret))))
                                bes (reduce1
                                     (fn [bes entry]
                                       (reduce1 #(assoc %1 %2 ((val entry) %2))
                                               (dissoc bes (key entry))
                                               ((key entry) bes)))
                                     (dissoc b :as :or)
                                     {:keys #(if (keyword? %) % (keyword (str %))),
                                      :strs str, :syms #(list `quote %)})]
                           (if (seq bes)
                             (let [bb (key (first bes))
                                   bk (val (first bes))
                                   bv (if (contains? defaults bb)
                                        (list `get gmap bk (defaults bb))
                                        (list `get gmap bk))]
                               (recur (cond
                                        (symbol? bb) (-> ret (conj (if (namespace bb) (symbol (name bb)) bb)) (conj bv))
                                        (keyword? bb) (-> ret (conj (symbol (name bb)) bv))
                                        :else (pb ret bb bv))
                                      (next bes)))
                             ret))))]
                 (cond
                   (symbol? b) (-> bvec (conj b) (conj v))
                   (vector? b) (pvec bvec b v)
                   (map? b) (pmap bvec b v)
                   :else (throw (new Exception (str "Unsupported binding form: " b))))))
        process-entry (fn [bvec b] (pb bvec (first b) (second b)))]
    (if (every? symbol? (map first bents))
      bindings
      (reduce1 process-entry [] bents))))

(defmacro let
  "binding => binding-form init-expr

  Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein."
  {:added "1.0", :special-form true, :forms '[(let [bindings*] exprs*)]}
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
  `(let* ~(destructure bindings) ~@body))

(defn ^{:private true}
  maybe-destructured
  [params body]
  (if (every? symbol? params)
    (cons params body)
    (loop [params params
           new-params (with-meta [] (meta params))
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        `(~new-params
          (let ~lets
            ~@body))))))

;redefine fn with destructuring and pre/post conditions
(defmacro fn
  "params => positional-params* , or positional-params* & next-param
  positional-param => binding-form
  next-param => binding-form
  name => symbol

  Defines a function"
  {:added "1.0", :special-form true,
   :forms '[(fn name? [params* ] exprs*) (fn name? ([params* ] exprs*)+)]}
  [& sigs]
    (let [name (if (symbol? (first sigs)) (first sigs) nil)
          sigs (if name (next sigs) sigs)
          sigs (if (vector? (first sigs)) 
                 (list sigs) 
                 (if (seq? (first sigs))
                   sigs
                   ;; Assume single arity syntax
                   (throw (IllegalArgumentException. 
                            (if (seq sigs)
                              (str "Parameter declaration " 
                                   (first sigs)
                                   " should be a vector")
                              (str "Parameter declaration missing"))))))
          psig (fn* [sig]
                 ;; Ensure correct type before destructuring sig
                 (when (not (seq? sig))
                   (throw (IllegalArgumentException.
                            (str "Invalid signature " sig
                                 " should be a list"))))
                 (let [[params & body] sig
                       _ (when (not (vector? params))
                           (throw (IllegalArgumentException. 
                                    (if (seq? (first sigs))
                                      (str "Parameter declaration " params
                                           " should be a vector")
                                      (str "Invalid signature " sig
                                           " should be a list")))))
                       conds (when (and (next body) (map? (first body))) 
                                           (first body))
                       body (if conds (next body) body)
                       conds (or conds (meta params))
                       pre (:pre conds)
                       post (:post conds)                       
                       body (if post
                              `((let [~'% ~(if (< 1 (count body)) 
                                            `(do ~@body) 
                                            (first body))]
                                 ~@(map (fn* [c] `(assert ~c)) post)
                                 ~'%))
                              body)
                       body (if pre
                              (concat (map (fn* [c] `(assert ~c)) pre) 
                                      body)
                              body)]
                   (maybe-destructured params body)))
          new-sigs (map psig sigs)]
      (with-meta
        (if name
          (list* 'fn* name new-sigs)
          (cons 'fn* new-sigs))
        (meta &form))))

(defmacro loop
  "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Acts as a recur target."
  {:added "1.0", :special-form true, :forms '[(loop [bindings*] exprs*)]}
  [bindings & body]
    (assert-args
      (vector? bindings) "a vector for its binding"
      (even? (count bindings)) "an even number of forms in binding vector")
    (let [db (destructure bindings)]
      (if (= db bindings)
        `(loop* ~bindings ~@body)
        (let [vs (take-nth 2 (drop 1 bindings))
              bs (take-nth 2 bindings)
              gs (map (fn [b] (if (symbol? b) b (gensym))) bs)
              bfs (reduce1 (fn [ret [b v g]]
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

  Roughly the same as (when (seq xs) (let [x (first xs)] body)) but xs is evaluated only once"
  {:added "1.0"}
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [[x xs] bindings]
    `(when-let [xs# (seq ~xs)]
       (let [~x (first xs#)]
           ~@body))))

(defmacro lazy-cat
  "Expands to code which yields a lazy sequence of the concatenation
  of the supplied colls.  Each coll expr is not evaluated until it is
  needed. 

  (lazy-cat xs ys zs) === (concat (lazy-seq xs) (lazy-seq ys) (lazy-seq zs))"
  {:added "1.0"}
  [& colls]
  `(concat ~@(map #(list `lazy-seq %) colls)))

(defmacro for
  "List comprehension. Takes a vector of one or more
   binding-form/collection-expr pairs, each followed by zero or more
   modifiers, and yields a lazy sequence of evaluations of expr.
   Collections are iterated in a nested fashion, rightmost fastest,
   and nested coll-exprs can refer to bindings created in prior
   binding-forms.  Supported modifiers are: :let [binding-form expr ...],
   :while test, :when test.

  (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)] [x y]))"
  {:added "1.0"}
  [seq-exprs body-expr]
  (assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [to-groups (fn [seq-exprs]
                    (reduce1 (fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
        err (fn [& msg] (throw (IllegalArgumentException. ^String (apply str msg))))
        emit-bind (fn emit-bind [[[bind expr & mod-pairs]
                                  & [[_ next-expr] :as next-groups]]]
                    (let [giter (gensym "iter__")
                          gxs (gensym "s__")
                          do-mod (fn do-mod [[[k v :as pair] & etc]]
                                   (cond
                                     (= k :let) `(let ~v ~(do-mod etc))
                                     (= k :while) `(when ~v ~(do-mod etc))
                                     (= k :when) `(if ~v
                                                    ~(do-mod etc)
                                                    (recur (rest ~gxs)))
                                     (keyword? k) (err "Invalid 'for' keyword " k)
                                     next-groups
                                      `(let [iterys# ~(emit-bind next-groups)
                                             fs# (seq (iterys# ~next-expr))]
                                         (if fs#
                                           (concat fs# (~giter (rest ~gxs)))
                                           (recur (rest ~gxs))))
                                     :else `(cons ~body-expr
                                                  (~giter (rest ~gxs)))))]
                      (if next-groups
                        #_"not the inner-most loop"
                        `(fn ~giter [~gxs]
                           (lazy-seq
                             (loop [~gxs ~gxs]
                               (when-first [~bind ~gxs]
                                 ~(do-mod mod-pairs)))))
                        #_"inner-most loop"
                        (let [gi (gensym "i__")
                              gb (gensym "b__")
                              do-cmod (fn do-cmod [[[k v :as pair] & etc]]
                                        (cond
                                          (= k :let) `(let ~v ~(do-cmod etc))
                                          (= k :while) `(when ~v ~(do-cmod etc))
                                          (= k :when) `(if ~v
                                                         ~(do-cmod etc)
                                                         (recur
                                                           (unchecked-inc ~gi)))
                                          (keyword? k)
                                            (err "Invalid 'for' keyword " k)
                                          :else
                                            `(do (chunk-append ~gb ~body-expr)
                                                 (recur (unchecked-inc ~gi)))))]
                          `(fn ~giter [~gxs]
                             (lazy-seq
                               (loop [~gxs ~gxs]
                                 (when-let [~gxs (seq ~gxs)]
                                   (if (chunked-seq? ~gxs)
                                     (let [c# (chunk-first ~gxs)
                                           size# (int (count c#))
                                           ~gb (chunk-buffer size#)]
                                       (if (loop [~gi (int 0)]
                                             (if (< ~gi size#)
                                               (let [~bind (.nth c# ~gi)]
                                                 ~(do-cmod mod-pairs))
                                               true))
                                         (chunk-cons
                                           (chunk ~gb)
                                           (~giter (chunk-rest ~gxs)))
                                         (chunk-cons (chunk ~gb) nil)))
                                     (let [~bind (first ~gxs)]
                                       ~(do-mod mod-pairs)))))))))))]
    `(let [iter# ~(emit-bind (to-groups seq-exprs))]
        (iter# ~(second seq-exprs)))))

(defmacro comment
  "Ignores body, yields nil"
  {:added "1.0"}
  [& body])

(defmacro with-out-str
  "Evaluates exprs in a context in which *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
  {:added "1.0"}
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       ~@body
       (str s#))))

(defmacro with-in-str
  "Evaluates body in a context in which *in* is bound to a fresh
  StringReader initialized with the string s."
  {:added "1.0"}
  [s & body]
  `(with-open [s# (-> (java.io.StringReader. ~s) clojure.lang.LineNumberingPushbackReader.)]
     (binding [*in* s#]
       ~@body)))

(defn pr-str
  "pr to a string, returning it"
  {:tag String
   :added "1.0"
   :static true}
  [& xs]
    (with-out-str
     (apply pr xs)))

(defn prn-str
  "prn to a string, returning it"
  {:tag String
   :added "1.0"
   :static true}
  [& xs]
  (with-out-str
   (apply prn xs)))

(defn print-str
  "print to a string, returning it"
  {:tag String
   :added "1.0"
   :static true}
  [& xs]
    (with-out-str
     (apply print xs)))

(defn println-str
  "println to a string, returning it"
  {:tag String
   :added "1.0"
   :static true}
  [& xs]
    (with-out-str
     (apply println xs)))

(import clojure.lang.ExceptionInfo clojure.lang.IExceptionInfo)
(defn ex-info
  "Create an instance of ExceptionInfo, a RuntimeException subclass
   that carries a map of additional data."
  {:added "1.4"}
  ([msg map]
     (ExceptionInfo. msg map))
  ([msg map cause]
     (ExceptionInfo. msg map cause)))

(defn ex-data
  "Returns exception data (a map) if ex is an IExceptionInfo.
   Otherwise returns nil."
  {:added "1.4"}
  [ex]
  (when (instance? IExceptionInfo ex)
    (.getData ^IExceptionInfo ex)))

(defmacro assert
  "Evaluates expr and throws an exception if it does not evaluate to
  logical true."
  {:added "1.0"}
  ([x]
     (when *assert*
       `(when-not ~x
          (throw (new AssertionError (str "Assert failed: " (pr-str '~x)))))))
  ([x message]
     (when *assert*
       `(when-not ~x
          (throw (new AssertionError (str "Assert failed: " ~message "\n" (pr-str '~x))))))))

(defn test
  "test [v] finds fn at key :test in var metadata and calls it,
  presuming failure will throw exception"
  {:added "1.0"}
  [v]
    (let [f (:test (meta v))]
      (if f
        (do (f) :ok)
        :no-test)))

(defn re-pattern
  "Returns an instance of java.util.regex.Pattern, for use, e.g. in
  re-matcher."
  {:tag java.util.regex.Pattern
   :added "1.0"
   :static true}
  [s] (if (instance? java.util.regex.Pattern s)
        s
        (. java.util.regex.Pattern (compile s))))

(defn re-matcher
  "Returns an instance of java.util.regex.Matcher, for use, e.g. in
  re-find."
  {:tag java.util.regex.Matcher
   :added "1.0"
   :static true}
  [^java.util.regex.Pattern re s]
    (. re (matcher s)))

(defn re-groups
  "Returns the groups from the most recent match/find. If there are no
  nested groups, returns a string of the entire match. If there are
  nested groups, returns a vector of the groups, the first element
  being the entire match."
  {:added "1.0"
   :static true}
  [^java.util.regex.Matcher m]
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
  {:added "1.0"
   :static true}
  [^java.util.regex.Pattern re s]
  (let [m (re-matcher re s)]
    ((fn step []
       (when (. m (find))
         (cons (re-groups m) (lazy-seq (step))))))))

(defn re-matches
  "Returns the match, if any, of string to pattern, using
  java.util.regex.Matcher.matches().  Uses re-groups to return the
  groups."
  {:added "1.0"
   :static true}
  [^java.util.regex.Pattern re s]
    (let [m (re-matcher re s)]
      (when (. m (matches))
        (re-groups m))))


(defn re-find
  "Returns the next regex match, if any, of string to pattern, using
  java.util.regex.Matcher.find().  Uses re-groups to return the
  groups."
  {:added "1.0"
   :static true}
  ([^java.util.regex.Matcher m]
   (when (. m (find))
     (re-groups m)))
  ([^java.util.regex.Pattern re s]
   (let [m (re-matcher re s)]
     (re-find m))))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive)."
  {:added "1.0"
   :static true}
  ([] (. Math (random)))
  ([n] (* n (rand))))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive)."
  {:added "1.0"
   :static true}
  [n] (int (rand n)))

(defmacro defn-
  "same as defn, yielding non-public def"
  {:added "1.0"}
  [name & decls]
    (list* `defn (with-meta name (assoc (meta name) :private true)) decls))

(defn tree-seq
  "Returns a lazy sequence of the nodes in a tree, via a depth-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
  tree."
  {:added "1.0"
   :static true}
   [branch? children root]
   (let [walk (fn walk [node]
                (lazy-seq
                 (cons node
                  (when (branch? node)
                    (mapcat walk (children node))))))]
     (walk root)))

(defn file-seq
  "A tree seq on java.io.Files"
  {:added "1.0"
   :static true}
  [dir]
    (tree-seq
     (fn [^java.io.File f] (. f (isDirectory)))
     (fn [^java.io.File d] (seq (. d (listFiles))))
     dir))

(defn xml-seq
  "A tree seq on the xml elements as per xml/parse"
  {:added "1.0"
   :static true}
  [root]
    (tree-seq
     (complement string?)
     (comp seq :content)
     root))

(defn special-symbol?
  "Returns true if s names a special form"
  {:added "1.0"
   :static true}
  [s]
    (contains? (. clojure.lang.Compiler specials) s))

(defn var?
  "Returns true if v is of type clojure.lang.Var"
  {:added "1.0"
   :static true}
  [v] (instance? clojure.lang.Var v))

(defn subs
  "Returns the substring of s beginning at start inclusive, and ending
  at end (defaults to length of string), exclusive."
  {:added "1.0"
   :static true}
  (^String [^String s start] (. s (substring start)))
  (^String [^String s start end] (. s (substring start end))))

(defn max-key
  "Returns the x for which (k x), a number, is greatest."
  {:added "1.0"
   :static true}
  ([k x] x)
  ([k x y] (if (> (k x) (k y)) x y))
  ([k x y & more]
   (reduce1 #(max-key k %1 %2) (max-key k x y) more)))

(defn min-key
  "Returns the x for which (k x), a number, is least."
  {:added "1.0"
   :static true}
  ([k x] x)
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
   (reduce1 #(min-key k %1 %2) (min-key k x y) more)))

(defn distinct
  "Returns a lazy sequence of the elements of coll with duplicates removed.
  Returns a stateful transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (contains? @seen input)
            result
            (do (vswap! seen conj input)
                (rf result input))))))))
  ([coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[f :as xs] seen]
                     (when-let [s (seq xs)]
                       (if (contains? seen f)
                         (recur (rest s) seen)
                         (cons f (step (rest s) (conj seen f))))))
                   xs seen)))]
     (step coll #{}))))



(defn replace
  "Given a map of replacement pairs and a vector/collection, returns a
  vector/seq with any elements = a key in smap replaced with the
  corresponding val in smap.  Returns a transducer when no collection
  is provided."
  {:added "1.0"
   :static true}
  ([smap]
     (map #(if-let [e (find smap %)] (val e) %)))
  ([smap coll]
     (if (vector? coll)
       (reduce1 (fn [v i]
                  (if-let [e (find smap (nth v i))]
                    (assoc v i (val e))
                    v))
                coll (range (count coll)))
       (map #(if-let [e (find smap %)] (val e) %) coll))))

(defmacro dosync
  "Runs the exprs (in an implicit do) in a transaction that encompasses
  exprs and any nested calls.  Starts a transaction if none is already
  running on this thread. Any uncaught exception will abort the
  transaction and flow out of dosync. The exprs may be run more than
  once, but any effects on Refs will be atomic."
  {:added "1.0"}
  [& exprs]
  `(sync nil ~@exprs))

(defmacro with-precision
  "Sets the precision and rounding mode to be used for BigDecimal operations.

  Usage: (with-precision 10 (/ 1M 3))
  or:    (with-precision 10 :rounding HALF_DOWN (/ 1M 3))

  The rounding mode is one of CEILING, FLOOR, HALF_UP, HALF_DOWN,
  HALF_EVEN, UP, DOWN and UNNECESSARY; it defaults to HALF_UP."
  {:added "1.0"}
  [precision & exprs]
    (let [[body rm] (if (= (first exprs) :rounding)
                      [(next (next exprs))
                       `((. java.math.RoundingMode ~(second exprs)))]
                      [exprs nil])]
      `(binding [*math-context* (java.math.MathContext. ~precision ~@rm)]
         ~@body)))

(defn mk-bound-fn
  {:private true}
  [^clojure.lang.Sorted sc test key]
  (fn [e]
    (test (.. sc comparator (compare (. sc entryKey e) key)) 0)))

(defn subseq
  "sc must be a sorted collection, test(s) one of <, <=, > or
  >=. Returns a seq of those entries with keys ek for
  which (test (.. sc comparator (compare ek key)) 0) is true"
  {:added "1.0"
   :static true}
  ([^clojure.lang.Sorted sc test key]
   (let [include (mk-bound-fn sc test key)]
     (if (#{> >=} test)
       (when-let [[e :as s] (. sc seqFrom key true)]
         (if (include e) s (next s)))
       (take-while include (. sc seq true)))))
  ([^clojure.lang.Sorted sc start-test start-key end-test end-key]
   (when-let [[e :as s] (. sc seqFrom start-key true)]
     (take-while (mk-bound-fn sc end-test end-key)
                 (if ((mk-bound-fn sc start-test start-key) e) s (next s))))))

(defn rsubseq
  "sc must be a sorted collection, test(s) one of <, <=, > or
  >=. Returns a reverse seq of those entries with keys ek for
  which (test (.. sc comparator (compare ek key)) 0) is true"
  {:added "1.0"
   :static true}
  ([^clojure.lang.Sorted sc test key]
   (let [include (mk-bound-fn sc test key)]
     (if (#{< <=} test)
       (when-let [[e :as s] (. sc seqFrom key false)]
         (if (include e) s (next s)))
       (take-while include (. sc seq false)))))
  ([^clojure.lang.Sorted sc start-test start-key end-test end-key]
   (when-let [[e :as s] (. sc seqFrom end-key false)]
     (take-while (mk-bound-fn sc start-test start-key)
                 (if ((mk-bound-fn sc end-test end-key) e) s (next s))))))

(defn repeatedly
  "Takes a function of no args, presumably with side effects, and
  returns an infinite (or length n if supplied) lazy sequence of calls
  to it"
  {:added "1.0"
   :static true}
  ([f] (lazy-seq (cons (f) (repeatedly f))))
  ([n f] (take n (repeatedly f))))

(defn add-classpath
  "DEPRECATED 

  Adds the url (String or URL object) to the classpath per
  URLClassLoader.addURL"
  {:added "1.0"
   :deprecated "1.1"}
  [url]
  (println "WARNING: add-classpath is deprecated")
  (clojure.lang.RT/addURL url))



(defn hash
  "Returns the hash code of its argument. Note this is the hash code
  consistent with =, and thus is different than .hashCode for Integer,
  Short, Byte and Clojure collections."

  {:added "1.0"
   :static true}
  [x] (. clojure.lang.Util (hasheq x)))


(defn mix-collection-hash
  "Mix final collection hash for ordered or unordered collections.
   hash-basis is the combined collection hash, count is the number
   of elements included in the basis. Note this is the hash code
   consistent with =, different from .hashCode.
   See http://clojure.org/data_structures#hash for full algorithms."
  {:added "1.6"
   :static true}
  ^long
  [^long hash-basis ^long count] (clojure.lang.Murmur3/mixCollHash hash-basis count))

(defn hash-ordered-coll
  "Returns the hash code, consistent with =, for an external ordered
   collection implementing Iterable.
   See http://clojure.org/data_structures#hash for full algorithms."
  {:added "1.6"
   :static true}
  ^long
  [coll] (clojure.lang.Murmur3/hashOrdered coll))

(defn hash-unordered-coll
  "Returns the hash code, consistent with =, for an external unordered
   collection implementing Iterable. For maps, the iterator should
   return map entries whose hash is computed as
     (hash-ordered-coll [k v]).
   See http://clojure.org/data_structures#hash for full algorithms."
  {:added "1.6"
   :static true}
  ^long
  [coll] (clojure.lang.Murmur3/hashUnordered coll))

(defn interpose
  "Returns a lazy seq of the elements of coll separated by sep.
  Returns a stateful transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([sep]
   (fn [rf]
     (let [started (volatile! false)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if @started
            (let [sepr (rf result sep)]
              (if (reduced? sepr)
                sepr
                (rf sepr input)))
            (do
              (vreset! started true)
              (rf result input))))))))
  ([sep coll]
   (drop 1 (interleave (repeat sep) coll))))

(defmacro definline
  "Experimental - like defmacro, except defines a named function whose
  body is the expansion, calls to which may be expanded inline as if
  it were a macro. Cannot be used with variadic (&) args."
  {:added "1.0"}
  [name & decl]
  (let [[pre-args [args expr]] (split-with (comp not vector?) decl)]
    `(do
       (defn ~name ~@pre-args ~args ~(apply (eval (list `fn args expr)) args))
       (alter-meta! (var ~name) assoc :inline (fn ~name ~args ~expr))
       (var ~name))))

(defn empty
  "Returns an empty collection of the same category as coll, or nil"
  {:added "1.0"
   :static true}
  [coll]
  (when (instance? clojure.lang.IPersistentCollection coll)
    (.empty ^clojure.lang.IPersistentCollection coll)))

(defmacro amap
  "Maps an expression across an array a, using an index named idx, and
  return value named ret, initialized to a clone of a, then setting 
  each element of ret to the evaluation of expr, returning the new 
  array ret."
  {:added "1.0"}
  [a idx ret expr]
  `(let [a# ~a
         ~ret (aclone a#)]
     (loop  [~idx 0]
       (if (< ~idx  (alength a#))
         (do
           (aset ~ret ~idx ~expr)
           (recur (unchecked-inc ~idx)))
         ~ret))))

(defmacro areduce
  "Reduces an expression across an array a, using an index named idx,
  and return value named ret, initialized to init, setting ret to the 
  evaluation of expr at each step, returning ret."
  {:added "1.0"}
  [a idx ret init expr]
  `(let [a# ~a l# (alength a#)]
     (loop  [~idx 0 ~ret ~init]
       (if (< ~idx l#)
         (recur (unchecked-inc-int ~idx) ~expr)
         ~ret))))

(defn float-array
  "Creates an array of floats"
  {:inline (fn [& args] `(. clojure.lang.Numbers float_array ~@args))
   :inline-arities #{1 2}
   :added "1.0"}
  ([size-or-seq] (. clojure.lang.Numbers float_array size-or-seq))
  ([size init-val-or-seq] (. clojure.lang.Numbers float_array size init-val-or-seq)))

(defn boolean-array
  "Creates an array of booleans"
  {:inline (fn [& args] `(. clojure.lang.Numbers boolean_array ~@args))
   :inline-arities #{1 2}
   :added "1.1"}
  ([size-or-seq] (. clojure.lang.Numbers boolean_array size-or-seq))
  ([size init-val-or-seq] (. clojure.lang.Numbers boolean_array size init-val-or-seq)))

(defn byte-array
  "Creates an array of bytes"
  {:inline (fn [& args] `(. clojure.lang.Numbers byte_array ~@args))
   :inline-arities #{1 2}
   :added "1.1"}
  ([size-or-seq] (. clojure.lang.Numbers byte_array size-or-seq))
  ([size init-val-or-seq] (. clojure.lang.Numbers byte_array size init-val-or-seq)))

(defn char-array
  "Creates an array of chars"
  {:inline (fn [& args] `(. clojure.lang.Numbers char_array ~@args))
   :inline-arities #{1 2}
   :added "1.1"}
  ([size-or-seq] (. clojure.lang.Numbers char_array size-or-seq))
  ([size init-val-or-seq] (. clojure.lang.Numbers char_array size init-val-or-seq)))

(defn short-array
  "Creates an array of shorts"
  {:inline (fn [& args] `(. clojure.lang.Numbers short_array ~@args))
   :inline-arities #{1 2}
   :added "1.1"}
  ([size-or-seq] (. clojure.lang.Numbers short_array size-or-seq))
  ([size init-val-or-seq] (. clojure.lang.Numbers short_array size init-val-or-seq)))

(defn double-array
  "Creates an array of doubles"
  {:inline (fn [& args] `(. clojure.lang.Numbers double_array ~@args))
   :inline-arities #{1 2}
   :added "1.0"}
  ([size-or-seq] (. clojure.lang.Numbers double_array size-or-seq))
  ([size init-val-or-seq] (. clojure.lang.Numbers double_array size init-val-or-seq)))

(defn object-array
  "Creates an array of objects"
  {:inline (fn [arg] `(. clojure.lang.RT object_array ~arg))
   :inline-arities #{1}
   :added "1.2"}
  ([size-or-seq] (. clojure.lang.RT object_array size-or-seq)))

(defn int-array
  "Creates an array of ints"
  {:inline (fn [& args] `(. clojure.lang.Numbers int_array ~@args))
   :inline-arities #{1 2}
   :added "1.0"}
  ([size-or-seq] (. clojure.lang.Numbers int_array size-or-seq))
  ([size init-val-or-seq] (. clojure.lang.Numbers int_array size init-val-or-seq)))

(defn long-array
  "Creates an array of longs"
  {:inline (fn [& args] `(. clojure.lang.Numbers long_array ~@args))
   :inline-arities #{1 2}
   :added "1.0"}
  ([size-or-seq] (. clojure.lang.Numbers long_array size-or-seq))
  ([size init-val-or-seq] (. clojure.lang.Numbers long_array size init-val-or-seq)))

(definline booleans
  "Casts to boolean[]"
  {:added "1.1"}
  [xs] `(. clojure.lang.Numbers booleans ~xs))

(definline bytes
  "Casts to bytes[]"
  {:added "1.1"}
  [xs] `(. clojure.lang.Numbers bytes ~xs))

(definline chars
  "Casts to chars[]"
  {:added "1.1"}
  [xs] `(. clojure.lang.Numbers chars ~xs))

(definline shorts
  "Casts to shorts[]"
  {:added "1.1"}
  [xs] `(. clojure.lang.Numbers shorts ~xs))

(definline floats
  "Casts to float[]"
  {:added "1.0"}
  [xs] `(. clojure.lang.Numbers floats ~xs))

(definline ints
  "Casts to int[]"
  {:added "1.0"}
  [xs] `(. clojure.lang.Numbers ints ~xs))

(definline doubles
  "Casts to double[]"
  {:added "1.0"}
  [xs] `(. clojure.lang.Numbers doubles ~xs))

(definline longs
  "Casts to long[]"
  {:added "1.0"}
  [xs] `(. clojure.lang.Numbers longs ~xs))

(import '(java.util.concurrent BlockingQueue LinkedBlockingQueue))

(defn seque
  "Creates a queued seq on another (presumably lazy) seq s. The queued
  seq will produce a concrete seq in the background, and can get up to
  n items ahead of the consumer. n-or-q can be an integer n buffer
  size, or an instance of java.util.concurrent BlockingQueue. Note
  that reading from a seque can block if the reader gets ahead of the
  producer."
  {:added "1.0"
   :static true}
  ([s] (seque 100 s))
  ([n-or-q s]
   (let [^BlockingQueue q (if (instance? BlockingQueue n-or-q)
                             n-or-q
                             (LinkedBlockingQueue. (int n-or-q)))
         NIL (Object.) ;nil sentinel since LBQ doesn't support nils
         agt (agent (lazy-seq s)) ; never start with nil; that signifies we've already put eos
         log-error (fn [q e]
                     (if (.offer q q)
                       (throw e)
                       e))
         fill (fn [s]
                (when s
                  (if (instance? Exception s) ; we failed to .offer an error earlier
                    (log-error q s)
                    (try
                      (loop [[x & xs :as s] (seq s)]
                        (if s
                          (if (.offer q (if (nil? x) NIL x))
                            (recur xs)
                            s)
                          (when-not (.offer q q) ; q itself is eos sentinel
                            ()))) ; empty seq, not nil, so we know to put eos next time
                      (catch Exception e
                        (log-error q e))))))
         drain (fn drain []
                 (lazy-seq
                  (let [x (.take q)]
                    (if (identical? x q) ;q itself is eos sentinel
                      (do @agt nil)  ;touch agent just to propagate errors
                      (do
                        (send-off agt fill)
                        (release-pending-sends)
                        (cons (if (identical? x NIL) nil x) (drain)))))))]
     (send-off agt fill)
     (drain))))

(defn class?
  "Returns true if x is an instance of Class"
  {:added "1.0"
   :static true}
  [x] (instance? Class x))

(defn- is-annotation? [c]
  (and (class? c)
       (.isAssignableFrom java.lang.annotation.Annotation c)))

(defn- is-runtime-annotation? [^Class c]
  (boolean 
   (and (is-annotation? c)
        (when-let [^java.lang.annotation.Retention r 
                   (.getAnnotation c java.lang.annotation.Retention)] 
          (= (.value r) java.lang.annotation.RetentionPolicy/RUNTIME)))))

(defn- descriptor [^Class c] (clojure.asm.Type/getDescriptor c))

(declare process-annotation)
(defn- add-annotation [^clojure.asm.AnnotationVisitor av name v]
  (cond
   (vector? v) (let [avec (.visitArray av name)]
                 (doseq [vval v]
                   (add-annotation avec "value" vval))
                 (.visitEnd avec))
   (symbol? v) (let [ev (eval v)]
                 (cond 
                  (instance? java.lang.Enum ev)
                  (.visitEnum av name (descriptor (class ev)) (str ev))
                  (class? ev) (.visit av name (clojure.asm.Type/getType ev))
                  :else (throw (IllegalArgumentException. 
                                (str "Unsupported annotation value: " v " of class " (class ev))))))
   (seq? v) (let [[nested nv] v
                  c (resolve nested)
                  nav (.visitAnnotation av name (descriptor c))]
              (process-annotation nav nv)
              (.visitEnd nav))
   :else (.visit av name v)))

(defn- process-annotation [av v]
  (if (map? v) 
    (doseq [[k v] v]
      (add-annotation av (name k) v))
    (add-annotation av "value" v)))

(defn- add-annotations
  ([visitor m] (add-annotations visitor m nil))
  ([visitor m i]
     (doseq [[k v] m]
       (when (symbol? k)
         (when-let [c (resolve k)]
           (when (is-annotation? c)
                                        ;this is known duck/reflective as no common base of ASM Visitors
             (let [av (if i
                        (.visitParameterAnnotation visitor i (descriptor c) 
                                                   (is-runtime-annotation? c))
                        (.visitAnnotation visitor (descriptor c) 
                                          (is-runtime-annotation? c)))]
               (process-annotation av v)
               (.visitEnd av))))))))

(defn alter-var-root
  "Atomically alters the root binding of var v by applying f to its
  current value plus any args"
  {:added "1.0"
   :static true}
  [^clojure.lang.Var v f & args] (.alterRoot v f args))

(defn bound?
  "Returns true if all of the vars provided as arguments have any bound value, root or thread-local.
   Implies that deref'ing the provided vars will succeed. Returns true if no vars are provided."
  {:added "1.2"
   :static true}
  [& vars]
  (every? #(.isBound ^clojure.lang.Var %) vars))

(defn thread-bound?
  "Returns true if all of the vars provided as arguments have thread-local bindings.
   Implies that set!'ing the provided vars will succeed.  Returns true if no vars are provided."
  {:added "1.2"
   :static true}
  [& vars]
  (every? #(.getThreadBinding ^clojure.lang.Var %) vars))

(defn make-hierarchy
  "Creates a hierarchy object for use with derive, isa? etc."
  {:added "1.0"
   :static true}
  [] {:parents {} :descendants {} :ancestors {}})

(def ^{:private true}
     global-hierarchy (make-hierarchy))

(defn not-empty
  "If coll is empty, returns nil, else coll"
  {:added "1.0"
   :static true}
  [coll] (when (seq coll) coll))

(defn bases
  "Returns the immediate superclass and direct interfaces of c, if any"
  {:added "1.0"
   :static true}
  [^Class c]
  (when c
    (let [i (seq (.getInterfaces c))
          s (.getSuperclass c)]
      (if s (cons s i) i))))

(defn supers
  "Returns the immediate and indirect superclasses and interfaces of c, if any"
  {:added "1.0"
   :static true}
  [^Class class]
  (loop [ret (set (bases class)) cs ret]
    (if (seq cs)
      (let [c (first cs) bs (bases c)]
        (recur (into1 ret bs) (into1 (disj cs c) bs)))
      (not-empty ret))))

(defn isa?
  "Returns true if (= child parent), or child is directly or indirectly derived from
  parent, either via a Java type inheritance relationship or a
  relationship established via derive. h must be a hierarchy obtained
  from make-hierarchy, if not supplied defaults to the global
  hierarchy"
  {:added "1.0"}
  ([child parent] (isa? global-hierarchy child parent))
  ([h child parent]
   (or (= child parent)
       (and (class? parent) (class? child)
            (. ^Class parent isAssignableFrom child))
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
  {:added "1.0"}
  ([tag] (parents global-hierarchy tag))
  ([h tag] (not-empty
            (let [tp (get (:parents h) tag)]
              (if (class? tag)
                (into1 (set (bases tag)) tp)
                tp)))))

(defn ancestors
  "Returns the immediate and indirect parents of tag, either via a Java type
  inheritance relationship or a relationship established via derive. h
  must be a hierarchy obtained from make-hierarchy, if not supplied
  defaults to the global hierarchy"
  {:added "1.0"}
  ([tag] (ancestors global-hierarchy tag))
  ([h tag] (not-empty
            (let [ta (get (:ancestors h) tag)]
              (if (class? tag)
                (let [superclasses (set (supers tag))]
                  (reduce1 into1 superclasses
                    (cons ta
                          (map #(get (:ancestors h) %) superclasses))))
                ta)))))

(defn descendants
  "Returns the immediate and indirect children of tag, through a
  relationship established via derive. h must be a hierarchy obtained
  from make-hierarchy, if not supplied defaults to the global
  hierarchy. Note: does not work on Java type inheritance
  relationships."
  {:added "1.0"}
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
  {:added "1.0"}
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
              (reduce1 (fn [ret k]
                        (assoc ret k
                               (reduce1 conj (get targets k #{}) (cons target (targets target)))))
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

(declare flatten)

(defn underive
  "Removes a parent/child relationship between parent and
  tag. h must be a hierarchy obtained from make-hierarchy, if not
  supplied defaults to, and modifies, the global hierarchy."
  {:added "1.0"}
  ([tag parent] (alter-var-root #'global-hierarchy underive tag parent) nil)
  ([h tag parent]
    (let [parentMap (:parents h)
	  childsParents (if (parentMap tag)
			  (disj (parentMap tag) parent) #{})
	  newParents (if (not-empty childsParents)
		       (assoc parentMap tag childsParents)
		       (dissoc parentMap tag))
	  deriv-seq (flatten (map #(cons (key %) (interpose (key %) (val %)))
				       (seq newParents)))]
      (if (contains? (parentMap tag) parent)
	(reduce1 #(apply derive %1 %2) (make-hierarchy)
		(partition 2 deriv-seq))
	h))))


(defn distinct?
  "Returns true if no two of the arguments are ="
  {:tag Boolean
   :added "1.0"
   :static true}
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

(defn resultset-seq
  "Creates and returns a lazy sequence of structmaps corresponding to
  the rows in the java.sql.ResultSet rs"
  {:added "1.0"}
  [^java.sql.ResultSet rs]
    (let [rsmeta (. rs (getMetaData))
          idxs (range 1 (inc (. rsmeta (getColumnCount))))
          keys (map (comp keyword #(.toLowerCase ^String %))
                    (map (fn [i] (. rsmeta (getColumnLabel i))) idxs))
          check-keys
                (or (apply distinct? keys)
                    (throw (Exception. "ResultSet must have unique column labels")))
          row-struct (apply create-struct keys)
          row-values (fn [] (map (fn [^Integer i] (. rs (getObject i))) idxs))
          rows (fn thisfn []
                 (when (. rs (next))
                   (cons (apply struct row-struct (row-values)) (lazy-seq (thisfn)))))]
      (rows)))

(defn iterator-seq
  "Returns a seq on a java.util.Iterator. Note that most collections
  providing iterators implement Iterable and thus support seq directly.
  Seqs cache values, thus iterator-seq should not be used on any
  iterator that repeatedly returns the same mutable object."
  {:added "1.0"
   :static true}
  [iter]
  (clojure.lang.RT/chunkIteratorSeq iter))

(defn enumeration-seq
  "Returns a seq on a java.util.Enumeration"
  {:added "1.0"
   :static true}
  [e]
  (clojure.lang.EnumerationSeq/create e))

(defn format
  "Formats a string using java.lang.String.format, see java.util.Formatter for format
  string syntax"
  {:added "1.0"
   :static true}
  ^String [fmt & args]
  (String/format fmt (to-array args)))

(defn printf
  "Prints formatted output, as per format"
  {:added "1.0"
   :static true}
  [fmt & args]
  (print (apply format fmt args)))

(declare gen-class)

(defmacro with-loading-context [& body]
  `((fn loading# [] 
        (. clojure.lang.Var (pushThreadBindings {clojure.lang.Compiler/LOADER  
                                                 (.getClassLoader (.getClass ^Object loading#))}))
        (try
         ~@body
         (finally
          (. clojure.lang.Var (popThreadBindings)))))))

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
  default (refer 'clojure.core) is used.  Use of ns is preferred to
  individual calls to in-ns/require/use/import:

  (ns foo.bar
    (:refer-clojure :exclude [ancestors printf])
    (:require (clojure.contrib sql combinatorics))
    (:use (my.lib this that))
    (:import (java.util Date Timer Random)
             (java.sql Connection Statement)))"
  {:arglists '([name docstring? attr-map? references*])
   :added "1.0"}
  [name & references]
  (let [process-reference
        (fn [[kname & args]]
          `(~(symbol "clojure.core" (clojure.core/name kname))
             ~@(map #(list 'quote %) args)))
        docstring  (when (string? (first references)) (first references))
        references (if docstring (next references) references)
        name (if docstring
               (vary-meta name assoc :doc docstring)
               name)
        metadata   (when (map? (first references)) (first references))
        references (if metadata (next references) references)
        name (if metadata
               (vary-meta name merge metadata)
               name)
        gen-class-clause (first (filter #(= :gen-class (first %)) references))
        gen-class-call
          (when gen-class-clause
            (list* `gen-class :name (.replace (str name) \- \_) :impl-ns name :main true (next gen-class-clause)))
        references (remove #(= :gen-class (first %)) references)
        ;ns-effect (clojure.core/in-ns name)
        name-metadata (meta name)]
    `(do
       (clojure.core/in-ns '~name)
       ~@(when name-metadata
           `((.resetMeta (clojure.lang.Namespace/find '~name) ~name-metadata)))
       (with-loading-context
        ~@(when gen-class-call (list gen-class-call))
        ~@(when (and (not= name 'clojure.core) (not-any? #(= :refer-clojure (first %)) references))
            `((clojure.core/refer '~'clojure.core)))
        ~@(map process-reference references))
        (if (.equals '~name 'clojure.core) 
          nil
          (do (dosync (commute @#'*loaded-libs* conj '~name)) nil)))))

(defmacro refer-clojure
  "Same as (refer 'clojure.core <filters>)"
  {:added "1.0"}
  [& filters]
  `(clojure.core/refer '~'clojure.core ~@filters))

(defmacro defonce
  "defs name to have the root value of the expr iff the named var has no root value,
  else expr is unevaluated"
  {:added "1.0"}
  [name expr]
  `(let [v# (def ~name)]
     (when-not (.hasRoot v#)
       (def ~name ~expr))))

;;;;;;;;;;; require/use/load, contributed by Stephen C. Gilardi ;;;;;;;;;;;;;;;;;;

(defonce ^:dynamic
  ^{:private true
     :doc "A ref to a sorted set of symbols representing loaded libs"}
  *loaded-libs* (ref (sorted-set)))

(defonce ^:dynamic
  ^{:private true
     :doc "A stack of paths currently being loaded by this thread"}
  *pending-paths* ())

(defonce ^:dynamic
  ^{:private true :doc
     "True while a verbose load is pending"}
  *loading-verbosely* false)

(defn- throw-if
  "Throws a CompilerException with a message if pred is true"
  [pred fmt & args]
  (when pred
    (let [^String message (apply format fmt args)
          exception (Exception. message)
          raw-trace (.getStackTrace exception)
          boring? #(not= (.getMethodName ^StackTraceElement %) "doInvoke")
          trace (into-array (drop 2 (drop-while boring? raw-trace)))]
      (.setStackTrace exception trace)
      (throw (clojure.lang.Compiler$CompilerException.
              *file*
              (.deref clojure.lang.Compiler/LINE)
              (.deref clojure.lang.Compiler/COLUMN)
              exception)))))

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

(def ^:declared ^:redef load)

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
   (commute *loaded-libs* #(reduce1 conj %1 %2)
            (binding [*loaded-libs* (ref (sorted-set))]
              (load-one lib need-ns require)
              @*loaded-libs*))))

(defn- load-lib
  "Loads a lib with options"
  [prefix lib & options]
  (throw-if (and prefix (pos? (.indexOf (name lib) (int \.))))
            "Found lib name '%s' containing period with prefix '%s'.  lib names inside prefix lists must not contain periods"
            (name lib) prefix)
  (let [lib (if prefix (symbol (str prefix \. lib)) lib)
        opts (apply hash-map options)
        {:keys [as reload reload-all require use verbose]} opts
        loaded (contains? @*loaded-libs* lib)
        load (cond reload-all
                   load-all
                   (or reload (not require) (not loaded))
                   load-one)
        need-ns (or as use)
        filter-opts (select-keys opts '(:exclude :only :rename :refer))
        undefined-on-entry (not (find-ns lib))]
    (binding [*loading-verbosely* (or *loading-verbosely* verbose)]
      (if load
        (try
          (load lib need-ns require)
          (catch Exception e
            (when undefined-on-entry
              (remove-ns lib))
            (throw e)))
        (throw-if (and need-ns (not (find-ns lib)))
                  "namespace '%s' not found" lib))
      (when (and need-ns *loading-verbosely*)
        (printf "(clojure.core/in-ns '%s)\n" (ns-name *ns*)))
      (when as
        (when *loading-verbosely*
          (printf "(clojure.core/alias '%s '%s)\n" as lib))
        (alias as lib))
      (when (or use (:refer filter-opts))
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
    ; check for unsupported options
    (let [supported #{:as :reload :reload-all :require :use :verbose :refer}
          unsupported (seq (remove supported flags))]
      (throw-if unsupported
                (apply str "Unsupported option(s) supplied: "
                     (interpose \, unsupported))))
    ; check a load target was specified
    (throw-if (not (seq args)) "Nothing specified to load")
    (doseq [arg args]
      (if (libspec? arg)
        (apply load-lib nil (prependss arg opts))
        (let [[prefix & args] arg]
          (throw-if (nil? prefix) "prefix cannot be nil")
          (doseq [arg args]
            (apply load-lib prefix (prependss arg opts))))))))

(defn- check-cyclic-dependency
  "Detects and rejects non-trivial cyclic load dependencies. The
  exception message shows the dependency chain with the cycle
  highlighted. Ignores the trivial case of a file attempting to load
  itself because that can occur when a gen-class'd class loads its
  implementation."
  [path]
  (when (some #{path} (rest *pending-paths*))
    (let [pending (map #(if (= % path) (str "[ " % " ]") %)
                       (cons path *pending-paths*))
          chain (apply str (interpose "->" pending))]
      (throw-if true "Cyclic load dependency: %s" chain))))

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
  is derived from the lib name in the following manner:
  Consider a lib named by the symbol 'x.y.z; it has the root directory
  <classpath>/x/y/, and its root resource is <classpath>/x/y/z.clj. The root
  resource should contain code to create the lib's namespace (usually by using
  the ns macro) and load any additional lib resources.

  Libspecs

  A libspec is a lib name or a vector containing a lib name followed by
  options expressed as sequential keywords and arguments.

  Recognized options:
  :as takes a symbol as its argument and makes that symbol an alias to the
    lib's namespace in the current namespace.
  :refer takes a list of symbols to refer from the namespace or the :all
    keyword to bring in all public vars.

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
  :verbose triggers printing information about each load, alias, and refer

  Example:

  The following would load the libraries clojure.zip and clojure.set
  abbreviated as 's'.

  (require '(clojure zip [set :as s]))"
  {:added "1.0"}

  [& args]
  (apply load-libs :require args))

(defn use
  "Like 'require, but also refers to each lib's namespace using
  clojure.core/refer. Use :use in the ns macro in preference to calling
  this directly.

  'use accepts additional options in libspecs: :exclude, :only, :rename.
  The arguments and semantics for :exclude, :only, and :rename are the same
  as those documented for clojure.core/refer."
  {:added "1.0"}
  [& args] (apply load-libs :require :use args))

(defn loaded-libs
  "Returns a sorted set of symbols naming the currently loaded libs"
  {:added "1.0"}
  [] @*loaded-libs*)

(defn load
  "Loads Clojure code from resources in classpath. A path is interpreted as
  classpath-relative if it begins with a slash or relative to the root
  directory for the current namespace otherwise."
  {:redef true
   :added "1.0"}
  [& paths]
  (doseq [^String path paths]
    (let [^String path (if (.startsWith path "/")
                          path
                          (str (root-directory (ns-name *ns*)) \/ path))]
      (when *loading-verbosely*
        (printf "(clojure.core/load \"%s\")\n" path)
        (flush))
      (check-cyclic-dependency path)
      (when-not (= path (first *pending-paths*))
        (binding [*pending-paths* (conj *pending-paths* path)]
          (clojure.lang.RT/load (.substring path 1)))))))

(defn compile
  "Compiles the namespace named by the symbol lib into a set of
  classfiles. The source for the lib must be in a proper
  classpath-relative directory. The output files will go into the
  directory specified by *compile-path*, and that directory too must
  be in the classpath."
  {:added "1.0"}
  [lib]
  (binding [*compile-files* true]
    (load-one lib true true))
  lib)

;;;;;;;;;;;;; nested associative ops ;;;;;;;;;;;

(defn get-in
  "Returns the value in a nested associative structure,
  where ks is a sequence of keys. Returns nil if the key
  is not present, or the not-found value if supplied."
  {:added "1.2"
   :static true}
  ([m ks]
     (reduce1 get m ks))
  ([m ks not-found]
     (loop [sentinel (Object.)
            m m
            ks (seq ks)]
       (if ks
         (let [m (get m (first ks) sentinel)]
           (if (identical? sentinel m)
             not-found
             (recur sentinel m (next ks))))
         m))))

(defn assoc-in
  "Associates a value in a nested associative structure, where ks is a
  sequence of keys and v is the new value and returns a new nested structure.
  If any levels do not exist, hash-maps will be created."
  {:added "1.0"
   :static true}
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
  {:added "1.0"
   :static true}
  ([m [k & ks] f & args]
   (if ks
     (assoc m k (apply update-in (get m k) ks f args))
     (assoc m k (apply f (get m k) args)))))

(defn update
  "'Updates' a value in an associative structure, where k is a
  key and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  structure.  If the key does not exist, nil is passed as the old value."
  {:added "1.7"
   :static true}
  ([m k f]
   (assoc m k (f (get m k))))
  ([m k f x]
   (assoc m k (f (get m k) x)))
  ([m k f x y]
   (assoc m k (f (get m k) x y)))
  ([m k f x y z]
   (assoc m k (f (get m k) x y z)))
  ([m k f x y z & more]
   (assoc m k (apply f (get m k) x y z more))))

(defn empty?
  "Returns true if coll has no items - same as (not (seq coll)).
  Please use the idiom (seq x) rather than (not (empty? x))"
  {:added "1.0"
   :static true}
  [coll] (not (seq coll)))

(defn coll?
  "Returns true if x implements IPersistentCollection"
  {:added "1.0"
   :static true}
  [x] (instance? clojure.lang.IPersistentCollection x))

(defn list?
  "Returns true if x implements IPersistentList"
  {:added "1.0"
   :static true}
  [x] (instance? clojure.lang.IPersistentList x))

(defn ifn?
  "Returns true if x implements IFn. Note that many data structures
  (e.g. sets and maps) implement IFn"
  {:added "1.0"
   :static true}
  [x] (instance? clojure.lang.IFn x))

(defn fn?
  "Returns true if x implements Fn, i.e. is an object created via fn."
  {:added "1.0"
   :static true}
  [x] (instance? clojure.lang.Fn x))


(defn associative?
 "Returns true if coll implements Associative"
 {:added "1.0"
  :static true}
  [coll] (instance? clojure.lang.Associative coll))

(defn sequential?
 "Returns true if coll implements Sequential"
 {:added "1.0"
  :static true}
  [coll] (instance? clojure.lang.Sequential coll))

(defn sorted?
 "Returns true if coll implements Sorted"
 {:added "1.0"
   :static true}
  [coll] (instance? clojure.lang.Sorted coll))

(defn counted?
 "Returns true if coll implements count in constant time"
 {:added "1.0"
   :static true}
  [coll] (instance? clojure.lang.Counted coll))

(defn reversible?
 "Returns true if coll implements Reversible"
 {:added "1.0"
   :static true}
  [coll] (instance? clojure.lang.Reversible coll))

(def ^:dynamic
 ^{:doc "bound in a repl thread to the most recent value printed"
   :added "1.0"}
 *1)

(def ^:dynamic
 ^{:doc "bound in a repl thread to the second most recent value printed"
   :added "1.0"}
 *2)

(def ^:dynamic
 ^{:doc "bound in a repl thread to the third most recent value printed"
   :added "1.0"}
 *3)

(def ^:dynamic
 ^{:doc "bound in a repl thread to the most recent exception caught by the repl"
   :added "1.0"}
 *e)

(defn trampoline
  "trampoline can be used to convert algorithms requiring mutual
  recursion without stack consumption. Calls f with supplied args, if
  any. If f returns a fn, calls that fn with no arguments, and
  continues to repeat, until the return value is not a fn, then
  returns that non-fn value. Note that if you want to return a fn as a
  final value, you must wrap it in some data structure and unpack it
  after trampoline returns."
  {:added "1.0"
   :static true}
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
  {:added "1.0"
   :static true}
  ([ns ^clojure.lang.Symbol name]
     (let [v (clojure.lang.Var/intern (the-ns ns) name)]
       (when (meta name) (.setMeta v (meta name)))
       v))
  ([ns name val]
     (let [v (clojure.lang.Var/intern (the-ns ns) name val)]
       (when (meta name) (.setMeta v (meta name)))
       v)))

(defmacro while
  "Repeatedly executes body while test expression is true. Presumes
  some side-effect will cause test to become false/nil. Returns nil"
  {:added "1.0"}
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
  {:added "1.0"
   :static true}
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
  {:added "1.0"}

  [pred expr & clauses]
  (let [gpred (gensym "pred__")
        gexpr (gensym "expr__")
        emit (fn emit [pred expr args]
               (let [[[a b c :as clause] more]
                       (split-at (if (= :>> (second args)) 3 2) args)
                       n (count clause)]
                 (cond
                  (= 0 n) `(throw (IllegalArgumentException. (str "No matching clause: " ~expr)))
                  (= 1 n) a
                  (= 2 n) `(if (~pred ~a ~expr)
                             ~b
                             ~(emit pred expr more))
                  :else `(if-let [p# (~pred ~a ~expr)]
                           (~c p#)
                           ~(emit pred expr more)))))]
    `(let [~gpred ~pred
           ~gexpr ~expr]
       ~(emit gpred gexpr clauses))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; var documentation ;;;;;;;;;;;;;;;;;;;;;;;;;;

(alter-meta! #'*agent* assoc :added "1.0")
(alter-meta! #'in-ns assoc :added "1.0")
(alter-meta! #'load-file assoc :added "1.0")

(defmacro add-doc-and-meta {:private true} [name docstring meta]
  `(alter-meta! (var ~name) merge (assoc ~meta :doc ~docstring)))

(add-doc-and-meta *file*
  "The path of the file being evaluated, as a String.

  When there is no file, e.g. in the REPL, the value is not defined."
  {:added "1.0"})

(add-doc-and-meta *command-line-args*
  "A sequence of the supplied command line arguments, or nil if
  none were supplied"
  {:added "1.0"})

(add-doc-and-meta *warn-on-reflection*
  "When set to true, the compiler will emit warnings when reflection is
  needed to resolve Java method calls or field accesses.

  Defaults to false."
  {:added "1.0"})

(add-doc-and-meta *compile-path*
  "Specifies the directory where 'compile' will write out .class
  files. This directory must be in the classpath for 'compile' to
  work.

  Defaults to \"classes\""
  {:added "1.0"})

(add-doc-and-meta *compile-files*
  "Set to true when compiling files, false otherwise."
  {:added "1.0"})

(add-doc-and-meta *unchecked-math*
  "While bound to true, compilations of +, -, *, inc, dec and the
  coercions will be done without overflow checks. While bound
  to :warn-on-boxed, same behavior as true, and a warning is emitted
  when compilation uses boxed math. Default: false."
  {:added "1.3"})

(add-doc-and-meta *compiler-options*
  "A map of keys to options.
  Note, when binding dynamically make sure to merge with previous value.
  Supported options:
  :elide-meta - a collection of metadata keys to elide during compilation.
  :disable-locals-clearing - set to true to disable clearing, useful for using a debugger
  Alpha, subject to change."
  {:added "1.4"})

(add-doc-and-meta *ns*
  "A clojure.lang.Namespace object representing the current namespace."
  {:added "1.0"})

(add-doc-and-meta *in*
  "A java.io.Reader object representing standard input for read operations.

  Defaults to System/in, wrapped in a LineNumberingPushbackReader"
  {:added "1.0"})

(add-doc-and-meta *out*
  "A java.io.Writer object representing standard output for print operations.

  Defaults to System/out, wrapped in an OutputStreamWriter"
  {:added "1.0"})

(add-doc-and-meta *err*
  "A java.io.Writer object representing standard error for print operations.

  Defaults to System/err, wrapped in a PrintWriter"
  {:added "1.0"})

(add-doc-and-meta *flush-on-newline*
  "When set to true, output will be flushed whenever a newline is printed.

  Defaults to true."
  {:added "1.0"})

(add-doc-and-meta *print-meta*
  "If set to logical true, when printing an object, its metadata will also
  be printed in a form that can be read back by the reader.

  Defaults to false."
  {:added "1.0"})

(add-doc-and-meta *print-dup*
  "When set to logical true, objects will be printed in a way that preserves
  their type when read in later.

  Defaults to false."
  {:added "1.0"})

(add-doc-and-meta *print-readably*
  "When set to logical false, strings and characters will be printed with
  non-alphanumeric characters converted to the appropriate escape sequences.

  Defaults to true"
  {:added "1.0"})

(add-doc-and-meta *read-eval*
 "Defaults to true (or value specified by system property, see below)
  ***This setting implies that the full power of the reader is in play,
  including syntax that can cause code to execute. It should never be
  used with untrusted sources. See also: clojure.edn/read.***

  When set to logical false in the thread-local binding,
  the eval reader (#=) and record/type literal syntax are disabled in read/load.
  Example (will fail): (binding [*read-eval* false] (read-string \"#=(* 2 21)\"))

  The default binding can be controlled by the system property
  'clojure.read.eval' System properties can be set on the command line
  like this:

  java -Dclojure.read.eval=false ...

  The system property can also be set to 'unknown' via
  -Dclojure.read.eval=unknown, in which case the default binding
  is :unknown and all reads will fail in contexts where *read-eval*
  has not been explicitly bound to either true or false. This setting
  can be a useful diagnostic tool to ensure that all of your reads
  occur in considered contexts. You can also accomplish this in a
  particular scope by binding *read-eval* to :unknown
  "
  {:added "1.0"})

(defn future?
  "Returns true if x is a future"
  {:added "1.1"
   :static true}
  [x] (instance? java.util.concurrent.Future x))

(defn future-done?
  "Returns true if future f is done"
  {:added "1.1"
   :static true}
  [^java.util.concurrent.Future f] (.isDone f))


(defmacro letfn 
  "fnspec ==> (fname [params*] exprs) or (fname ([params*] exprs)+)

  Takes a vector of function specs and a body, and generates a set of
  bindings of functions to their names. All of the names are available
  in all of the definitions of the functions, as well as the body."
  {:added "1.0", :forms '[(letfn [fnspecs*] exprs*)],
   :special-form true, :url nil}
  [fnspecs & body] 
  `(letfn* ~(vec (interleave (map first fnspecs) 
                             (map #(cons `fn %) fnspecs)))
           ~@body))

(defn fnil
  "Takes a function f, and returns a function that calls f, replacing
  a nil first argument to f with the supplied value x. Higher arity
  versions can replace arguments in the second and third
  positions (y, z). Note that the function f can take any number of
  arguments, not just the one(s) being nil-patched."
  {:added "1.2"
   :static true}
  ([f x]
   (fn
     ([a] (f (if (nil? a) x a)))
     ([a b] (f (if (nil? a) x a) b))
     ([a b c] (f (if (nil? a) x a) b c))
     ([a b c & ds] (apply f (if (nil? a) x a) b c ds))))
  ([f x y]
   (fn
     ([a b] (f (if (nil? a) x a) (if (nil? b) y b)))
     ([a b c] (f (if (nil? a) x a) (if (nil? b) y b) c))
     ([a b c & ds] (apply f (if (nil? a) x a) (if (nil? b) y b) c ds))))
  ([f x y z]
   (fn
     ([a b] (f (if (nil? a) x a) (if (nil? b) y b)))
     ([a b c] (f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c)))
     ([a b c & ds] (apply f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c) ds)))))


;;;;;;; case ;;;;;;;;;;;;;
(defn- shift-mask [shift mask x]
  (-> x (bit-shift-right shift) (bit-and mask)))

(def ^:private max-mask-bits 13)
(def ^:private max-switch-table-size (bit-shift-left 1 max-mask-bits))

(defn- maybe-min-hash
  "takes a collection of hashes and returns [shift mask] or nil if none found"
  [hashes]
  (first
    (filter (fn [[s m]]
              (apply distinct? (map #(shift-mask s m %) hashes)))
            (for [mask (map #(dec (bit-shift-left 1 %)) (range 1 (inc max-mask-bits)))
                  shift (range 0 31)]
              [shift mask]))))

(defn- case-map
  "Transforms a sequence of test constants and a corresponding sequence of then
  expressions into a sorted map to be consumed by case*. The form of the map
  entries are {(case-f test) [(test-f test) then]}."
  [case-f test-f tests thens]
  (into1 (sorted-map)
    (zipmap (map case-f tests)
            (map vector
              (map test-f tests)
              thens))))

(defn- fits-table?
  "Returns true if the collection of ints can fit within the
  max-table-switch-size, false otherwise."
  [ints]
  (< (- (apply max (seq ints)) (apply min (seq ints))) max-switch-table-size))

(defn- prep-ints
  "Takes a sequence of int-sized test constants and a corresponding sequence of
  then expressions. Returns a tuple of [shift mask case-map switch-type] where
  case-map is a map of int case values to [test then] tuples, and switch-type
  is either :sparse or :compact."
  [tests thens]
  (if (fits-table? tests)
    ; compact case ints, no shift-mask
    [0 0 (case-map int int tests thens) :compact]
    (let [[shift mask] (or (maybe-min-hash (map int tests)) [0 0])]
      (if (zero? mask)
        ; sparse case ints, no shift-mask
        [0 0 (case-map int int tests thens) :sparse]
        ; compact case ints, with shift-mask
        [shift mask (case-map #(shift-mask shift mask (int %)) int tests thens) :compact]))))

(defn- merge-hash-collisions
  "Takes a case expression, default expression, and a sequence of test constants
  and a corresponding sequence of then expressions. Returns a tuple of
  [tests thens skip-check-set] where no tests have the same hash. Each set of
  input test constants with the same hash is replaced with a single test
  constant (the case int), and their respective thens are combined into:
  (condp = expr
    test-1 then-1
    ...
    test-n then-n
    default).
  The skip-check is a set of case ints for which post-switch equivalence
  checking must not be done (the cases holding the above condp thens)."
  [expr-sym default tests thens]
  (let [buckets (loop [m {} ks tests vs thens]
                  (if (and ks vs)
                    (recur
                      (update m (clojure.lang.Util/hash (first ks)) (fnil conj []) [(first ks) (first vs)])
                      (next ks) (next vs))
                    m))
        assoc-multi (fn [m h bucket]
                      (let [testexprs (apply concat bucket)
                            expr `(condp = ~expr-sym ~@testexprs ~default)]
                        (assoc m h expr)))
        hmap (reduce1
               (fn [m [h bucket]]
                 (if (== 1 (count bucket))
                   (assoc m (ffirst bucket) (second (first bucket)))
                   (assoc-multi m h bucket)))
               {} buckets)
        skip-check (->> buckets
                     (filter #(< 1 (count (second %))))
                     (map first)
                     (into1 #{}))]
    [(keys hmap) (vals hmap) skip-check]))

(defn- prep-hashes
  "Takes a sequence of test constants and a corresponding sequence of then
  expressions. Returns a tuple of [shift mask case-map switch-type skip-check]
  where case-map is a map of int case values to [test then] tuples, switch-type
  is either :sparse or :compact, and skip-check is a set of case ints for which
  post-switch equivalence checking must not be done (occurs with hash
  collisions)."
  [expr-sym default tests thens]
  (let [hashcode #(clojure.lang.Util/hash %)
        hashes (into1 #{} (map hashcode tests))]
    (if (== (count tests) (count hashes))
      (if (fits-table? hashes)
        ; compact case ints, no shift-mask
        [0 0 (case-map hashcode identity tests thens) :compact]
        (let [[shift mask] (or (maybe-min-hash hashes) [0 0])]
          (if (zero? mask)
            ; sparse case ints, no shift-mask
            [0 0 (case-map hashcode identity tests thens) :sparse]
            ; compact case ints, with shift-mask
            [shift mask (case-map #(shift-mask shift mask (hashcode %)) identity tests thens) :compact])))
      ; resolve hash collisions and try again
      (let [[tests thens skip-check] (merge-hash-collisions expr-sym default tests thens)
            [shift mask case-map switch-type] (prep-hashes expr-sym default tests thens)
            skip-check (if (zero? mask)
                         skip-check
                         (into1 #{} (map #(shift-mask shift mask %) skip-check)))]
        [shift mask case-map switch-type skip-check]))))


(defmacro case 
  "Takes an expression, and a set of clauses.

  Each clause can take the form of either:

  test-constant result-expr

  (test-constant1 ... test-constantN)  result-expr

  The test-constants are not evaluated. They must be compile-time
  literals, and need not be quoted.  If the expression is equal to a
  test-constant, the corresponding result-expr is returned. A single
  default expression can follow the clauses, and its value will be
  returned if no clause matches. If no default expression is provided
  and no clause matches, an IllegalArgumentException is thrown.

  Unlike cond and condp, case does a constant-time dispatch, the
  clauses are not considered sequentially.  All manner of constant
  expressions are acceptable in case, including numbers, strings,
  symbols, keywords, and (Clojure) composites thereof. Note that since
  lists are used to group multiple constants that map to the same
  expression, a vector can be used to match a list if needed. The
  test-constants need not be all of the same type."
  {:added "1.2"}

  [e & clauses]
  (let [ge (with-meta (gensym) {:tag Object})
        default (if (odd? (count clauses)) 
                  (last clauses)
                  `(throw (IllegalArgumentException. (str "No matching clause: " ~ge))))]
    (if (> 2 (count clauses))
      `(let [~ge ~e] ~default)
      (let [pairs (partition 2 clauses)
            assoc-test (fn assoc-test [m test expr]
                         (if (contains? m test)
                           (throw (IllegalArgumentException. (str "Duplicate case test constant: " test)))
                           (assoc m test expr)))
            pairs (reduce1
                       (fn [m [test expr]]
                         (if (seq? test)
                           (reduce1 #(assoc-test %1 %2 expr) m test)
                           (assoc-test m test expr)))
                       {} pairs)
            tests (keys pairs)
            thens (vals pairs)
            mode (cond
                   (every? #(and (integer? %) (<= Integer/MIN_VALUE % Integer/MAX_VALUE)) tests)
                   :ints
                   (every? keyword? tests)
                   :identity
                   :else :hashes)]
        (condp = mode
          :ints
          (let [[shift mask imap switch-type] (prep-ints tests thens)]
            `(let [~ge ~e] (case* ~ge ~shift ~mask ~default ~imap ~switch-type :int)))
          :hashes
          (let [[shift mask imap switch-type skip-check] (prep-hashes ge default tests thens)]
            `(let [~ge ~e] (case* ~ge ~shift ~mask ~default ~imap ~switch-type :hash-equiv ~skip-check)))
          :identity
          (let [[shift mask imap switch-type skip-check] (prep-hashes ge default tests thens)]
            `(let [~ge ~e] (case* ~ge ~shift ~mask ~default ~imap ~switch-type :hash-identity ~skip-check))))))))


;; redefine reduce with internal-reduce

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; helper files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(alter-meta! (find-ns 'clojure.core) assoc :doc "Fundamental library of the Clojure language")
(load "core_proxy")
(load "core_print")
(load "genclass")
(load "core_deftype")
(load "core/protocols")
(load "gvec")
(load "instant")
(load "uuid")

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
  {:added "1.0"}
  ([f coll]
     (if (instance? clojure.lang.IReduce coll)
       (.reduce ^clojure.lang.IReduce coll f)
       (clojure.core.protocols/coll-reduce coll f)))
  ([f val coll]
     (if (instance? clojure.lang.IReduceInit coll)
       (.reduce ^clojure.lang.IReduceInit coll f val)
       (clojure.core.protocols/coll-reduce coll f val))))

(extend-protocol clojure.core.protocols/IKVReduce
 nil
 (kv-reduce
  [_ f init]
  init)

 ;;slow path default
 clojure.lang.IPersistentMap
 (kv-reduce 
  [amap f init]
  (reduce (fn [ret [k v]] (f ret k v)) init amap))

 clojure.lang.IKVReduce
 (kv-reduce 
  [amap f init]
  (.kvreduce amap f init)))

(defn reduce-kv
  "Reduces an associative collection. f should be a function of 3
  arguments. Returns the result of applying f to init, the first key
  and the first value in coll, then applying f to that result and the
  2nd key and value, etc. If coll contains no entries, returns init
  and f is not called. Note that reduce-kv is supported on vectors,
  where the keys will be the ordinals."  
  {:added "1.4"}
  ([f init coll]
     (clojure.core.protocols/kv-reduce coll f init)))

(defn completing
  "Takes a reducing function f of 2 args and returns a fn suitable for
  transduce by adding an arity-1 signature that calls cf (default -
  identity) on the result argument."
  {:added "1.7"}
  ([f] (completing f identity))
  ([f cf]
     (fn
       ([] (f))
       ([x] (cf x))
       ([x y] (f x y)))))

(defn transduce
  "reduce with a transformation of f (xf). If init is not
  supplied, (f) will be called to produce it. f should be a reducing
  step function that accepts both 1 and 2 arguments, if it accepts
  only 2 you can add the arity-1 with 'completing'. Returns the result
  of applying (the transformed) xf to init and the first item in coll,
  then applying xf to that result and the 2nd item, etc. If coll
  contains no items, returns init and f is not called. Note that
  certain transforms may inject or skip items."  {:added "1.7"}
  ([xform f coll] (transduce xform f (f) coll))
  ([xform f init coll]
     (let [f (xform f)
           ret (if (instance? clojure.lang.IReduceInit coll)
                 (.reduce ^clojure.lang.IReduceInit coll f init)
                 (clojure.core.protocols/coll-reduce coll f init))]
       (f ret))))

(defn into
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined. A transducer may be supplied."
  {:added "1.0"
   :static true}
  ([to from]
     (if (instance? clojure.lang.IEditableCollection to)
       (with-meta (persistent! (reduce conj! (transient to) from)) (meta to))
       (reduce conj to from)))
  ([to xform from]
     (if (instance? clojure.lang.IEditableCollection to)
       (with-meta (persistent! (transduce xform conj! (transient to) from)) (meta to))
       (transduce xform conj to from))))

(defn mapv
  "Returns a vector consisting of the result of applying f to the
  set of first items of each coll, followed by applying f to the set
  of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments."
  {:added "1.4"
   :static true}
  ([f coll]
     (-> (reduce (fn [v o] (conj! v (f o))) (transient []) coll)
         persistent!))
  ([f c1 c2]
     (into [] (map f c1 c2)))
  ([f c1 c2 c3]
     (into [] (map f c1 c2 c3)))
  ([f c1 c2 c3 & colls]
     (into [] (apply map f c1 c2 c3 colls))))

(defn filterv
  "Returns a vector of the items in coll for which
  (pred item) returns true. pred must be free of side-effects."
  {:added "1.4"
   :static true}
  [pred coll]
  (-> (reduce (fn [v o] (if (pred o) (conj! v o) v))
              (transient [])
              coll)
      persistent!))

(require '[clojure.java.io :as jio])

(defn- normalize-slurp-opts
  [opts]
  (if (string? (first opts))
    (do
      (println "WARNING: (slurp f enc) is deprecated, use (slurp f :encoding enc).")
      [:encoding (first opts)])
    opts))

(defn slurp
  "Opens a reader on f and reads all its contents, returning a string.
  See clojure.java.io/reader for a complete list of supported arguments."
  {:added "1.0"}
  ([f & opts]
     (let [opts (normalize-slurp-opts opts)
           sw (java.io.StringWriter.)]
       (with-open [^java.io.Reader r (apply jio/reader f opts)]
         (jio/copy r sw)
         (.toString sw)))))

(defn spit
  "Opposite of slurp.  Opens f with writer, writes content, then
  closes f. Options passed to clojure.java.io/writer."
  {:added "1.2"}
  [f content & options]
  (with-open [^java.io.Writer w (apply jio/writer f options)]
    (.write w (str content))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; futures (needs proxy);;;;;;;;;;;;;;;;;;
(defn future-call 
  "Takes a function of no args and yields a future object that will
  invoke the function in another thread, and will cache the result and
  return it on all subsequent calls to deref/@. If the computation has
  not yet finished, calls to deref/@ will block, unless the variant
  of deref with timeout is used. See also - realized?."
  {:added "1.1"
   :static true}
  [f]
  (let [f (binding-conveyor-fn f)
        fut (.submit clojure.lang.Agent/soloExecutor ^Callable f)]
    (reify 
     clojure.lang.IDeref 
     (deref [_] (deref-future fut))
     clojure.lang.IBlockingDeref
     (deref
      [_ timeout-ms timeout-val]
      (deref-future fut timeout-ms timeout-val))
     clojure.lang.IPending
     (isRealized [_] (.isDone fut))
     java.util.concurrent.Future
      (get [_] (.get fut))
      (get [_ timeout unit] (.get fut timeout unit))
      (isCancelled [_] (.isCancelled fut))
      (isDone [_] (.isDone fut))
      (cancel [_ interrupt?] (.cancel fut interrupt?)))))
  
(defmacro future
  "Takes a body of expressions and yields a future object that will
  invoke the body in another thread, and will cache the result and
  return it on all subsequent calls to deref/@. If the computation has
  not yet finished, calls to deref/@ will block, unless the variant of
  deref with timeout is used. See also - realized?."
  {:added "1.1"}
  [& body] `(future-call (^{:once true} fn* [] ~@body)))


(defn future-cancel
  "Cancels the future, if possible."
  {:added "1.1"
   :static true}
  [^java.util.concurrent.Future f] (.cancel f true))

(defn future-cancelled?
  "Returns true if future f is cancelled"
  {:added "1.1"
   :static true}
  [^java.util.concurrent.Future f] (.isCancelled f))

(defn pmap
  "Like map, except f is applied in parallel. Semi-lazy in that the
  parallel computation stays ahead of the consumption, but doesn't
  realize the entire result unless required. Only useful for
  computationally intensive functions where the time of f dominates
  the coordination overhead."
  {:added "1.0"
   :static true}
  ([f coll]
   (let [n (+ 2 (.. Runtime getRuntime availableProcessors))
         rets (map #(future (f %)) coll)
         step (fn step [[x & xs :as vs] fs]
                (lazy-seq
                 (if-let [s (seq fs)]
                   (cons (deref x) (step xs (rest s)))
                   (map deref vs))))]
     (step rets (drop n rets))))
  ([f coll & colls]
   (let [step (fn step [cs]
                (lazy-seq
                 (let [ss (map seq cs)]
                   (when (every? identity ss)
                     (cons (map first ss) (step (map rest ss)))))))]
     (pmap #(apply f %) (step (cons coll colls))))))

(defn pcalls
  "Executes the no-arg fns in parallel, returning a lazy sequence of
  their values"
  {:added "1.0"
   :static true}
  [& fns] (pmap #(%) fns))

(defmacro pvalues
  "Returns a lazy sequence of the values of the exprs, which are
  evaluated in parallel"
  {:added "1.0"
   :static true}
  [& exprs]
  `(pcalls ~@(map #(list `fn [] %) exprs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; clojure version number ;;;;;;;;;;;;;;;;;;;;;;

(let [properties (with-open [version-stream (.getResourceAsStream
                                             (clojure.lang.RT/baseLoader)
                                             "clojure/version.properties")]
                   (doto (new java.util.Properties)
                     (.load version-stream)))
      version-string (.getProperty properties "version")
      [_ major minor incremental qualifier snapshot]
      (re-matches
       #"(\d+)\.(\d+)\.(\d+)(?:-([a-zA-Z0-9_]+))?(?:-(SNAPSHOT))?"
       version-string)
      clojure-version {:major       (Integer/valueOf ^String major)
                       :minor       (Integer/valueOf ^String minor)
                       :incremental (Integer/valueOf ^String incremental)
                       :qualifier   (if (= qualifier "SNAPSHOT") nil qualifier)}]
  (def ^:dynamic *clojure-version*
    (if (.contains version-string "SNAPSHOT")
      (clojure.lang.RT/assoc clojure-version :interim true)
      clojure-version)))

(add-doc-and-meta *clojure-version*
  "The version info for Clojure core, as a map containing :major :minor 
  :incremental and :qualifier keys. Feature releases may increment 
  :minor and/or :major, bugfix releases will increment :incremental. 
  Possible values of :qualifier include \"GA\", \"SNAPSHOT\", \"RC-x\" \"BETA-x\""
  {:added "1.0"})

(defn
  clojure-version 
  "Returns clojure version as a printable string."
  {:added "1.0"}
  []
  (str (:major *clojure-version*)
       "."
       (:minor *clojure-version*)
       (when-let [i (:incremental *clojure-version*)]
         (str "." i))
       (when-let [q (:qualifier *clojure-version*)]
         (when (pos? (count q)) (str "-" q)))
       (when (:interim *clojure-version*)
         "-SNAPSHOT")))

(defn promise
  "Returns a promise object that can be read with deref/@, and set,
  once only, with deliver. Calls to deref/@ prior to delivery will
  block, unless the variant of deref with timeout is used. All
  subsequent derefs will return the same delivered value without
  blocking. See also - realized?."
  {:added "1.1"
   :static true}
  []
  (let [d (java.util.concurrent.CountDownLatch. 1)
        v (atom d)]
    (reify 
     clojure.lang.IDeref
       (deref [_] (.await d) @v)
     clojure.lang.IBlockingDeref
       (deref
        [_ timeout-ms timeout-val]
        (if (.await d timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
          @v
          timeout-val))  
     clojure.lang.IPending
      (isRealized [this]
       (zero? (.getCount d)))
     clojure.lang.IFn
     (invoke
      [this x]
      (when (and (pos? (.getCount d))
                 (compare-and-set! v d x))
        (.countDown d)
        this)))))

(defn deliver
  "Delivers the supplied value to the promise, releasing any pending
  derefs. A subsequent call to deliver on a promise will have no effect."
  {:added "1.1"
   :static true}
  [promise val] (promise val))



(defn flatten
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat sequence.
  (flatten nil) returns an empty sequence."
  {:added "1.2"
   :static true}
  [x]
  (filter (complement sequential?)
          (rest (tree-seq sequential? seq x))))

(defn group-by 
  "Returns a map of the elements of coll keyed by the result of
  f on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll."
  {:added "1.2"
   :static true}
  [f coll]  
  (persistent!
   (reduce
    (fn [ret x]
      (let [k (f x)]
        (assoc! ret k (conj (get ret k []) x))))
    (transient {}) coll)))

(defn partition-by
  "Applies f to each value in coll, splitting it each time f returns a
   new value.  Returns a lazy seq of partitions.  Returns a stateful
   transducer when no collection is provided."
  {:added "1.2"
   :static true}
  ([f]
  (fn [rf]
    (let [a (java.util.ArrayList.)
          pv (volatile! ::none)]
      (fn
        ([] (rf))
        ([result]
           (let [result (if (.isEmpty a)
                          result
                          (let [v (vec (.toArray a))]
                            ;;clear first!
                            (.clear a)
                            (unreduced (rf result v))))]
             (rf result)))
        ([result input]
           (let [pval @pv
                 val (f input)]
             (vreset! pv val)
             (if (or (identical? pval ::none)
                     (= val pval))
               (do
                 (.add a input)
                 result)
               (let [v (vec (.toArray a))]
                 (.clear a)
                 (let [ret (rf result v)]
                   (when-not (reduced? ret)
                     (.add a input))
                   ret)))))))))
  ([f coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (let [fst (first s)
              fv (f fst)
              run (cons fst (take-while #(= fv (f %)) (next s)))]
          (cons run (partition-by f (seq (drop (count run) s)))))))))

(defn frequencies
  "Returns a map from distinct items in coll to the number of times
  they appear."
  {:added "1.2"
   :static true}
  [coll]
  (persistent!
   (reduce (fn [counts x]
             (assoc! counts x (inc (get counts x 0))))
           (transient {}) coll)))

(defn reductions
  "Returns a lazy seq of the intermediate values of the reduction (as
  per reduce) of coll by f, starting with init."
  {:added "1.2"}
  ([f coll]
     (lazy-seq
      (if-let [s (seq coll)]
        (reductions f (first s) (rest s))
        (list (f)))))
  ([f init coll]
     (if (reduced? init)
       (list @init)
       (cons init
             (lazy-seq
              (when-let [s (seq coll)]
                (reductions f (f init (first s)) (rest s))))))))

(defn rand-nth
  "Return a random element of the (sequential) collection. Will have
  the same performance characteristics as nth for the given
  collection."
  {:added "1.2"
   :static true}
  [coll]
  (nth coll (rand-int (count coll))))

(defn partition-all
  "Returns a lazy sequence of lists like partition, but may include
  partitions with fewer than n items at the end.  Returns a stateful
  transducer when no collection is provided."
  {:added "1.2"
   :static true}
  ([^long n]
   (fn [rf]
     (let [a (java.util.ArrayList. n)]
       (fn
         ([] (rf))
         ([result]
            (let [result (if (.isEmpty a)
                           result
                           (let [v (vec (.toArray a))]
                             ;;clear first!
                             (.clear a)
                             (unreduced (rf result v))))]
              (rf result)))
         ([result input]
            (.add a input)
            (if (= n (.size a))
              (let [v (vec (.toArray a))]
                (.clear a)
                (rf result v))
              result))))))
  ([n coll]
     (partition-all n n coll))
  ([n step coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (let [seg (doall (take n s))]
          (cons seg (partition-all n step (nthrest s step))))))))

(defn shuffle
  "Return a random permutation of coll"
  {:added "1.2"
   :static true}
  [^java.util.Collection coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al)
    (clojure.lang.RT/vector (.toArray al))))

(defn map-indexed
  "Returns a lazy sequence consisting of the result of applying f to 0
  and the first item of coll, followed by applying f to 1 and the second
  item in coll, etc, until coll is exhausted. Thus function f should
  accept 2 arguments, index and item. Returns a stateful transducer when
  no collection is provided."
  {:added "1.2"
   :static true}
  ([f]
   (fn [rf]
     (let [i (volatile! -1)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (rf result (f (vswap! i inc) input)))))))
  ([f coll]
   (letfn [(mapi [idx coll]
                 (lazy-seq
                   (when-let [s (seq coll)]
                     (if (chunked-seq? s)
                       (let [c (chunk-first s)
                             size (int (count c))
                             b (chunk-buffer size)]
                         (dotimes [i size]
                           (chunk-append b (f (+ idx i) (.nth c i))))
                         (chunk-cons (chunk b) (mapi (+ idx size) (chunk-rest s))))
                       (cons (f idx (first s)) (mapi (inc idx) (rest s)))))))]
     (mapi 0 coll))))

(defn keep
  "Returns a lazy sequence of the non-nil results of (f item). Note,
  this means false return values will be included.  f must be free of
  side-effects.  Returns a transducer when no collection is provided."
  {:added "1.2"
   :static true}
  ([f]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
          (let [v (f input)]
            (if (nil? v)
              result
              (rf result v)))))))
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if (chunked-seq? s)
        (let [c (chunk-first s)
              size (count c)
              b (chunk-buffer size)]
          (dotimes [i size]
            (let [x (f (.nth c i))]
              (when-not (nil? x)
                (chunk-append b x))))
          (chunk-cons (chunk b) (keep f (chunk-rest s))))
        (let [x (f (first s))]
          (if (nil? x)
            (keep f (rest s))
            (cons x (keep f (rest s))))))))))

(defn keep-indexed
  "Returns a lazy sequence of the non-nil results of (f index item). Note,
  this means false return values will be included.  f must be free of
  side-effects.  Returns a stateful transducer when no collection is
  provided."
  {:added "1.2"
   :static true}
  ([f]
   (fn [rf]
     (let [iv (volatile! -1)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
            (let [i (vswap! iv inc)
                  v (f i input)]
              (if (nil? v)
                result
                (rf result v))))))))
  ([f coll]
     (letfn [(keepi [idx coll]
               (lazy-seq
                (when-let [s (seq coll)]
                  (if (chunked-seq? s)
                    (let [c (chunk-first s)
                          size (count c)
                          b (chunk-buffer size)]
                      (dotimes [i size]
                        (let [x (f (+ idx i) (.nth c i))]
                          (when-not (nil? x)
                            (chunk-append b x))))
                      (chunk-cons (chunk b) (keepi (+ idx size) (chunk-rest s))))
                    (let [x (f idx (first s))]
                      (if (nil? x)
                        (keepi (inc idx) (rest s))
                        (cons x (keepi (inc idx) (rest s)))))))))]
       (keepi 0 coll))))

(defn every-pred
  "Takes a set of predicates and returns a function f that returns true if all of its
  composing predicates return a logical true value against all of its arguments, else it returns
  false. Note that f is short-circuiting in that it will stop execution on the first
  argument that triggers a logical false result against the original predicates."
  {:added "1.3"}
  ([p]
     (fn ep1
       ([] true)
       ([x] (boolean (p x)))
       ([x y] (boolean (and (p x) (p y))))
       ([x y z] (boolean (and (p x) (p y) (p z))))
       ([x y z & args] (boolean (and (ep1 x y z)
                                     (every? p args))))))
  ([p1 p2]
     (fn ep2
       ([] true)
       ([x] (boolean (and (p1 x) (p2 x))))
       ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y))))
       ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z))))
       ([x y z & args] (boolean (and (ep2 x y z)
                                     (every? #(and (p1 %) (p2 %)) args))))))
  ([p1 p2 p3]
     (fn ep3
       ([] true)
       ([x] (boolean (and (p1 x) (p2 x) (p3 x))))
       ([x y] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y))))
       ([x y z] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z))))
       ([x y z & args] (boolean (and (ep3 x y z)
                                     (every? #(and (p1 %) (p2 %) (p3 %)) args))))))
  ([p1 p2 p3 & ps]
     (let [ps (list* p1 p2 p3 ps)]
       (fn epn
         ([] true)
         ([x] (every? #(% x) ps))
         ([x y] (every? #(and (% x) (% y)) ps))
         ([x y z] (every? #(and (% x) (% y) (% z)) ps))
         ([x y z & args] (boolean (and (epn x y z)
                                       (every? #(every? % args) ps))))))))

(defn some-fn
  "Takes a set of predicates and returns a function f that returns the first logical true value
  returned by one of its composing predicates against any of its arguments, else it returns
  logical false. Note that f is short-circuiting in that it will stop execution on the first
  argument that triggers a logical true result against the original predicates."
  {:added "1.3"}
  ([p]
     (fn sp1
       ([] nil)
       ([x] (p x))
       ([x y] (or (p x) (p y)))
       ([x y z] (or (p x) (p y) (p z)))
       ([x y z & args] (or (sp1 x y z)
                           (some p args)))))
  ([p1 p2]
     (fn sp2
       ([] nil)
       ([x] (or (p1 x) (p2 x)))
       ([x y] (or (p1 x) (p1 y) (p2 x) (p2 y)))
       ([x y z] (or (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z)))
       ([x y z & args] (or (sp2 x y z)
                           (some #(or (p1 %) (p2 %)) args)))))
  ([p1 p2 p3]
     (fn sp3
       ([] nil)
       ([x] (or (p1 x) (p2 x) (p3 x)))
       ([x y] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y)))
       ([x y z] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z)))
       ([x y z & args] (or (sp3 x y z)
                           (some #(or (p1 %) (p2 %) (p3 %)) args)))))
  ([p1 p2 p3 & ps]
     (let [ps (list* p1 p2 p3 ps)]
       (fn spn
         ([] nil)
         ([x] (some #(% x) ps))
         ([x y] (some #(or (% x) (% y)) ps))
         ([x y z] (some #(or (% x) (% y) (% z)) ps))
         ([x y z & args] (or (spn x y z)
                             (some #(some % args) ps)))))))

(defn- ^{:dynamic true} assert-valid-fdecl
  "A good fdecl looks like (([a] ...) ([a b] ...)) near the end of defn."
  [fdecl]
  (when (empty? fdecl) (throw (IllegalArgumentException.
                                "Parameter declaration missing")))
  (let [argdecls (map 
                   #(if (seq? %)
                      (first %)
                      (throw (IllegalArgumentException. 
                        (if (seq? (first fdecl))
                          (str "Invalid signature \""
                               %
                               "\" should be a list")
                          (str "Parameter declaration \""
                               %
                               "\" should be a vector")))))
                   fdecl)
        bad-args (seq (remove #(vector? %) argdecls))]
    (when bad-args
      (throw (IllegalArgumentException. (str "Parameter declaration \"" (first bad-args) 
                                             "\" should be a vector"))))))

(defn with-redefs-fn
  "Temporarily redefines Vars during a call to func.  Each val of
  binding-map will replace the root value of its key which must be
  a Var.  After func is called with no args, the root values of all
  the Vars will be set back to their old values.  These temporary
  changes will be visible in all threads.  Useful for mocking out
  functions during testing."
  {:added "1.3"}
  [binding-map func]
  (let [root-bind (fn [m]
                    (doseq [[a-var a-val] m]
                      (.bindRoot ^clojure.lang.Var a-var a-val)))
        old-vals (zipmap (keys binding-map)
                         (map #(.getRawRoot ^clojure.lang.Var %) (keys binding-map)))]
    (try
      (root-bind binding-map)
      (func)
      (finally
        (root-bind old-vals)))))

(defmacro with-redefs
  "binding => var-symbol temp-value-expr

  Temporarily redefines Vars while executing the body.  The
  temp-value-exprs will be evaluated and each resulting value will
  replace in parallel the root value of its Var.  After the body is
  executed, the root values of all the Vars will be set back to their
  old values.  These temporary changes will be visible in all threads.
  Useful for mocking out functions during testing."
  {:added "1.3"}
  [bindings & body]
  `(with-redefs-fn ~(zipmap (map #(list `var %) (take-nth 2 bindings))
                            (take-nth 2 (next bindings)))
                    (fn [] ~@body)))

(defn realized?
  "Returns true if a value has been produced for a promise, delay, future or lazy sequence."
  {:added "1.3"}
  [^clojure.lang.IPending x] (.isRealized x))

(defmacro cond->
  "Takes an expression and a set of test/form pairs. Threads expr (via ->)
  through each form for which the corresponding test
  expression is true. Note that, unlike cond branching, cond-> threading does
  not short circuit after the first true test expression."
  {:added "1.5"}
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        steps (map (fn [[test step]] `(if ~test (-> ~g ~step) ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro cond->>
  "Takes an expression and a set of test/form pairs. Threads expr (via ->>)
  through each form for which the corresponding test expression
  is true.  Note that, unlike cond branching, cond->> threading does not short circuit
  after the first true test expression."
  {:added "1.5"}
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        steps (map (fn [[test step]] `(if ~test (->> ~g ~step) ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro as->
  "Binds name to expr, evaluates the first form in the lexical context
  of that binding, then binds name to that result, repeating for each
  successive form, returning the result of the last form."
  {:added "1.5"}
  [expr name & forms]
  `(let [~name ~expr
         ~@(interleave (repeat name) (butlast forms))]
     ~(if (empty? forms)
        name
        (last forms))))

(defmacro some->
  "When expr is not nil, threads it into the first form (via ->),
  and when that result is not nil, through the next etc"
  {:added "1.5"}
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (nil? ~g) nil (-> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro some->>
  "When expr is not nil, threads it into the first form (via ->>),
  and when that result is not nil, through the next etc"
  {:added "1.5"}
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (nil? ~g) nil (->> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defn ^:private preserving-reduced
  [rf]
  #(let [ret (rf %1 %2)]
     (if (reduced? ret)
       (reduced ret)
       ret)))

(defn cat
  "A transducer which concatenates the contents of each input, which must be a
  collection, into the reduction."
  {:added "1.7"}
  [rf]
  (let [rrf (preserving-reduced rf)]  
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
         (reduce rrf result input)))))

(defn dedupe
  "Returns a lazy sequence removing consecutive duplicates in coll.
  Returns a transducer when no collection is provided."
  {:added "1.7"}
  ([]
   (fn [rf]
     (let [pv (volatile! ::none)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
            (let [prior @pv]
              (vreset! pv input)
              (if (= prior input)
                result
                (rf result input))))))))
  ([coll] (sequence (dedupe) coll)))

(defn random-sample
  "Returns items from coll with random probability of prob (0.0 -
  1.0).  Returns a transducer when no collection is provided."
  {:added "1.7"}
  ([prob]
     (filter (fn [_] (< (rand) prob))))
  ([prob coll]
     (filter (fn [_] (< (rand) prob)) coll)))

(deftype Eduction [xform coll]
   Iterable
   (iterator [_]
     (clojure.lang.TransformerIterator/create xform (clojure.lang.RT/iter coll)))

   clojure.lang.IReduceInit
   (reduce [_ f init]
     ;; NB (completing f) isolates completion of inner rf from outer rf
     (transduce xform (completing f) init coll))

   clojure.lang.Sequential)

(defn eduction
  "Returns a reducible/iterable application of the transducers
  to the items in coll. Transducers are applied in order as if
  combined with comp. Note that these applications will be
  performed every time reduce/iterator is called."
  {:arglists '([xform* coll])
   :added "1.7"}
  [& xforms]
  (Eduction. (apply comp (butlast xforms)) (last xforms)))

(defmethod print-method Eduction [c, ^Writer w]
  (if *print-readably*
    (do
      (print-sequential "(" pr-on " " ")" c w))
    (print-object c w)))

(defn run!
  "Runs the supplied procedure (via reduce), for purposes of side
  effects, on successive items in the collection. Returns nil"
  {:added "1.7"}
  [proc coll]
  (reduce #(proc %2) nil coll)
  nil)


(defn tagged-literal?
  "Return true if the value is the data representation of a tagged literal"
  {:added "1.7"}
  [value]
  (instance? clojure.lang.TaggedLiteral value))

(defn tagged-literal
  "Construct a data representation of a tagged literal from a
  tag symbol and a form."
  {:added "1.7"}
  [^clojure.lang.Symbol tag form]
  (clojure.lang.TaggedLiteral/create tag form))

(defn reader-conditional?
  "Return true if the value is the data representation of a reader conditional"
  {:added "1.7"}
  [value]
  (instance? clojure.lang.ReaderConditional value))

(defn reader-conditional
  "Construct a data representation of a reader conditional.
  If true, splicing? indicates read-cond-splicing."
  {:added "1.7"}
  [form ^Boolean splicing?]
  (clojure.lang.ReaderConditional/create form splicing?))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; data readers ;;;;;;;;;;;;;;;;;;

(def ^{:added "1.4"} default-data-readers
  "Default map of data reader functions provided by Clojure. May be
  overridden by binding *data-readers*."
  {'inst #'clojure.instant/read-instant-date
   'uuid #'clojure.uuid/default-uuid-reader})

(def ^{:added "1.4" :dynamic true} *data-readers*
  "Map from reader tag symbols to data reader Vars.

  When Clojure starts, it searches for files named 'data_readers.clj'
  at the root of the classpath. Each such file must contain a literal
  map of symbols, like this:

      {foo/bar my.project.foo/bar
       foo/baz my.project/baz}

  The first symbol in each pair is a tag that will be recognized by
  the Clojure reader. The second symbol in the pair is the
  fully-qualified name of a Var which will be invoked by the reader to
  parse the form following the tag. For example, given the
  data_readers.clj file above, the Clojure reader would parse this
  form:

      #foo/bar [1 2 3]

  by invoking the Var #'my.project.foo/bar on the vector [1 2 3]. The
  data reader function is invoked on the form AFTER it has been read
  as a normal Clojure data structure by the reader.

  Reader tags without namespace qualifiers are reserved for
  Clojure. Default reader tags are defined in
  clojure.core/default-data-readers but may be overridden in
  data_readers.clj or by rebinding this Var."
  {})

(def ^{:added "1.5" :dynamic true} *default-data-reader-fn* 
  "When no data reader is found for a tag and *default-data-reader-fn*
  is non-nil, it will be called with two arguments,
  the tag and the value.  If *default-data-reader-fn* is nil (the
  default), an exception will be thrown for the unknown tag."
  nil)

(defn- data-reader-urls []
  (let [cl (.. Thread currentThread getContextClassLoader)]
    (concat
      (enumeration-seq (.getResources cl "data_readers.clj"))
      (enumeration-seq (.getResources cl "data_readers.cljc")))))

(defn- data-reader-var [sym]
  (intern (create-ns (symbol (namespace sym)))
          (symbol (name sym))))

(defn- load-data-reader-file [mappings ^java.net.URL url]
  (with-open [rdr (clojure.lang.LineNumberingPushbackReader.
                   (java.io.InputStreamReader.
                    (.openStream url) "UTF-8"))]
    (binding [*file* (.getFile url)]
      (let [read-opts (if (.endsWith (.getPath url) "cljc")
                        {:eof nil :read-cond :allow}
                        {:eof nil})
            new-mappings (read read-opts rdr)]
        (when (not (map? new-mappings))
          (throw (ex-info (str "Not a valid data-reader map")
                          {:url url})))
        (reduce
         (fn [m [k v]]
           (when (not (symbol? k))
             (throw (ex-info (str "Invalid form in data-reader file")
                             {:url url
                              :form k})))
           (let [v-var (data-reader-var v)]
             (when (and (contains? mappings k)
                        (not= (mappings k) v-var))
               (throw (ex-info "Conflicting data-reader mapping"
                               {:url url
                                :conflict k
                                :mappings m})))
             (assoc m k v-var)))
         mappings
         new-mappings)))))

(defn- load-data-readers []
  (alter-var-root #'*data-readers*
                  (fn [mappings]
                    (reduce load-data-reader-file
                            mappings (data-reader-urls)))))

(try
 (load-data-readers)
 (catch Throwable t
   (.printStackTrace t)
   (throw t)))
