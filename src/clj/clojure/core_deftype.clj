;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; definterface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;for now, built on gen-interface
(defmacro definterface 
  [name & sigs]
  (let [tag (fn [x] (or (:tag (meta x)) Object))
        psig (fn [[name [& args]]]
               (vector name (vec (map tag args)) (tag name)))]
    `(gen-interface :name ~(symbol (str *ns* "." name)) :methods ~(vec (map psig sigs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; reify/deftype ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- parse-opts [s]
  (loop [opts {} [k v & rs :as s] s]
    (if (keyword? k)
      (recur (assoc opts k v) rs)
      [opts s])))

(defn- parse-impls [specs]
  (loop [ret {} s specs]
    (if (seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
             (drop-while seq? (next s)))
      ret)))

(defn- parse-opts+specs [opts+specs]
  (let [[opts specs] (parse-opts opts+specs)
        impls (parse-impls specs)
        interfaces (-> (map #(if (var? (resolve %)) 
                               (:on (deref (resolve %)))
                               %)
                            (keys impls))
                       set
                       (disj 'Object 'java.lang.Object)
                       vec)
        methods (mapcat #(map (fn [[nm [& args] & body]]
                                `(~nm [~(:as opts) ~@args] ~@body)) %) 
                        (vals impls))]  
    [interfaces methods opts]))

(defmacro reify 
  "reify is a macro with the following structure:

 (reify options* specs*)
  
  Currently there is only one option:

  :as this-name

  which can be used to provide a name to refer to the target
  object ('this' in Java/C# parlance) within the method bodies, if
  needed.

  Each spec consists of the protocol or interface name followed by zero
  or more method bodies:

  protocol-or-interface-or-Object
  (methodName [args*] body)*

  Methods should be supplied for all methods of the desired
  protocol(s) and interface(s). You can also define overrides for
  methods of Object. Note that no parameter is supplied to correspond
  to the target object ('this' in Java parlance). Thus methods for
  protocols will take one fewer arguments than do the
  protocol functions.

  The return type can be indicated by a type hint on the method name,
  and arg types can be indicated by a type hint on arg names. If you
  leave out all hints, reify will try to match on same name/arity
  method in the protocol(s)/interface(s) - this is preferred. If you
  supply any hints at all, no inference is done, so all hints (or
  default of Object) must be correct, for both arguments and return
  type. If a method is overloaded in a protocol/interface, multiple
  independent method definitions must be supplied.  If overloaded with
  same arity in an interface you must specify complete hints to
  disambiguate - a missing hint implies Object.

  recur works to method heads The method bodies of reify are lexical
  closures, and can refer to the surrounding local scope:
  
  (str (let [f \"foo\"] 
       (reify Object 
         (toString [] f))))
  == \"foo\"

  (seq (let [f \"foo\"] 
       (reify clojure.lang.Seqable 
         (seq [] (seq f)))))
  == (\\f \\o \\o))"

  [& opts+specs]
  (let [[interfaces methods] (parse-opts+specs opts+specs)]
    `(reify* ~interfaces ~@methods)))

(defn hash-combine [x y] 
  (clojure.lang.Util/hashCombine x (clojure.lang.Util/hash y)))

(defn munge [s]
  ((if (symbol? s) symbol str) (clojure.lang.Compiler/munge (str s))))

(defn- emit-deftype* 
  "Do not use this directly - use deftype"
  [tagname name fields interfaces methods]
  (let [tag (keyword (str *ns*) (str tagname))
        classname (symbol (str *ns* "." name))
        interfaces (vec interfaces)
        interface-set (set (map resolve interfaces))
        methodname-set (set (map first methods))
        dynamic-type (contains? interface-set clojure.lang.IDynamicType)
        implement? (fn [iface] (not (contains? interface-set iface)))
        hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))
        base-fields fields
        fields (conj fields '__meta '__extmap)]
    (letfn 
     [(eqhash [[i m]] 
        (if (not (or (contains? methodname-set 'equals) (contains? methodname-set 'hashCode)))
          [i
           (conj m 
                 `(hashCode [~'this] (-> ~tag hash ~@(map #(list `hash-combine %) (remove #{'__meta} fields))))
                 `(equals [~'this ~'o] 
                    (boolean 
                     (or (identical? ~'this ~'o)
                         (when (instance? clojure.lang.IDynamicType ~'o)
                           (let [~'o ~(with-meta 'o {:tag 'clojure.lang.IDynamicType})]
                             (and (= ~tag (.getDynamicType ~'o)) 
                                  ~@(map (fn [fld] `(= ~fld (.getDynamicField ~'o ~(keyword fld) ~'this))) base-fields)
                                  (= ~'__extmap (.getExtensionMap ~'o)))))))))]
          [i m]))
      (iobj [[i m]] 
        (if (and (implement? clojure.lang.IObj) (implement? clojure.lang.IMeta))
          [(conj i 'clojure.lang.IObj)
           (conj m `(meta [~'this] ~'__meta)
                 `(withMeta [~'this ~'m] (new ~tagname ~@(replace {'__meta 'm} fields))))]
          [i m]))
      (ilookup [[i m]] 
        (if (not (methodname-set 'valAt))
          [(conj i 'clojure.lang.ILookup 'clojure.lang.IKeywordLookup)
           (conj m `(valAt [~'this k#] (.valAt ~'this k# nil))
                 `(valAt [~'this k# else#] 
                    (case k# ~@(mapcat (fn [fld] [(keyword fld) fld]) 
                                                   base-fields)
                           (get ~'__extmap k# else#)))
                 `(getLookupThunk [~'this k#]
                    (case k#
                          ~@(mapcat 
                             (fn [fld]
                               (let [cstr (str (clojure.core/name classname) "$__lookup__" (clojure.core/name fld))]
                                 [(keyword fld) 
                                  `(-> ~cstr (Class/forName) (.newInstance))]))
                             base-fields)
                          nil)))]
          [i m]))
      (idynamictype [[i m]]
        [(conj i 'clojure.lang.IDynamicType)
         (conj m
               `(getDynamicType [~'this] ~tag)
               `(getExtensionMap [~'this] ~'__extmap)
               `(getDynamicField [~'this k# else#] 
                  (condp identical? k# ~@(mapcat (fn [fld] [(keyword fld) fld]) base-fields)
                         (get ~'__extmap k# else#))))])
      (imap [[i m]] 
         (if (and (interface-set clojure.lang.IPersistentMap) (not (methodname-set 'assoc)))
           [i
            (conj m 
                  `(count [~'this] (+ ~(count base-fields) (count ~'__extmap)))
                  `(empty [~'this] (throw (UnsupportedOperationException. (str "Can't create empty: " ~(str classname)))))
                  `(cons [~'this e#] (let [[k# v#] e#] (.assoc ~'this k# v#)))
                  `(equiv [~'this o#] (.equals ~'this o#))
                  `(containsKey [~'this k#] (not (identical? ~'this (.valAt ~'this k# ~'this))))
                  `(entryAt [~'this k#] (let [v# (.valAt ~'this k# ~'this)]
                                     (when-not (identical? ~'this v#)
                                       (clojure.lang.MapEntry. k# v#))))
                  `(seq [~'this] (concat [~@(map #(list `new `clojure.lang.MapEntry (keyword %) %) base-fields)] 
                                     ~'__extmap))
                  (let [gk (gensym) gv (gensym)]
                    `(assoc [~'this ~gk ~gv]
                       (condp identical? ~gk
                         ~@(mapcat (fn [fld]
                                     [(keyword fld) (list* `new tagname (replace {fld gv} fields))])
                                   base-fields)
                         (new ~tagname ~@(remove #{'__extmap} fields) (assoc ~'__extmap ~gk ~gv)))))
                  `(without [~'this k#] (if (contains? #{~@(map keyword base-fields)} k#)
                                     (dissoc (with-meta (into {} ~'this) ~'__meta) k#)
                                     (new ~tagname ~@(remove #{'__extmap} fields) 
                                          (not-empty (dissoc ~'__extmap k#))))))]
           [i m]))]
     (let [[i m] (-> [interfaces methods] eqhash iobj ilookup imap idynamictype)]
       `(deftype* ~tagname ~classname ~(conj hinted-fields '__meta '__extmap) 
          :implements ~(vec i) 
          ~@m)))))

(defmacro deftype
  "Alpha - subject to change
  
  (deftype name [fields*]  options* specs*)
  
  Currently there is only one option:

  :as this-name

  which can be used to provide a name to refer to the target
  object ('this' in Java/C# parlance) within the method bodies, if
  needed.

  Each spec consists of a protocol or interface name followed by zero
  or more method bodies:

  protocol-or-interface-or-Object
  (methodName [args*] body)*

  Dynamically generates compiled bytecode for an anonymous class with
  the given fields, and, optionally, methods for protocols and/or
  interfaces. The Name will be used to create a dynamic type tag
  keyword of the form :current.ns/Name. This tag will be returned
  from (type an-instance).

  A factory function of current.ns/Name will be defined,
  overloaded on 2 arities, the first taking the designated fields in
  the same order specified, and the second taking the fields followed
  by a metadata map (nil for none) and an extension field map (nil for
  none). 

  The class will have the (by default, immutable) fields named by
  fields, which can have type hints. Protocols/interfaces and methods
  are optional. The only methods that can be supplied are those
  declared in the protocols/interfaces.  Note that method bodies are
  not closures, the local environment includes only the named fields,
  and those fields can be accessed directy. Fields can be qualified
  with the metadata :volatile-mutable true or :unsynchronized-mutable
  true, at which point (set! afield aval) will be supported in method
  bodies. Note well that mutable fields are extremely difficult to use
  correctly, and are present only to facilitate the building of higher
  level constructs, such as Clojure's reference types, in Clojure
  itself. They are for experts only - if the semantics and
  implications of :volatile-mutable or :unsynchronized-mutable are not
  immediately apparent to you, you should not be using them.

  Method definitions take the form:

  (methodname [args*] body)

  The argument and return types can be hinted on the arg and
  methodname symbols. If not supplied, they will be inferred, so type
  hints should be reserved for disambiguation.

  Methods should be supplied for all methods of the desired
  protocol(s) and interface(s). You can also define overrides for
  methods of Object. Note that no parameter is supplied to correspond
  to the target object ('this' in Java parlance). Thus methods for
  protocols will take one fewer arguments than do the
  protocol functions.

  In the method bodies, the (unqualified) name can be used to name the
  class (for calls to new, instance? etc).

  The class will have implementations of two (clojure.lang) interfaces
  generated automatically: IObj (metadata support), ILookup (get and
  keyword lookup for fields). If you specify IPersistentMap as an
  interface, but don't define methods for it, an implementation will
  be generated automatically.

  In addition, unless you supply a version of hashCode or equals,
  deftype/class will define type-and-value-based equality and
  hashCode.

  When AOT compiling, generates compiled bytecode for a class with the
  given name (a symbol), prepends the current ns as the package, and
  writes the .class file to the *compile-path* directory.

  Two constructors will be defined, one taking the designated fields
  followed by a metadata map (nil for none) and an extension field
  map (nil for none), and one taking only the fields (using nil for
  meta and extension fields).

  When dynamically evaluated, the class will have a generated name."

  [name [& fields] & opts+specs]
  (let [gname (if *compile-files* name (gensym (str name "__")))
        [interfaces methods opts] (parse-opts+specs opts+specs)
        classname (symbol (str *ns* "." gname))
        tag (keyword (str *ns*) (str name))
        hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))]
    `(do
       ~(emit-deftype* name gname (vec hinted-fields) (vec interfaces) methods)
       (defmethod print-method ~tag [o# w#]
         ((var print-deftype) ~(vec (map #(-> % str keyword) fields)) o# w#))
       (defn ~name
         ([~@fields] (new ~classname ~@fields nil nil))
         ([~@fields meta# extmap#] (new ~classname ~@fields meta# extmap#))))))

(defn- print-deftype [fields, #^clojure.lang.IDynamicType o, #^Writer w]
  (print-meta o w)
  (.write w "#:")
  (.write w (str (name (.getDynamicType o))))
  (print-map
    (concat
      (map #(clojure.lang.MapEntry. % (.getDynamicField o % nil)) fields)
      (.getExtensionMap o))
    pr-on w))


;;;;;;;;;;;;;;;;;;;;;;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;

(defn dtype 
  "Returns the dynamic type of x, or its Class if none"
  [x]
  (if (instance? clojure.lang.IDynamicType x)
    (let [x #^ clojure.lang.IDynamicType x]
      (.getDynamicType x))
    (class x)))

(defn- expand-method-impl-cache [#^clojure.lang.MethodImplCache cache c f]
  (let [cs (into {} (remove (fn [[c f]] (nil? f)) (map vec (partition 2 (.table cache)))))
        cs (assoc cs c f)
        [shift mask] (min-hash (keys cs))
        table (make-array Object (* 2 (inc mask)))
        table (reduce (fn [#^objects t [c f]]
                        (let [i (* 2 (int (shift-mask shift mask (hash c))))]
                          (aset t i c)
                          (aset t (inc i) f)
                          t))
                      table cs)]
    (clojure.lang.MethodImplCache. (.protocol cache) (.methodk cache) shift mask table)))

(defn- super-chain [#^Class c]
  (when c
    (cons c (super-chain (.getSuperclass c)))))

(defn find-protocol-impl [protocol x]
  (if (and (:on-interface protocol) (instance? (:on-interface protocol) x))
    x
  (let [t (dtype x)
        c (class x)
        impl #(get (:impls protocol) %)]
    (or (impl t)
        (impl c)
        (and c (or (first (remove nil? (map impl (butlast (super-chain c)))))
                   (first (remove nil? (map impl (disj (supers c) Object))))
                     (impl Object)))))))

(defn find-protocol-method [protocol methodk x]
  (get (find-protocol-impl protocol x) methodk))

(defn extends? 
  "Returns true if atype explicitly extends protocol"
  [protocol atype]
  (when (get (:impls protocol) atype) true))

(defn extenders 
  "Returns a collection of the types explicitly extending protocol"
  [protocol]
  (keys (:impls protocol)))

(defn satisfies? 
  "Returns true if x satisfies the protocol"
  [protocol x]
  (when
      (or (and (:on-interface protocol) (instance? (:on-interface protocol) x))
          (find-protocol-impl protocol x))
    true))

(defn -cache-protocol-fn [#^clojure.lang.AFunction pf x]
  (let [cache  (.__methodImplCache pf)
        f (find-protocol-method (.protocol cache) (.methodk cache) x)]
    (when-not f
      (throw (IllegalArgumentException. (str "No implementation of method: " (.methodk cache) 
                                             " of protocol: " (:var (.protocol cache)) 
                                             " found for class: " (if (nil? x) "nil" (.getName (class x)))))))
    (set! (.__methodImplCache pf) (expand-method-impl-cache cache (class x) f))
    f))

(defn- emit-method-builder [on-interface method on-method arglists]
  (let [methodk (keyword method)
        gthis (with-meta (gensym) {:tag 'clojure.lang.AFunction})]
    `(fn [cache#]
       (let [#^clojure.lang.AFunction f#
             (fn ~gthis
               ~@(map 
                  (fn [args]
                    (let [gargs (map #(gensym (str "g__" % "__")) args)
                          target (first gargs)]
                      `([~@gargs]
                          (~@(if on-interface
                               `(if (instance? ~on-interface ~target)
                                  (. ~(with-meta target {:tag on-interface})  ~(or on-method method) ~@(rest gargs)))
                               `(do))
                          (let [cache# (.__methodImplCache ~gthis)]
                            (if (clojure.lang.Util/identical (clojure.lang.Util/classOf ~target)
                                                             (.lastClass cache#))
                              ((.lastImpl cache#) ~@gargs)
                              (let [f# (or (.fnFor cache# (clojure.lang.Util/classOf ~target))
                                           (-cache-protocol-fn ~gthis ~target))]
                                 (f# ~@gargs))))))))
                  arglists))]
         (set! (.__methodImplCache f#) cache#)
         f#))))

(defn -reset-methods [protocol]
  (doseq [[#^clojure.lang.Var v build] (:method-builders protocol)]
    (let [cache (clojure.lang.MethodImplCache. protocol (keyword (.sym v)))]
      (.bindRoot v (build cache)))))

(defn- assert-same-protocol [protocol-var method-syms]
  (doseq [m method-syms]
    (let [v (resolve m)
          p (:protocol (meta v))]
      (when-not (or (nil? v) (= protocol-var p))
        (binding [*out* *err*]
          (println "Warning: protocol" protocol-var "is overwriting"
                   (if p
                     (str "method " (.sym v) " of protocol " (.sym p))
                     (str "function " (.sym v)))))))))

(defn- emit-protocol [name opts+sigs]
  (let [iname (symbol (str (munge *ns*) "." (munge name)))
        [opts sigs]
        (loop [opts {:on (list 'quote iname) :on-interface iname} sigs opts+sigs]
          (condp #(%1 %2) (first sigs) 
            string? (recur (assoc opts :doc (first sigs)) (next sigs))
            keyword? (recur (assoc opts (first sigs) (second sigs)) (nnext sigs))
            [opts sigs]))
        sigs (reduce (fn [m s]
                       (let [mname (with-meta (first s) nil)
                             [arglists doc]
                               (loop [as [] rs (rest s)]
                                 (if (vector? (first rs))
                                   (recur (conj as (first rs)) (next rs))
                                   [(seq as) (first rs)]))]
                         (when (some #{0} (map count arglists))
                           (throw (IllegalArgumentException. (str "Protocol fn: " mname " must take at least one arg"))))
                         (assoc m (keyword mname)  
                                {:name (vary-meta mname assoc :doc doc :arglists arglists)
                                 :arglists arglists
                                 :doc doc})))
                     {} sigs)
        meths (mapcat (fn [sig]
                        (let [m (munge (:name sig))]
                          (map #(vector m (vec (repeat (dec (count %))'Object)) 'Object) 
                               (:arglists sig))))
                      (vals sigs))]
  `(do
     (defonce ~name {})
     (gen-interface :name ~iname :methods ~meths)
     (alter-meta! (var ~name) assoc :doc ~(:doc opts))
     (#'assert-same-protocol (var ~name) '~(map :name (vals sigs)))
     (alter-var-root (var ~name) merge 
                     (assoc ~opts 
                       :sigs '~sigs 
                       :var (var ~name)
                       :method-map 
                         ~(and (:on opts)
                               (apply hash-map 
                                      (mapcat 
                                       (fn [s] 
                                         [(keyword (:name s)) (keyword (or (:on s) (:name s)))])
                                       (vals sigs))))
                       :method-builders 
                        ~(apply hash-map 
                                (mapcat 
                                 (fn [s] 
                                   [`(intern *ns* (with-meta '~(:name s) {:protocol (var ~name)}))
                                    (emit-method-builder (:on-interface opts) (:name s) (:on s) (:arglists s))])
                                 (vals sigs)))))
     (-reset-methods ~name)
     '~name)))

(defmacro defprotocol 
  "A protocol is a named set of named methods and their signatures:
  (defprotocol AProtocolName

    ;optional doc string
    \"A doc string for AProtocol abstraction\"

  ;method signatures
    (bar [a b] \"bar docs\")
    (baz [a] [a b] [a b c] \"baz docs\"))

  No implementations are provided. Docs can be specified for the
  protocol overall and for each method. The above yields a set of
  polymorphic functions and a protocol object. All are
  namespace-qualified by the ns enclosing the definition The resulting
  functions dispatch on the type of their first argument, and thus
  must have at least one argument. defprotocol is dynamic, has no
  special compile-time effect, and defines no new types or classes
  Implementations of the protocol methods can be provided using
  extend.

  defprotocol will automatically generate a corresponding interface,
  with the same name as the protocol, i.e. given a protocol:
  my.ns/Protocol, an interface: my.ns.Protocol. The interface will
  have methods corresponding to the protocol functions, and the
  protocol will automatically work with instances of the interface.

  Note that you should not use this interface with deftype or
  reify, as they support the protocol directly:

  (defprotocol P 
    (foo [x]) 
    (bar-me [x] [x y]))

  (deftype Foo [a b c] 
   P
    (foo [] a)
    (bar-me [] b)
    (bar-me [y] (+ c y)))
  
  (bar-me (Foo 1 2 3) 42)

  (foo 
    (let [x 42]
      (reify P 
        (foo [] 17)
        (bar-me [] x)
        (bar-me [y] x))))"

  [name & opts+sigs]
  (emit-protocol name opts+sigs))

(defn extend 
  "Implementations of protocol methods can be provided using the extend construct:

  (extend ::AType ;or AClass or AnInterface 
    AProtocol
     {:foo an-existing-fn
      :bar (fn [a b] ...)
      :baz (fn ([a]...) ([a b] ...)...)}
    BProtocol 
      {...} 
    ...)
 

  extend takes a type/class (or interface, see below), and one or more
  protocol + method map pairs. It will extend the polymorphism of the
  protocol's methods to call the supplied methods when an AType is
  provided as the first argument. Note that deftype types are specified
  using their keyword tags:

  ::MyType or :my.ns/MyType

  Method maps are maps of the keyword-ized method names to ordinary
  fns. This facilitates easy reuse of existing fns and fn maps, for
  code reuse/mixins without derivation or composition. You can extend
  an interface to a protocol. This is primarily to facilitate interop
  with the host (e.g. Java) but opens the door to incidental multiple
  inheritance of implementation since a class can inherit from more
  than one interface, both of which extend the protocol. It is TBD how
  to specify which impl to use. You can extend a protocol on nil.

  If you are supplying the definitions explicitly (i.e. not reusing
  exsting functions or mixin maps), you may find it more convenient to
  use the extend-type, extend-class or extend-protocol macros.

  Note that multiple independent extend clauses can exist for the same
  type, not all protocols need be defined in a single extend call.

  See also:
  extends?, satisfies?, extenders"

  [atype & proto+mmaps]
  (doseq [[proto mmap] (partition 2 proto+mmaps)]
    (-reset-methods (alter-var-root (:var proto) assoc-in [:impls atype] mmap))))

(defn- emit-impl [[p fs]]
  [p (zipmap (map #(-> % first keyword) fs)
             (map #(cons 'fn (drop 1 %)) fs))])

(defn- emit-hinted-impl [c [p fs]]
  (let [hint (fn [specs]
               (let [specs (if (vector? (first specs)) 
                                        (list specs) 
                                        specs)]
                 (map (fn [[[target & args] & body]]
                        (cons (apply vector (vary-meta target assoc :tag c) args)
                              body))
                      specs)))]
    [p (zipmap (map #(-> % first keyword) fs)
               (map #(cons 'fn (hint (drop 1 %))) fs))]))

(defn- emit-extend-type [t specs]
  (let [impls (parse-impls specs)]
    `(extend ~t
             ~@(mapcat emit-impl impls))))

(defn- emit-extend-class [c specs]
  (let [impls (parse-impls specs)]
    `(extend ~c
             ~@(mapcat (partial emit-hinted-impl c) impls))))

(defmacro extend-type 
  "A macro that expands into an extend call. Useful when you are
  supplying the definitions explicitly inline, extend-type
  automatically creates the maps required by extend.

  (extend-type ::MyType 
    Countable
      (cnt [c] ...)
    Foo
      (bar [x y] ...)
      (baz ([x] ...) ([x y & zs] ...)))

  expands into:

  (extend ::MyType
   Countable
     {:cnt (fn [c] ...)}
   Foo
     {:baz (fn ([x] ...) ([x y & zs] ...))
      :bar (fn [x y] ...)})"

  [t & specs]
  (emit-extend-type t specs))

(defmacro extend-class 
  "Like extend-type, for the case when the extended type is a
  class. Propagates the class as a type hint on the first argument of
  all fns" 
  [c & specs]
  (emit-extend-class c specs))

(defn- emit-extend-protocol [p specs]
  (let [impls (parse-impls specs)]
    `(do
       ~@(map (fn [[t fs]]
                (if (symbol? t)
                  `(extend-class ~t ~p ~@fs)
                  `(extend-type ~t ~p ~@fs)))
              impls))))

(defmacro extend-protocol 
  "Useful when you want to provide several implementations of the same
  protocol all at once. Takes a single protocol and the implementation
  of that protocol for one or more types. Expands into calls to
  extend-type and extend-class:

  (extend-protocol Protocol
    ::AType
      (foo [x] ...)
      (bar [x y] ...)
    ::BType
      (foo [x] ...)
      (bar [x y] ...)
    AClass
      (foo [x] ...)
      (bar [x y] ...)
    nil
      (foo [x] ...)
      (bar [x y] ...))

  expands into:

  (do
   (clojure.core/extend-type ::AType Protocol 
     (foo [x] ...) 
     (bar [x y] ...))
   (clojure.core/extend-type ::BType Protocol 
     (foo [x] ...) 
     (bar [x y] ...))
   (clojure.core/extend-class AClass Protocol 
     (foo [x] ...) 
     (bar [x y] ...))
   (clojure.core/extend-type nil Protocol 
     (foo [x] ...) 
     (bar [x y] ...)))"

  [p & specs]
  (emit-extend-protocol p specs))

