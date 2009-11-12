;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; defclass/deftype ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn hash-combine [x y] 
  (clojure.lang.Util/hashCombine x (clojure.lang.Util/hash y)))

(defn create-defclass* 
  "Do not use this directly - use defclass/deftype"
  [name fields interfaces methods]
  (let [tag (keyword (str *ns*) (str name))
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
        (if (not (or (contains? methodname-set '.equals) (contains? methodname-set '.hashCode)))
          [i
           (conj m 
                 `(.hashCode [] (-> ~tag hash ~@(map #(list `hash-combine %) (remove #{'__meta} fields))))
                 `(.equals [~'o] 
                    (boolean 
                     (or (identical? ~'this ~'o)
                         (when (instance? ~name ~'o)
                           (let [~'o ~(with-meta 'o {:tag name})]
                             (and ~@(map (fn [fld] `(= ~fld (. ~'o ~fld))) (remove #{'__meta} fields)))))))))]
          [i m]))
      (iobj [[i m]] 
        (if (and (implement? clojure.lang.IObj) (implement? clojure.lang.IMeta))
          [(conj i 'clojure.lang.IObj)
           (conj m `(.meta [] ~'__meta)
                 `(.withMeta [~'m] (new ~name ~@(replace {'__meta 'm} fields))))]
          [i m]))
      (ilookup [[i m]] 
        (if (implement? clojure.lang.ILookup)
          [(conj i 'clojure.lang.ILookup)
           (conj m `(.valAt [k#] (.valAt ~'this k# nil))
                 `(.valAt [k# else#] 
                    (case k# ~@(mapcat (fn [fld] [(keyword fld) fld]) 
                                                   base-fields)
                           (get ~'__extmap k# else#))))]
          [i m]))
      (ikeywordlookup [[i m]]
        [(conj i 'clojure.lang.IKeywordLookup)
         (conj m
               `(.getLookupThunk [k#]
                  (case k#
                        ~@(mapcat 
                           (fn [fld]
                             (let [cstr (str (clojure.core/name classname) "$__lookup__" (clojure.core/name fld))]
                               [(keyword fld) 
                                `(-> ~cstr (Class/forName) (.newInstance))]))
                             base-fields)
                        nil)))])
      (imap [[i m]] 
         (if (and (interface-set clojure.lang.IPersistentMap) (not (methodname-set '.assoc)))
           [i
            (conj m 
                  `(.count [] (+ ~(count base-fields) (count ~'__extmap)))
                  `(.empty [] (throw (UnsupportedOperationException. (str "Can't create empty: " ~(str classname)))))
                  `(.cons [e#] (let [[k# v#] e#] (.assoc ~'this k# v#)))
                  `(.equiv [o#] (.equals ~'this o#))
                  `(.containsKey [k#] (not (identical? ~'this (.valAt ~'this k# ~'this))))
                  `(.entryAt [k#] (let [v# (.valAt ~'this k# ~'this)]
                                     (when-not (identical? ~'this v#)
                                       (clojure.lang.MapEntry. k# v#))))
                  `(.seq [] (concat [~@(map #(list `new `clojure.lang.MapEntry (keyword %) %) base-fields)] 
                                     ~'__extmap))
                  (let [gk (gensym) gv (gensym)]
                    `(.assoc [~gk ~gv]
                       (condp identical? ~gk
                         ~@(mapcat (fn [fld]
                                     [(keyword fld) (list* `new name (replace {fld gv} fields))])
                                   base-fields)
                         (new ~name ~@(remove #{'__extmap} fields) (assoc ~'__extmap ~gk ~gv)))))
                  `(.without [k#] (if (contains? #{~@(map keyword base-fields)} k#)
                                     (dissoc (with-meta (into {} ~'this) ~'__meta) k#)
                                     (new ~name ~@(remove #{'__extmap} fields) 
                                          (not-empty (dissoc ~'__extmap k#))))))]
           [i m]))]
     (let [[i m] (-> [interfaces methods] eqhash iobj ilookup imap ikeywordlookup)]
       `(defclass* ~classname ~(conj hinted-fields '__meta '__extmap) 
          :implements ~(vec i) 
          ~@m)))))

(defmacro defclass 
  "Alpha - subject to change
  
  When compiling, generates compiled bytecode for a class with the
  given name (a symbol), prepends the current ns as the package, and
  writes the .class file to the *compile-path* directory.  When not
  compiling, does nothing. 

  Two constructors will be defined, one taking the designated fields
  followed by a metadata map (nil for none) and an extension field
  map (nil for none), and one taking only the fields (using nil for
  meta and extension fields). In the method bodies, the (unqualified)
  name can be used to name the class (for calls to new etc).

  See deftype for a description of fields, methods, equality and
  generated interfaces."
 
  [name [& fields] & [[& interfaces] & methods]]
  (let [o (gensym)
        classname (symbol (str *ns* "." name))]
    `(do
       ~(create-defclass* name (vec fields) (vec interfaces) methods)
       (defmethod print-method ~classname [~(with-meta o {:tag classname}) w#]
         ((var print-defclass)
            (.__extmap ~o)
            ~(apply array-map (interleave
                                (map #(-> % str keyword) fields)
                                (map #(list '. o %) fields)))
            ~o w#)))))

(defn- print-defclass [extmap fieldmap o, #^Writer w]
  (print-meta o w)
  (.write w "#:")
  (.write w (.getSimpleName (class o)))
  (print-map (concat fieldmap extmap) pr-on w))

(defmacro deftype
  "Alpha - subject to change
  
  Dynamically generates compiled bytecode for an anonymous class with
  the given fields, and, optionally, interfaces and methods. The Name
  will be used to create a dynamic type tag keyword of the
  form :current.ns/Name. This tag will be returned from (type
  an-instance). 

  A factory function of current.ns/Name will be defined,
  overloaded on 2 arities, the first taking the designated fields in
  the same order specified, and the second taking the fields followed
  by a metadata map (nil for none) and an extension field map (nil for
  none). 

  The class will have the (immutable) fields named by fields, which
  can have type hints. Interfaces and methods are optional. The only
  methods that can be supplied are those declared in the interfaces.
  'this' is impliclty bound to the target object (i.e. same meaning as
  in Java). Note that method bodies are not closures, the local
  environment includes only the named fields, and those fields can be
  accessed directy, i.e. with just foo, instead of (.foo this).

  Method definitions take the form:

  (.methodname [args] body) ;note the dot on the methodname!

  The argument and return types can be hinted on the arg and
  methodname symbols. If not supplied, they will be inferred, so type
  hints should be reserved for disambiguation.

  The class will have implementations of two (clojure.lang) interfaces
  generated automatically: IObj (metadata support), ILookup (get and
  keyword lookup for fields). If you specify IPersistentMap as an
  interface, but don't define methods for it, an implementation will
  be generated automatically.

  In addition, unless you supply a version of .hashCode or .equals,
  deftype/class will define type-and-value-based equality and hashCode.

  Note that overriding equals and hashCode is not supported at this
  time for deftype - you must use the generated versions."

  [name [& fields] & [[& interfaces] & methods]]
  (let [gname (gensym (str name "__"))
        classname (symbol (str *ns* "." gname))
        tag (keyword (str *ns*) (str name))
        interfaces (conj interfaces 'clojure.lang.IDynamicType)
        hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))
        methods (conj methods 
                      `(.getDynamicType [] ~tag)
                      `(.getExtensionMap [] ~'__extmap)
                      `(.getDynamicField [k# else#] 
                         (condp identical? k# ~@(mapcat (fn [fld] [(keyword fld) fld]) fields)
                                (get ~'__extmap k# else#)))
                      `(.hashCode [] (-> ~(hash tag) 
                                          ~@(map #(list `hash-combine %) fields)
                                          (hash-combine ~'__extmap)))
                      `(.equals [~'o] 
                         (boolean 
                          (or (identical? ~'this ~'o)
                              (when (instance? clojure.lang.IDynamicType ~'o)
                                (let [~'o ~(with-meta 'o {:tag 'clojure.lang.IDynamicType})]
                                  (and (= (.getDynamicType ~'this) (.getDynamicType ~'o)) 
                                       ~@(map (fn [fld] `(= ~fld (.getDynamicField ~'o ~(keyword fld) ~'this))) fields)
                                       (= ~'__extmap (.getExtensionMap ~'o)))))))))]
    `(do
       ~(create-defclass* gname (vec hinted-fields) (vec interfaces) methods)
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

(defn find-protocol-impl [protocol x]
  (if (and (:on protocol) (instance? (:on protocol) x))
    x
    (let [t (type x)
          c (class x)
          impl #(get (:impls protocol) %)]
      (or (impl t)
          (impl c)
          ;todo - better path,  how to prioritize supers vs interfaces?
          (first (remove nil? (map impl (supers c))))))))

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
      (or (and (:on protocol) (instance? (:on protocol) x))
          (find-protocol-impl protocol x))
    true))

(defn -cache-protocol-fn [#^clojure.lang.Box cache-box x]
  (let [#^clojure.lang.MethodImplCache cache  (.val cache-box)
        f (find-protocol-method (.protocol cache) (.methodk cache) x)]
    (when-not f
      (throw (IllegalArgumentException. (str "No implementation of method: " (.methodk cache) 
                                             " of protocol: " (:var (.protocol cache)) 
                                             " found for class: " (.getName (class x))))))
    (set! (.val cache-box) (expand-method-impl-cache cache (class x) f))
    f))

(defn- emit-method-builder [on-interface method on-method arglists]
  (let [methodk (keyword method)
        gcache-box (with-meta (gensym "gcache-box__") {:tag 'clojure.lang.Box})]
    `(fn [~gcache-box]
       (fn 
         ~@(map 
            (fn [args]
              (let [gargs (map #(gensym (str "g__" % "__")) args)
                    target (first gargs)]
                `([~@gargs]
                    (~@(if on-interface
                         `(if (instance? ~on-interface ~target)
                            (. ~(with-meta target {:tag on-interface})  ~(or on-method method) ~@(rest gargs)))
                         `(do))
                     (let [#^clojure.lang.MethodImplCache cache#  (.val ~gcache-box)
                           c# (class ~target)
                           f# (or (.fnFor cache# c#)
                                  (-cache-protocol-fn ~gcache-box ~target))]
                       (f# ~@gargs))))))
            arglists)))))

(defn -reset-methods [protocol]
  (doseq [[#^clojure.lang.Var v build] (:method-builders protocol)]
    (let [cache-box (clojure.lang.Box. (clojure.lang.MethodImplCache. protocol (keyword (.sym v))))]
      (.bindRoot v (build cache-box)))))

(defn- emit-protocol [name opts+sigs]
  (let [[opts sigs]
        (loop [opts {} sigs opts+sigs]
          (condp #(%1 %2) (first sigs) 
            string? (recur (assoc opts :doc (first sigs)) (next sigs))
            keyword? (recur (assoc opts (first sigs) (second sigs)) (nnext sigs))
            [opts sigs]))
        sigs (reduce (fn [m s]
                       (let [mname (with-meta (first s) nil)
                             arglists (if (vector? (second s)) (list (second s)) (second s))
                             fx (nth s 2 nil)
                             doc (when (string? fx) fx)
                             mopts (apply hash-map (nthnext s (if (string? fx) 3 2)))]
                         (when (some #{0} (map count arglists))
                           (throw (IllegalArgumentException. (str "Protocol fn: " mname " must take at least one arg"))))
                         (assoc m (keyword mname) 
                                (merge mopts 
                                       {:name (vary-meta mname assoc :doc doc :arglists arglists)
                                        :arglists arglists
                                        :doc doc}))))
                     {} sigs)]
  `(do
     (defonce ~name {})
     (alter-meta! (var ~name) assoc :doc  ~(:doc opts))
     (alter-var-root (var ~name) merge 
                     (assoc ~opts 
                       :sigs '~sigs 
                       :var (var ~name)
                       :method-builders 
                        ~(apply hash-map 
                                (mapcat 
                                 (fn [s] 
                                   [`(intern *ns* '~(:name s)) 
                                    (emit-method-builder (:on opts) (:name s) (:on s) (:arglists s))])
                                 (vals sigs)))))
     (-reset-methods ~name)
     '~name)))

(defmacro defprotocol 
  "A protocol is a named set of named methods and their signatures:
  (defprotocol AProtocolName

  ;optional :on interface  
  :on AnInterface

  ;optional doc string
  \"A doc string for AProtocol abstraction\"

  ;method signatures
  (bar [a b] \"bar docs\" :on barMethod)
  (baz ([a] [a b] [a b & c]) \"baz docs\"))

  No implementations are provided. Docs can be specified for the
  protocol overall and for each method. The above yields a set of
  polymorphic functions and a protocol object. All are
  namespace-qualified by the ns enclosing the definition The resulting
  functions dispatch on the type of their first argument, and thus
  must have at least one argument. defprotocol is dynamic, has no
  special compile-time effect, and defines no new types or classes
  Implementations of the protocol methods can be provided using
  extend.

  If an :on interface is provided, the protocol will have a default
  mapping to the methods of the specified interface. The interface
  must exist, it is not defined by defprotocol. By default, methods of
  the protocol map to methods of the same name in the interface. A
  different mapping can be provided on a per-method basis using :on
  inside the signature. Note that if a protocol method is called with
  an instance of the :on interface, the interface will be used,
  regardless of any extends clauses that might otherwise apply to the
  object. The use of an :on interface in no way precludes extending
  the protocol to other interfaces using extend."

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

  Note that multiple independent extend clauses can exist for the same
  type, not all protocols need be defined in a single extend call.

  See also:
  extends?, satisfies?, extenders"

  [atype & proto+mmaps]
  (doseq [[proto mmap] (partition 2 proto+mmaps)]
    (-reset-methods (alter-var-root (:var proto) assoc-in [:impls atype] mmap))))
