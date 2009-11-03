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

  A constructor will be defined, taking the designated fields followed
  by a metadata map (nil for none) and an extension field map (nil for
  none). In the method bodies, the (unqualified) name can be used
  to name the class (for calls to new etc).

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
