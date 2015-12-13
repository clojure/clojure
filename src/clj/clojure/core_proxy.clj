;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; proxy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import
 '(clojure.asm ClassWriter ClassVisitor Opcodes Type) 
 '(java.lang.reflect Modifier Constructor)
 '(clojure.asm.commons Method GeneratorAdapter)
 '(clojure.lang IProxy Reflector DynamicClassLoader IPersistentMap PersistentHashMap RT))

(defn method-sig [^java.lang.reflect.Method meth]
  [(. meth (getName)) (seq (. meth (getParameterTypes))) (. meth getReturnType)])

(defn- most-specific [rtypes]
  (or (some (fn [t] (when (every? #(isa? t %) rtypes) t)) rtypes)
    (throw (Exception. "Incompatible return types"))))

(defn- group-by-sig [coll]
 "takes a collection of [msig meth] and returns a seq of maps from return-types to meths."
  (vals (reduce1 (fn [m [msig meth]]
                  (let [rtype (peek msig)
                        argsig (pop msig)]
                    (assoc m argsig (assoc (m argsig {}) rtype meth))))
          {} coll)))

(defn proxy-name
 {:tag String} 
 [^Class super interfaces]
  (let [inames (into1 (sorted-set) (map #(.getName ^Class %) interfaces))]
    (apply str (.replace (str *ns*) \- \_) ".proxy"
      (interleave (repeat "$")
        (concat
          [(.getName super)]
          (map #(subs % (inc (.lastIndexOf ^String % "."))) inames)
          [(Integer/toHexString (hash inames))])))))

(defn- generate-proxy [^Class super interfaces]
  (let [cv (new ClassWriter (. ClassWriter COMPUTE_MAXS))
        cname (.replace (proxy-name super interfaces) \. \/) ;(str "clojure/lang/" (gensym "Proxy__"))
        ctype (. Type (getObjectType cname))
        iname (fn [^Class c] (.. Type (getType c) (getInternalName)))
        fmap "__clojureFnMap"
        totype (fn [^Class c] (. Type (getType c)))
        to-types (fn [cs] (if (pos? (count cs))
                            (into-array (map totype cs))
                            (make-array Type 0)))
        super-type ^Type (totype super)
        imap-type ^Type (totype IPersistentMap)
        ifn-type (totype clojure.lang.IFn)
        obj-type (totype Object)
        sym-type (totype clojure.lang.Symbol)
        rt-type  (totype clojure.lang.RT)
        ex-type  (totype java.lang.UnsupportedOperationException)
        gen-bridge 
        (fn [^java.lang.reflect.Method meth ^java.lang.reflect.Method dest]
            (let [pclasses (. meth (getParameterTypes))
                  ptypes (to-types pclasses)
                  rtype ^Type (totype (. meth (getReturnType)))
                  m (new Method (. meth (getName)) rtype ptypes)
                  dtype (totype (.getDeclaringClass dest))
                  dm (new Method (. dest (getName)) (totype (. dest (getReturnType))) (to-types (. dest (getParameterTypes))))
                  gen (new GeneratorAdapter (bit-or (. Opcodes ACC_PUBLIC) (. Opcodes ACC_BRIDGE)) m nil nil cv)]
              (. gen (visitCode))
              (. gen (loadThis))
              (dotimes [i (count ptypes)]
                  (. gen (loadArg i)))
              (if (-> dest .getDeclaringClass .isInterface)
                (. gen (invokeInterface dtype dm))
                (. gen (invokeVirtual dtype dm)))
              (. gen (returnValue))
              (. gen (endMethod))))
        gen-method
        (fn [^java.lang.reflect.Method meth else-gen]
            (let [pclasses (. meth (getParameterTypes))
                  ptypes (to-types pclasses)
                  rtype ^Type (totype (. meth (getReturnType)))
                  m (new Method (. meth (getName)) rtype ptypes)
                  gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)
                  else-label (. gen (newLabel))
                  end-label (. gen (newLabel))
                  decl-type (. Type (getType (. meth (getDeclaringClass))))]
              (. gen (visitCode))
              (if (> (count pclasses) 18)
                (else-gen gen m)
                (do
                  (. gen (loadThis))
                  (. gen (getField ctype fmap imap-type))
                  
                  (. gen (push (. meth (getName))))
                                        ;lookup fn in map
                  (. gen (invokeStatic rt-type (. Method (getMethod "Object get(Object, Object)"))))
                  (. gen (dup))
                  (. gen (ifNull else-label))
                                        ;if found
                  (.checkCast gen ifn-type)
                  (. gen (loadThis))
                                        ;box args
                  (dotimes [i (count ptypes)]
                      (. gen (loadArg i))
                    (. clojure.lang.Compiler$HostExpr (emitBoxReturn nil gen (nth pclasses i))))
                                        ;call fn
                  (. gen (invokeInterface ifn-type (new Method "invoke" obj-type 
                                                        (into-array (cons obj-type 
                                                                          (replicate (count ptypes) obj-type))))))
                                        ;unbox return
                  (. gen (unbox rtype))
                  (when (= (. rtype (getSort)) (. Type VOID))
                    (. gen (pop)))
                  (. gen (goTo end-label))
                  
                                        ;else call supplied alternative generator
                  (. gen (mark else-label))
                  (. gen (pop))
                  
                  (else-gen gen m)
                  
                  (. gen (mark end-label))))
              (. gen (returnValue))
              (. gen (endMethod))))]
    
                                        ;start class definition
    (. cv (visit (. Opcodes V1_5) (+ (. Opcodes ACC_PUBLIC) (. Opcodes ACC_SUPER))
                 cname nil (iname super) 
                 (into-array (map iname (cons IProxy interfaces)))))
                                        ;add field for fn mappings
    (. cv (visitField (+ (. Opcodes ACC_PRIVATE) (. Opcodes ACC_VOLATILE))
                      fmap (. imap-type (getDescriptor)) nil nil))          
                                        ;add ctors matching/calling super's
    (doseq [^Constructor ctor (. super (getDeclaredConstructors))]
        (when-not (. Modifier (isPrivate (. ctor (getModifiers))))
          (let [ptypes (to-types (. ctor (getParameterTypes)))
                m (new Method "<init>" (. Type VOID_TYPE) ptypes)
                gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)]
            (. gen (visitCode))
                                        ;call super ctor
            (. gen (loadThis))
            (. gen (dup))
            (. gen (loadArgs))
            (. gen (invokeConstructor super-type m))
            
            (. gen (returnValue))
            (. gen (endMethod)))))
                                        ;add IProxy methods
    (let [m (. Method (getMethod "void __initClojureFnMappings(clojure.lang.IPersistentMap)"))
          gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)]
      (. gen (visitCode))
      (. gen (loadThis))
      (. gen (loadArgs))
      (. gen (putField ctype fmap imap-type))
      
      (. gen (returnValue))
      (. gen (endMethod)))
    (let [m (. Method (getMethod "void __updateClojureFnMappings(clojure.lang.IPersistentMap)"))
          gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)]
      (. gen (visitCode))
      (. gen (loadThis))
      (. gen (dup))
      (. gen (getField ctype fmap imap-type))
      (.checkCast gen (totype clojure.lang.IPersistentCollection))
      (. gen (loadArgs))
      (. gen (invokeInterface (totype clojure.lang.IPersistentCollection)
                              (. Method (getMethod "clojure.lang.IPersistentCollection cons(Object)"))))
      (. gen (checkCast imap-type))
      (. gen (putField ctype fmap imap-type))
      
      (. gen (returnValue))
      (. gen (endMethod)))
    (let [m (. Method (getMethod "clojure.lang.IPersistentMap __getClojureFnMappings()"))
          gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)]
      (. gen (visitCode))
      (. gen (loadThis))
      (. gen (getField ctype fmap imap-type))
      (. gen (returnValue))
      (. gen (endMethod)))
    
                                        ;calc set of supers' non-private instance methods
    (let [[mm considered]
            (loop [mm {} considered #{} c super]
              (if c
                (let [[mm considered]
                      (loop [mm mm 
                             considered considered 
                             meths (concat 
                                    (seq (. c (getDeclaredMethods)))
                                    (seq (. c (getMethods))))]
                        (if (seq meths)
                          (let [^java.lang.reflect.Method meth (first meths)
                                mods (. meth (getModifiers))
                                mk (method-sig meth)]
                            (if (or (considered mk)
                                    (not (or (Modifier/isPublic mods) (Modifier/isProtected mods)))
                                    ;(. Modifier (isPrivate mods)) 
                                    (. Modifier (isStatic mods))
                                    (. Modifier (isFinal mods))
                                    (= "finalize" (.getName meth)))
                              (recur mm (conj considered mk) (next meths))
                              (recur (assoc mm mk meth) (conj considered mk) (next meths))))
                          [mm considered]))]
                  (recur mm considered (. c (getSuperclass))))
                [mm considered]))
          ifaces-meths (into1 {} 
                         (for [^Class iface interfaces meth (. iface (getMethods))
                               :let [msig (method-sig meth)] :when (not (considered msig))]
                           {msig meth}))
          ;; Treat abstract methods as interface methods
          [mm ifaces-meths] (let [abstract? (fn [[_ ^Method meth]]
                                              (Modifier/isAbstract (. meth (getModifiers))))
                                  mm-no-abstract (remove abstract? mm)
                                  abstract-meths (filter abstract? mm)]
                              [mm-no-abstract (concat ifaces-meths abstract-meths)])
          mgroups (group-by-sig (concat mm ifaces-meths))
          rtypes (map #(most-specific (keys %)) mgroups)
          mb (map #(vector (%1 %2) (vals (dissoc %1 %2))) mgroups rtypes)
          bridge? (reduce1 into1 #{} (map second mb))
          ifaces-meths (remove bridge? (vals ifaces-meths))
          mm (remove bridge? (vals mm))]
                                        ;add methods matching supers', if no mapping -> call super
      (doseq [[^java.lang.reflect.Method dest bridges] mb
              ^java.lang.reflect.Method meth bridges]
          (gen-bridge meth dest))
      (doseq [^java.lang.reflect.Method meth mm]
          (gen-method meth 
                      (fn [^GeneratorAdapter gen ^Method m]
                          (. gen (loadThis))
                                        ;push args
                        (. gen (loadArgs))
                                        ;call super
                        (. gen (visitMethodInsn (. Opcodes INVOKESPECIAL) 
                                                (. super-type (getInternalName))
                                                (. m (getName))
                                                (. m (getDescriptor)))))))
      
                                        ;add methods matching interfaces', if no mapping -> throw
      (doseq [^java.lang.reflect.Method meth ifaces-meths]
                (gen-method meth 
                            (fn [^GeneratorAdapter gen ^Method m]
                                (. gen (throwException ex-type (. m (getName))))))))
    
                                        ;finish class def
    (. cv (visitEnd))
    [cname (. cv toByteArray)]))

(defn- get-super-and-interfaces [bases]
  (if (. ^Class (first bases) (isInterface))
    [Object bases]
    [(first bases) (next bases)]))

(defn get-proxy-class 
  "Takes an optional single class followed by zero or more
  interfaces. If not supplied class defaults to Object.  Creates an
  returns an instance of a proxy class derived from the supplied
  classes. The resulting value is cached and used for any subsequent
  requests for the same class set. Returns a Class object."
  {:added "1.0"}
  [& bases]
    (let [[super interfaces] (get-super-and-interfaces bases)
          pname (proxy-name super interfaces)]
      (or (RT/loadClassForName pname)
          (let [[cname bytecode] (generate-proxy super interfaces)]
            (. ^DynamicClassLoader (deref clojure.lang.Compiler/LOADER) (defineClass pname bytecode [super interfaces]))))))

(defn construct-proxy
  "Takes a proxy class and any arguments for its superclass ctor and
  creates and returns an instance of the proxy."
  {:added "1.0"}
  [c & ctor-args]
    (. Reflector (invokeConstructor c (to-array ctor-args))))

(defn init-proxy
  "Takes a proxy instance and a map of strings (which must
  correspond to methods of the proxy superclass/superinterfaces) to
  fns (which must take arguments matching the corresponding method,
  plus an additional (explicit) first arg corresponding to this, and
  sets the proxy's fn map.  Returns the proxy."
  {:added "1.0"}
  [^IProxy proxy mappings]
    (. proxy (__initClojureFnMappings mappings))
    proxy)

(defn update-proxy
  "Takes a proxy instance and a map of strings (which must
  correspond to methods of the proxy superclass/superinterfaces) to
  fns (which must take arguments matching the corresponding method,
  plus an additional (explicit) first arg corresponding to this, and
  updates (via assoc) the proxy's fn map. nil can be passed instead of
  a fn, in which case the corresponding method will revert to the
  default behavior. Note that this function can be used to update the
  behavior of an existing instance without changing its identity.
  Returns the proxy."
  {:added "1.0"}
  [^IProxy proxy mappings]
    (. proxy (__updateClojureFnMappings mappings))
    proxy)

(defn proxy-mappings
  "Takes a proxy instance and returns the proxy's fn map."
  {:added "1.0"}
  [^IProxy proxy]
    (. proxy (__getClojureFnMappings)))

(defmacro proxy
  "class-and-interfaces - a vector of class names

  args - a (possibly empty) vector of arguments to the superclass
  constructor.

  f => (name [params*] body) or
  (name ([params*] body) ([params+] body) ...)

  Expands to code which creates a instance of a proxy class that
  implements the named class/interface(s) by calling the supplied
  fns. A single class, if provided, must be first. If not provided it
  defaults to Object.

  The interfaces names must be valid interface types. If a method fn
  is not provided for a class method, the superclass methd will be
  called. If a method fn is not provided for an interface method, an
  UnsupportedOperationException will be thrown should it be
  called. Method fns are closures and can capture the environment in
  which proxy is called. Each method fn takes an additional implicit
  first arg, which is bound to 'this. Note that while method fns can
  be provided to override protected methods, they have no other access
  to protected members, nor to super, as these capabilities cannot be
  proxied."
  {:added "1.0"}
  [class-and-interfaces args & fs]
   (let [bases (map #(or (resolve %) (throw (Exception. (str "Can't resolve: " %)))) 
                    class-and-interfaces)
         [super interfaces] (get-super-and-interfaces bases)
         compile-effect (when *compile-files*
                          (let [[cname bytecode] (generate-proxy super interfaces)]
                            (clojure.lang.Compiler/writeClassFile cname bytecode)))
         pc-effect (apply get-proxy-class bases)
         pname (proxy-name super interfaces)]
     ;remember the class to prevent it from disappearing before use
     (intern *ns* (symbol pname) pc-effect)
     `(let [;pc# (get-proxy-class ~@class-and-interfaces)
            p# (new ~(symbol pname) ~@args)] ;(construct-proxy pc# ~@args)]   
        (init-proxy p#
         ~(loop [fmap {} fs fs]
            (if fs
              (let [[sym & meths] (first fs)
                    meths (if (vector? (first meths))
                            (list meths)
                            meths)
                    meths (map (fn [[params & body]]
                                   (cons (apply vector 'this params) body))
                               meths)]
                (if-not (contains? fmap (name sym))		  
                (recur (assoc fmap (name sym) (cons `fn meths)) (next fs))
		           (throw (IllegalArgumentException.
			              (str "Method '" (name sym) "' redefined")))))
              fmap)))
        p#)))

(defn proxy-call-with-super [call this meth]
 (let [m (proxy-mappings this)]
    (update-proxy this (assoc m meth nil))
    (try
      (call)
      (finally (update-proxy this m)))))

(defmacro proxy-super 
  "Use to call a superclass method in the body of a proxy method. 
  Note, expansion captures 'this"
  {:added "1.0"}
  [meth & args]
 `(proxy-call-with-super (fn [] (. ~'this ~meth ~@args))  ~'this ~(name meth)))

(defn bean
  "Takes a Java object and returns a read-only implementation of the
  map abstraction based upon its JavaBean properties."
  {:added "1.0"}
  [^Object x]
  (let [c (. x (getClass))
	pmap (reduce1 (fn [m ^java.beans.PropertyDescriptor pd]
			 (let [name (. pd (getName))
			       method (. pd (getReadMethod))]
			   (if (and method (zero? (alength (. method (getParameterTypes)))))
			     (assoc m (keyword name) (fn [] (clojure.lang.Reflector/prepRet (.getPropertyType pd) (. method (invoke x nil)))))
			     m)))
		     {}
		     (seq (.. java.beans.Introspector
			      (getBeanInfo c)
			      (getPropertyDescriptors))))
	v (fn [k] ((pmap k)))
        snapshot (fn []
                   (reduce1 (fn [m e]
                             (assoc m (key e) ((val e))))
                           {} (seq pmap)))]
    (proxy [clojure.lang.APersistentMap]
           []
      (iterator [] (.iterator ^Iterable pmap))
      (containsKey [k] (contains? pmap k))
      (entryAt [k] (when (contains? pmap k) (clojure.lang.MapEntry/create k (v k))))
      (valAt ([k] (when (contains? pmap k) (v k)))
	     ([k default] (if (contains? pmap k) (v k) default)))
      (cons [m] (conj (snapshot) m))
      (count [] (count pmap))
      (assoc [k v] (assoc (snapshot) k v))
      (without [k] (dissoc (snapshot) k))
      (seq [] ((fn thisfn [plseq]
		  (lazy-seq
                   (when-let [pseq (seq plseq)]
                     (cons (clojure.lang.MapEntry/create (first pseq) (v (first pseq)))
                           (thisfn (rest pseq)))))) (keys pmap))))))



