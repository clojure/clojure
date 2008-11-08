;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure.core)

(import
 '(clojure.asm ClassWriter ClassVisitor Opcodes Type) 
 '(java.lang.reflect Modifier Constructor)
 '(clojure.asm.commons Method GeneratorAdapter)
 '(clojure.lang IProxy Reflector DynamicClassLoader IPersistentMap PersistentHashMap RT))

(def *proxy-classes* (ref {}))

(defn method-sig [#^java.lang.reflect.Method meth]
  [(. meth (getName)) (seq (. meth (getParameterTypes))) (. meth getReturnType)])

(defn get-proxy-class 
  "Takes an optional single class followed by zero or more
  interfaces. If not supplied class defaults to Object.  Creates an
  returns an instance of a proxy class derived from the supplied
  classes. The resulting value is cached and used for any subsequent
  requests for the same class set. Returns a Class object."  
  [& bases]
    (let [bases (if (. (first bases) (isInterface))
                  (cons Object bases)
                  bases)
          [super & interfaces] bases]
      (or (get @*proxy-classes* bases)
          (let [cv (new ClassWriter (. ClassWriter COMPUTE_MAXS))
                cname (str "clojure/lang/" (gensym "Proxy__"))
                ctype (. Type (getObjectType cname))
                iname (fn [c] (.. Type (getType c) (getInternalName)))
                fmap "__clojureFnMap"
                totype (fn [c] (. Type (getType c)))
                to-types (fn [cs] (if (pos? (count cs))
                                    (into-array (map totype cs))
                                    (make-array Type 0)))
                super-type (totype super)
                map-type (totype PersistentHashMap)
                ifn-type (totype clojure.lang.IFn)
                obj-type (totype Object)
                sym-type (totype clojure.lang.Symbol)
                rt-type  (totype clojure.lang.RT)
                ex-type  (totype java.lang.UnsupportedOperationException)
                gen-method
                (fn [#^java.lang.reflect.Method meth else-gen]
                  (let [pclasses (. meth (getParameterTypes))
                        ptypes (to-types pclasses)
                        rtype (totype (. meth (getReturnType)))
                        m (new Method (. meth (getName)) rtype ptypes)
                        gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)
                        else-label (. gen (newLabel))
                        end-label (. gen (newLabel))
                        decl-type (. Type (getType (. meth (getDeclaringClass))))]
                    (. gen (visitCode))
                    (. gen (loadThis))
                    (. gen (getField ctype fmap map-type))
                                        ;get symbol corresponding to name
                    (. gen (push (. meth (getName))))
                    (. gen (invokeStatic sym-type (. Method (getMethod "clojure.lang.Symbol create(String)"))))
                                        ;lookup fn in map
                    (. gen (invokeStatic rt-type (. Method (getMethod "Object get(Object, Object)"))))
                    (. gen (dup))
                    (. gen (ifNull else-label))
                                        ;if found
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
                    
                    (. gen (mark end-label))
                    (. gen (returnValue))
                    (. gen (endMethod))))]
            
                                        ;start class definition
            (. cv (visit (. Opcodes V1_5) (+ (. Opcodes ACC_PUBLIC) (. Opcodes ACC_SUPER))
                         cname nil (iname super) 
                         (into-array (map iname (cons IProxy interfaces)))))
                                        ;add field for fn mappings
            (. cv (visitField (+ (. Opcodes ACC_PRIVATE) (. Opcodes ACC_VOLATILE))
                              fmap (. map-type (getDescriptor)) nil nil))          
                                        ;add ctors matching/calling super's
            (doseq [#^Constructor ctor (. super (getDeclaredConstructors))]
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
                                        ;init fmap
                  (. gen (getStatic map-type "EMPTY" map-type))
                  (. gen (putField ctype fmap map-type))
                  
                  (. gen (returnValue))
                  (. gen (endMethod)))))
                                        ;add IProxy methods
            (let [m (. Method (getMethod "void __updateClojureFnMappings(clojure.lang.IPersistentMap)"))
                  gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)]
              (. gen (visitCode))
              (. gen (loadThis))
              (. gen (dup))
              (. gen (getField ctype fmap map-type))
              (. gen (loadArgs))
              (. gen (invokeInterface (totype clojure.lang.IPersistentCollection)
                                      (. Method (getMethod "clojure.lang.IPersistentCollection cons(Object)"))))
              (. gen (checkCast map-type))
              (. gen (putField ctype fmap map-type))
              
              (. gen (returnValue))
              (. gen (endMethod)))
            (let [m (. Method (getMethod "clojure.lang.IPersistentMap __getClojureFnMappings()"))
                  gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)]
              (. gen (visitCode))
              (. gen (loadThis))
              (. gen (getField ctype fmap map-type))
              (. gen (returnValue))
              (. gen (endMethod)))
            
                                        ;calc set of supers' non-private instance methods
            (let [mm (loop [mm {} considered #{} c super]
                       (if c
                         (let [[mm considered]
                               (loop [mm mm 
                                      considered considered 
                                      meths (concat 
                                             (seq (. c (getDeclaredMethods)))
                                             (seq (. c (getMethods))))]
                                 (if meths 
                                   (let [#^java.lang.reflect.Method meth (first meths)
                                         mods (. meth (getModifiers))
                                         mk (method-sig meth)]
                                     (if (or (considered mk)
                                             (. Modifier (isPrivate mods)) 
                                             (. Modifier (isStatic mods))
                                             (. Modifier (isFinal mods))
                                             (= "finalize" (.getName meth)))
                                       (recur mm (conj considered mk) (rest meths))
                                       (recur (assoc mm mk meth) (conj considered mk) (rest meths))))
                                   [mm considered]))]
                           (recur mm considered (. c (getSuperclass))))
                         mm))]
                                        ;add methods matching supers', if no mapping -> call super
              (doseq [#^java.lang.reflect.Method meth (vals mm)]
                     (gen-method meth 
                                 (fn [gen m]
                                   (. gen (loadThis))
                                        ;push args
                                   (. gen (loadArgs))
                                        ;call super
                                   (. gen (visitMethodInsn (. Opcodes INVOKESPECIAL) 
                                                           (. super-type (getInternalName))
                                                           (. m (getName))
                                                           (. m (getDescriptor)))))))
              
                                        ;add methods matching interfaces', if no mapping -> throw
              (doseq [#^Class iface interfaces]
                (doseq [#^java.lang.reflect.Method meth (. iface (getMethods))]
                   (when-not (contains? mm (method-sig meth))
                     (gen-method meth 
                                 (fn [gen m]
                                   (. gen (throwException ex-type (. m (getName))))))))))

                                        ;finish class def
            (. cv (visitEnd))
                                        ;generate, cache and return class object
            (let [loader (. RT ROOT_CLASSLOADER)
                  c (. loader (defineClass (. cname (replace "/" ".")) 
                                (. cv (toByteArray))))]
              (sync nil (commute *proxy-classes* assoc bases c))
              c)))))

(defn construct-proxy
  "Takes a proxy class and any arguments for its superclass ctor and
  creates and returns an instance of the proxy."  
  [c & ctor-args]
    (. Reflector (invokeConstructor c (to-array ctor-args))))

(defn update-proxy
  "Takes a proxy instance and a map of symbols (whose names must
  correspond to methods of the proxy superclass/superinterfaces) to
  fns (which must take arguments matching the corresponding method,
  plus an additional (explicit) first arg corresponding to this, and
  updates (via assoc) the proxy's fn map. nil can be passed instead of
  a fn, in which case the corresponding method will revert to the
  default behavior. Note that this function can be used to update the
  behavior of an existing instance without changing its identity."
  [#^IProxy proxy mappings]
    (. proxy (__updateClojureFnMappings mappings)))

(defn proxy-mappings
  "Takes a proxy instance and returns the proxy's fn map."
  [#^IProxy proxy]
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
  [class-and-interfaces args & fs]
  `(let [pc# (get-proxy-class ~@class-and-interfaces)
         p# (construct-proxy pc# ~@args)]   
     (update-proxy p#
       ~(loop [fmap {} fs fs]
          (if fs
            (let [[sym & meths] (first fs)
                  meths (if (vector? (first meths))
                          (list meths)
                          meths)
                  meths (map (fn [[params & body]]
                               (cons (apply vector 'this params) body))
                             meths)]
              (recur (assoc fmap (list `quote (symbol (name sym))) (cons `fn meths)) (rest fs)))
            fmap)))
     p#))

(defn proxy-call-with-super [call this meth]
 (let [m (proxy-mappings this)]
    (update-proxy this (assoc m meth nil))
    (let [ret (call)]
      (update-proxy this m)
      ret)))

(defmacro proxy-super 
  "Use to call a superclass method in the body of a proxy method. 
  Note, expansion captures 'this"
  [meth & args]
 `(proxy-call-with-super (fn [] (. ~'this ~meth ~@args))  ~'this '~(symbol (name meth))))

(defn bean
  "Takes a Java object and returns a read-only implementation of the
  map abstraction based upon its JavaBean properties."
  [#^Object x]
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
    (proxy [clojure.lang.APersistentMap]
           []
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
