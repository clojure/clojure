;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure)

(import
 '(clojure.asm ClassWriter ClassVisitor Opcodes Type) 
 '(java.lang.reflect Modifier Constructor)
 '(clojure.asm.commons Method GeneratorAdapter)
 '(clojure.lang IProxy Reflector DynamicClassLoader IPersistentMap PersistentHashMap))

(def *proxy-classes* (ref {}))

(defn get-proxy-class [& bases]
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
                (fn [meth else-gen]
                  (let [ptypes (to-types (. meth (getParameterTypes)))
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
                     (dotimes i (count ptypes)
                       (. gen (loadArg i))
                       (. gen (box (nth ptypes i))))
                                        ;call fn
                     (. gen (invokeInterface ifn-type (new Method "invoke" obj-type 
                                                           (into-array (cons obj-type 
                                                                             (replicate (count ptypes) obj-type))))))
                                        ;unbox return
                     (. gen (unbox rtype))
                     (. gen (goTo end-label))
                     
                                        ;else call supplied alternative generator
                     (. gen (mark else-label))
                     (. gen (pop))

                     (else-gen gen m)
                     
                     (. gen (mark end-label))
                     (. gen (returnValue))
                     (. gen (endMethod))))]

          ;start class definition
          (. cv (visit (. Opcodes V1_5) (. Opcodes ACC_PUBLIC) 
                       cname nil (iname super) 
                       (into-array (map iname (cons IProxy interfaces)))))
          ;add field for fn mappings
          (. cv (visitField (+ (. Opcodes ACC_PRIVATE) (. Opcodes ACC_VOLATILE))
                            fmap (. map-type (getDescriptor)) nil nil))          
          ;add ctors matching/calling super's
          (doseq #^Constructor ctor (. super (getDeclaredConstructors))
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
          (let [mm (loop [mm {} c super]
                     (if c
                       (recur
                        (loop [mm mm meths (seq (. super (getDeclaredMethods)))]
                          (if meths 
                            (let [#^java.lang.reflect.Method meth (first meths)
                                  mods (. meth (getModifiers))
                                  mk [(. meth (getName)) (seq (. meth (getParameterTypes)))]]
                              (if (or (contains? mm mk)
                                      (. Modifier (isPrivate mods)) 
                                      (. Modifier (isStatic mods))
                                      (. Modifier (isFinal mods)))
                                (recur mm (rest meths))
                                (recur (assoc mm mk meth) (rest meths))))
                            mm))
                        (. c (getSuperclass)))
                       mm))]
            ;add methods matching supers', if no mapping -> call super
            (doseq #^java.lang.reflect.Method meth (vals mm)
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
            (doseq #^Class iface interfaces
              (doseq #^java.lang.reflect.Method meth (. iface (getMethods))
                 (gen-method meth 
                             (fn [gen m]
                               (. gen (throwException ex-type (. m (getName)))))))))

          ;finish class def
          (. cv (visitEnd))
          ;generate, cache and return class object
          (let [loader (new DynamicClassLoader)
                c (. loader (defineClass (. cname (replace "/" ".")) 
                              (. cv (toByteArray))))]
            (sync nil (commute *proxy-classes* assoc bases c))
            c)))))

(defn construct-proxy [c & ctor-args]
  (. Reflector (invokeConstructor c (to-array ctor-args))))

(defn update-proxy [#^IProxy proxy mappings]
  (. proxy (__updateClojureFnMappings mappings)))

(defn proxy-mappings [#^IProxy proxy]
  (. proxy (__getClojureFnMappings)))