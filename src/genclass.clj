;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(import '(java.lang.reflect Modifier Constructor)
        '(clojure.asm ClassWriter ClassVisitor Opcodes Type)
        '(clojure.asm.commons Method GeneratorAdapter)
        '(clojure.lang IPersistentMap))

(defn method-sig [#^java.lang.reflect.Method meth]
  [(. meth (getName)) (seq (. meth (getParameterTypes)))])

(defn non-private-methods [#^Class c]
  (loop [mm {}
         considered #{}
         c c]
    (if c
      (let [[mm considered]
            (loop [mm mm
                   considered considered
                   meths (concat
                          (seq (. c (getDeclaredMethods)))
                          (seq (. c (getMethods))))]
              (if meths
                (let [#^Method meth (first meths)
                      mods (. meth (getModifiers))
                      mk (method-sig meth)]
                  (if (or (considered mk)
                          (. Modifier (isPrivate mods))
                          (. Modifier (isStatic mods))
                          (. Modifier (isFinal mods)))
                    (recur mm (conj considered mk) (rest meths))
                    (recur (assoc mm mk meth) (conj considered mk) (rest meths))))
                [mm considered]))]
        (recur mm considered (. c (getSuperclass))))
      mm)))

(defn ctor-sigs [super]
  (for [#^Constructor ctor (. super (getDeclaredConstructors))
        :when (not (. Modifier (isPrivate (. ctor (getModifiers)))))]
    (apply vector (. ctor (getParameterTypes)))))

(distinct (map first(keys (mapcat non-private-methods [Object IPersistentMap]))))

(defn gen-class [name & options]
  (let [{:keys [extends implements constructors methods main factory state init exposes]} (apply hash-map options)
        super (or extends Object)
        interfaces implements
        supers (cons super (seq interfaces))
        ctor-sig-map (or constructors (zipmap (ctor-sigs super) (ctor-sigs super)))
        cv (new ClassWriter (. ClassWriter COMPUTE_MAXS))
        cname (. name (replace "." "/"))
        ctype (. Type (getObjectType cname))
        iname (fn [c] (.. Type (getType c) (getInternalName)))
        totype (fn [c] (. Type (getType c)))
        to-types (fn [cs] (if (pos? (count cs))
                            (into-array (map totype cs))
                            (make-array Type 0)))
        obj-type (totype Object)
        arg-types (fn [n] (if (pos? n)
                            (into-array (replicate n obj-type))
                            (make-array Type 0)))
        super-type (totype super)
        init-name (str init)
        factory-name (str factory)
        state-name (str state)
        main-name "main"
        var-name (fn [s] (str s "__var"))
        rt-type  (totype clojure.lang.RT)
        var-type  (totype clojure.lang.Var)
        ifn-type (totype clojure.lang.IFn)
        ex-type  (totype java.lang.UnsupportedOperationException)
        var-fields (concat (and init [init-name]) 
                           (and main [main-name])
                           (distinct (concat (map first (keys (mapcat non-private-methods supers)))
                                             (keys methods))))
        emit-get-var (fn [gen v]
                       (let [false-label (. gen newLabel)
                             end-label (. gen newLabel)]
                         (. gen getStatic ctype (var-name v) var-type)
                         (. gen dup)
                         (. gen invokeVirtual var-type (. Method (getMethod "boolean isBound()")))
                         (. gen ifZCmp (GeneratorAdapter.EQ) false-label)
                         (. gen invokeVirtual var-type (. Method (getMethod "Object get()")))
                         (. gen goTo end-label)
                         (. gen mark false-label)
                         (. gen pop)
                         (. gen visitInsn (Opcodes.ACONST_NULL))
                         (. gen mark end-label)))
        emit-forwarding-method
        (fn [#^java.lang.reflect.Method meth else-gen]
          (let [pclasses (. meth (getParameterTypes))
                ptypes (to-types pclasses)
                rtype (totype (. meth (getReturnType)))
                m (new Method (. meth (getName)) rtype ptypes)
                gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)
                else-label (. gen (newLabel))
                end-label (. gen (newLabel))
                                        ;decl-type (. Type (getType (. meth (getDeclaringClass))))
                ]
            (. gen (visitCode))
            (emit-get-var gen (. meth (getName)))
            (. gen (dup))
            (. gen (ifNull else-label))
                                        ;if found
            (. gen (loadThis))
                                        ;box args
            (dotimes i (count ptypes)
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
            (. gen (endMethod))))
        ]
                                        ;start class definition
    (. cv (visit (. Opcodes V1_5) (. Opcodes ACC_PUBLIC)
                 cname nil (iname super)
                 (when interfaces
                   (into-array (map iname interfaces)))))
    
                                        ;static fields for vars
    (doseq v var-fields
      (. cv (visitField (+ (Opcodes.ACC_PUBLIC) (Opcodes.ACC_FINAL) (Opcodes.ACC_STATIC))
                        (var-name v) 
                        (. var-type getDescriptor)
                        nil nil)))
    
                                        ;instance field for state
    (when state
      (. cv (visitField (+ (Opcodes.ACC_PUBLIC) (Opcodes.ACC_FINAL))
                        state-name 
                        (. obj-type getDescriptor)
                        nil nil)))
    
                                        ;static init to set up var fields
    (let [gen (new GeneratorAdapter (+ (Opcodes.ACC_PUBLIC) (Opcodes.ACC_STATIC)) 
                   (Method.getMethod "void <clinit> ()")
                   nil nil cv)]
      (. gen (visitCode))
      (doseq v var-fields
        (. gen push name)
        (. gen push v)
        (. gen (invokeStatic rt-type (. Method (getMethod "clojure.lang.Var var(String,String)"))))
        (. gen putStatic ctype (var-name v) var-type))              
      (. gen (returnValue))
      (. gen (endMethod)))
    
                                        ;ctors
    (doseq [pclasses super-pclasses] ctor-sig-map
      (let [ptypes (to-types pclasses)
            super-ptypes (to-types super-pclasses)
            m (new Method "<init>" (. Type VOID_TYPE) ptypes)
            super-m (new Method "<init>" (. Type VOID_TYPE) super-ptypes)
            gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)
            no-init-label (. gen newLabel)
            end-label (. gen newLabel)
            nth-method (. Method (getMethod "Object nth(Object,int)"))
            local (. gen newLocal obj-type)]
        (. gen (visitCode))
        
        (if init
          (do
            (emit-get-var gen init-name)
            (. gen dup)
            (. gen ifNull no-init-label)
                                        ;box init args
            (dotimes i (count pclasses)
              (. gen (loadArg i))
              (. clojure.lang.Compiler$HostExpr (emitBoxReturn nil gen (nth pclasses i))))
                                        ;call init fn
            (. gen (invokeInterface ifn-type (new Method "invoke" obj-type 
                                                  (arg-types (count ptypes)))))
                                        ;expecting [[super-ctor-args] state] returned
            (. gen dup)
            (. gen push 0)
            (. gen (invokeStatic rt-type nth-method))
            (. gen storeLocal local)
            
            (. gen (loadThis))
            (. gen dupX1)
            (dotimes i (count super-pclasses)
              (. gen loadLocal local)
              (. gen push i)
              (. gen (invokeStatic rt-type nth-method))
              (. clojure.lang.Compiler$HostExpr (emitUnboxArg nil gen (nth super-pclasses i))))
            (. gen (invokeConstructor super-type super-m))
            
            (if state
              (do
                (. gen push 1)
                (. gen (invokeStatic rt-type nth-method))
                (. gen (putField ctype state-name obj-type)))
              (. gen pop))
            
            (. gen goTo end-label)
                                        ;no init found
            (. gen mark no-init-label)
            (. gen (throwException ex-type (str init-name " not defined")))
            (. gen mark end-label))
          (if (= pclasses super-pclasses)
            (do
              (. gen (loadThis))
              (. gen (loadArgs))
              (. gen (invokeConstructor super-type super-m)))
            (throw (new Exception ":init not specified, but ctor and super ctor args differ"))))

        (. gen (returnValue))
        (. gen (endMethod))))
    
                                        ;add methods matching supers', if no fn -> call super
    (let [mm (non-private-methods super)]
      (doseq #^java.lang.reflect.Method meth (vals mm)
             (emit-forwarding-method meth 
                                     (fn [gen m]
                                       (. gen (loadThis))
                                        ;push args
                                       (. gen (loadArgs))
                                        ;call super
                                       (. gen (visitMethodInsn (. Opcodes INVOKESPECIAL) 
                                                               (. super-type (getInternalName))
                                                               (. m (getName))
                                                               (. m (getDescriptor)))))))
                                        ;add methods matching interfaces', if no fn -> throw
       (doseq #^Class iface interfaces
              (doseq #^java.lang.reflect.Method meth (. iface (getMethods))
                     (when-not (contains? mm (method-sig meth))
                       (emit-forwarding-method meth 
                                               (fn [gen m]
                                                 (. gen (throwException ex-type (. m (getName))))))))))
    
                                        ;finish class def
    (. cv (visitEnd))
    {:name name :bytecode (. cv (toByteArray))}))

(comment

(gen-class 
 package-qualified-name
  ;all below are optional
 :extends aclass
 :implements [interface ...]
 :constructors {[param-types] [super-param-types], ...}
 :methods {name [return-type [param-types]], ...}
 :main boolean
 :factory name
 :state name
 :init name
 :exposes {protected-field {:get name :set name}, ...})

(let [{:keys [name bytecode]} 
      (gen-class (str (gensym "fred.lucy.Ethel__")) 
                 :implements [IPersistentMap]
                 :state state)];{[Object] []})]
  (.. clojure.lang.RT ROOT_CLASSLOADER (defineClass name bytecode)))

(def ethel (new fred.lucy.Ethel__2174))

(in-ns 'fred.lucy.Ethel__2102)
(clojure/refer 'clojure :exclude '(assoc seq count cons))
(defn __init [n] [[] n])
(in-ns 'user)
)

(gen-class org.clojure.MyComparator :implements [Comparator])
(in-ns 'org.clojure.MyComparator)
(defn compare [this x y] ...)
