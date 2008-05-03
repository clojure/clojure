;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure)

(import '(java.lang.reflect Modifier Constructor)
        '(clojure.asm ClassWriter ClassVisitor Opcodes Type)
        '(clojure.asm.commons Method GeneratorAdapter)
        '(clojure.lang IPersistentMap))

;(defn method-sig [#^java.lang.reflect.Method meth]
;  [(. meth (getName)) (seq (. meth (getParameterTypes)))])

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

;(distinct (map first(keys (mapcat non-private-methods [Object IPersistentMap]))))

(defn gen-class [mname & options]
  (let [name (str mname)
        {:keys [extends implements constructors methods main factory state init exposes]} (apply hash-map options)
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
        iseq-type (totype clojure.lang.ISeq)
        ex-type  (totype java.lang.UnsupportedOperationException)
        var-fields (concat (and init [init-name]) 
                           (and main [main-name])
                           (distinct (concat (map first (keys (mapcat non-private-methods supers)))
                                             (map (comp str first) methods)
                                             (mapcat (comp (partial map str) vals val) exposes))))
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
        (fn [mname pclasses rclass else-gen]
          (let [ptypes (to-types pclasses)
                rtype (totype rclass)
                m (new Method mname rtype ptypes)
                gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)
                else-label (. gen (newLabel))
                end-label (. gen (newLabel))]
            (. gen (visitCode))
            (emit-get-var gen mname)
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
    
                                        ;static init to set up var fields and load clj
    (let [gen (new GeneratorAdapter (+ (Opcodes.ACC_PUBLIC) (Opcodes.ACC_STATIC)) 
                   (Method.getMethod "void <clinit> ()")
                   nil nil cv)]
      (. gen (visitCode))
      (doseq v var-fields
        (. gen push name)
        (. gen push v)
        (. gen (invokeStatic rt-type (. Method (getMethod "clojure.lang.Var var(String,String)"))))
        (. gen putStatic ctype (var-name v) var-type))
      
      (. gen push ctype)
      (. gen push (str (name.replace \. (java.io.File.separatorChar)) ".clj"))
      (. gen (invokeStatic rt-type (. Method (getMethod "void loadResourceScript(Class,String)"))))
      
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
        (. gen (endMethod))
                                        ;factory
        (when factory
          (let [fm (new Method factory-name ctype ptypes)
                gen (new GeneratorAdapter (+ (. Opcodes ACC_PUBLIC) (Opcodes.ACC_STATIC)) 
                         fm nil nil cv)]
            (. gen (visitCode))
            (. gen newInstance ctype)
            (. gen dup)
            (. gen (loadArgs))
            (. gen (invokeConstructor ctype m))            
            (. gen (returnValue))
            (. gen (endMethod))))))
    
                                        ;add methods matching supers', if no fn -> call super
    (let [mm (non-private-methods super)]
      (doseq #^java.lang.reflect.Method meth (vals mm)
             (emit-forwarding-method (.getName meth) (.getParameterTypes meth) (.getReturnType meth) 
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
                       (emit-forwarding-method (.getName meth) (.getParameterTypes meth) (.getReturnType meth) 
                                               (fn [gen m]
                                                 (. gen (throwException ex-type (. m (getName)))))))))
                                        ;extra methods
       (doseq [mname pclasses rclass :as msig] methods
         (emit-forwarding-method (str mname) pclasses rclass 
                                 (fn [gen m]
                                     (. gen (throwException ex-type (. m (getName))))))))

                                        ;main
    (when main
      (let [m (Method.getMethod "void main (String[])")
            gen (new GeneratorAdapter (+ (. Opcodes ACC_PUBLIC) (Opcodes.ACC_STATIC)) 
                     m nil nil cv)
            no-main-label (. gen newLabel)
            end-label (. gen newLabel)]
        (. gen (visitCode))

        (emit-get-var gen main-name)
        (. gen dup)
        (. gen ifNull no-main-label)
        (. gen loadArgs)
        (. gen (invokeStatic rt-type (. Method (getMethod "clojure.lang.ISeq seq(Object)"))))
        (. gen (invokeInterface ifn-type (new Method "applyTo" obj-type 
                                              (into-array [iseq-type]))))
        (. gen pop)
        (. gen goTo end-label)
                                        ;no main found
        (. gen mark no-main-label)
        (. gen (throwException ex-type (str main-name " not defined")))
        (. gen mark end-label)
        (. gen (returnValue))
        (. gen (endMethod))))
                                        ;field exposers
    (doseq [f {getter :get setter :set}] exposes
      (let [fld (.getField super (str f))
            ftype (totype (.getType fld))]
        (when getter
          (let [m (new Method (str getter) ftype (to-types []))
                gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)]
            (. gen (visitCode))
            (. gen loadThis)
            (. gen getField ctype (str f) ftype)
            (. gen (returnValue))
            (. gen (endMethod))))
        (when setter
          (let [m (new Method (str setter) (Type.VOID_TYPE) (into-array [ftype]))
                gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)]
            (. gen (visitCode))
            (. gen loadThis)
            (. gen loadArgs)
            (. gen putField ctype (str f) ftype)
            (. gen (returnValue))
            (. gen (endMethod))))))
                                        ;finish class def
    (. cv (visitEnd))
    {:name name :bytecode (. cv (toByteArray))}))

(defn gen-and-load-class [name & options]
  (let [{:keys [name bytecode]}
        (apply gen-class (str name) options)]
    (.. clojure.lang.RT ROOT_CLASSLOADER (defineClass (str name) bytecode))))

(defn gen-and-save-class [path name & options]
  (let [{:keys [name bytecode]} (apply gen-class (str name) options)
        file (java.io.File. path (str (name.replace \. (java.io.File.separatorChar)) ".class"))]
    (.createNewFile file)
    (with-open f (java.io.FileOutputStream. file)
      (.write f bytecode))))

(comment
;usage
(gen-class 
 package-qualified-name
  ;all below are optional
 :extends aclass
 :implements [interface ...]
 :constructors {[param-types] [super-param-types], }
 :methods [[name [param-types] return-type], ]
 :main boolean
 :factory name
 :state name
 :init name
 :exposes {protected-field {:get name :set name}, })
 
;(gen-and-load-class 
(clojure/gen-and-save-class 
 "/Users/rich/Downloads"
 'fred.lucy.Ethel 
 :extends clojure.lang.Box ;APersistentMap
 :implements [clojure.lang.IPersistentMap]
 :state 'state
                                        ;:constructors {[Object] [Object]}
                                        ;:init 'init
 :main true
 :factory 'create
 :methods [['foo [Object] Object]
           ['foo [] Object]]
 :exposes {'val {:get 'getVal :set 'setVal}})

(in-ns 'fred.lucy.Ethel__2276)
(clojure/refer 'clojure :exclude '(assoc seq count cons))
(defn init [n] [[] n])
(defn foo 
  ([this] :foo) 
  ([this x] x))
(defn main [x y] (println x y))
(in-ns 'user)
(def ethel (new fred.lucy.Ethel__2276 42))
(def ethel (fred.lucy.Ethel__2276.create 21))
(fred.lucy.Ethel__2276.main (into-array ["lucy" "ricky"]))
(.state ethel)
(.foo ethel 7)
(.foo ethel)
(.getVal ethel)
(.setVal ethel 12)

(gen-class org.clojure.MyComparator :implements [Comparator])
(in-ns 'org.clojure.MyComparator)
(defn compare [this x y] ...)

)

