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
              to-types (fn [cs] (if (pos? (count cs))
                                  (into-array (map #(. Type (getType %)) cs))
                                  (make-array Type 0)))
              map-type (. Type (getType PersistentHashMap))]
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
                (. gen (invokeConstructor (. Type (getType super)) m))
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
            (. gen (invokeInterface (. Type (getType clojure.lang.IPersistentCollection))
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

          ;add methods matching super's, if no mapping -> call super
          ;add methods matching interfaces', if no mapping -> return void or throw

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
  (. proxy (updateMappings mappings)))

(defn proxy-mappings [#^IProxy proxy]
  (. proxy (getMappings)))