;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Java-specific parts of clojure.reflect
(in-ns 'clojure.reflect)

(require '[clojure.set :as set]
         '[clojure.string :as str])
(import '[clojure.asm ClassReader ClassVisitor Type Opcodes]
         '[java.lang.reflect Modifier]
         java.io.InputStream)

(extend-protocol TypeReference
  clojure.lang.Symbol
  (typename [s] (str/replace (str s) "<>" "[]"))
  
  Class
  ;; neither .getName not .getSimpleName returns the right thing, so best to delegate to Type
  (typename
   [c]
   (typename (Type/getType c)))
  
  Type
  (typename
   [t]
   (-> (.getClassName t))))

(defn- typesym
  "Given a typeref, create a legal Clojure symbol version of the
   type's name."
  [t]
  (-> (typename t)
      (str/replace "[]" "<>")
      (symbol)))

(defn- resource-name
  "Given a typeref, return implied resource name. Used by Reflectors
   such as ASM that need to find and read classbytes from files."
  [typeref]
  (-> (typename typeref)
      (str/replace "." "/")
      (str ".class")))

(defn- access-flag
  [[name flag & contexts]]
  {:name name :flag flag :contexts (set (map keyword contexts))})

(defn- field-descriptor->class-symbol
  "Convert a Java field descriptor to a Clojure class symbol. Field
   descriptors are described in section 4.3.2 of the JVM spec, 2nd ed.:
   http://java.sun.com/docs/books/jvms/second_edition/html/ClassFile.doc.html#14152"
  [^String d]
  {:pre [(string? d)]}
  (typesym (Type/getType d)))

(defn- internal-name->class-symbol
  "Convert a Java internal name to a Clojure class symbol. Internal
   names uses slashes instead of dots, e.g. java/lang/String. See
   Section 4.2 of the JVM spec, 2nd ed.:

   http://java.sun.com/docs/books/jvms/second_edition/html/ClassFile.doc.html#14757"
  [d]
  {:pre [(string? d)]}
  (typesym (Type/getObjectType d)))

(def ^{:doc "The Java access bitflags, along with their friendly names and
the kinds of objects to which they can apply."}
  flag-descriptors
  (vec
   (map access-flag
        [[:public 0x0001 :class :field :method]
         [:private 0x002 :class :field :method]
         [:protected 0x0004  :class :field :method]
         [:static 0x0008  :field :method]
         [:final 0x0010  :class :field :method]
         ;; :super is ancient history and is unfindable (?) by
         ;; reflection. skip it
         #_[:super 0x0020  :class]        
         [:synchronized 0x0020  :method]
         [:volatile 0x0040  :field]
         [:bridge 0x0040  :method]
         [:varargs 0x0080  :method]
         [:transient 0x0080  :field]
         [:native 0x0100  :method]
         [:interface 0x0200  :class]
         [:abstract 0x0400  :class :method]
         [:strict 0x0800  :method]
         [:synthetic 0x1000  :class :field :method]
         [:annotation 0x2000  :class]
         [:enum 0x4000  :class :field :inner]])))

(defn- parse-flags
  "Convert reflection bitflags into a set of keywords."
  [flags context]
  (reduce
   (fn [result fd]
     (if (and (get (:contexts fd) context)
              (not (zero? (bit-and flags (:flag fd)))))
       (conj result (:name fd))
       result))
   #{}
   flag-descriptors))

(defrecord Constructor
  [name declaring-class parameter-types exception-types flags])

(defn- constructor->map
  [^java.lang.reflect.Constructor constructor]
  (Constructor.
   (symbol (.getName constructor))
   (typesym (.getDeclaringClass constructor))
   (vec (map typesym (.getParameterTypes constructor)))
   (vec (map typesym (.getExceptionTypes constructor)))
   (parse-flags (.getModifiers constructor) :method)))

(defn- declared-constructors
  "Return a set of the declared constructors of class as a Clojure map."
  [^Class cls]
  (set (map
        constructor->map
        (.getDeclaredConstructors cls))))

(defrecord Method
  [name return-type declaring-class parameter-types exception-types flags])

(defn- method->map
  [^java.lang.reflect.Method method]
  (Method.
   (symbol (.getName method))
   (typesym (.getReturnType method))
   (typesym (.getDeclaringClass method))
   (vec (map typesym (.getParameterTypes method)))
   (vec (map typesym (.getExceptionTypes method)))
   (parse-flags (.getModifiers method) :method)))

(defn- declared-methods
  "Return a set of the declared constructors of class as a Clojure map."
  [^Class cls]
  (set (map
        method->map
        (.getDeclaredMethods cls))))

(defrecord Field
  [name type declaring-class flags])

(defn- field->map
  [^java.lang.reflect.Field field]
  (Field.
   (symbol (.getName field))
   (typesym (.getType field))
   (typesym (.getDeclaringClass field))
   (parse-flags (.getModifiers field) :field)))

(defn- declared-fields
  "Return a set of the declared fields of class as a Clojure map."
  [^Class cls]
  (set (map
        field->map
        (.getDeclaredFields cls))))

(deftype JavaReflector [classloader]
  Reflector
  (do-reflect [_ typeref]
           (let [cls (clojure.lang.RT/classForName (typename typeref) false classloader)]
             {:bases (not-empty (set (map typesym (bases cls))))
              :flags (parse-flags (.getModifiers cls) :class)
              :members (set/union (declared-fields cls)
                                  (declared-methods cls)
                                  (declared-constructors cls))})))

(def ^:private default-reflector
     (JavaReflector. (.getContextClassLoader (Thread/currentThread))))

(defn- parse-method-descriptor
  [^String md]
  {:parameter-types (vec (map typesym (Type/getArgumentTypes md)))
   :return-type (typesym (Type/getReturnType md))})

(defprotocol ClassResolver
  (^InputStream resolve-class [this name]
                "Given a class name, return that typeref's class bytes as an InputStream."))

(extend-protocol ClassResolver
  clojure.lang.Fn
  (resolve-class [this typeref] (this typeref))
  
  ClassLoader
  (resolve-class [this typeref]
                 (.getResourceAsStream this (resource-name typeref))))

(deftype AsmReflector [class-resolver]
  Reflector
  (do-reflect [_ typeref]
    (with-open [is (resolve-class class-resolver typeref)]
      (let [class-symbol (typesym typeref)
            r (ClassReader. is)
            result (atom {:bases #{} :flags #{} :members #{}})]
        (.accept
         r
         (proxy
          [ClassVisitor]
          [Opcodes/ASM4]
          (visit [version access name signature superName interfaces]
                 (let [flags (parse-flags access :class)
                       ;; ignore java.lang.Object on interfaces to match reflection
                       superName (if (and (flags :interface)
                                          (= superName "java/lang/Object"))
                                   nil
                                   superName)
                       bases (->> (cons superName interfaces)
                                  (remove nil?)
                                  (map internal-name->class-symbol)
                                  (map symbol)
                                  (set)
                                  (not-empty))]
                   (swap! result merge {:bases bases 
                                        :flags flags})))
          (visitAnnotation [desc visible])
          (visitSource [name debug])
          (visitInnerClass [name outerName innerName access])
          (visitField [access name desc signature value]
                      (swap! result update :members (fnil conj #{})
                             (Field. (symbol name)
                                     (field-descriptor->class-symbol desc)
                                     class-symbol
                                     (parse-flags access :field)))
                      nil)
          (visitMethod [access name desc signature exceptions]
                       (when-not (= name "<clinit>")
                         (let [constructor? (= name "<init>")]
                           (swap! result update :members (fnil conj #{})
                                  (let [{:keys [parameter-types return-type]} (parse-method-descriptor desc)
                                        flags (parse-flags access :method)]
                                    (if constructor?
                                      (Constructor. class-symbol
                                                    class-symbol
                                                    parameter-types
                                                    (vec (map internal-name->class-symbol exceptions))
                                                    flags)
                                      (Method. (symbol name)
                                               return-type
                                               class-symbol
                                               parameter-types
                                               (vec (map internal-name->class-symbol exceptions))
                                               flags))))))
                       nil)
          (visitEnd [])
          ) 0)
        @result))))

