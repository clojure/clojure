;   Copyright (c) Chris Houser, July 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Functions for generating interface classes, which can then be loaded
; or saved to a .class file.

(clojure/in-ns 'clojure.contrib.gen-interface)
(clojure/refer 'clojure)

(import '(clojure.asm ClassWriter Opcodes Type)
        '(java.io File FileOutputStream IOException))

(defn asm-type
  "Returns an asm Type object for c, which may be a primitive class
  (such as Integer/TYPE), any other class (such as Double), or a
  fully-qualified class name given as a string or symbol
  (such as 'java.lang.String)"
  [c]
  (if (instance? Class c)
    (Type/getType c)
    (Type/getObjectType (.replace (str c) "." "/"))))

(defn iname
  "Returns the internal name of given class or class name. Cannot be
  used for primitive types."
  [c] (.getInternalName (asm-type c)))

(defn make-spec
  "Returns an interface spec object based on the given description.
   cname is the fully-qualified classname (string or symbol) of the
   interface to be created (note that the appropriate sub-directories
   under path must already exist or this will throw an exception).
   extends is a collection of classes this interface will extend (each
   may be a string, symbol, or a class). These are followed by the
   method descriptions, each of which is a vector: [methodName,
   arg types, return type]"
  [cname extends & methods]
  {:cname (str cname)
   :iname (iname cname)
   :extends (set (map iname extends))
   :methods (set (map (fn [[mname pclasses rclass]]
                          [(str mname)
                           (map asm-type pclasses)
                           (asm-type rclass)])
                      methods))})

(defn spec-from-class
  "Returns an interface spec object based on the given class."
  [c]
  {:cname (.getName c)
   :iname (iname c)
   :extends (set (map iname (.getInterfaces c)))
   :methods (set (map (fn [m]
                          [(.getName m)
                           (map asm-type (.getParameterTypes m))
                           (asm-type (.getReturnType m))])
                      (.getDeclaredMethods c)))})


(defn spec-bytecode
  "Uses the given interface spec object (such as created by make-spec)
   to generate a Java interface.  Returns a byte array containing the
   Java bytecode for the interface.  You'll almost always want to use
   gen-interface instead."
  [spec]
  (let [cv (ClassWriter. ClassWriter/COMPUTE_MAXS)]
    (. cv visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT
                                Opcodes/ACC_INTERFACE)
       (:iname spec) nil "java/lang/Object"
       (when (seq (:extends spec))
         (into-array (:extends spec))))
    (doseq [mname pclasses rclass] (:methods spec)
      (. cv visitMethod (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT)
         mname
         (Type/getMethodDescriptor rclass (if pclasses
                                            (into-array pclasses)
                                            (make-array Type 0)))
         nil nil))
    (. cv visitEnd)
    (. cv toByteArray)))


(defn load-interface
  "Uses the given interface spec object (such as created by make-spec)
   to generate a Java interface and immediately load it.  This is not
   generally useful since you'll usually want a .class file in order
   to write Java code that uses the generated interface.  See
   gen-interface instead."
  [spec]
  (let [old-class (try (Class/forName (:cname spec)) (catch Throwable t nil))]
    (if old-class
      (when-not (= spec (spec-from-class old-class))
        (throw (Exception. (str "A different class named "
                                (:cname spec) " already loaded"))))
      (.. clojure.lang.RT
          ROOT_CLASSLOADER (defineClass (:cname spec) (spec-bytecode spec))))))

(defn save-interface
  "Uses the given interface spec object (such as created by make-spec)
   to generate a Java interface and save it to a .class file.  The
   .class file will be written into a sub-directory of the given base
   path.  If you intend to use this interface immediately (for example
   to refer to it in a later gen-interface or gen-class call), you'll
   want to use gen-interface instead."
  [path spec]
  (let [file (File. path (str (.replace (:cname spec) \. File/separatorChar)
                              ".class"))]
    (try
      (.createNewFile file)
      (catch IOException e
        (throw (Exception. (str "Failed to create " file) e))))
    (with-open f (FileOutputStream. file)
      (.write f (spec-bytecode spec)))))

(defn gen-interface
  "Uses the given interface description to generate a Java interface,
   save it to a .class file, and immediately load it so it's ready
   for use by subsequent gen-interface or gen-class calls.  The .class
   file will be written into a sub-directory of the given base path.
   make-spec-args is the interface description as documented in
   make-spec."
  [path & make-spec-args]
  (let [spec (apply make-spec make-spec-args)]
    (load-interface spec)
    (save-interface path spec)))

(comment

(gen-interface "/tmp" 'net.n01se.Foo [Appendable]
               ['foo [] Integer]
               ['bar [Integer/TYPE String] Double])

; re-genning an identical interface doesn't try to load anything
(gen-interface "/tmp" 'net.n01se.Foo [Appendable]
               ['foo [] Integer]
               ['bar [Integer/TYPE String] Double])

; re-genning a different interface throws an exception
;(gen-interface "/tmp" 'net.n01se.Foo [Appendable]
;               ['foo [] Integer])

; save-interface is used directly in this example because I want
; to refer to a class that's not yet defined in this runtime (Other).
; It's possible by not loading the interface and specifying the class
; via a quoted symbol, but this isn't really recommended.  Instead,
; why not make sure Other is defined -- then you can use gen-interface.
(save-interface "/tmp"
                (make-spec 'net.n01se.Bar ['net.n01se.Other Iterable]
                           [['baz [] net.n01se.Foo]]))

(prn :isInterface (.isInterface (identity net.n01se.Foo)))
(prn :interfaces (seq (.getGenericInterfaces (identity net.n01se.Foo))))
(doseq m (seq (.getMethods (identity net.n01se.Foo)))
  (prn m))

)
