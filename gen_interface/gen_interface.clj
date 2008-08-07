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
        '(clojure.asm.commons Method GeneratorAdapter)
        '(java.io File))

(defn gen-and-return-interface
  "Uses the given specs to generate a Java interface.  Returns a map
   with :iname being the Java internal name of the interface, and
   :bytecode being an array of Bytes of the Java bytecode.  You'll
   almost always want to use gen-interface instead."
  [cname extends & methods]
  (let [to-iname (fn [c] (if (instance? Class c)
                           (.. Type (getType c) getInternalName)
                           (.replace (str c) "." "/")))
        to-types (fn [cs] (if (seq cs)
                            (into-array (map #(Type/getType %) cs))
                            (make-array Type 0)))
        iname (to-iname cname)
        cv (ClassWriter. ClassWriter/COMPUTE_MAXS)]
    (.visit cv Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT
                               Opcodes/ACC_INTERFACE)
            iname nil (to-iname Object)
            (when (seq extends)
              (into-array (map to-iname extends))))
    (doseq [mname pclasses rclass :as msig] (partition 3 methods)
      (GeneratorAdapter. (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT)
                         (Method. (str mname)
                                  (Type/getType rclass)
                                  (to-types pclasses))
                         nil nil cv))
    (.visitEnd cv)
    {:iname iname :bytecode (.toByteArray cv)}))

(defn gen-and-load-interface
  "Uses the given specs to generate and immediately load a Java
   interface.  This is not generally useful since you'll usually want
   a .class file in order to write Java code that uses the generated
   interface.  See gen-interface instead."
  [cname & args]
  (let [{:keys [iname bytecode]} (apply gen-and-return-interface cname args)]
    (.. clojure.lang.RT ROOT_CLASSLOADER (defineClass (str cname) bytecode))))

(defn gen-and-save-interface
  "Uses the given specs to generate a Java interface and save it to a
   .class file.  Returns the bytecode of the interface.  If you intend
   to use this interface immediately (for example to refer to it in a
   later gen-interface call as a parameter type, return type, or super
   class), you'll want to use gen-interface instead."
  [path cname & args]
  (let [{:keys [iname bytecode]} (apply gen-and-return-interface cname args)
        file (File. path (str (.replace (str cname) \. File/separatorChar)
                              ".class"))]
    (try
      (.createNewFile file)
      (catch java.io.IOException e
        (throw (Exception. (str "Failed to create " file) e))))
    (with-open f (java.io.FileOutputStream. file)
      (.write f bytecode))
    bytecode))

(defn gen-interface
  "Uses the given specs to generate a Java interface, save it to a
   .class file, and immediately load it so it's ready for use by
   subsequent gen-interface calls.  The path is the base classpath
   under which the .class file will be written.  cname is the
   fully-qualified classname (string or symbol) of the interface to be
   created (note that the appropriate sub-directories under path must
   already exist or this will throw an exception).  The next arg must
   be a collection of classes this interface will extend (each may be
   a string, symbol, or a class).  This may be followed by any number
   of: methodName, arg types, return type."
  [path cname & args]
  (.. clojure.lang.RT ROOT_CLASSLOADER
      (defineClass (str cname) (apply gen-and-save-interface path cname args))))

(comment

(gen-interface "/tmp" 'net.n01se.Foo [Appendable]
               'foo [] Integer
               'bar [Integer String] Double)

; save is used in this example because I want to refer to a class
; that's not yet defined in this runtime (Other).  It's possible by
; not loading the interface and specifying the class via a quoted
; symbol, but this isn't really recommended.  Why not make sure Other
; is defined and then just use gen-interface?
(gen-and-save-interface "/tmp" 'net.n01se.Bar ['net.n01se.Other Iterable]
                        'baz [] net.n01se.Foo)

(prn :isInterface (.isInterface (identity net.n01se.Foo)))
(prn :interfaces (seq (.getGenericInterfaces (identity net.n01se.Foo))))
(doseq m (seq (.getMethods (identity net.n01se.Foo)))
  (prn m))

)
