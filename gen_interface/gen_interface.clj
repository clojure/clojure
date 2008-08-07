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

(defn gen-interface
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
  [cname & args]
  (let [{:keys [iname bytecode]} (apply gen-interface cname args)]
    (.. clojure.lang.RT ROOT_CLASSLOADER (defineClass (str cname) bytecode))))

(defn gen-and-save-interface
  [path cname & args]
  (let [{:keys [iname bytecode]} (apply gen-interface cname args)
        file (File. path (str (.replace (str cname) \. File/separatorChar)
                              ".class"))]
    (try
      (.createNewFile file)
      (catch java.io.IOException e
        (throw (Exception. (str "Failed to create " file) e))))
    (with-open f (java.io.FileOutputStream. file)
      (.write f bytecode))))


(comment

(gen-and-load-interface 'net.n01se.Foo [Appendable]
                        'foo [] Integer
                        'bar [Integer String] Double)
(gen-and-save-interface "/tmp" 'net.n01se.Bar ['net.n01se.Other Iterable])

(prn :isInterface (.isInterface (identity net.n01se.Foo)))
(prn :interfaces (seq (.getGenericInterfaces (identity net.n01se.Foo))))
(doseq m (seq (.getMethods (identity net.n01se.Foo)))
  (prn m))

)
