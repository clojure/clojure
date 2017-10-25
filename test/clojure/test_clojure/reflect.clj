(ns clojure.test-clojure.reflect
  (:use clojure.data [clojure.reflect :as reflect] clojure.test clojure.pprint)
  (:import [clojure.reflect AsmReflector JavaReflector]
           [reflector IBar$Factory]))

(defn nodiff
  [x y]
  (let [[x-only y-only common] (diff x y)]
    (when (or x-only y-only)
      (is false (with-out-str (pprint {:x-only x-only
                                       :y-only y-only
                                       :common common}))))))

#_(deftest compare-reflect-and-asm
  (let [cl (.getContextClassLoader (Thread/currentThread))
        asm-reflector (AsmReflector. cl)
        java-reflector (JavaReflector. cl)]
    (doseq [classname '[java.lang.Runnable
                        java.lang.Object
                        java.io.FileInputStream
                        clojure.lang.Compiler
                        clojure.lang.PersistentVector
                        java.lang.SuppressWarnings]]
      (nodiff (type-reflect classname :reflector asm-reflector)
              (type-reflect classname :reflector java-reflector)))))

(deftest field-descriptor->class-symbol-test
  (are [s d] (= s (@#'reflect/field-descriptor->class-symbol d))
       'clojure.asm.Type<><> "[[Lclojure/asm/Type;"
       'int "I"
       'java.lang.Object "Ljava.lang.Object;"))

(deftest internal-name->class-symbol-test
  (are [s n] (= s (@#'reflect/internal-name->class-symbol n))
       'java.lang.Exception "java/lang/Exception"))

(def inst (IBar$Factory/get))
(deftest invoking-nonpublic-super
  (is (= "stuff" (.stuff inst))))