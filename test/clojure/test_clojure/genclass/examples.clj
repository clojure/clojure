;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Test classes that are AOT-compile for the tests in
           clojure.test-clojure.genclass."
      :author "Stuart Halloway, Daniel Solano Gómez"}
  clojure.test-clojure.genclass.examples)

(definterface ExampleInterface
  (foo [a])
  (foo [a b])
  (foo [a #^int b]))

(gen-class :name clojure.test_clojure.genclass.examples.ExampleClass
           :implements [clojure.test_clojure.genclass.examples.ExampleInterface])

;; -foo-Object unimplemented to test missing fn case

(defn -foo-Object-Object
  [_ o1 o2]
  "foo with o, o")

(defn -foo-Object-int
  [_ o i]
  "foo with o, i")

(gen-class :name ^{Deprecated {}
                   SuppressWarnings ["Warning1"] ; discarded
                   java.lang.annotation.Target []}
                 clojure.test_clojure.genclass.examples.ExampleAnnotationClass
           :prefix "annot-"
           :methods [[^{Deprecated {}
                        Override {}} ;discarded
                      foo [^{java.lang.annotation.Retention java.lang.annotation.RetentionPolicy/SOURCE
                             java.lang.annotation.Target    [java.lang.annotation.ElementType/TYPE
                                                             java.lang.annotation.ElementType/PARAMETER]}
                           String] void]])

(definterface ArrayDefInterface
  ; primitive array sugar
  (^void takesByteArray [^bytes a])
  (^void takesCharArray [^chars a])
  (^void takesShortArray [^shorts a])
  (^void takesIntArray [^ints a])
  (^void takesLongArray [^longs a])
  (^void takesFloatArray [^floats a])
  (^void takesDoubleArray [^doubles a])
  (^void takesBooleanArray [^booleans a])
  ; raw primitive arrays
  (^"[B" returnsByteArray [])
  (^"[C" returnsCharArray [])
  (^"[I" returnsIntArray [])
  (^"[S" returnsShortArray [])
  (^"[J" returnsLongArray [])
  (^"[F" returnsFloatArray [])
  (^"[D" returnsDoubleArray [])
  (^"[Z" returnsBooleanArray [])
  ; Object arrays
  (^void takesMapArray [^"[Ljava.util.Map;" a])
  (^"[[Ljava.util.Map;" returnsMap2dArray []))

(gen-interface
  :name clojure.test_clojure.genclass.examples.ArrayGenInterface
  :methods [; sugar
            [takesByteArray [bytes] void]
            [takesCharArray [chars] void]
            [takesShortArray [shorts] void]
            [takesIntArray [ints] void]
            [takesLongArray [longs] void]
            [takesFloatArray [floats] void]
            [takesDoubleArray [doubles] void]
            [takesBooleanArray [booleans] void]
            ; raw primitive types
            [returnsByteArray [] "[B"]
            [returnsCharArray [] "[C"]
            [returnsShortArray [] "[S"]
            [returnsIntArray [] "[I"]
            [returnsLongArray [] "[J"]
            [returnsFloatArray [] "[F"]
            [returnsDoubleArray [] "[D"]
            [returnsBooleanArray [] "[Z"]
            ; object arrays
            [takesMapArray ["[Ljava.util.Map;"] void]
            [returnsMap2dArray [] "[[Ljava.util.Map;"]])
