;;  Copyright (c) Frantisek Sodomka. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.

(ns clojure.contrib.test-clojure.java-interop
  (:use clojure.contrib.test-is))

; http://clojure.org/java_interop
; http://clojure.org/compilation


(deftest test-dot
  ; (.instanceMember instance args*)
  (are (= _ "FRED")
      (.toUpperCase "fred") 
      (. "fred" toUpperCase)
      (. "fred" (toUpperCase)) )

  (are (= _ true)
      (.startsWith "abcde" "ab")
      (. "abcde" startsWith "ab")
      (. "abcde" (startsWith "ab")) )

  ; (.instanceMember Classname args*)
  (are (= _ "java.lang.String")
      (.getName String)
      (. (identity String) getName)
      (. (identity String) (getName)) )

  ; (Classname/staticMethod args*)
  (are (= _ 7)
      (Math/abs -7)
      (. Math abs -7)
      (. Math (abs -7)) )

  ; Classname/staticField
  (are (= _ 2147483647)
      Integer/MAX_VALUE
      (. Integer MAX_VALUE) ))


(deftest test-double-dot
  (is (= (.. System (getProperties) (get "os.name"))
         (. (. System (getProperties)) (get "os.name")))))


(deftest test-doto
  (let [m (doto (new java.util.HashMap)
            (.put "a" 1)
            (.put "b" 2))]
    (are (= _1 _2)
        (class m) java.util.HashMap
        m {"a" 1 "b" 2} )))


(deftest test-new
  ; Integer
  (are (and (= (class _1) _2)
            (= _1 _3))
      (new java.lang.Integer 42) java.lang.Integer 42
      (java.lang.Integer. 123) java.lang.Integer 123 )

  ; Date
  (are (= (class _) java.util.Date)
      (new java.util.Date)
      (java.util.Date.) ))


(deftest test-instance?
  ; evaluation
  (are (= _1 _2)
      (instance? java.lang.Integer (+ 1 2)) true
      (instance? java.lang.Long (+ 1 2)) false )

  ; different types
  (are (instance? _2 _1)
      1   java.lang.Integer
      1.0 java.lang.Double
      1M  java.math.BigDecimal
      \a  java.lang.Character
      "a" java.lang.String )

  ; it is an int, nothing else
  (are (= (instance? _1 42) _2)
      java.lang.Integer true
      java.lang.Long false
      java.lang.Character false
      java.lang.String false ))


; set!

; memfn


(deftest test-bean
  (let [b (bean java.awt.Color/black)]
    (are (= _1 _2)
        (map? b) true

        (:red b) 0
        (:green b) 0
        (:blue b) 0
        (:RGB b) -16777216

        (:alpha b) 255
        (:transparency b) 1

        (:class b) java.awt.Color )))


; proxy, proxy-super


(deftest test-bases
  (are (= _1 _2)
      (bases java.lang.Math)
        (list java.lang.Object)
      (bases java.lang.Integer)
        (list java.lang.Number java.lang.Comparable) ))

(deftest test-supers
  (are (= _1 _2)
      (supers java.lang.Math)
        #{java.lang.Object}
      (supers java.lang.Integer)
        #{java.lang.Number java.lang.Object
          java.lang.Comparable java.io.Serializable} ))


; Arrays: [alength] aget aset [make-array to-array into-array to-array-2d aclone]
;   [float-array, int-array, etc]
;   amap, areduce

(defmacro deftest-type-array [type-array type]
  `(deftest ~(symbol (str "test-" type-array))
      ; correct type
      (is (= (class (first (~type-array [1 2]))) (class (~type 1))))

      ; given size (and empty)
      (are (and (= (alength (~type-array _)) _)
                (= (vec (~type-array _)) (repeat _ 0)))
          0 1 5 )

      ; copy of a sequence
      (are (and (= (alength (~type-array _)) (count _))
                (= (vec (~type-array _)) _))
;;        []    ;; ERROR
          [1]
          [1 -2 3 0 5] )

      ; given size and init-value
      (are (and (= (alength (~type-array _ 42)) _)
                (= (vec (~type-array _ 42)) (repeat _ 42)))
          0 1 5 )

      ; given size and init-seq
      (are (and (= (alength (~type-array _1 _2)) _1)
                (= (vec (~type-array _1 _2)) _3))
          0 [] []
          0 [1] []
          0 [1 2 3] []
          1 [] [0]
          1 [1] [1]
          1 [1 2 3] [1]
          5 [] [0 0 0 0 0]
          5 [1] [1 0 0 0 0]
          5 [1 2 3] [1 2 3 0 0]
          5 [1 2 3 4 5] [1 2 3 4 5]
          5 [1 2 3 4 5 6 7] [1 2 3 4 5] )))

(deftest-type-array int-array int)
(deftest-type-array long-array long)
(deftest-type-array float-array float)
(deftest-type-array double-array double)

; separate test for exceptions (doesn't work with above macro...)
(deftest test-type-array-exceptions
  (are (thrown? NegativeArraySizeException _)
      (int-array -1)
      (long-array -1)
      (float-array -1)
      (double-array -1) ))


(deftest test-make-array
  ; negative size
  (is (thrown? NegativeArraySizeException (make-array Integer -1)))

  ; one-dimensional
  (are (= (alength (make-array Integer _)) _)
      0 1 5 )

  (let [a (make-array Integer 5)]
    (aset a 3 42)
    (are (= _1 _2)
        (aget a 3) 42
        (class (aget a 3)) Integer ))
      
  ; multi-dimensional
  (let [a (make-array Integer 3 2 4)]
    (aset a 0 1 2 987)
    (are (= _1 _2)
        (alength a) 3
        (alength (first a)) 2
        (alength (first (first a))) 4

        (aget a 0 1 2) 987
        (class (aget a 0 1 2)) Integer )))


(deftest test-to-array
  (let [v [1 "abc" :kw \c []]
        a (to-array v)]
    (are (= _1 _2)
        ; length
        (alength a) (count v)

        ; content
        (vec a) v
        (class (aget a 0)) (class (nth v 0))
        (class (aget a 1)) (class (nth v 1))
        (class (aget a 2)) (class (nth v 2))
        (class (aget a 3)) (class (nth v 3))
        (class (aget a 4)) (class (nth v 4)) ))

  ; different kinds of collections
  (are (and (= (alength (to-array _)) (count _))
            (= (vec (to-array _)) (vec _)))
      ()
      '(1 2)
      []
      [1 2]
      (sorted-set)
      (sorted-set 1 2)
      
      (int-array 0)
      (int-array [1 2 3])

      (to-array [])
      (to-array [1 2 3]) ))


(deftest test-into-array
  ; compatible types only
  (is (thrown? IllegalArgumentException (into-array [1 "abc" :kw])))
  (is (thrown? IllegalArgumentException (into-array [1.2 4])))
  (is (thrown? IllegalArgumentException (into-array [(byte 2) (short 3)])))

  ; simple case
  (let [v [1 2 3 4 5]
        a (into-array v)]
    (are (= _1 _2)
        (alength a) (count v)
        (vec a) v
        (class (first a)) (class (first v)) ))

  ; given type
  (let [a (into-array Integer/TYPE [(byte 2) (short 3) (int 4)])]
    (are (= _ Integer)
        (class (aget a 0))
        (class (aget a 1))
        (class (aget a 2)) ))

  ; different kinds of collections
  (are (and (= (alength (into-array _)) (count _))
            (= (vec (into-array _)) (vec _))
            (= (alength (into-array Integer/TYPE _)) (count _))
            (= (vec (into-array Integer/TYPE _)) (vec _)))
      ()
      '(1 2)
      []
      [1 2]
      (sorted-set)
      (sorted-set 1 2)

      (int-array 0)
      (int-array [1 2 3])

      (to-array [])
      (to-array [1 2 3]) ))


(deftest test-to-array-2d
  ; needs to be a collection of collection(s)
  (is (thrown? Exception (to-array-2d [1 2 3])))

  ; ragged array
  (let [v [[1] [2 3] [4 5 6]]
        a (to-array-2d v)]
    (are (= _1 _2)
        (alength a) (count v)
        (alength (aget a 0)) (count (nth v 0))
        (alength (aget a 1)) (count (nth v 1))
        (alength (aget a 2)) (count (nth v 2))

        (vec (aget a 0)) (nth v 0)
        (vec (aget a 1)) (nth v 1)
        (vec (aget a 2)) (nth v 2) ))

  ; empty array
  (let [a (to-array-2d [])]
    (are (= _1 _2)
        (alength a) 0
        (vec a) [] )))


(deftest test-alength
  (are (= (alength _) 0)
      (int-array 0)
      (long-array 0)
      (float-array 0)
      (double-array 0)
      (make-array Integer/TYPE 0)
      (to-array [])
      (into-array [])
      (to-array-2d []) )

  (are (= (alength _) 1)
      (int-array 1)
      (long-array 1)
      (float-array 1)
      (double-array 1)
      (make-array Integer/TYPE 1)
      (to-array [1])
      (into-array [1])
      (to-array-2d [[1]]) )

  (are (= (alength _) 3)
      (int-array 3)
      (long-array 3)
      (float-array 3)
      (double-array 3)
      (make-array Integer/TYPE 3)
      (to-array [1 "a" :k])
      (into-array [1 2 3])
      (to-array-2d [[1] [2 3] [4 5 6]]) ))


(deftest test-aclone
  ; clone all arrays except 2D
  (are (and (= (alength (aclone _)) (alength _))
            (= (vec (aclone _)) (vec _)))
      (int-array 0)
      (long-array 0)
      (float-array 0)
      (double-array 0)
      (make-array Integer/TYPE 0)
      (to-array [])
      (into-array [])

      (int-array [1 2 3])
      (long-array [1 2 3])
      (float-array [1 2 3])
      (double-array [1 2 3])
      (make-array Integer/TYPE 3)
      (to-array [1 "a" :k])
      (into-array [1 2 3]) )

  ; clone 2D
  (are (and (= (alength (aclone _)) (alength _))
            (= (map alength (aclone _)) (map alength _))
            (= (map vec (aclone _)) (map vec _)))
      (to-array-2d [])
      (to-array-2d [[1] [2 3] [4 5 6]]) ))


; Type Hints, *warn-on-reflection*
;   #^ints, #^floats, #^longs, #^doubles

; Coercions: [int, long, float, double, char, boolean, short, byte]
;   num
;   ints/longs/floats/doubles

(deftest test-boolean
  (are (and (instance? java.lang.Boolean (boolean _1))
            (= (boolean _1) _2))
      nil false
      false false
      true true

      0 true
      1 true
      () true
      [1] true

      "" true
      \space true
      :kw true ))


(deftest test-char
  ; int -> char
  (is (instance? java.lang.Character (char 65)))

  ; char -> char
  (is (instance? java.lang.Character (char \a)))
  (is (= (char \a) \a)))

;; Note: More coercions in numbers.clj
