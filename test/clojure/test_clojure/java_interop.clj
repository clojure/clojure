;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka


(ns clojure.test-clojure.java-interop
  (:use clojure.test))

; http://clojure.org/java_interop
; http://clojure.org/compilation


(deftest test-dot
  ; (.instanceMember instance args*)
  (are [x] (= x "FRED")
      (.toUpperCase "fred") 
      (. "fred" toUpperCase)
      (. "fred" (toUpperCase)) )

  (are [x] (= x true)
      (.startsWith "abcde" "ab")
      (. "abcde" startsWith "ab")
      (. "abcde" (startsWith "ab")) )

  ; (.instanceMember Classname args*)
  (are [x] (= x "java.lang.String")
      (.getName String)
      (. (identity String) getName)
      (. (identity String) (getName)) )

  ; (Classname/staticMethod args*)
  (are [x] (= x 7)
      (Math/abs -7)
      (. Math abs -7)
      (. Math (abs -7)) )

  ; (. target -prop)
  (let [p (java.awt.Point. 1 2)]
    (are [x y] (= x y)
       1 (.-x p)
       2 (.-y p)
       1 (. p -x)
       2 (. p -y)
       1 (. (java.awt.Point. 1 2) -x)
       2 (. (java.awt.Point. 1 2) -y)))
  
  ; Classname/staticField
  (are [x] (= x 2147483647)
      Integer/MAX_VALUE
      (. Integer MAX_VALUE) ))

(definterface I (a []))
(deftype T [a] I (a [_] "method"))

(deftest test-reflective-field-name-ambiguous
  (let [t (->T "field")]
    (is (= "method" (. ^T t a)))
    (is (= "field" (. ^T t -a)))
    (is (= "method" (. t a)))
    (is (= "field" (. t -a)))
    (is (thrown? IllegalArgumentException (. t -BOGUS)))))

(deftest test-double-dot
  (is (= (.. System (getProperties) (get "os.name"))
         (. (. System (getProperties)) (get "os.name")))))


(deftest test-doto
  (let [m (doto (new java.util.HashMap)
            (.put "a" 1)
            (.put "b" 2))]
    (are [x y] (= x y)
        (class m) java.util.HashMap
        m {"a" 1 "b" 2} )))


(deftest test-new
  ; Integer
  (are [expr cls value] (and (= (class expr) cls)
                            (= expr value))
      (new java.lang.Integer 42) java.lang.Integer 42
      (java.lang.Integer. 123) java.lang.Integer 123 )

  ; Date
  (are [x] (= (class x) java.util.Date)
      (new java.util.Date)
      (java.util.Date.) ))


(deftest test-instance?
  ; evaluation
  (are [x y] (= x y)
      (instance? java.lang.Integer (+ 1 2)) false
      (instance? java.lang.Long (+ 1 2)) true )

  ; different types
  (are [type literal] (instance? literal type)
      1   java.lang.Long
      1.0 java.lang.Double
      1M  java.math.BigDecimal
      \a  java.lang.Character
      "a" java.lang.String )

  ; it is a Long, nothing else
  (are [x y] (= (instance? x 42) y)
      java.lang.Integer false
      java.lang.Long true
      java.lang.Character false
      java.lang.String false )

  ; test compiler macro
  (is (let [Long String] (instance? Long "abc")))
  (is (thrown? clojure.lang.ArityException (instance? Long))))

; set!

(defprotocol p (f [_]))
(deftype t [^:unsynchronized-mutable x] p (f [_] (set! (.x _) 1)))

(deftest test-set!
  (is (= 1 (f (t. 1)))))

; memfn


(deftest test-bean
  (let [b (bean java.awt.Color/black)]
    (are [x y] (= x y)
        (map? b) true

        (:red b) 0
        (:green b) 0
        (:blue b) 0
        (:RGB b) -16777216

        (:alpha b) 255
        (:transparency b) 1

        (:missing b) nil
        (:missing b :default) :default
        (get b :missing) nil
        (get b :missing :default) :default

        (:class b) java.awt.Color )))

(deftest test-iterable-bean
  (is (.iterator ^Iterable (bean (java.util.Date.))))
  (is (hash (bean (java.util.Date.)))))

; proxy, proxy-super

(deftest test-proxy-chain
  (testing "That the proxy functions can chain"
    (are [x y] (= x y)
        (-> (get-proxy-class Object) 
            construct-proxy
            (init-proxy {}) 
            (update-proxy {"toString" (fn [_] "chain chain chain")}) 
            str)
        "chain chain chain"

        (-> (proxy [Object] [] (toString [] "superfuzz bigmuff")) 
            (update-proxy {"toString" (fn [_] "chain chain chain")}) 
            str)
        "chain chain chain")))


(deftest test-bases
  (are [x y] (= x y)
      (bases java.lang.Math)
        (list java.lang.Object)
      (bases java.util.Collection)
        (list java.lang.Iterable)
      (bases java.lang.Object)
        nil
      (bases java.lang.Comparable)
        nil
      (bases java.lang.Integer)
        (list java.lang.Number java.lang.Comparable) ))

(deftest test-supers
  (are [x y] (= x y)
      (supers java.lang.Math)
        #{java.lang.Object}
      (supers java.lang.Integer)
        #{java.lang.Number java.lang.Object
          java.lang.Comparable java.io.Serializable} ))

(deftest test-proxy-super
  (let [d (proxy [java.util.BitSet] []
            (flip [bitIndex]
              (try
                (proxy-super flip bitIndex)
                (catch IndexOutOfBoundsException e
                  (throw (IllegalArgumentException. "replaced"))))))]
    ;; normal call
    (is (nil? (.flip d 0)))
    ;; exception should use proxied form and return IllegalArg
    (is (thrown? IllegalArgumentException (.flip d -1)))
    ;; same behavior on second call
    (is (thrown? IllegalArgumentException (.flip d -1)))))

;; http://dev.clojure.org/jira/browse/CLJ-1657
(deftest test-proxy-abstract-super
  (let [p (proxy [java.io.Writer] [])]
    (is (thrown? UnsupportedOperationException (.close p)))))

; Arrays: [alength] aget aset [make-array to-array into-array to-array-2d aclone]
;   [float-array, int-array, etc]
;   amap, areduce

(defmacro deftest-type-array [type-array type]
  `(deftest ~(symbol (str "test-" type-array))
      ; correct type
      #_(is (= (class (first (~type-array [1 2]))) (class (~type 1))))

      ; given size (and empty)
      (are [x] (and (= (alength (~type-array x)) x)
                    (= (vec (~type-array x)) (repeat x 0)))
          0 1 5 )

      ; copy of a sequence
      (are [x] (and (= (alength (~type-array x)) (count x))
                    (= (vec (~type-array x)) x))
          []
          [1]
          [1 -2 3 0 5] )

      ; given size and init-value
      (are [x] (and (= (alength (~type-array x 42)) x)
                    (= (vec (~type-array x 42)) (repeat x 42)))
          0 1 5 )

      ; given size and init-seq
      (are [x y z] (and (= (alength (~type-array x y)) x)
                        (= (vec (~type-array x y)) z))
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
;todo, fix, test broken for float/double, should compare to 1.0 2.0 etc
#_(deftest-type-array float-array float)
#_(deftest-type-array double-array double)

; separate test for exceptions (doesn't work with above macro...)
(deftest test-type-array-exceptions
  (are [x] (thrown? NegativeArraySizeException x)
       (int-array -1)
       (long-array -1)
       (float-array -1)
       (double-array -1) ))


(deftest test-make-array
  ; negative size
  (is (thrown? NegativeArraySizeException (make-array Integer -1)))

  ; one-dimensional
  (are [x] (= (alength (make-array Integer x)) x)
      0 1 5 )

  (let [a (make-array Long 5)]
    (aset a 3 42)
    (are [x y] (= x y)
        (aget a 3) 42
        (class (aget a 3)) Long ))
      
  ; multi-dimensional
  (let [a (make-array Long 3 2 4)]
    (aset a 0 1 2 987)
    (are [x y] (= x y)
        (alength a) 3
        (alength (first a)) 2
        (alength (first (first a))) 4

        (aget a 0 1 2) 987
        (class (aget a 0 1 2)) Long )))


(deftest test-to-array
  (let [v [1 "abc" :kw \c []]
        a (to-array v)]
    (are [x y] (= x y)
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
  (are [x] (and (= (alength (to-array x)) (count x))
                (= (vec (to-array x)) (vec x)))
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

(defn queue [& contents]
  (apply conj (clojure.lang.PersistentQueue/EMPTY) contents))

(defn array-typed-equals [expected actual]
  (and (= (class expected) (class actual))
       (java.util.Arrays/equals expected actual)))

(defmacro test-to-passed-array-for [collection-type]
  `(deftest ~(symbol (str "test-to-passed-array-for-" collection-type))
     (let [string-array# (make-array String 5)
           shorter# (~collection-type "1" "2" "3")
           same-length# (~collection-type "1" "2" "3" "4" "5")
           longer# (~collection-type "1" "2" "3" "4" "5" "6")]
       (are [expected actual] (array-typed-equals expected actual)
            (into-array String ["1" "2" "3" nil nil]) (.toArray shorter# string-array#)
            (into-array String ["1" "2" "3" "4" "5"]) (.toArray same-length# string-array#)
            (into-array String ["1" "2" "3" "4" "5" "6"]) (.toArray longer# string-array#)))))


(test-to-passed-array-for vector)
(test-to-passed-array-for list)
;;(test-to-passed-array-for hash-set)
(test-to-passed-array-for queue)

(deftest test-into-array
  ; compatible types only
  (is (thrown? IllegalArgumentException (into-array [1 "abc" :kw])))
  (is (thrown? IllegalArgumentException (into-array [1.2 4])))
  (is (thrown? IllegalArgumentException (into-array [(byte 2) (short 3)])))
  (is (thrown? IllegalArgumentException (into-array Byte/TYPE [100000000000000])))
  
  ; simple case
  (let [v [1 2 3 4 5]
        a (into-array v)]
    (are [x y] (= x y)
        (alength a) (count v)
        (vec a) v
        (class (first a)) (class (first v)) ))

  (is (= \a (aget (into-array Character/TYPE [\a \b \c]) 0)))

  (is (= [nil 1 2] (seq (into-array [nil 1 2]))))
  
  (let [types [Integer/TYPE
               Byte/TYPE
               Float/TYPE
               Short/TYPE
               Double/TYPE
               Long/TYPE]
        values [(byte 2) (short 3) (int 4) 5]]
    (for [t types]
      (let [a (into-array t values)]
        (is (== (aget a 0) 2))
        (is (== (aget a 1) 3))
        (is (== (aget a 2) 4))
        (is (== (aget a 3) 5)))))
  
  ; different kinds of collections
  (are [x] (and (= (alength (into-array x)) (count x))
                (= (vec (into-array x)) (vec x))
                (= (alength (into-array Long/TYPE x)) (count x))
                (= (vec (into-array Long/TYPE x)) (vec x)))
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
    (are [x y] (= x y)
        (alength a) (count v)
        (alength (aget a 0)) (count (nth v 0))
        (alength (aget a 1)) (count (nth v 1))
        (alength (aget a 2)) (count (nth v 2))

        (vec (aget a 0)) (nth v 0)
        (vec (aget a 1)) (nth v 1)
        (vec (aget a 2)) (nth v 2) ))

  ; empty array
  (let [a (to-array-2d [])]
    (are [x y] (= x y)
        (alength a) 0
        (vec a) [] )))


(deftest test-alength
  (are [x] (= (alength x) 0)
      (int-array 0)
      (long-array 0)
      (float-array 0)
      (double-array 0)
      (boolean-array 0)
      (byte-array 0)
      (char-array 0)
      (short-array 0)
      (make-array Integer/TYPE 0)
      (to-array [])
      (into-array [])
      (to-array-2d []) )

  (are [x] (= (alength x) 1)
      (int-array 1)
      (long-array 1)
      (float-array 1)
      (double-array 1)
      (boolean-array 1)
      (byte-array 1)
      (char-array 1)
      (short-array 1)
      (make-array Integer/TYPE 1)
      (to-array [1])
      (into-array [1])
      (to-array-2d [[1]]) )

  (are [x] (= (alength x) 3)
      (int-array 3)
      (long-array 3)
      (float-array 3)
      (double-array 3)
      (boolean-array 3)
      (byte-array 3)
      (char-array 3)
      (short-array 3)
      (make-array Integer/TYPE 3)
      (to-array [1 "a" :k])
      (into-array [1 2 3])
      (to-array-2d [[1] [2 3] [4 5 6]]) ))


(deftest test-aclone
  ; clone all arrays except 2D
  (are [x] (and (= (alength (aclone x)) (alength x))
                (= (vec (aclone x)) (vec x)))
      (int-array 0)
      (long-array 0)
      (float-array 0)
      (double-array 0)
      (boolean-array 0)
      (byte-array 0)
      (char-array 0)
      (short-array 0)
      (make-array Integer/TYPE 0)
      (to-array [])
      (into-array [])

      (int-array [1 2 3])
      (long-array [1 2 3])
      (float-array [1 2 3])
      (double-array [1 2 3])
      (boolean-array [true false])
      (byte-array [(byte 1) (byte 2)])
      (byte-array [1 2])
      (byte-array 2 [1 2])
      (char-array [\a \b \c])
      (short-array [(short 1) (short 2)])
      (short-array [1 2])
      (short-array 2 [1 2])
      (make-array Integer/TYPE 3)
      (to-array [1 "a" :k])
      (into-array [1 2 3]) )

  ; clone 2D
  (are [x] (and (= (alength (aclone x)) (alength x))
                (= (map alength (aclone x)) (map alength x))
                (= (map vec (aclone x)) (map vec x)))
      (to-array-2d [])
      (to-array-2d [[1] [2 3] [4 5 6]]) ))


; Type Hints, *warn-on-reflection*
;   #^ints, #^floats, #^longs, #^doubles

; Coercions: [int, long, float, double, char, boolean, short, byte]
;   num
;   ints/longs/floats/doubles

(deftest test-boolean
  (are [x y] (and (instance? java.lang.Boolean (boolean x))
                  (= (boolean x) y))
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
