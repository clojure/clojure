;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Author: Chas Emerick
;;         cemerick@snowtide.com

(ns clojure.test-clojure.serialization
  (:use clojure.test)
  (:import (java.io ObjectOutputStream ObjectInputStream
             ByteArrayOutputStream ByteArrayInputStream)))

(defn- serialize
  "Serializes a single object, returning a byte array."
  [v]
  (with-open [bout (ByteArrayOutputStream.)
              oos (ObjectOutputStream. bout)]
    (.writeObject oos v)
    (.flush oos)
    (.toByteArray bout)))

(defn- deserialize
  "Deserializes and returns a single object from the given byte array."
  [bytes]
  (with-open [ois (-> bytes ByteArrayInputStream. ObjectInputStream.)]
    (.readObject ois)))

(defrecord SerializationRecord [a b c])
(defstruct SerializationStruct :a :b :c)

(defn- build-via-transient
  [coll]
  (persistent!
    (reduce conj! (transient coll) (map vec (partition 2 (range 1000))))))

(defn- roundtrip
  [v]
  (let [rt (-> v serialize deserialize)
        rt-seq (-> v seq serialize deserialize)]
    (and (= v rt)
      (= (seq v) (seq rt))
      (= (seq v) rt-seq)
      (= (hash v) (hash rt))
      (= (.hashCode v) (.hashCode rt)))))

(deftest sequable-serialization
  (are [val] (roundtrip val)
    ; lists and related
    (list)
    (apply list (range 10))
    (cons 0 nil)
    (clojure.lang.Cons. 0 nil)

    ; vectors
    []
    (into [] (range 10))
    (into [] (range 25))
    (into [] (range 100))
    (into [] (range 500))
    (into [] (range 1000))

    ; maps
    {}
    {:a 5 :b 0}
    (apply array-map (range 100))
    (apply hash-map (range 100))

    ; sets
    #{}
    #{'a 'b 'c}
    (set (range 10))
    (set (range 25))
    (set (range 100))
    (set (range 500))
    (set (range 1000))
    (sorted-set)
    (sorted-set 'a 'b 'c)
    (apply sorted-set (reverse (range 10)))
    (apply sorted-set (reverse (range 25)))
    (apply sorted-set (reverse (range 100)))
    (apply sorted-set (reverse (range 500)))
    (apply sorted-set (reverse (range 1000)))

    ; queues
    clojure.lang.PersistentQueue/EMPTY
    (into clojure.lang.PersistentQueue/EMPTY (range 50))

    ; lazy seqs
    (lazy-seq nil)
    (lazy-seq (list* (range 50)))

    ; transient / persistent! round-trip
    (build-via-transient [])
    (build-via-transient {})
    (build-via-transient #{})

    ; array-seqs
    (seq (make-array Object 10))
    (seq (make-array Boolean/TYPE 10))
    (seq (make-array Byte/TYPE 10))
    (seq (make-array Character/TYPE 10))
    (seq (make-array Double/TYPE 10))
    (seq (make-array Float/TYPE 10))
    (seq (make-array Integer/TYPE 10))
    (seq (make-array Long/TYPE 10))

    ; "records"
    (SerializationRecord. 0 :foo (range 20))
    (struct SerializationStruct 0 :foo (range 20))

    ; misc seqs
    (seq "s11n")
    (range 50)
    (rseq (apply sorted-set (reverse (range 100))))

    ;; partially realized chunked range
    (let [r (range 50)]
      (nth r 35)
      r)))

(deftest misc-serialization
  (are [v] (= v (-> v serialize deserialize))
    25/3
    :keyword
    ::namespaced-keyword
    'symbol))

(deftest tostringed-bytes
  (let [rt #(-> % serialize seq)
        s1 (rt 'sym123)
        k1 (rt :kw123)
        _ (.toString 'sym123)
        _ (.toString :kw123)
        s2 (rt 'sym123)
        k2 (rt :kw123)]
    (is (= s1 s2))
    (is (= k1 k2))))

(deftest interned-serializations
  (are [v] (identical? v (-> v serialize deserialize))
    clojure.lang.RT/DEFAULT_COMPARATOR
    
    ; namespaces just get deserialized back into the same-named ns in the present runtime
    ; (they're referred to by defrecord instances)
    *ns*))

(deftest function-serialization
  (let [capture 5]
    (are [f] (= capture ((-> f serialize deserialize)))
      (constantly 5)
      (fn [] 5)
      #(do 5)
      (constantly capture)
      (fn [] capture)
      #(do capture))))

(deftest check-unserializable-objects
  (are [t] (thrown? java.io.NotSerializableException (serialize t))
    ;; transients
    (transient [])
    (transient {})
    (transient #{})

    ;; reference types
    (atom nil)
    (ref nil)
    (agent nil)
    #'+

    ;; stateful seqs
    (enumeration-seq (java.util.Collections/enumeration (range 50)))
    (iterator-seq (.iterator (range 50)))))