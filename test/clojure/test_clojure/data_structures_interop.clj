;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.data-structures-interop
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer (defspec)]))

(defn gen-range [min max]
  (gen/bind (gen/choose min max) (fn [n] (gen/tuple (gen/return n)
                                                    (gen/choose n max)))))

(defn gen-subvec [generator]
  (gen/bind (gen/not-empty generator)
            (fn [v] (gen/bind (gen-range 0 (dec (count v)))
                              (fn [[n m]] (gen/return (subvec v n m)))))))

(defn gen-gvec
  ([]
   (gen/bind (gen/elements {:int gen/int
                            :short (gen/fmap short gen/byte)
                            :long (gen/fmap long gen/int)
                            :float (gen/fmap float gen/int)
                            :double (gen/fmap double gen/int)
                            :byte gen/byte
                            :char gen/char
                            :boolean gen/boolean})
             #(apply gen-gvec %)))
  ([type generator]
   (gen/bind (gen/list generator) #(gen/return (apply vector-of type %)))))

(defn gen-hash-set [generator]
  (gen/fmap (partial apply hash-set) (gen/list generator)))

(defn gen-sorted-set [generator]
  (gen/fmap (partial apply sorted-set) (gen/list generator)))

(defn gen-array-map [key-gen val-gen]
  (gen/fmap (partial into (array-map)) (gen/map key-gen val-gen)))

(defn gen-sorted-map [key-gen val-gen]
  (gen/fmap (partial into (sorted-map)) (gen/map key-gen val-gen)))

(defn gen-array
  ([]
   (gen/bind (gen/elements {int-array gen/int
                            short-array gen/int
                            long-array (gen/fmap long gen/int)
                            float-array (gen/fmap float gen/int)
                            double-array (gen/fmap double gen/int)
                            byte-array gen/byte
                            char-array gen/char
                            boolean-array gen/boolean
                            object-array gen/string})
             #(apply gen-array %)))
  ([array-fn generator]
   (gen/fmap array-fn (gen/list generator))))

(defn exaust-iterator-forward [^java.util.Iterator iter]
  (loop [_ iter] (when (.hasNext iter) (recur (.next iter))))
  (try (.next iter) nil (catch Throwable t t)))

(defn exaust-iterator-backward [^java.util.ListIterator iter]
  (loop [_ iter] (when (.hasPrevious iter) (recur (.previous iter))))
  (try (.previous iter) nil (catch Throwable t t)))

(defspec iterator-throws-exception-on-exaustion 100
  (prop/for-all [[_ x] (gen/bind (gen/elements [['list       (gen/list gen/int)]
                                                ['vector     (gen/vector gen/int)]
                                                ['vector-of  (gen-gvec)]
                                                ['subvec     (gen-subvec (gen/vector gen/int))]
                                                ['hash-set   (gen-hash-set gen/int)]
                                                ['sorted-set (gen-sorted-set gen/int)]
                                                ['hash-map   (gen/hash-map gen/symbol gen/int)]
                                                ['array-map  (gen-array-map gen/symbol gen/int)]
                                                ['sorted-map (gen-sorted-map gen/symbol gen/int)]])
                                 (fn [[s g]] (gen/tuple (gen/return s) g)))]
    (instance? java.util.NoSuchElementException (exaust-iterator-forward (.iterator x)))))

(defspec array-iterator-throws-exception-on-exaustion 100
  (prop/for-all [arr (gen-array)]
    (let [iter (clojure.lang.ArrayIter/createFromObject arr)]
      (instance? java.util.NoSuchElementException (exaust-iterator-forward iter)))))

(defspec list-iterator-throws-exception-on-forward-exaustion 50
  (prop/for-all [[_ x] (gen/bind (gen/elements [['vector    (gen/vector gen/int)]
                                                ['subvec    (gen-subvec (gen/vector gen/int))]
                                                ['vector-of (gen-gvec)]])
                                 (fn [[s g]] (gen/tuple (gen/return s) g)))]
    (instance? java.util.NoSuchElementException (exaust-iterator-forward (.listIterator x)))))

(defspec list-iterator-throws-exception-on-backward-exaustion 50
  (prop/for-all [[_ x] (gen/bind (gen/elements [['vector    (gen/vector gen/int)]
                                                ['subvec    (gen-subvec (gen/vector gen/int))]
                                                ['vector-of (gen-gvec)]])
                                 (fn [[s g]] (gen/tuple (gen/return s) g)))]
    (instance? java.util.NoSuchElementException (exaust-iterator-backward (.listIterator x)))))

(defspec map-keyset-iterator-throws-exception-on-exaustion 50
  (prop/for-all [[_ m] (gen/bind (gen/elements [['hash-map   (gen/hash-map gen/symbol gen/int)
                                                 'array-map  (gen-array-map gen/symbol gen/int)
                                                 'sorted-map (gen-sorted-map gen/symbol gen/int)]])
                                 (fn [[s g]] (gen/tuple (gen/return s) g)))]
    (let [iter (.iterator (.keySet m))]
      (instance? java.util.NoSuchElementException (exaust-iterator-forward iter)))))

(defspec map-values-iterator-throws-exception-on-exaustion 50
  (prop/for-all [[_ m] (gen/bind (gen/elements [['hash-map   (gen/hash-map gen/symbol gen/int)
                                                 'array-map  (gen-array-map gen/symbol gen/int)
                                                 'sorted-map (gen-sorted-map gen/symbol gen/int)]])
                                 (fn [[s g]] (gen/tuple (gen/return s) g)))]
    (let [iter (.iterator (.values m))]
      (instance? java.util.NoSuchElementException (exaust-iterator-forward iter)))))

(defspec map-keys-iterator-throws-exception-on-exaustion 50
  (prop/for-all [m (gen-sorted-map gen/symbol gen/int)]
    (instance? java.util.NoSuchElementException (exaust-iterator-forward (.keys m)))))

(defspec map-vals-iterator-throws-exception-on-exaustion 50
  (prop/for-all [m (gen-sorted-map gen/symbol gen/int)]
    (instance? java.util.NoSuchElementException (exaust-iterator-forward (.vals m)))))

(defspec map-reverse-iterator-throws-exception-on-exaustion 50
  (prop/for-all [m (gen-sorted-map gen/symbol gen/int)]
    (instance? java.util.NoSuchElementException (exaust-iterator-forward (.reverseIterator m)))))
