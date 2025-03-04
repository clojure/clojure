;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka


(ns clojure.test-clojure.java-interop
  (:use clojure.test)
  (:require [clojure.data :as data]
            [clojure.inspector]
            [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test-clojure.proxy.examples :as proxy-examples]
            [clojure.test-helper :refer [should-not-reflect]])
  (:import java.util.Base64
           (java.io File FileFilter FilenameFilter)
           (java.util UUID)
           (java.util.concurrent.atomic AtomicLong AtomicInteger)
           (clojure.test FIConstructor FIStatic FunctionalTester AdapterExerciser)))

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
  (let [b (bean (java.util.Date.))]
    (is (.iterator ^Iterable b))
    (is (= (into [] b) (into [] (seq b))))
    (is (hash b))))

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

;https://clojure.atlassian.net/browse/CLJ-1973
(deftest test-proxy-method-order
  (let [class-reader (clojure.asm.ClassReader. proxy-examples/proxy1-class-name)
        method-order (atom [])
        method-visitor (proxy [clojure.asm.ClassVisitor] [clojure.asm.Opcodes/ASM4 nil]
                         (visitMethod [access name descriptor signature exceptions]
                           (swap! method-order conj {:name name :descriptor descriptor})
                           nil))
        _ (.accept class-reader method-visitor 0)
        expected [{:name "<init>", :descriptor "()V"}
                  {:name "__initClojureFnMappings", :descriptor "(Lclojure/lang/IPersistentMap;)V"}
                  {:name "__updateClojureFnMappings", :descriptor "(Lclojure/lang/IPersistentMap;)V"}
                  {:name "__getClojureFnMappings", :descriptor "()Lclojure/lang/IPersistentMap;"}
                  {:name "clone", :descriptor "()Ljava/lang/Object;"}
                  {:name "hashCode", :descriptor "()I"}
                  {:name "toString", :descriptor "()Ljava/lang/String;"}
                  {:name "equals", :descriptor "(Ljava/lang/Object;)Z"}
                  {:name "a", :descriptor "(Ljava/io/File;)Z"}
                  {:name "a", :descriptor "(Ljava/lang/Boolean;)Ljava/lang/Object;"}
                  {:name "a", :descriptor "(Ljava/lang/Runnable;)Z"}
                  {:name "a", :descriptor "(Ljava/lang/String;)I"}
                  {:name "b", :descriptor "(Ljava/lang/String;)Ljava/lang/Object;"}
                  {:name "c", :descriptor "(Ljava/lang/String;)Ljava/lang/Object;"}
                  {:name "d", :descriptor "(Ljava/lang/String;)Ljava/lang/Object;"}
                  {:name "a", :descriptor "(Ljava/lang/Boolean;Ljava/lang/String;)I"}
                  {:name "a", :descriptor "(Ljava/lang/String;Ljava/io/File;)Z"}
                  {:name "a", :descriptor "(Ljava/lang/String;Ljava/lang/Runnable;)Z"}
                  {:name "a", :descriptor "(Ljava/lang/String;Ljava/lang/String;)I"}]
        actual @method-order]
    (is (= expected actual)
        (with-out-str (pp/pprint (data/diff expected actual))))))

;; serialized-proxy can be regenerated using a modified version of
;; Clojure with the proxy serialization prohibition disabled and the
;; following code:
;; revert 271674c9b484d798484d134a5ac40a6df15d3ac3 to allow serialization
(comment
  (require 'clojure.inspector)
  (let [baos (java.io.ByteArrayOutputStream.)]
    (with-open [baos baos]
      (.writeObject (java.io.ObjectOutputStream. baos) (clojure.inspector/list-model nil)))
    (prn (vector (System/getProperty "java.specification.version")
                 (.encodeToString (java.util.Base64/getEncoder) (.toByteArray baos))))))

(def serialized-proxies
  {"1.8" "rO0ABXNyAEVjbG9qdXJlLmluc3BlY3Rvci5wcm94eSRqYXZheC5zd2luZy50YWJsZS5BYnN0cmFjdFRhYmxlTW9kZWwkZmYxOTI3NGFydNi2XwhNRQIAAUwADl9fY2xvanVyZUZuTWFwdAAdTGNsb2p1cmUvbGFuZy9JUGVyc2lzdGVudE1hcDt4cgAkamF2YXguc3dpbmcudGFibGUuQWJzdHJhY3RUYWJsZU1vZGVscsvrOK4B/74CAAFMAAxsaXN0ZW5lckxpc3R0ACVMamF2YXgvc3dpbmcvZXZlbnQvRXZlbnRMaXN0ZW5lckxpc3Q7eHBzcgAjamF2YXguc3dpbmcuZXZlbnQuRXZlbnRMaXN0ZW5lckxpc3SxNsZ9hOrWRAMAAHhwcHhzcgAfY2xvanVyZS5sYW5nLlBlcnNpc3RlbnRBcnJheU1hcOM3cA+YxfTfAgACTAAFX21ldGFxAH4AAVsABWFycmF5dAATW0xqYXZhL2xhbmcvT2JqZWN0O3hyABtjbG9qdXJlLmxhbmcuQVBlcnNpc3RlbnRNYXBdfC8DdCByewIAAkkABV9oYXNoSQAHX2hhc2hlcXhwAAAAAAAAAABwdXIAE1tMamF2YS5sYW5nLk9iamVjdDuQzlifEHMpbAIAAHhwAAAABnQADmdldENvbHVtbkNvdW50c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzkxNTbQ1M9FYoOj9wIAAHhyABZjbG9qdXJlLmxhbmcuQUZ1bmN0aW9uPgZwnJ5G/csCAAFMABFfX21ldGhvZEltcGxDYWNoZXQAHkxjbG9qdXJlL2xhbmcvTWV0aG9kSW1wbENhY2hlO3hwcHQAC2dldFJvd0NvdW50c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzkxNTgf1DHD2//pRAIAAUwABW5yb3dzdAASTGphdmEvbGFuZy9PYmplY3Q7eHEAfgAPcHB0AApnZXRWYWx1ZUF0c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzkxNjBYQ6uzEwbd+gIAAkwACWdldF9sYWJlbHEAfgAUTAAJZ2V0X3ZhbHVlcQB+ABR4cQB+AA9wcHA="
 "9" "rO0ABXNyAEVjbG9qdXJlLmluc3BlY3Rvci5wcm94eSRqYXZheC5zd2luZy50YWJsZS5BYnN0cmFjdFRhYmxlTW9kZWwkZmYxOTI3NGFydNi2XwhNRQIAAUwADl9fY2xvanVyZUZuTWFwdAAdTGNsb2p1cmUvbGFuZy9JUGVyc2lzdGVudE1hcDt4cgAkamF2YXguc3dpbmcudGFibGUuQWJzdHJhY3RUYWJsZU1vZGVscsvrOK4B/74CAAFMAAxsaXN0ZW5lckxpc3R0ACVMamF2YXgvc3dpbmcvZXZlbnQvRXZlbnRMaXN0ZW5lckxpc3Q7eHBzcgAjamF2YXguc3dpbmcuZXZlbnQuRXZlbnRMaXN0ZW5lckxpc3SxNsZ9hOrWRAMAAHhwcHhzcgAfY2xvanVyZS5sYW5nLlBlcnNpc3RlbnRBcnJheU1hcOM3cA+YxfTfAgACTAAFX21ldGFxAH4AAVsABWFycmF5dAATW0xqYXZhL2xhbmcvT2JqZWN0O3hyABtjbG9qdXJlLmxhbmcuQVBlcnNpc3RlbnRNYXBdfC8DdCByewIAAkkABV9oYXNoSQAHX2hhc2hlcXhwAAAAAAAAAABwdXIAE1tMamF2YS5sYW5nLk9iamVjdDuQzlifEHMpbAIAAHhwAAAABnQADmdldENvbHVtbkNvdW50c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzkxNTbQ1M9FYoOj9wIAAHhyABZjbG9qdXJlLmxhbmcuQUZ1bmN0aW9uPgZwnJ5G/csCAAFMABFfX21ldGhvZEltcGxDYWNoZXQAHkxjbG9qdXJlL2xhbmcvTWV0aG9kSW1wbENhY2hlO3hwcHQAC2dldFJvd0NvdW50c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzkxNTgf1DHD2//pRAIAAUwABW5yb3dzdAASTGphdmEvbGFuZy9PYmplY3Q7eHEAfgAPcHB0AApnZXRWYWx1ZUF0c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzkxNjBYQ6uzEwbd+gIAAkwACWdldF9sYWJlbHEAfgAUTAAJZ2V0X3ZhbHVlcQB+ABR4cQB+AA9wcHA="
 "10" "rO0ABXNyAEVjbG9qdXJlLmluc3BlY3Rvci5wcm94eSRqYXZheC5zd2luZy50YWJsZS5BYnN0cmFjdFRhYmxlTW9kZWwkZmYxOTI3NGFydNi2XwhNRQIAAUwADl9fY2xvanVyZUZuTWFwdAAdTGNsb2p1cmUvbGFuZy9JUGVyc2lzdGVudE1hcDt4cgAkamF2YXguc3dpbmcudGFibGUuQWJzdHJhY3RUYWJsZU1vZGVscsvrOK4B/74CAAFMAAxsaXN0ZW5lckxpc3R0ACVMamF2YXgvc3dpbmcvZXZlbnQvRXZlbnRMaXN0ZW5lckxpc3Q7eHBzcgAjamF2YXguc3dpbmcuZXZlbnQuRXZlbnRMaXN0ZW5lckxpc3SRSMwtc98O3gMAAHhwcHhzcgAfY2xvanVyZS5sYW5nLlBlcnNpc3RlbnRBcnJheU1hcOM3cA+YxfTfAgACTAAFX21ldGFxAH4AAVsABWFycmF5dAATW0xqYXZhL2xhbmcvT2JqZWN0O3hyABtjbG9qdXJlLmxhbmcuQVBlcnNpc3RlbnRNYXBdfC8DdCByewIAAkkABV9oYXNoSQAHX2hhc2hlcXhwAAAAAAAAAABwdXIAE1tMamF2YS5sYW5nLk9iamVjdDuQzlifEHMpbAIAAHhwAAAABnQADmdldENvbHVtbkNvdW50c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzkxNTbQ1M9FYoOj9wIAAHhyABZjbG9qdXJlLmxhbmcuQUZ1bmN0aW9uPgZwnJ5G/csCAAFMABFfX21ldGhvZEltcGxDYWNoZXQAHkxjbG9qdXJlL2xhbmcvTWV0aG9kSW1wbENhY2hlO3hwcHQAC2dldFJvd0NvdW50c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzkxNTgf1DHD2//pRAIAAUwABW5yb3dzdAASTGphdmEvbGFuZy9PYmplY3Q7eHEAfgAPcHB0AApnZXRWYWx1ZUF0c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzkxNjBYQ6uzEwbd+gIAAkwACWdldF9sYWJlbHEAfgAUTAAJZ2V0X3ZhbHVlcQB+ABR4cQB+AA9wcHA="
 "11" "rO0ABXNyAEVjbG9qdXJlLmluc3BlY3Rvci5wcm94eSRqYXZheC5zd2luZy50YWJsZS5BYnN0cmFjdFRhYmxlTW9kZWwkZmYxOTI3NGFydNi2XwhNRQIAAUwADl9fY2xvanVyZUZuTWFwdAAdTGNsb2p1cmUvbGFuZy9JUGVyc2lzdGVudE1hcDt4cgAkamF2YXguc3dpbmcudGFibGUuQWJzdHJhY3RUYWJsZU1vZGVscsvrOK4B/74CAAFMAAxsaXN0ZW5lckxpc3R0ACVMamF2YXgvc3dpbmcvZXZlbnQvRXZlbnRMaXN0ZW5lckxpc3Q7eHBzcgAjamF2YXguc3dpbmcuZXZlbnQuRXZlbnRMaXN0ZW5lckxpc3SRSMwtc98O3gMAAHhwcHhzcgAfY2xvanVyZS5sYW5nLlBlcnNpc3RlbnRBcnJheU1hcOM3cA+YxfTfAgACTAAFX21ldGFxAH4AAVsABWFycmF5dAATW0xqYXZhL2xhbmcvT2JqZWN0O3hyABtjbG9qdXJlLmxhbmcuQVBlcnNpc3RlbnRNYXBdfC8DdCByewIAAkkABV9oYXNoSQAHX2hhc2hlcXhwAAAAAAAAAABwdXIAE1tMamF2YS5sYW5nLk9iamVjdDuQzlifEHMpbAIAAHhwAAAABnQADmdldENvbHVtbkNvdW50c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzkxNTbQ1M9FYoOj9wIAAHhyABZjbG9qdXJlLmxhbmcuQUZ1bmN0aW9uPgZwnJ5G/csCAAFMABFfX21ldGhvZEltcGxDYWNoZXQAHkxjbG9qdXJlL2xhbmcvTWV0aG9kSW1wbENhY2hlO3hwcHQAC2dldFJvd0NvdW50c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzkxNTgf1DHD2//pRAIAAUwABW5yb3dzdAASTGphdmEvbGFuZy9PYmplY3Q7eHEAfgAPcHB0AApnZXRWYWx1ZUF0c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzkxNjBYQ6uzEwbd+gIAAkwACWdldF9sYWJlbHEAfgAUTAAJZ2V0X3ZhbHVlcQB+ABR4cQB+AA9wcHA="
 "12" "rO0ABXNyAEVjbG9qdXJlLmluc3BlY3Rvci5wcm94eSRqYXZheC5zd2luZy50YWJsZS5BYnN0cmFjdFRhYmxlTW9kZWwkZmYxOTI3NGFydNi2XwhNRQIAAUwADl9fY2xvanVyZUZuTWFwdAAdTGNsb2p1cmUvbGFuZy9JUGVyc2lzdGVudE1hcDt4cgAkamF2YXguc3dpbmcudGFibGUuQWJzdHJhY3RUYWJsZU1vZGVscsvrOK4B/74CAAFMAAxsaXN0ZW5lckxpc3R0ACVMamF2YXgvc3dpbmcvZXZlbnQvRXZlbnRMaXN0ZW5lckxpc3Q7eHBzcgAjamF2YXguc3dpbmcuZXZlbnQuRXZlbnRMaXN0ZW5lckxpc3SRSMwtc98O3gMAAHhwcHhzcgAfY2xvanVyZS5sYW5nLlBlcnNpc3RlbnRBcnJheU1hcOM3cA+YxfTfAgACTAAFX21ldGFxAH4AAVsABWFycmF5dAATW0xqYXZhL2xhbmcvT2JqZWN0O3hyABtjbG9qdXJlLmxhbmcuQVBlcnNpc3RlbnRNYXBdfC8DdCByewIAAkkABV9oYXNoSQAHX2hhc2hlcXhwAAAAAAAAAABwdXIAE1tMamF2YS5sYW5nLk9iamVjdDuQzlifEHMpbAIAAHhwAAAABnQADmdldENvbHVtbkNvdW50c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzk0ODSK6FCjrbDduAIAAHhyABZjbG9qdXJlLmxhbmcuQUZ1bmN0aW9uPgZwnJ5G/csCAAFMABFfX21ldGhvZEltcGxDYWNoZXQAHkxjbG9qdXJlL2xhbmcvTWV0aG9kSW1wbENhY2hlO3hwcHQAC2dldFJvd0NvdW50c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzk0ODZ7gA7CIBYdJAIAAUwABW5yb3dzdAASTGphdmEvbGFuZy9PYmplY3Q7eHEAfgAPcHB0AApnZXRWYWx1ZUF0c3IAJWNsb2p1cmUuaW5zcGVjdG9yJGxpc3RfbW9kZWwkZm5fXzk0ODiLldew+D3/eAIAAkwACWdldF9sYWJlbHEAfgAUTAAJZ2V0X3ZhbHVlcQB+ABR4cQB+AA9wcHA="
  })

(defn- decode-base64
  [^String s]
  (.decode (Base64/getDecoder) s))

(deftest test-proxy-non-serializable
  (testing "That proxy classes refuse serialization and deserialization"
    ;; Serializable listed directly in interface list:
    (is (thrown? java.io.NotSerializableException
                 (-> (java.io.ByteArrayOutputStream.)
                     (java.io.ObjectOutputStream.)
                     (.writeObject (proxy [Object java.io.Serializable] [])))))
    ;; Serializable included via inheritence:
    (is (thrown? java.io.NotSerializableException
                 (-> (java.io.ByteArrayOutputStream.)
                     (java.io.ObjectOutputStream.)
                     (.writeObject (clojure.inspector/list-model nil)))))
    ;; Deserialization also prohibited:
    (let [java-version (System/getProperty "java.specification.version")
          serialized-proxy (get serialized-proxies java-version)]
      (if serialized-proxy
        (is (thrown? java.io.NotSerializableException
                     (-> serialized-proxy
                         decode-base64
                         java.io.ByteArrayInputStream. java.io.ObjectInputStream.
                         .readObject)))
        (println "WARNING: Missing serialized proxy for Java" java-version "in test/clojure/test_clojure/java_interop.clj")))))

(deftest test-bases
  (are [x] (nil? (bases x))
    java.lang.Object ;; no super classes/interfaces
    java.lang.Comparable) ;; no super interfaces
  (are [x y] (set/subset? (set y) (set x))
    (bases java.lang.Math) [java.lang.Object]
    (bases java.util.Collection) [java.lang.Iterable]
    (bases java.lang.Integer) [java.lang.Number java.lang.Comparable]))

(deftest test-supers
  (are [x y] (set/subset? y (set x))
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
  (apply conj clojure.lang.PersistentQueue/EMPTY contents))

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

; Test that primitive boxing elision in statement context works
; correctly (CLJ-2621)

(defn inc-atomic-int [^AtomicInteger l]
  (.incrementAndGet l)
  nil)

(defn inc-atomic-long [^AtomicLong l]
  (.incrementAndGet l)
  nil)

(deftest test-boxing-prevention-when-compiling-statements
  (is (= 1 (.get (doto (AtomicInteger. 0) inc-atomic-int))))
  (is (= 1 (.get (doto (AtomicLong. 0) inc-atomic-long)))))

(deftest array-type-symbols
  (is (= long/1 (class (make-array Long/TYPE 0))))
  (is (= int/1 (class (make-array Integer/TYPE 0))))
  (is (= double/1 (class (make-array Double/TYPE 0))))
  (is (= short/1 (class (make-array Short/TYPE 0))))
  (is (= boolean/1 (class (make-array Boolean/TYPE 0))))
  (is (= byte/1 (class (make-array Byte/TYPE 0))))
  (is (= float/1 (class (make-array Float/TYPE 0))))
  (is (= String/1 (class (make-array String 0))))
  (is (= java.lang.String/1 (class (make-array String 0))))
  (is (= java.util.UUID/1 (class (make-array java.util.UUID 0))))
  (is (= `byte/1 'byte/1))
  (is (= `byte/3 'byte/3))
  (is (= `java.util.UUID/1 'java.util.UUID/1))
  (is (= `String/1 'java.lang.String/1))
  (is (= `java.lang.String/1 'java.lang.String/1))
  (is (= ['long/2] `[~'long/2])))


(defn make-test-files []
  (let [id (str (UUID/randomUUID))
        temp-1 (java.io.File/createTempFile (str "test-1-" id)".edn")
        temp-2 (java.io.File/createTempFile "test-2"".xml")
        temp-3 (java.io.File/createTempFile (str "test-3-" id)".edn")
        dir (File. (.getParent temp-3))]
    {:dir dir :file-id id}))

(defn return-long ^long []
  (let [^java.util.function.ToLongFunction f (fn ^long [x] 1)]
    (Long/highestOneBit (.applyAsLong f :x))))

(deftest clojure-fn-as-java-fn
  ;; pass Clojure fn as Java Predicate
  (let [coll (java.util.ArrayList. [1 2 3 4 5])]
    (is (true? (.removeIf coll even?)))
    (is (= coll [1 3 5])))
  
  ;; binding type hint triggers coercion
  (is (instance? FileFilter
    (let [^FileFilter ff (fn [f] true)] ff)))

  ;; coercion in let - reflection has types that should work
  (let [{:keys [dir file-id]} (make-test-files)
        ^FileFilter ff (fn [^File f]
                                 (str/includes? (.getName f) file-id))
        filtered (.listFiles dir ff)]
    (is (= 2 (count filtered))))

  ;; coercion in let
  (let [{:keys [dir file-id]} (make-test-files)
        ^FileFilter ff (fn [^File f]
                                 (str/includes? (.getName f) file-id))
        filtered (.listFiles ^File dir ff)]
    (is (= 2 (count filtered))))

  ;;; resolve method ambiguity using member symbol and param-tags
  (let [{:keys [dir file-id]} (make-test-files)
        ^FileFilter ff (fn [^File f]
                                 (str/includes? (.getName f) file-id))
        filtered (^[FileFilter] File/.listFiles dir ff)]
    (is (= 2 (count filtered))))

  (defn files-with-ext [^File dir ext]
    (vec (.list dir ^FilenameFilter #(str/ends-with? % ext))))

  (let [{:keys [dir file-id]} (make-test-files)
        ^FilenameFilter ff (fn [dir file-name]
                             (str/includes? file-name file-id))
        filtered (.list ^File dir ff)]
    (is (= 2 (count filtered))))

  (let [^java.util.function.DoubleToLongFunction f (fn [d] (int d))]
    (is (instance? java.util.function.DoubleToLongFunction f))
    (is (= 10 (.applyAsLong f (double 10.6)))))

  (let [^java.util.function.IntConsumer f (fn [i] nil)]
    (is (nil? (.accept f 42))))

  (let [^java.util.function.IntPredicate f (fn [i] true)]
    (is (true? (.test f 42))))

  (let [arr (java.util.ArrayList. [1 2 3 4 5])
        ^java.util.function.ObjDoubleConsumer f (fn [arr i] nil)]
    (is (nil? (.accept f arr 42))))

  (let [f (constantly 100)
        ^Runnable g f]
    (is (identical? f g) "has been unintentionally adapted"))

  (let [^java.util.function.Predicate pred even?
        coll1 (java.util.ArrayList. [1 2 3 4 5])
        coll2 (java.util.ArrayList. [6 7 8 9 10])]
    (is (instance? java.util.function.Predicate pred))
    (is (true? (.removeIf coll1 pred)))
    (is (= coll1 [1 3 5]))
    (is (true? (.removeIf coll2 pred)))
    (is (= coll2 [7 9])))

  (let [^java.util.function.Predicate pred even?
        coll1 (java.util.ArrayList. [1 2 3 4 5])
        cup-fn (java.util.ArrayList. [1 2 3 4 5])]
    (is (instance? java.util.function.Predicate pred))
    (is (true? (.removeIf coll1 pred)))
    (is (= coll1 [1 3 5]))
    (is (true? (.removeIf cup-fn pred)))
    (is (= cup-fn [1 3 5])))

  (should-not-reflect #(clojure.test-clojure.java-interop/return-long))

  ;; FI in class constructor
  (let [^java.util.function.Predicate hinted-pred (fn [i] (> i 0))
        clj-pred (fn [i] (> i 0))
        fi-constructor-1 (FIConstructor. hinted-pred)
        fi-constructor-2 (FIConstructor. clj-pred)
        fi-constructor-3 (FIConstructor. (fn [i] (> i 0)))]
    (is (= [1 2] (.numbers fi-constructor-1)))
    (is (= [1 2] (.numbers fi-constructor-2)))
    (is (= [1 2] (.numbers fi-constructor-3))))

  ;; FI as arg to static
  (let [^java.util.function.Predicate hinted-pred (fn [i] (> i 0))
        res (FIStatic/numbers hinted-pred)]
    (is (= [1 2] res))))

(deftest eval-in-place-supplier-instance
  (def stream (java.util.stream.Stream/generate ^java.util.function.Supplier (atom 42)))
  (is (instance? java.util.stream.Stream stream)))

(deftest eval-in-place-as-java-fn
  (def filtered-list (.removeIf (java.util.ArrayList. [1 2 3 4 5]) even?))
  (is (true? filtered-list))

  (def fi-constructor-numbers (.numbers (FIConstructor. (fn [i] (> i 0)))))
  (is (= [1 2] fi-constructor-numbers))

  (def fi-static (FIStatic/numbers (fn [i] (< i 0))))
  (is (= [-2 -1] fi-static)))

;; newDirectoryStream is overloaded, takes ^[Path String] or ^[Path DirectoryStream$Filter]
;; so this method will reflect
(defn get-dir-stream [^java.nio.file.Path dir-path glob-pattern]
  (let [path (.toPath (java.io.File. dir-path))]
    (java.nio.file.Files/newDirectoryStream path glob-pattern)))

(deftest test-reflection-to-overloaded-method-taking-FI
  ;; all of these should resolve at runtime in reflection
  (is (not (nil? (get-dir-stream "." "*"))))
  (is (not (nil? (get-dir-stream "." (reify java.nio.file.DirectoryStream$Filter (accept [_ path] (.isDirectory (.toFile path))))))))
  ;; this one gets FI converted from IFn to DirectoryStream$Filter
  (is (not (nil? (get-dir-stream "." (fn [^java.nio.file.Path path] (.isDirectory (.toFile path))))))))

;; we only support FI invoke coercion up to 10 args, this has 11
(definterface ^{java.lang.FunctionalInterface true} FIWontWork
  (invoke [a b c d e f g h i j k]))

(definterface ReceivesFI
  (call [^clojure.test_clojure.java_interop.FIWontWork fi]))

(deftest test-reify-to-FI-allowed
  ;; throws because there is no 11-arity invoker method and thus it is not possible to coerce
  (is (thrown? ClassCastException
        (eval '(let [^clojure.test_clojure.java_interop.FIWontWork f
                     (fn [p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11] p11)]
                 (.invoke f 1 2 3 4 5 6 7 8 9 10 11)))))

  (let [r (reify clojure.test_clojure.java_interop.ReceivesFI
            (call [_ fi] (.invoke fi 0 0 0 0 0 0 0 0 0 0 1)))]

    ;; doesn't throw at compilation time, but throws at runtime
    ;; because IFn cannot be implicitly converted
    (is (thrown? ClassCastException
      (.call r (fn [a b c d e f g h i j k] k))))

    ;; works because the reify implements the FI, no conversion necessary
    (is (= 1 (.call r (reify clojure.test_clojure.java_interop.FIWontWork (invoke [_ a b c d e f g h i j k] k)))))))

(definterface ^{java.lang.FunctionalInterface true} FIPrims
  (^long invoke [^long a ^long b ^long c ^long d]))

(definterface ReceivesFIPrims
  (call [^clojure.test_clojure.java_interop.FIPrims fi]))

(deftest test-match-prim-args-only-to-2
  (let [r (reify clojure.test_clojure.java_interop.ReceivesFIPrims
            (call [_ fi] (.invoke fi 1 2 3 4)))]
    (is (= 4 (.call r (fn [^long a ^long b ^long c ^long d] d))))))

(deftest test-invoke-fiprim-rets
  (let [^clojure.test_clojure.java_interop.FIPrims f (fn [a b c d] a)]
    (is (instance? clojure.test_clojure.java_interop.FIPrims f))
    (is (= 1 (.invoke f 1 2 3 4))))

  (is (= "LLL" (AdapterExerciser/.methodLLL (AdapterExerciser.) (fn ^long [^long a ^long b]))))
  (is (= "OOOO" (AdapterExerciser/.methodOOOO (AdapterExerciser.) (fn ^long [^long a ^long b ^long c]))))
  )

(deftest test-null-reify
  (is (= "null" ((fn [x] (FIStatic/allowsNullFI x)) nil))))

(deftest test-FI-subtype
  (is (= [1 2 3 4 5] (->> (java.util.stream.Stream/iterate 1 inc) stream-seq! (take 5)))))

(deftest class-methods-with-fi-args
  (testing "Constructor accepting FI arg, provided overloaded static class FI"
    (let [fi (FunctionalTester. "Constructor" 0 FunctionalTester/getChar)]
      (is (= \C (.testVar fi)))))

   (testing "Instance method accepting FI arg, provided overloaded static class FI"
     (let [fi (FunctionalTester. "asf" 0 FunctionalTester/getChar)]
       (.instanceMethodWithFIArg fi "Instance" 0 FunctionalTester/getChar)
       (is (= \I (.testVar fi)))))

   (testing "Static method accepting FI arg, provided overloaded static class FI"
     (is (= \S (FunctionalTester/staticMethodWithFIArg "Static" 0 FunctionalTester/getChar)))))

;; call is reflective and one overload takes an FI (Supplier)
(definterface TakesFIOverloaded
  (call [^java.util.function.Supplier s])
  (call [^String s]))

(deftest CLJ-2898-reified-objs-both-IFn-and-FI
  ;; f is both IFn and FI (Supplier)
  (let [f (reify
            java.util.function.Supplier
            (get [_] 100)

            clojure.lang.IFn
            (applyTo [_ _] 201)
            (invoke [_] 200))]

    ;; should not be adapted. use Supplier.get() impl on tl
    (is (= 100 (.get (ThreadLocal/withInitial f))))

    (let [tfio (reify TakesFIOverloaded
                 (call [_ ^java.util.function.Supplier o] (.get o))
                 (call [_ ^String s] "string"))]
      ;; reflective call to TakesFIOverloaded.call()
      ;; as above, should not be adapted and use Supplier.get()
      (is (= 100 (.call tfio (identity f)))))))