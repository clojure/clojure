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
           (clojure.test FIConstructor FIStatic FunctionalTester
                         AdapterExerciser AdapterExerciser$LL AdapterExerciser$DL AdapterExerciser$OL AdapterExerciser$LI AdapterExerciser$DI
                         AdapterExerciser$LF AdapterExerciser$DF AdapterExerciser$OF
                         AdapterExerciser$OI AdapterExerciser$LB AdapterExerciser$DB AdapterExerciser$OB AdapterExerciser$OD AdapterExerciser$LD
                         AdapterExerciser$DD AdapterExerciser$LO AdapterExerciser$DO AdapterExerciser$OO
                         AdapterExerciser$LLL AdapterExerciser$LOL AdapterExerciser$OLL AdapterExerciser$DDL
                         AdapterExerciser$LDL AdapterExerciser$DLL AdapterExerciser$OOL AdapterExerciser$ODL
                         AdapterExerciser$DOL AdapterExerciser$LLF AdapterExerciser$LOF AdapterExerciser$OLF AdapterExerciser$DDF AdapterExerciser$LDF
                         AdapterExerciser$DLF AdapterExerciser$OOF AdapterExerciser$ODF AdapterExerciser$DOF
                         AdapterExerciser$LLI AdapterExerciser$LOI AdapterExerciser$OLI AdapterExerciser$DDI AdapterExerciser$LDI
                         AdapterExerciser$DLI AdapterExerciser$OOI AdapterExerciser$ODI AdapterExerciser$DOI AdapterExerciser$LLB AdapterExerciser$LOB
                         AdapterExerciser$OLB AdapterExerciser$DDB AdapterExerciser$LDB AdapterExerciser$DLB AdapterExerciser$OOB AdapterExerciser$ODB
                         AdapterExerciser$DOB AdapterExerciser$LLD AdapterExerciser$LOD AdapterExerciser$OLD AdapterExerciser$DDD AdapterExerciser$LDD
                         AdapterExerciser$DLD AdapterExerciser$OOD AdapterExerciser$ODD AdapterExerciser$DOD AdapterExerciser$LLO AdapterExerciser$LOO
                         AdapterExerciser$OLO AdapterExerciser$DDO AdapterExerciser$LDO AdapterExerciser$DLO
                         AdapterExerciser$OOO AdapterExerciser$ODO AdapterExerciser$DOO
                         AdapterExerciser$OOOO AdapterExerciser$OOOOO AdapterExerciser$OOOOOO AdapterExerciser$OOOOOOO
                         AdapterExerciser$OOOOOOOO AdapterExerciser$OOOOOOOOO AdapterExerciser$OOOOOOOOOO AdapterExerciser$OOOOOOOOOOO
                         AdapterExerciser$OOOB AdapterExerciser$OOOOB AdapterExerciser$OOOOOB AdapterExerciser$OOOOOOB
                         AdapterExerciser$OOOOOOOB AdapterExerciser$OOOOOOOOB AdapterExerciser$OOOOOOOOOB AdapterExerciser$OOOOOOOOOOB)))

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
  (is (= long* (class (make-array Long/TYPE 0))))
  (is (= int* (class (make-array Integer/TYPE 0))))
  (is (= double* (class (make-array Double/TYPE 0))))
  (is (= short* (class (make-array Short/TYPE 0))))
  (is (= boolean* (class (make-array Boolean/TYPE 0))))
  (is (= byte* (class (make-array Byte/TYPE 0))))
  (is (= float* (class (make-array Float/TYPE 0))))
  (is (= String* (class (make-array String 0))))
  (is (= java.lang.String* (class (make-array String 0))))
  (is (= java.util.UUID* (class (make-array java.util.UUID 0))))
  (is (= `byte* 'byte*))
  (is (= `byte*** 'byte***))
  (is (= `java.util.UUID* 'java.util.UUID*))
  (is (= `String* 'java.lang.String*))
  (is (= `java.lang.String* 'java.lang.String*))
  (is (= `[NotAClassThatWasImported*] '[clojure.test-clojure.java-interop/NotAClassThatWasImported*]))
  (is (= [long**] `[~long**]))
  (is (= [42] (let [long** 42] `[~long**]))))


(defn make-test-files []
  (let [id (str (UUID/randomUUID))
        temp-1 (java.io.File/createTempFile (str "test-1-" id)".edn")
        temp-2 (java.io.File/createTempFile "test-2"".xml")
        temp-3 (java.io.File/createTempFile (str "test-3-" id)".edn")
        dir (File. (.getParent temp-3))]
    {:dir dir :file-id id}))

(defn return-long ^long []
  (let [^java.util.function.ToLongFunction f (fn ^long [x] 1)]
    (Long/highestOneBit (f :x))))

(deftest clojure-fn-as-java-fn
  ;; pass Clojure fn as Java Predicate
  (let [coll (java.util.ArrayList. [1 2 3 4 5])]
    (is (true? (.removeIf coll even?)))
    (is (= coll [1 3 5])))

  ;; pass Clojure set as Java predicate - function return
  ;; should use Clojure semantics to return logical true
  (let [coll (java.util.ArrayList. [1 2 3 4 5])]
    (is (true? (.removeIf coll #{1 2})))
    (is (= coll [3 4 5])))

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
        filtered (^[FileFilter] File/listFiles dir ff)]
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

(deftest test-all-fi-adapters-in-let

  (let [^AdapterExerciser exerciser (AdapterExerciser.)
        ^AdapterExerciser$LL LLadapter (fn [^long a] (long 1))
        ^AdapterExerciser$DL DLadapter (fn [^double a] (long 1))
        ^AdapterExerciser$OL OLadapter (fn [^AdapterExerciser a] (long 1))
        ^AdapterExerciser$LI LIadapter (fn [^long a] 1)
        ^AdapterExerciser$DI DIadapter (fn [^double a] 1)
        ^AdapterExerciser$OI OIadapter (fn [^AdapterExerciser a] 1)
        ^AdapterExerciser$LB LBadapter (fn [^long a] false)
        ^AdapterExerciser$DB DBadapter (fn [^double a] false)
        ^AdapterExerciser$OB OBadapter (fn [^AdapterExerciser a] false)
        ^AdapterExerciser$OD ODadapter (fn [^AdapterExerciser a] (double 1))
        ^AdapterExerciser$LD LDadapter (fn [^long a] (double 1))
        ^AdapterExerciser$DD DDadapter (fn [^double a] (double 1))
        ^AdapterExerciser$LO LOadapter (fn [^long a] exerciser)
        ^AdapterExerciser$DO DOadapter (fn [^double a] exerciser)
        ^AdapterExerciser$OO OOadapter (fn [^AdapterExerciser a] exerciser)
        ^AdapterExerciser$LF LFadapter (fn [^long a] 1)
        ^AdapterExerciser$DF DFadapter (fn [^double a] (float 1))
        ^AdapterExerciser$OF OFadapter (fn [^AdapterExerciser a] (float 1))
        ^AdapterExerciser$LLL LLLadapter (fn [^long a ^long a] (long 1))
        ^AdapterExerciser$LOL LOLadapter (fn [^long a ^AdapterExerciser b] (long 1))
        ^AdapterExerciser$OLL OLLadapter (fn [^AdapterExerciser a ^long b] (long 1))
        ^AdapterExerciser$DDL DDLadapter (fn [^double a ^double b] (long 1))
        ^AdapterExerciser$LDL LDLadapter (fn [^long a ^double b] (long 1))
        ^AdapterExerciser$DLL DLLadapter (fn [^double d ^long b] (long 1))
        ^AdapterExerciser$OOL OOLadapter (fn [^AdapterExerciser a ^AdapterExerciser b] (long 1))
        ^AdapterExerciser$ODL ODLadapter (fn [^AdapterExerciser a ^double b] (long 1))
        ^AdapterExerciser$DOL DOLadapter (fn [^double a ^AdapterExerciser b] (long 1))
        ^AdapterExerciser$LLI LLIadapter (fn [^long a ^long a] 1)
        ^AdapterExerciser$LOI LOIadapter (fn [^long a ^AdapterExerciser b] 1)
        ^AdapterExerciser$OLI OLIadapter (fn [^AdapterExerciser a ^long b] 1)
        ^AdapterExerciser$DDI DDIadapter (fn [^double a ^double b] 1)
        ^AdapterExerciser$LDI LDIadapter (fn [^long a ^double b] 1)
        ^AdapterExerciser$DLI DLIadapter (fn [^double d ^long b] 1)
        ^AdapterExerciser$OOI OOIadapter (fn [^AdapterExerciser a ^AdapterExerciser b] 1)
        ^AdapterExerciser$ODI ODIadapter (fn [^AdapterExerciser a ^double b] 1)
        ^AdapterExerciser$DOI DOIadapter (fn [^double a ^AdapterExerciser b] 1)
        ^AdapterExerciser$LLF LLFadapter (fn [^long a ^long a] (float 1))
        ^AdapterExerciser$LOF LOFadapter (fn [^long a ^AdapterExerciser b] (float 1))
        ^AdapterExerciser$OLF OLFadapter (fn [^AdapterExerciser a ^long b] (float 1))
        ^AdapterExerciser$DDF DDFadapter (fn [^double a ^double b] (float 1))
        ^AdapterExerciser$LDF LDFadapter (fn [^long a ^double b] (float 1))
        ^AdapterExerciser$DLF DLFadapter (fn [^double d ^long b] (float 1))
        ^AdapterExerciser$OOF OOFadapter (fn [^AdapterExerciser a ^AdapterExerciser b] (float 1))
        ^AdapterExerciser$ODF ODFadapter (fn [^AdapterExerciser a ^double b] (float 1))
        ^AdapterExerciser$DOF DOFadapter (fn [^double a ^AdapterExerciser b] (float 1))
        ^AdapterExerciser$LLB LLBadapter (fn [^long a ^long a] false)
        ^AdapterExerciser$LOB LOBadapter (fn [^long a ^AdapterExerciser b] false)
        ^AdapterExerciser$OLB OLBadapter (fn [^AdapterExerciser a ^long b] false)
        ^AdapterExerciser$DDB DDBadapter (fn [^double a ^double b] false)
        ^AdapterExerciser$LDB LDBadapter (fn [^long a ^double b] false)
        ^AdapterExerciser$DLB DLBadapter (fn [^double d ^long b] false)
        ^AdapterExerciser$OOB OOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b] false)
        ^AdapterExerciser$ODB ODBadapter (fn [^AdapterExerciser a ^double b] false)
        ^AdapterExerciser$DOB DOBadapter (fn [^double a ^AdapterExerciser b] false)
        ^AdapterExerciser$LLD LLDadapter (fn [^long a ^long a] (double 1))
        ^AdapterExerciser$LOD LODadapter (fn [^long a ^AdapterExerciser b] (double 1))
        ^AdapterExerciser$OLD OLDadapter (fn [^AdapterExerciser a ^long b] (double 1))
        ^AdapterExerciser$DDD DDDadapter (fn [^double a ^double b] (double 1))
        ^AdapterExerciser$LDD LDDadapter (fn [^long a ^double b] (double 1))
        ^AdapterExerciser$DLD DLDadapter (fn [^double d ^long b] (double 1))
        ^AdapterExerciser$OOD OODadapter (fn [^AdapterExerciser a ^AdapterExerciser b] (double 1))
        ^AdapterExerciser$ODD ODDadapter (fn [^AdapterExerciser a ^double b] (double 1))
        ^AdapterExerciser$DOD DODadapter (fn [^double a ^AdapterExerciser b] (double 1))
        ^AdapterExerciser$LLO LLOadapter (fn [^long a ^long a] exerciser)
        ^AdapterExerciser$LOO LOOadapter (fn [^long a ^AdapterExerciser b] exerciser)
        ^AdapterExerciser$OLO OLOadapter (fn [^AdapterExerciser a ^long b] exerciser)
        ^AdapterExerciser$DDO DDOadapter (fn [^double a ^double b] exerciser)
        ^AdapterExerciser$LDO LDOadapter (fn [^long a ^double b] exerciser)
        ^AdapterExerciser$DLO DLOadapter (fn [^double d ^long b] exerciser)
        ^AdapterExerciser$OOO OOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b] exerciser)
        ^AdapterExerciser$ODO ODOadapter (fn [^AdapterExerciser a ^double b] exerciser)
        ^AdapterExerciser$DOO DOOadapter (fn [^double a ^AdapterExerciser b] exerciser)
        ^AdapterExerciser$OOOO OOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c] exerciser)
        ^AdapterExerciser$OOOOO OOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d] exerciser)
        ^AdapterExerciser$OOOOOO OOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e] exerciser)
        ^AdapterExerciser$OOOOOOO OOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f] exerciser)
        ^AdapterExerciser$OOOOOOOO OOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g] exerciser)
        ^AdapterExerciser$OOOOOOOOO OOOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h] exerciser)
        ^AdapterExerciser$OOOOOOOOOO OOOOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h ^AdapterExerciser i] exerciser)
        ^AdapterExerciser$OOOOOOOOOOO OOOOOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h ^AdapterExerciser i ^AdapterExerciser j] exerciser)
        ^AdapterExerciser$OOOB OOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c] false)
        ^AdapterExerciser$OOOOB OOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d] false)
        ^AdapterExerciser$OOOOOB OOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e] false)
        ^AdapterExerciser$OOOOOOB OOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f] false)
        ^AdapterExerciser$OOOOOOOB OOOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g] false)
        ^AdapterExerciser$OOOOOOOOB OOOOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h] false)
        ^AdapterExerciser$OOOOOOOOOB OOOOOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h ^AdapterExerciser i] false)
        ^AdapterExerciser$OOOOOOOOOOB OOOOOOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h ^AdapterExerciser i ^AdapterExerciser j] false)]
    (is (= (.takesLRetL LLadapter (long 1)) 1))
    (is (= (.takesDRetL DLadapter (double 1)) 1))
    (is (= (.takesORetL OLadapter exerciser) 1))
    (is (= (.takesLRetI LIadapter (long 1)) 1))
    (is (= (.takesDRetI DIadapter (double 1)) 1))
    (is (= (.takesORetI OIadapter exerciser) 1))
    (is (= (.takesLRetB LBadapter (long 1)) false))
    (is (= (.takesDRetB DBadapter (double 1)) false))
    (is (= (.takesORetB OBadapter exerciser) false))
    (is (= (.takesORetD ODadapter exerciser) (double 1)))
    (is (= (.takesLRetD LDadapter (long 1)) (double 1)))
    (is (= (.takesDRetD DDadapter (double 1)) (double 1)))
    (is (= (.takesLRetO LOadapter (long 1)) exerciser))
    (is (= (.takesDRetO DOadapter (double 1)) exerciser))
    (is (= (.takesORetO OOadapter exerciser) exerciser))
    (is (= (.takesLRetF LFadapter (long 1)) (float 1)))
    (is (= (.takesDRetF DFadapter (double 1)) (float 1)))
    (is (= (.takesORetF OFadapter exerciser) (float 1)))
    (is (= (.takesLLRetL LLLadapter (long 1) (long 1)) (long 1)))
    (is (= (.takesLORetL LOLadapter (long 1) exerciser) (long 1)))
    (is (= (.takesOLRetL OLLadapter exerciser (long 1)) (long 1)))
    (is (= (.takesDDRetL DDLadapter (double 1) (double 1)) (long 1)))
    (is (= (.takesLDRetL LDLadapter (long 1) (double 1)) (long 1)))
    (is (= (.takesDLRetL DLLadapter (double 1) (long 1)) (long 1)))
    (is (= (.takesOORetL OOLadapter exerciser exerciser) (long 1)))
    (is (= (.takesODRetL ODLadapter exerciser (double 1)) (long 1)))
    (is (= (.takesDORetL DOLadapter (double 1) exerciser) (long 1)))
    (is (= (.takesLLRetI LLIadapter (long 1) (long 1)) 1))
    (is (= (.takesLORetI LOIadapter (long 1) exerciser) 1))
    (is (= (.takesOLRetI OLIadapter exerciser (long 1)) 1))
    (is (= (.takesDDRetI DDIadapter (double 1) (double 1)) 1))
    (is (= (.takesLDRetI LDIadapter (long 1) (double 1)) 1))
    (is (= (.takesDLRetI DLIadapter (double 1) (long 1)) 1))
    (is (= (.takesOORetI OOIadapter exerciser exerciser) 1))
    (is (= (.takesODRetI ODIadapter exerciser (double 1)) 1))
    (is (= (.takesDORetI DOIadapter (double 1) exerciser) 1))
    (is (= (.takesLLRetF LLFadapter (long 1) (long 1)) (float 1)))
    (is (= (.takesLORetF LOFadapter (long 1) exerciser) (float 1)))
    (is (= (.takesOLRetF OLFadapter exerciser (long 1)) (float 1)))
    (is (= (.takesDDRetF DDFadapter (double 1) (double 1)) (float 1)))
    (is (= (.takesLDRetF LDFadapter (long 1) (double 1)) (float 1)))
    (is (= (.takesDLRetF DLFadapter (double 1) (long 1)) (float 1)))
    (is (= (.takesOORetF OOFadapter exerciser exerciser) (float 1)))
    (is (= (.takesODRetF ODFadapter exerciser (double 1)) (float 1)))
    (is (= (.takesDORetF DOFadapter (double 1) exerciser) (float 1)))
    (is (= (.takesLLRetB LLBadapter (long 1) (long 1)) false))
    (is (= (.takesLORetB LOBadapter (long 1) exerciser) false))
    (is (= (.takesOLRetB OLBadapter exerciser (long 1)) false))
    (is (= (.takesDDRetB DDBadapter (double 1) (double 1)) false))
    (is (= (.takesLDRetB LDBadapter (long 1) (double 1)) false))
    (is (= (.takesDLRetB DLBadapter (double 1) (long 1)) false))
    (is (= (.takesOORetB OOBadapter exerciser exerciser) false))
    (is (= (.takesODRetB ODBadapter exerciser (double 1)) false))
    (is (= (.takesDORetB DOBadapter (double 1) exerciser) false))
    (is (= (.takesLLRetD LLDadapter (long 1) (long 1)) (double 1)))
    (is (= (.takesLORetD LODadapter (long 1) exerciser) (double 1)))
    (is (= (.takesOLRetD OLDadapter exerciser (long 1)) (double 1)))
    (is (= (.takesDDRetD DDDadapter (double 1) (double 1)) (double 1)))
    (is (= (.takesLDRetD LDDadapter (long 1) (double 1)) (double 1)))
    (is (= (.takesDLRetD DLDadapter (double 1) (long 1)) (double 1)))
    (is (= (.takesOORetD OODadapter exerciser exerciser) (double 1)))
    (is (= (.takesODRetD ODDadapter exerciser (double 1)) (double 1)))
    (is (= (.takesDORetD DODadapter (double 1) exerciser) (double 1)))
    (is (= (.takesLLRetO LLOadapter (long 1) (long 1)) exerciser))
    (is (= (.takesLORetO LOOadapter (long 1) exerciser) exerciser))
    (is (= (.takesOLRetO OLOadapter exerciser (long 1)) exerciser))
    (is (= (.takesDDRetO DDOadapter (double 1) (double 1)) exerciser))
    (is (= (.takesLDRetO LDOadapter (long 1) (double 1)) exerciser))
    (is (= (.takesDLRetO DLOadapter (double 1) (long 1)) exerciser))
    (is (= (.takesOORetO OOOadapter exerciser exerciser) exerciser))
    (is (= (.takesODRetO ODOadapter exerciser (double 1)) exerciser))
    (is (= (.takesDORetO DOOadapter (double 1) exerciser) exerciser))
    (is (= (.takesOOORetO OOOOadapter exerciser exerciser exerciser) exerciser))
    (is (= (.takesOOOORetO OOOOOadapter exerciser exerciser exerciser exerciser) exerciser))
    (is (= (.takesOOOOORetO OOOOOOadapter exerciser exerciser exerciser exerciser exerciser) exerciser))
    (is (= (.takesOOOOOORetO OOOOOOOadapter exerciser exerciser exerciser exerciser exerciser exerciser) exerciser))
    (is (= (.takesOOOOOOORetO OOOOOOOOadapter exerciser exerciser exerciser exerciser exerciser exerciser exerciser) exerciser))
    (is (= (.takesOOOOOOOORetO OOOOOOOOOadapter exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser) exerciser))
    (is (= (.takesOOOOOOOOORetO OOOOOOOOOOadapter exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser) exerciser))
    (is (= (.takesOOOOOOOOOORetO OOOOOOOOOOOadapter exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser) exerciser))
    (is (= (.takesOOORetB OOOBadapter exerciser exerciser exerciser) false))
    (is (= (.takesOOOORetB OOOOBadapter exerciser exerciser exerciser exerciser) false))
    (is (= (.takesOOOOORetB OOOOOBadapter exerciser exerciser exerciser exerciser exerciser) false))
    (is (= (.takesOOOOOORetB OOOOOOBadapter exerciser exerciser exerciser exerciser exerciser exerciser) false))
    (is (= (.takesOOOOOOORetB OOOOOOOBadapter exerciser exerciser exerciser exerciser exerciser exerciser exerciser) false))
    (is (= (.takesOOOOOOOORetB OOOOOOOOBadapter exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser) false))
    (is (= (.takesOOOOOOOOORetB OOOOOOOOOBadapter exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser) false))
    (is (= (.takesOOOOOOOOOORetB OOOOOOOOOOBadapter exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser) false))))

(deftest functional-adapters-in-def
  (def exerciser (AdapterExerciser.))
  (def LLadapter (fn [^long a] (long 1)))
  (is (= (.methodLL ^AdapterExerciser exerciser LLadapter) 6))
  (def DLadapter (fn [^double a] (long 1)))
  (is (= (.methodDL ^AdapterExerciser exerciser DLadapter) 7))
  (def OLadapter (fn [^AdapterExerciser a] (long 1)))
  (is (= (.methodOL ^AdapterExerciser exerciser OLadapter) 8))
  (def LIadapter (fn [^long a] 1))
  (is (= (.methodLI ^AdapterExerciser exerciser LIadapter) 9))
  (def DIadapter (fn [^double a] 1))
  (is (= (.methodDI ^AdapterExerciser exerciser DIadapter) 10))
  (def OIadapter (fn [^AdapterExerciser a] 1))
  (is (= (.methodOI ^AdapterExerciser exerciser OIadapter) 11))
  (def LBadapter (fn [^long a] false))
  (is (= (.methodLB ^AdapterExerciser exerciser LBadapter) 12))
  (def DBadapter (fn [^double a] false))
  (is (= (.methodDB ^AdapterExerciser exerciser DBadapter) 13))
  (def OBadapter (fn [^AdapterExerciser a] false))
  (is (= (.methodOB ^AdapterExerciser exerciser OBadapter) 14))
  (def ODadapter (fn [^AdapterExerciser a] (double 1)))
  (is (= (.methodOD ^AdapterExerciser exerciser ODadapter) 15))
  (def LDadapter (fn [^long a] (double 1)))
  (is (= (.methodLD ^AdapterExerciser exerciser LDadapter) 16))
  (def DDadapter (fn [^double a] (double 1)))
  (is (= (.methodDD ^AdapterExerciser exerciser DDadapter) 17))
  (def LOadapter (fn [^long a] exerciser))
  (is (= (.methodLO ^AdapterExerciser exerciser LOadapter) 18))
  (def DOadapter (fn [^double a] exerciser))
  (is (= (.methodDO ^AdapterExerciser exerciser DOadapter) 19))
  (def OOadapter (fn [^AdapterExerciser a] exerciser))
  (is (= (.methodOO ^AdapterExerciser exerciser OOadapter) 20))
  (def LFadapter (fn [^long a] 1))
  (is (= (.methodLF ^AdapterExerciser exerciser LFadapter) 18))
  (def DFadapter (fn [^double a] 2))
  (is (= (.methodDF ^AdapterExerciser exerciser DFadapter) 19))
  (def OFadapter (fn [^AdapterExerciser a] 3))
  (is (= (.methodOF ^AdapterExerciser exerciser OFadapter) 20))
  (def LLLadapter (fn [^long a ^long a] (long 1)))
  (is (= (.methodLLL ^AdapterExerciser exerciser LLLadapter) 21))
  (def LOLadapter (fn [^long a ^AdapterExerciser b] (long 1)))
  (is (= (.methodLOL ^AdapterExerciser exerciser LOLadapter) 22))
  (def OLLadapter (fn [^AdapterExerciser a ^long b] (long 1)))
  (is (= (.methodOLL ^AdapterExerciser exerciser OLLadapter) 23))
  (def DDLadapter (fn [^double a ^double b] (long 1)))
  (is (= (.methodDDL ^AdapterExerciser exerciser DDLadapter) 24))
  (def LDLadapter (fn [^long a ^double b] (long 1)))
  (is (= (.methodLDL ^AdapterExerciser exerciser LDLadapter) 25))
  (def DLLadapter (fn [^double d ^long b] (long 1)))
  (is (= (.methodDLL ^AdapterExerciser exerciser DLLadapter) 26))
  (def OOLadapter (fn [^AdapterExerciser a ^AdapterExerciser b] (long 1)))
  (is (= (.methodOOL ^AdapterExerciser exerciser OOLadapter) 27))
  (def ODLadapter (fn [^AdapterExerciser a ^double b] (long 1)))
  (is (= (.methodODL ^AdapterExerciser exerciser ODLadapter) 28))
  (def DOLadapter (fn [^double a ^AdapterExerciser b] (long 1)))
  (is (= (.methodDOL ^AdapterExerciser exerciser DOLadapter) 29))
  (def LLfadapter (fn [^long a ^long a] (float 1)))
  (is (= (.methodLLF ^AdapterExerciser exerciser LLLadapter) 211))
  (def LOfadapter (fn [^long a ^AdapterExerciser b] (float 1)))
  (is (= (.methodLOF ^AdapterExerciser exerciser LOLadapter) 221))
  (def OLfadapter (fn [^AdapterExerciser a ^long b] (float 1)))
  (is (= (.methodOLF ^AdapterExerciser exerciser OLLadapter) 231))
  (def DDfadapter (fn [^double a ^double b] (float 1)))
  (is (= (.methodDDF ^AdapterExerciser exerciser DDLadapter) 241))
  (def LDfadapter (fn [^long a ^double b] (float 1)))
  (is (= (.methodLDF ^AdapterExerciser exerciser LDLadapter) 251))
  (def DLfadapter (fn [^double d ^long b] (float 1)))
  (is (= (.methodDLF ^AdapterExerciser exerciser DLLadapter) 261))
  (def OOfadapter (fn [^AdapterExerciser a ^AdapterExerciser b] (float 1)))
  (is (= (.methodOOF ^AdapterExerciser exerciser OOLadapter) 271))
  (def ODfadapter (fn [^AdapterExerciser a ^double b] (float 1)))
  (is (= (.methodODF ^AdapterExerciser exerciser ODLadapter) 281))
  (def DOfadapter (fn [^double a ^AdapterExerciser b] (float 1)))
  (is (= (.methodDOF ^AdapterExerciser exerciser DOLadapter) 291))
  (def LLIadapter (fn [^long a ^long a] 1))
  (is (= (.methodLLI ^AdapterExerciser exerciser LLIadapter) 30))
  (def LOIadapter (fn [^long a ^AdapterExerciser b] 1))
  (is (= (.methodLOI ^AdapterExerciser exerciser LOIadapter) 31))
  (def OLIadapter (fn [^AdapterExerciser a ^long b] 1))
  (is (= (.methodOLI ^AdapterExerciser exerciser OLIadapter) 32))
  (def DDIadapter (fn [^double a ^double b] 1))
  (is (= (.methodDDI ^AdapterExerciser exerciser DDIadapter) 33))
  (def LDIadapter (fn [^long a ^double b] 1))
  (is (= (.methodLDI ^AdapterExerciser exerciser LDIadapter) 34))
  (def DLIadapter (fn [^double d ^long b] 1))
  (is (= (.methodDLI ^AdapterExerciser exerciser DLIadapter) 35))
  (def OOIadapter (fn [^AdapterExerciser a ^AdapterExerciser b] 1))
  (is (= (.methodOOI ^AdapterExerciser exerciser OOIadapter) 36))
  (def ODIadapter (fn [^AdapterExerciser a ^double b] 1))
  (is (= (.methodODI ^AdapterExerciser exerciser ODIadapter) 37))
  (def DOIadapter (fn [^double a ^AdapterExerciser b] 1))
  (is (= (.methodDOI ^AdapterExerciser exerciser DOIadapter) 38))
  (def LLBadapter (fn [^long a ^long a] false))
  (is (= (.methodLLB ^AdapterExerciser exerciser LLBadapter) 39))
  (def LOBadapter (fn [^long a ^AdapterExerciser b] false))
  (is (= (.methodLOB ^AdapterExerciser exerciser LOBadapter) 40))
  (def OLBadapter (fn [^AdapterExerciser a ^long b] false))
  (is (= (.methodOLB ^AdapterExerciser exerciser OLBadapter) 41))
  (def DDBadapter (fn [^double a ^double b] false))
  (is (= (.methodDDB ^AdapterExerciser exerciser DDBadapter) 42))
  (def LDBadapter (fn [^long a ^double b] false))
  (is (= (.methodLDB ^AdapterExerciser exerciser LDBadapter) 43))
  (def DLBadapter (fn [^double d ^long b] false))
  (is (= (.methodDLB ^AdapterExerciser exerciser DLBadapter) 44))
  (def OOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b] false))
  (is (= (.methodOOB ^AdapterExerciser exerciser OOBadapter) 45))
  (def ODBadapter (fn [^AdapterExerciser a ^double b] false))
  (is (= (.methodODB ^AdapterExerciser exerciser ODBadapter) 46))
  (def DOBadapter (fn [^double a ^AdapterExerciser b] false))
  (is (= (.methodDOB ^AdapterExerciser exerciser DOBadapter) 47))
  (def LLDadapter (fn [^long a ^long a] (double 1)))
  (is (= (.methodLLD ^AdapterExerciser exerciser LLDadapter) 48))
  (def LODadapter (fn [^long a ^AdapterExerciser b] (double 1)))
  (is (= (.methodLOD ^AdapterExerciser exerciser LODadapter) 49))
  (def OLDadapter (fn [^AdapterExerciser a ^long b] (double 1)))
  (is (= (.methodOLD ^AdapterExerciser exerciser OLDadapter) 50))
  (def DDDadapter (fn [^double a ^double b] (double 1)))
  (is (= (.methodDDD ^AdapterExerciser exerciser DDDadapter) 51))
  (def LDDadapter (fn [^long a ^double b] (double 1)))
  (is (= (.methodLDD ^AdapterExerciser exerciser LDDadapter) 52))
  (def DLDadapter (fn [^double d ^long b] (double 1)))
  (is (= (.methodDLD ^AdapterExerciser exerciser DLDadapter) 53))
  (def OODadapter (fn [^AdapterExerciser a ^AdapterExerciser b] (double 1)))
  (is (= (.methodOOD ^AdapterExerciser exerciser OODadapter) 54))
  (def ODDadapter (fn [^AdapterExerciser a ^double b] (double 1)))
  (is (= (.methodODD ^AdapterExerciser exerciser ODDadapter) 55))
  (def DODadapter (fn [^double a ^AdapterExerciser b] (double 1)))
  (is (= (.methodDOD ^AdapterExerciser exerciser DODadapter) 56))
  (def LLOadapter (fn [^long a ^long a] exerciser))
  (is (= (.methodLLO ^AdapterExerciser exerciser LLOadapter) 57))
  (def LOOadapter (fn [^long a ^AdapterExerciser b] exerciser))
  (is (= (.methodLOO ^AdapterExerciser exerciser LOOadapter) 58))
  (def OLOadapter (fn [^AdapterExerciser a ^long b] exerciser))
  (is (= (.methodOLO ^AdapterExerciser exerciser OLOadapter) 59))
  (def DDOadapter (fn [^double a ^double b] exerciser))
  (is (= (.methodDDO ^AdapterExerciser exerciser DDOadapter) 60))
  (def LDOadapter (fn [^long a ^double b] exerciser))
  (is (= (.methodLDO ^AdapterExerciser exerciser LDOadapter) 61))
  (def DLOadapter (fn [^double d ^long b] exerciser))
  (is (= (.methodDLO ^AdapterExerciser exerciser DLOadapter) 62))
  (def OOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b] exerciser))
  (is (= (.methodOOO ^AdapterExerciser exerciser OOOadapter) 63))
  (def ODOadapter (fn [^AdapterExerciser a ^double b] exerciser))
  (is (= (.methodODO ^AdapterExerciser exerciser ODOadapter) 64))
  (def DOOadapter (fn [^double a ^AdapterExerciser b] exerciser))
  (is (= (.methodDOO ^AdapterExerciser exerciser DOOadapter) 65))
  (def OOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c] exerciser))
  (is (= (.methodOOOO ^AdapterExerciser exerciser OOOOadapter) 66))
  (def OOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d] exerciser))
  (is (= (.methodOOOOO ^AdapterExerciser exerciser OOOOOadapter) 67))
  (def OOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e] exerciser))
  (is (= (.methodOOOOOO ^AdapterExerciser exerciser OOOOOOadapter) 68))
  (def OOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f] exerciser))
  (is (= (.methodOOOOOOO ^AdapterExerciser exerciser OOOOOOOadapter) 69))
  (def OOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g] exerciser))
  (is (= (.methodOOOOOOOO ^AdapterExerciser exerciser OOOOOOOOadapter) 70))
  (def OOOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h] exerciser))
  (is (= (.methodOOOOOOOOO ^AdapterExerciser exerciser OOOOOOOOOadapter) 71))
  (def OOOOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h ^AdapterExerciser i] exerciser))
  (is (= (.methodOOOOOOOOOO ^AdapterExerciser exerciser OOOOOOOOOOadapter) 72))
  (def OOOOOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h ^AdapterExerciser i ^AdapterExerciser j] exerciser))
  (is (= (.methodOOOOOOOOOOO ^AdapterExerciser exerciser OOOOOOOOOOOadapter) 73))
  (def OOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c] false))
  (is (= (.methodOOOB ^AdapterExerciser exerciser OOOBadapter) 74))
  (def OOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d] false))
  (is (= (.methodOOOOB ^AdapterExerciser exerciser OOOOBadapter) 75))
  (def OOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e] false))
  (is (= (.methodOOOOOB ^AdapterExerciser exerciser OOOOOBadapter) 76))
  (def OOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f] false))
  (is (= (.methodOOOOOOB ^AdapterExerciser exerciser OOOOOOBadapter) 77))
  (def OOOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g] false))
  (is (= (.methodOOOOOOOB ^AdapterExerciser exerciser OOOOOOOBadapter) 78))
  (def OOOOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h] false))
  (is (= (.methodOOOOOOOOB ^AdapterExerciser exerciser OOOOOOOOBadapter) 79))
  (def OOOOOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h ^AdapterExerciser i] false))
  (is (= (.methodOOOOOOOOOB ^AdapterExerciser exerciser OOOOOOOOOBadapter) 80))
  (def OOOOOOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h ^AdapterExerciser i ^AdapterExerciser j] false))
  (is (= (.methodOOOOOOOOOOB ^AdapterExerciser exerciser OOOOOOOOOOBadapter) 81)))

(deftest functional-adapters-in-def-reflected
  (def exerciser (AdapterExerciser.))
  (def LLadapter (fn [^long a] (long 1)))
  (is (= (.methodLL exerciser LLadapter) 6))
  (def DLadapter (fn [^double a] (long 1)))
  (is (= (.methodDL exerciser DLadapter) 7))
  (def OLadapter (fn [^AdapterExerciser a] (long 1)))
  (is (= (.methodOL exerciser OLadapter) 8))
  (def LIadapter (fn [^long a] 1))
  (is (= (.methodLI exerciser LIadapter) 9))
  (def DIadapter (fn [^double a] 1))
  (is (= (.methodDI exerciser DIadapter) 10))
  (def OIadapter (fn [^AdapterExerciser a] 1))
  (is (= (.methodOI exerciser OIadapter) 11))
  (def LBadapter (fn [^long a] false))
  (is (= (.methodLB exerciser LBadapter) 12))
  (def DBadapter (fn [^double a] false))
  (is (= (.methodDB exerciser DBadapter) 13))
  (def OBadapter (fn [^AdapterExerciser a] false))
  (is (= (.methodOB exerciser OBadapter) 14))
  (def ODadapter (fn [^AdapterExerciser a] (double 1)))
  (is (= (.methodOD exerciser ODadapter) 15))
  (def LDadapter (fn [^long a] (double 1)))
  (is (= (.methodLD exerciser LDadapter) 16))
  (def DDadapter (fn [^double a] (double 1)))
  (is (= (.methodDD exerciser DDadapter) 17))
  (def LOadapter (fn [^long a] exerciser))
  (is (= (.methodLO exerciser LOadapter) 18))
  (def DOadapter (fn [^double a] exerciser))
  (is (= (.methodDO exerciser DOadapter) 19))
  (def OOadapter (fn [^AdapterExerciser a] exerciser))
  (is (= (.methodOO exerciser OOadapter) 20))
  (def LLLadapter (fn [^long a ^long a] (long 1)))
  (is (= (.methodLLL exerciser LLLadapter) 21))
  (def LOLadapter (fn [^long a ^AdapterExerciser b] (long 1)))
  (is (= (.methodLOL exerciser LOLadapter) 22))
  (def OLLadapter (fn [^AdapterExerciser a ^long b] (long 1)))
  (is (= (.methodOLL exerciser OLLadapter) 23))
  (def DDLadapter (fn [^double a ^double b] (long 1)))
  (is (= (.methodDDL exerciser DDLadapter) 24))
  (def LDLadapter (fn [^long a ^double b] (long 1)))
  (is (= (.methodLDL exerciser LDLadapter) 25))
  (def DLLadapter (fn [^double d ^long b] (long 1)))
  (is (= (.methodDLL exerciser DLLadapter) 26))
  (def OOLadapter (fn [^AdapterExerciser a ^AdapterExerciser b] (long 1)))
  (is (= (.methodOOL exerciser OOLadapter) 27))
  (def ODLadapter (fn [^AdapterExerciser a ^double b] (long 1)))
  (is (= (.methodODL exerciser ODLadapter) 28))
  (def DOLadapter (fn [^double a ^AdapterExerciser b] (long 1)))
  (is (= (.methodDOL exerciser DOLadapter) 29))
  (def LLIadapter (fn [^long a ^long a] 1))
  (is (= (.methodLLI exerciser LLIadapter) 30))
  (def LOIadapter (fn [^long a ^AdapterExerciser b] 1))
  (is (= (.methodLOI exerciser LOIadapter) 31))
  (def OLIadapter (fn [^AdapterExerciser a ^long b] 1))
  (is (= (.methodOLI exerciser OLIadapter) 32))
  (def DDIadapter (fn [^double a ^double b] 1))
  (is (= (.methodDDI exerciser DDIadapter) 33))
  (def LDIadapter (fn [^long a ^double b] 1))
  (is (= (.methodLDI exerciser LDIadapter) 34))
  (def DLIadapter (fn [^double d ^long b] 1))
  (is (= (.methodDLI exerciser DLIadapter) 35))
  (def OOIadapter (fn [^AdapterExerciser a ^AdapterExerciser b] 1))
  (is (= (.methodOOI exerciser OOIadapter) 36))
  (def ODIadapter (fn [^AdapterExerciser a ^double b] 1))
  (is (= (.methodODI exerciser ODIadapter) 37))
  (def DOIadapter (fn [^double a ^AdapterExerciser b] 1))
  (is (= (.methodDOI exerciser DOIadapter) 38))
  (def LLBadapter (fn [^long a ^long a] false))
  (is (= (.methodLLB exerciser LLBadapter) 39))
  (def LLfadapter (fn [^long a ^long a] (float 1)))
  (is (= (.methodLLF exerciser LLLadapter) 211))
  (def LOfadapter (fn [^long a ^AdapterExerciser b] (float 1)))
  (is (= (.methodLOF exerciser LOLadapter) 221))
  (def OLfadapter (fn [^AdapterExerciser a ^long b] (float 1)))
  (is (= (.methodOLF exerciser OLLadapter) 231))
  (def DDfadapter (fn [^double a ^double b] (float 1)))
  (is (= (.methodDDF exerciser DDLadapter) 241))
  (def LDfadapter (fn [^long a ^double b] (float 1)))
  (is (= (.methodLDF exerciser LDLadapter) 251))
  (def DLfadapter (fn [^double d ^long b] (float 1)))
  (is (= (.methodDLF exerciser DLLadapter) 261))
  (def OOfadapter (fn [^AdapterExerciser a ^AdapterExerciser b] (float 1)))
  (is (= (.methodOOF exerciser OOLadapter) 271))
  (def ODfadapter (fn [^AdapterExerciser a ^double b] (float 1)))
  (is (= (.methodODF exerciser ODLadapter) 281))
  (def DOfadapter (fn [^double a ^AdapterExerciser b] (float 1)))
  (is (= (.methodDOF exerciser DOLadapter) 291))
  (def LOBadapter (fn [^long a ^AdapterExerciser b] false))
  (is (= (.methodLOB exerciser LOBadapter) 40))
  (def OLBadapter (fn [^AdapterExerciser a ^long b] false))
  (is (= (.methodOLB exerciser OLBadapter) 41))
  (def DDBadapter (fn [^double a ^double b] false))
  (is (= (.methodDDB exerciser DDBadapter) 42))
  (def LDBadapter (fn [^long a ^double b] false))
  (is (= (.methodLDB exerciser LDBadapter) 43))
  (def DLBadapter (fn [^double d ^long b] false))
  (is (= (.methodDLB exerciser DLBadapter) 44))
  (def OOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b] false))
  (is (= (.methodOOB exerciser OOBadapter) 45))
  (def ODBadapter (fn [^AdapterExerciser a ^double b] false))
  (is (= (.methodODB exerciser ODBadapter) 46))
  (def DOBadapter (fn [^double a ^AdapterExerciser b] false))
  (is (= (.methodDOB exerciser DOBadapter) 47))
  (def LLDadapter (fn [^long a ^long a] (double 1)))
  (is (= (.methodLLD exerciser LLDadapter) 48))
  (def LODadapter (fn [^long a ^AdapterExerciser b] (double 1)))
  (is (= (.methodLOD exerciser LODadapter) 49))
  (def OLDadapter (fn [^AdapterExerciser a ^long b] (double 1)))
  (is (= (.methodOLD exerciser OLDadapter) 50))
  (def DDDadapter (fn [^double a ^double b] (double 1)))
  (is (= (.methodDDD exerciser DDDadapter) 51))
  (def LDDadapter (fn [^long a ^double b] (double 1)))
  (is (= (.methodLDD exerciser LDDadapter) 52))
  (def DLDadapter (fn [^double d ^long b] (double 1)))
  (is (= (.methodDLD exerciser DLDadapter) 53))
  (def OODadapter (fn [^AdapterExerciser a ^AdapterExerciser b] (double 1)))
  (is (= (.methodOOD exerciser OODadapter) 54))
  (def ODDadapter (fn [^AdapterExerciser a ^double b] (double 1)))
  (is (= (.methodODD exerciser ODDadapter) 55))
  (def DODadapter (fn [^double a ^AdapterExerciser b] (double 1)))
  (is (= (.methodDOD exerciser DODadapter) 56))
  (def LLOadapter (fn [^long a ^long a] exerciser))
  (is (= (.methodLLO exerciser LLOadapter) 57))
  (def LOOadapter (fn [^long a ^AdapterExerciser b] exerciser))
  (is (= (.methodLOO exerciser LOOadapter) 58))
  (def OLOadapter (fn [^AdapterExerciser a ^long b] exerciser))
  (is (= (.methodOLO exerciser OLOadapter) 59))
  (def DDOadapter (fn [^double a ^double b] exerciser))
  (is (= (.methodDDO exerciser DDOadapter) 60))
  (def LDOadapter (fn [^long a ^double b] exerciser))
  (is (= (.methodLDO exerciser LDOadapter) 61))
  (def DLOadapter (fn [^double d ^long b] exerciser))
  (is (= (.methodDLO exerciser DLOadapter) 62))
  (def OOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b] exerciser))
  (is (= (.methodOOO exerciser OOOadapter) 63))
  (def ODOadapter (fn [^AdapterExerciser a ^double b] exerciser))
  (is (= (.methodODO exerciser ODOadapter) 64))
  (def DOOadapter (fn [^double a ^AdapterExerciser b] exerciser))
  (is (= (.methodDOO exerciser DOOadapter) 65))
  (def OOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c] exerciser))
  (is (= (.methodOOOO exerciser OOOOadapter) 66))
  (def OOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d] exerciser))
  (is (= (.methodOOOOO exerciser OOOOOadapter) 67))
  (def OOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e] exerciser))
  (is (= (.methodOOOOOO exerciser OOOOOOadapter) 68))
  (def OOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f] exerciser))
  (is (= (.methodOOOOOOO exerciser OOOOOOOadapter) 69))
  (def OOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g] exerciser))
  (is (= (.methodOOOOOOOO exerciser OOOOOOOOadapter) 70))
  (def OOOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h] exerciser))
  (is (= (.methodOOOOOOOOO exerciser OOOOOOOOOadapter) 71))
  (def OOOOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h ^AdapterExerciser i] exerciser))
  (is (= (.methodOOOOOOOOOO exerciser OOOOOOOOOOadapter) 72))
  (def OOOOOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h ^AdapterExerciser i ^AdapterExerciser j] exerciser))
  (is (= (.methodOOOOOOOOOOO exerciser OOOOOOOOOOOadapter) 73))
  (def OOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c] false))
  (is (= (.methodOOOB exerciser OOOBadapter) 74))
  (def OOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d] false))
  (is (= (.methodOOOOB exerciser OOOOBadapter) 75))
  (def OOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e] false))
  (is (= (.methodOOOOOB exerciser OOOOOBadapter) 76))
  (def OOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f] false))
  (is (= (.methodOOOOOOB exerciser OOOOOOBadapter) 77))
  (def OOOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g] false))
  (is (= (.methodOOOOOOOB exerciser OOOOOOOBadapter) 78))
  (def OOOOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h] false))
  (is (= (.methodOOOOOOOOB exerciser OOOOOOOOBadapter) 79))
  (def OOOOOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h ^AdapterExerciser i] false))
  (is (= (.methodOOOOOOOOOB exerciser OOOOOOOOOBadapter) 80))
  (def OOOOOOOOOOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h ^AdapterExerciser i ^AdapterExerciser j] false))
  (is (= (.methodOOOOOOOOOOB exerciser OOOOOOOOOOBadapter) 81)))

;(deftest class-methods-with-fi-args
;  (testing "Constructor accepting FI arg, provided overloaded static class FI"
;    (let [fi (FunctionalTester. "Constructor" 0 FunctionalTester/getChar)]
;      (is (= \C (.testVar fi)))))
;
;   (testing "Instance method accepting FI arg, provided overloaded static class FI"
;     (let [fi (FunctionalTester. "asf" 0 FunctionalTester/getChar)]
;       (.instanceMethodWithFIArg fi "Instance" 0 FunctionalTester/getChar)
;       (is (= \I (.testVar fi)))))
;
;   (testing "Static method accepting FI arg, provided overloaded static class FI"
;     (is (= \S (FunctionalTester/staticMethodWithFIArg "Static" 0 FunctionalTester/getChar)))))