;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
; Authors: Fogus

(ns clojure.test-clojure.param-tags
  (:use clojure.test)
  (:require
    [clojure.string :as str]
    [clojure.reflect :as r]
    [clojure.test-helper :refer [should-not-reflect]])
  (:import
    (clojure.test SwissArmy ConcreteClass)
    (clojure.lang Tuple Compiler Compiler$CompilerException)
    (java.util Arrays UUID Locale)))

(set! *warn-on-reflection* true)

(deftest no-hints-with-param-tags
  (should-not-reflect
   (defn touc-no-reflect [s]
     (^[] String/.toUpperCase s)))
  (should-not-reflect
   (defn touc-no-reflectq [s]
     (^[] java.lang.String/.toUpperCase s)))
  (should-not-reflect
   (defn touc-no-reflect-arg-tags [s]
     (^[java.util.Locale] String/.toUpperCase s java.util.Locale/ENGLISH)))
  (should-not-reflect
   (defn no-overloads-no-reflect [v]
     (java.time.OffsetDateTime/.getYear v))))

(deftest no-param-tags-use-qualifier
  ;; both Date and OffsetDateTime have .getYear - want to show here the qualifier is used
  (let [f (fn [^java.util.Date d] (java.time.OffsetDateTime/.getYear d))
        date (java.util.Date. 1714495523100)]
    ;; works when passed OffsetDateTime
    (is (= 2024 (f (-> date .toInstant (.atOffset java.time.ZoneOffset/UTC)))))

    ;; fails when passed Date, expects OffsetDateTime
    (is (thrown? ClassCastException
          (f date)))))

(deftest param-tags-in-invocation-positions
  (testing "qualified static method invocation"
    (is (= 3 (^[long] Math/abs -3)))
    (is (= [1 2] (^[_ _] Tuple/create 1 2)))
    (is (= "42" (Long/toString 42))))
  (testing "qualified ctor invocation"
    (is (= (^[long long] UUID/new 1 2) #uuid "00000000-0000-0001-0000-000000000002"))
    (is (= (^[long long] java.util.UUID/new 1 2) #uuid "00000000-0000-0001-0000-000000000002"))
    (is (= "a" (^[String] String/new "a"))))
  (testing "qualified instance method invocation"
    (is (= \A (String/.charAt "A" 0)))
    (is (= "A" (^[java.util.Locale] String/.toUpperCase "a" java.util.Locale/ENGLISH)))
    (is (= "A" (^[Locale] String/.toUpperCase "a" java.util.Locale/ENGLISH)))
    (is (= 65 (aget (^[String] String/.getBytes "A" "US-ASCII") 0)))
    (is (= "42" (^[] Long/.toString 42))))
  (testing "string repr array type resolutions"
     (let [lary (long-array [1 2 3 4 99 100])
           oary (into-array [1 2 3 4 99 100])
           sary (into-array String ["a" "b" "c"])]
       (is (= 4 (^[longs long] Arrays/binarySearch lary (long 99))))
       (is (= 4 (^[objects _] Arrays/binarySearch oary 99)))
       (is (= 4 (^["[Ljava.lang.Object;" _] Arrays/binarySearch oary 99)))
       (is (= 1 (^["[Ljava.lang.Object;" _] Arrays/binarySearch sary "b")))))
  (testing "bad method names"
    (is (thrown? Exception (eval '(^[] java.lang.String/foo "a"))))
    (is (thrown? Exception (eval '(^[] java.lang.String/.foo "a"))))
    (is (thrown? Exception (eval '(^[] Math/new "a"))))))


;; Mapping of symbols returned from reflect call to :parameter-type used as arguments to .getDeclaredMethod,
;; :arg-type used as arguments to the methods and constructors being tested, :arg-tag used as arg-tags
;; to the methods and constructors being tested.
(def reflected-parameter-types {'int {:parameter-type Integer/TYPE
                                      :arg-type "(int 42)"
                                      :arg-tag "int"}
                                'boolean {:parameter-type Boolean/TYPE
                                          :arg-type "true"
                                          :arg-tag "boolean"}
                                'long {:parameter-type Long/TYPE
                                       :arg-type "42"
                                       :arg-tag "long"}
                                'long<> {:parameter-type (Class/forName "[J")
                                         :arg-type "(long-array [1 2])"
                                         :arg-tag "long*"}
                                'int<><> {:parameter-type (Class/forName "[[I")
                                          :arg-type "(make-array Integer/TYPE 1 2)"
                                          :arg-tag "int**"}
                                'java.lang.Object<> {:parameter-type (Class/forName "[Ljava.lang.Object;")
                                                     :arg-type "(into-array [1 2])"
                                                     :arg-tag "\"[Ljava.lang.Object;\""}
                                'java.lang.String<> {:parameter-type (Class/forName "[Ljava.lang.String;")
                                                     :arg-type "(into-array [\"a\" \"b\"])"
                                                     :arg-tag "\"[Ljava.lang.String;\""}})

(defn is-static-method? [class method-name params]
  (let [method (.getDeclaredMethod ^Class class ^String (name method-name) ^"[Ljava.lang.Object;" params)]
    (java.lang.reflect.Modifier/isStatic (.getModifiers method))))

(defn get-methods
  "Reflect the class located at `path`, filter out the public members, add a :type
   of :constructor, :static, or :instance to each."
  [path]
  (let [reflected-class (r/reflect (resolve path))
        public (filter #(contains? (:flags %) :public) (:members reflected-class))]
    (reduce (fn [res m]
              (let [class (-> m :declaring-class resolve)
                    params (into-array Class (map #(-> % reflected-parameter-types :parameter-type) (:parameter-types m)))]
                (cond
                  (not (contains? m :return-type)) (conj res (assoc m :type :constructor))
                  (is-static-method? class (:name m) params) (conj res (assoc m :type :static))
                  :else (conj res (assoc m :type :instance)))))
            [] public)))

(defn exercise-constructor
  "Provided a map of data returned from a call to reflect representing a constructor.
   Construct a new instance of the class providing the appropriate arg-tags and return
   a map containing the new instance and expected target class"
  [{:keys [declaring-class parameter-types] :as m}]
  (let [target-class (-> declaring-class str Class/forName)
        args (str/join " " (map #(-> % reflected-parameter-types :arg-type) parameter-types))
        arg-tags (str/join " " (map #(-> % reflected-parameter-types :arg-tag) parameter-types))
        fun-call-str (read-string (str "(^[" arg-tags "] " declaring-class ". " args ")"))
        _ (should-not-reflect #(eval 'fun-call-str))
        new-instance (eval fun-call-str)]
    {:expected target-class :actual new-instance}))

(defn exercise-static-method
  "Provided a map of data returned from a call to reflect representing a static class method.
   Call the static method providing the appropriate arg-tags and return a map containing
   the actual and expected response."
  [{:keys [name declaring-class parameter-types]}]
  (let [class (str declaring-class)
        method (str name)
        args (str/join " " (map #(-> % reflected-parameter-types :arg-type) parameter-types))
        arg-tags (str/join " " (map #(-> % reflected-parameter-types :arg-tag) parameter-types))
        expected-response (str/join "-" parameter-types)
        fun-call-str (read-string (str "(^[" arg-tags "] " class "/" method " " args ")"))
        _ (should-not-reflect #(eval 'fun-call-str))
        response (eval fun-call-str)]
    {:expected expected-response :actual response}))

(defn exercise-instance-method
  "Provided a map of data returned from a call to reflect representing a class instance method.
   Call the method providing the appropriate arg-tags and return a map containing
   the actual and expected response."
  [{:keys [name declaring-class parameter-types]}]
  (let [method (str "." name)
        args (str/join " " (map #(-> % reflected-parameter-types :arg-type) parameter-types))
        arg-tags (str/join " " (map #(-> % reflected-parameter-types :arg-tag) parameter-types))
        expected-response (str/join "-" parameter-types)
        fun-call-str (read-string (str "(^[" arg-tags "] " declaring-class "/" method " " "(" declaring-class ".)" " " args ")"))
        _ (should-not-reflect #(eval 'fun-call-str))
        response (eval fun-call-str)]
    {:expected expected-response :actual response}))

(deftest arg-tags-in-constructors-and-static-and-instance-methods
  (doseq [m (get-methods 'clojure.test.SwissArmy)]
    (case (:type m)
      :constructor (let [{:keys [expected actual]} (exercise-constructor m)]
                     (is (instance? expected actual)))
      :static (let [{:keys [expected actual]} (exercise-static-method m)]
                (is (= expected actual)))
      :instance (let [{:keys [expected actual]} (exercise-instance-method m)]
                  (is (= expected actual))))))

(deftest field-overloads-method-CLJ-2899-regression
  (testing "overloaded in value position"
    (is (= "static-field" clojure.test.SwissArmy/doppelganger)))

  (testing "overloaded in value position, w/paramtags"
    (is (= "" (apply ^[] clojure.test.SwissArmy/doppelganger []))))

  (testing "overloaded, invoke no args"
    (is (= "" (clojure.test.SwissArmy/doppelganger))))

  (testing "overloaded, invoke w/args"
    (is (= "int-int-long" (clojure.test.SwissArmy/doppelganger (int 1) (int 2) (long 42)))))

  (testing "non-overloaded, field holds IFn, invoke w/args fails"
    (is (thrown? Exception (eval '(clojure.test.SwissArmy/idFn 42))))
    (is (= #'clojure.core/identity clojure.test.SwissArmy/idFn)))

  (testing "non-overloaded, field holds IFn, invoke  no args"
    (is (= #'clojure.core/identity (clojure.test.SwissArmy/idFn))))

  (testing "instance method overloads"
    (is (= "int-int" (clojure.test.SwissArmy/.doppelganger (clojure.test.SwissArmy/new) (int 1) (int 2))))
    (is (= "int-int" (apply clojure.test.SwissArmy/.doppelganger (clojure.test.SwissArmy/new) (int 1) (int 2) [])))))

(defmacro arg-tags-called-in-macro
  [a-type b-type a b]
  `(^[~a-type ~b-type] SwissArmy/staticArityOverloadMethod ~a ~b))

(deftest arg-tags-in-macro
  (is (= "int-int" (arg-tags-called-in-macro int int 1 2))))

(deftest bridge-methods
  (testing "Allows correct intended usage."
    (let [concrete (ConcreteClass.)]
     (is (= 42 (^[Integer] ConcreteClass/.stampWidgets concrete (int 99))))))
  (testing "Will not call bridge method."
    (is (thrown? Compiler$CompilerException
                 (eval '(let [concrete (clojure.test.ConcreteClass.)]
                          (^[Object] ConcreteClass/.stampWidgets concrete (int 99))))))))


(deftest incorrect-arity-invocation-error-messages

  (testing "Invocation with param-tags having incorrect number of args"
    (let [e (try
              (eval '(^[long] Math/abs -1 -2 -3))
              (catch Compiler$CompilerException e (str "-> " (.getMessage (.getCause e)))))]
      (is (not (nil? (re-find #"expected 1.*received 3" e))) "Error message was expected to indicate 1 argument was expected but 2 were provided"))))
