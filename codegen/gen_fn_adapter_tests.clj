;; This code was used to generate:
;;     generated_all_fi_adapters_in_let.clj
;;     generated_functional_adapters_in_def_requiring_reflection.clj
;;     generated_functional_adapters_in_def.clj
;;     AdapterExerciser.java
;; This code is not intended to be reused but might be
;; useful in the future as a template for other code gen.

(ns gen-fn-adapter-tests
  (:require
    [clojure.string :as str])
  (:import
    [java.io StringWriter Writer]))

(defn let-test-header [imported-methods]
      (format "
(ns clojure.test-clojure.generated-all-fi-adapters-in-let
  (:use clojure.test)
  (:require [clojure.string :as str])
  (:import (clojure.test AdapterExerciser %s)))

  (deftest test-all-fi-adapters-in-let
    (let [^AdapterExerciser exerciser (AdapterExerciser.)" imported-methods))

(defn def-test-header [filename]
      (format "
(ns clojure.test-clojure.%s
  (:use clojure.test)
  (:require [clojure.string :as str])
  (:import (clojure.test AdapterExerciser)))

  (deftest functional-adapters-in-def
           (def exerciser (AdapterExerciser.))" filename))

(def adapter-exerciser-header "
package clojure.test;

public class AdapterExerciser {")

(defn sigs [args return-types]
      (let [fun-sig-reducer (fn [res ret]
                                (mapcat seq [res (map (fn [params]
                                                          (str params ret)) args)]))]
           (reduce fun-sig-reducer [] return-types)))

(defn gen-sigs []
      (let [small-rets ["L" "I" "S" "B" "D" "F" "O"]
            zero-arity (sigs [""] small-rets)
            single-arity (sigs ["L" "D" "O"] small-rets)
            two-arity (sigs ["LL" "LO" "OL" "DD" "LD" "DL" "OO" "OD" "DO"] small-rets)
            big-rets ["O"]
            three-arity (sigs ["OOO"] big-rets)
            four-arity  (sigs ["OOOO"] big-rets)
            five-arity  (sigs ["OOOOO"] big-rets)
            six-arity   (sigs ["OOOOOO"] big-rets)
            seven-arity (sigs ["OOOOOOO"] big-rets)
            eight-arity (sigs ["OOOOOOOO"] big-rets)
            nine-arity  (sigs ["OOOOOOOOO"] big-rets)
            ten-arity   (sigs ["OOOOOOOOOO"] big-rets)]
           (mapcat seq [zero-arity single-arity two-arity three-arity four-arity five-arity six-arity seven-arity eight-arity nine-arity ten-arity])))

(def alphabet (map char (range 97 122)))
(def type-hints {:D "^double "
                 :O "^AdapterExerciser "
                 :L "^long "
                 :I "^int "
                 :F "^float "
                 :Z "^boolean "
                 :S "^short "
                 :B "^byte "})
(def types {:D "double"
            :O "AdapterExerciser"
            :L "long"
            :I "int"
            :F "float"
            :Z "boolean"
            :S "short"
            :B "byte"})
(def method-args {:D "(double 1)"
                  :O "exerciser"
                  :L "(long 1)"
                  :I "1"
                  :F "(float 1)"
                  :Z "false"
                  :S "(short 1)"
                  :B "(byte 1)"})

(defn format-parts [sig]
      (let [return-type-initial (str (last sig))
            return-type (get types (keyword return-type-initial))
            input-types (map str (butlast sig))
            arg-type-hints (map #(get type-hints (keyword %)) input-types)
            java-types (map #(get types (keyword %)) input-types)
            fn-vars (str/join " " (map #(str %1 %2) arg-type-hints (take (count input-types) alphabet)))
            fn-args (str/join " " (map #(get method-args (keyword %)) input-types))
            java-vars (str/join ", " (map #(str %1 " " %2) java-types (take (count input-types) alphabet)))
            fn-body (get method-args (keyword return-type-initial))
            expected-val (get method-args (keyword return-type-initial))]
           {:return-type return-type :fn-args fn-args :return-type-initial return-type-initial :fn-vars fn-vars :fn-body fn-body :input-types input-types :java-vars java-vars :expected-val expected-val}))

(defn gen-imported-methods [sigs]
      (let [sb (StringBuilder. " ")]
           (doseq [sig sigs]
                  (.append sb (format "AdapterExerciser$%s" sig))
                  (.append sb "\n"))
           (.toString sb)))

(defn gen-test-all-fi-adapters-in-let []
      (let [adapter-signatures (gen-sigs)
            imported-methods (gen-imported-methods adapter-signatures)
            sb (StringBuilder. ^String (let-test-header imported-methods))]
           ;; Assemble let
           (doseq [sig adapter-signatures]
                  (let [{:keys [fn-vars fn-body]} (format-parts sig)]
                       (.append sb "\n")
                       (.append sb (format "          ^AdapterExerciser$%s %sadapter (fn [%s] %s)" sig sig fn-vars fn-body))))
           (.append sb "]")
           ;; Assemble test cases
           (doseq [sig adapter-signatures]
                  (let [{:keys [return-type-initial fn-args expected-val]} (format-parts sig)]
                       (.append sb "\n")
                       (.append sb (format "      (is (= (.takes%sRet%s %sadapter %s) %s))" (str/join "" (butlast sig)) return-type-initial sig fn-args expected-val))))
           (.append sb "))")
           (spit "generated_all_fi_adapters_in_let.clj" (.toString sb))))

(defn gen-test-functional-adapters-in-def []
      (let [sb (StringBuilder. ^String (def-test-header "generated-functional-adapters-in-def"))
            adapter-signatures (gen-sigs)]
           (doseq [sig adapter-signatures]
                  (let [{:keys [fn-vars fn-body]} (format-parts sig)]
                       (.append sb "\n")
                       (.append sb (format "           (def %sadapter (fn [%s] %s))" sig fn-vars fn-body))
                       (.append sb "\n")
                       (.append sb (format "           (is (= (.method%s ^AdapterExerciser exerciser %sadapter) %s))" sig sig (str "\"" sig "\"")))))
           (.append sb ")")
           (spit "generated_functional_adapters_in_def.clj" (.toString sb))))

(defn gen-test-functional-adapters-in-def-requiring-reflection []
      (let [sb (StringBuilder. ^String (def-test-header "generated-functional-adapters-in-def-requiring-reflection"))
            adapter-signatures (gen-sigs)]
           (doseq [sig adapter-signatures]
                  (let [{:keys [fn-vars fn-body]} (format-parts sig)]
                       (.append sb "\n")
                       (.append sb (format "           (def %sadapter (fn [%s] %s))" sig fn-vars fn-body))
                       (.append sb "\n")
                       (.append sb (format "           (is (= (.method%s exerciser %sadapter) %s))" sig sig (str "\"" sig "\"")))))
           (.append sb ")")
           (spit "generated_functional_adapters_in_def_requiring_reflection.clj" (.toString sb))))

(defn gen-adapter-exerciser-class []
      (let [sb (StringBuilder. ^String adapter-exerciser-header)
            adapter-signatures (gen-sigs)]
           (doseq [sig adapter-signatures]
                  (let [{:keys [return-type return-type-initial input-types java-vars]} (format-parts sig)]
                       (.append sb "\n")
                       (.append sb "    @FunctionalInterface\n")
                       (.append sb (format "    public interface %s {\n" sig))
                       (.append sb (format "        public %s takes%sRet%s(%s);\n" return-type (str/join "" input-types) return-type-initial java-vars))
                       (.append sb "    }")))
           (doseq [sig adapter-signatures]
                  (.append sb "\n")
                  (.append sb (format "   public String method%s(%s a) { return %s; }" sig sig (str "\"" sig "\""))))
           (.append sb "}")
           (spit "AdapterExerciser.java" (.toString sb))))

(defn gen-all []
  (gen-test-all-fi-adapters-in-let)
  (gen-test-functional-adapters-in-def)
  (gen-test-functional-adapters-in-def-requiring-reflection)
  (gen-adapter-exerciser-class))

(comment
  (gen-all)
  (gen-test-all-fi-adapters-in-let)
  (gen-test-functional-adapters-in-def)
  (gen-test-functional-adapters-in-def-requiring-reflection)
  (gen-adapter-exerciser-class))