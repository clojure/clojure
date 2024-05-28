;; This code was used to generate the clojure.lang.FnInvokers class in
;; Clojure 1.12. This code is not intended to be reused but might be
;; useful in the future as a template for other code gen.

(ns gen-fn-invokers
  (:require
    [clojure.string :as str]))

(def header
  "/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

public class FnInvokers {

    // Encode invoker param/return class to code for method name
    static char encodeInvokerType(Class c) {
        if(Long.TYPE.equals(c)) {
            return 'L';
        } else if(Double.TYPE.equals(c)) {
            return 'D';
        } else if(Integer.TYPE.equals(c)) {
            return 'I';
        } else if(Short.TYPE.equals(c)) {
            return 'S';
        } else if(Byte.TYPE.equals(c)) {
            return 'B';
        } else if(Float.TYPE.equals(c)) {
            return 'F';
        } else if(Boolean.TYPE.equals(c)) {
            return 'Z';
        } else {
            return 'O';
        }
    }

")

(def footer
  "}")

(def invokeO-format
  "    public static Object invoke%sO(IFn f0%s) {
        return f0.invoke(%s);
    }")

(def invokeO-with-l-or-d-arg-format
  "    public static Object invoke%sO(IFn f0%s) {
        if(f0 instanceof IFn.%sO) {
            return ((IFn.%sO)f0).invokePrim(%s);
        } else {
            return f0.invoke(%s);
        }
    }")

(def invokeZ-format
  "    public static boolean invoke%sZ(IFn f0%s) {
        return RT.booleanCast(f0.invoke(%s));
    }")

(def invokeD-format
  "    public static double invoke%sD(IFn f0%s) {
        if(f0 instanceof IFn.%sD) {
            return ((IFn.%sD)f0).invokePrim(%s);
        } else {
            return RT.doubleCast(f0.invoke(%s));
        }
    }")

(def invokeF-format
  "    public static float invoke%sF(IFn f0%s) {
        if(f0 instanceof IFn.%sD) {
            return RT.floatCast(((IFn.%sD)f0).invokePrim(%s));
        } else {
            return RT.floatCast(f0.invoke(%s));
        }
    }")

(def invokeL-format
  "    public static long invoke%sL(IFn f0%s) {
        if(f0 instanceof IFn.%sL) {
            return ((IFn.%sL)f0).invokePrim(%s);
        } else {
            return RT.longCast(f0.invoke(%s));
        }
    }")

(def invokeI-format
  "    public static int invoke%sI(IFn f0%s) {
        if(f0 instanceof IFn.%sL) {
            return RT.intCast(((IFn.%sL)f0).invokePrim(%s));
        } else {
            return RT.intCast(f0.invoke(%s));
        }
    }")

(def invokeS-format
  "    public static short invoke%sS(IFn f0%s) {
        if(f0 instanceof IFn.%sL) {
            return RT.shortCast(((IFn.%sL)f0).invokePrim(%s));
        } else {
            return RT.shortCast(f0.invoke(%s));
        }
    }")

(def invokeB-format
  "    public static byte invoke%sB(IFn f0%s) {
        if(f0 instanceof IFn.%sL) {
            return RT.byteCast(((IFn.%sL)f0).invokePrim(%s));
        } else {
            return RT.byteCast(f0.invoke(%s));
        }
    }")

(def alphabet (map char (range 97 122)))

(def arg-types {:D ", double "
                :L ", long "
                :O ", Object "})

(defn gen-invoke [sig]
  (let [formatter (str (last sig))
        args (map str (butlast sig))
        arg-types (map #(get arg-types (keyword %)) args)
        fn-vars (str/join "" (map #(str %1 %2) arg-types (take (count args) alphabet)))
        fn-vars-sans-type (str/join ", " (take (count args) alphabet))
        arg-str (str/join args)]
    (case formatter
      "O" (if (some #{"D" "L"} args)
            (format invokeO-with-l-or-d-arg-format arg-str fn-vars arg-str arg-str fn-vars-sans-type fn-vars-sans-type)
            (format invokeO-format arg-str fn-vars fn-vars-sans-type))
      "L" (format invokeL-format arg-str fn-vars arg-str arg-str fn-vars-sans-type fn-vars-sans-type)
      "I" (format invokeI-format arg-str fn-vars arg-str arg-str fn-vars-sans-type fn-vars-sans-type)
      "S" (format invokeS-format arg-str fn-vars arg-str arg-str fn-vars-sans-type fn-vars-sans-type)
      "B" (format invokeB-format arg-str fn-vars arg-str arg-str fn-vars-sans-type fn-vars-sans-type)
      "D" (format invokeD-format arg-str fn-vars arg-str arg-str fn-vars-sans-type fn-vars-sans-type)
      "F" (format invokeF-format arg-str fn-vars arg-str arg-str fn-vars-sans-type fn-vars-sans-type)
      "Z" (format invokeZ-format arg-str fn-vars fn-vars-sans-type))))

(defn sigs [args return-types]
  (let [fun-sig-reducer (fn [res ret]
                          (mapcat seq [res (map (fn [params]
                                                  (str params ret)) args)]))]
    (reduce fun-sig-reducer [] return-types)))

(defn gen-sigs []
  (let [small-rets ["L" "I" "S" "B" "D" "F" "Z" "O"]
        zero-arity (sigs [""] small-rets)
        single-arity (sigs ["L" "D" "O"] small-rets)
        two-arity (sigs ["LL" "LO" "OL" "DD" "LD" "DL" "OO" "OD" "DO"] small-rets)
        big-rets ["Z" "O"]
        three-arity (sigs ["OOO"] big-rets)
        four-arity  (sigs ["OOOO"] big-rets)
        five-arity  (sigs ["OOOOO"] big-rets)
        six-arity   (sigs ["OOOOOO"] big-rets)
        seven-arity (sigs ["OOOOOOO"] big-rets)
        eight-arity (sigs ["OOOOOOOO"] big-rets)
        nine-arity  (sigs ["OOOOOOOOO"] big-rets)
        ten-arity   (sigs ["OOOOOOOOOO"] big-rets)]
    (mapcat seq [zero-arity single-arity two-arity three-arity four-arity five-arity six-arity seven-arity eight-arity nine-arity ten-arity])))

(defn gen-invokers []
  (let [sb (StringBuilder. ^String header)
        invoker-signatures (gen-sigs)]
    (doseq [sig invoker-signatures]
      (.append sb (gen-invoke sig))
      (.append sb "\n\n"))
    (.append sb footer)
    (spit "src/jvm/clojure/lang/FnInvokers.java" (.toString sb))))

(comment
  (gen-invokers)
  )