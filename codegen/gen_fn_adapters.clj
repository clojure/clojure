;; This code was used to generate the clojure.lang.FnAdapters class in
;; Clojure 1.12. This code is not intended to be reused but might be
;; useful in the future as a template for other code gen.

(ns gen-fn-adapters
  (:require
    [clojure.reflect :as reflect]
    [clojure.set :as set]
    [clojure.string :as str])
  (:import
    [java.io StringWriter Writer]))

(def header
  "/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

public class FnAdapters {

    private static RuntimeException notIFnError(Object f) {
        return new RuntimeException(\"Expected function, but found \" + (f == null ? \"null\" : f.getClass().getName()));
    }

")

(def footer
  "}")

;; 1 - arity O's: OO
;; 2 - arity params:  Object a1, Object a2
;; 3 - arity invoke:  a1, a2
(def adaptO-format
  "   public static Object adapt%sO(Object f, %s) {
      if(f instanceof IFn) {
          return ((IFn)f).invoke(%s);
      } else {
          throw notIFnError(f);
      }
  }

")

;; 1 - arity O's: OO
;; 2 - arity params:  Object a1, Object a2
;; 3 - arity invoke:  a1, a2
(def adaptB-format
  "   public static boolean adapt%sB(Object f, %s) {
      if(f instanceof IFn) {
          return RT.booleanCast(((IFn)f).invoke(%s));
      } else {
          throw notIFnError(f);
      }
  }

")

(defn gen-ifn
  "Gen adaptOO and adaptOB for the given arity"
  [^StringBuilder sb arity]
  (let [os nil ;; TODO
        params nil ;; TODO
        invokes nil] ;; TODO
    (.append sb (format adaptO-format os params invokes))
    (.append sb (format adaptB-format os params invokes))))

(defn gen
  []
  (let [sb (StringBuilder. ^String header)]
    ;; generate IFn and boolean IFn (O...O and O...B) arities
    (doseq [i (range 1 11)]
      (gen-ifn sb i))

    (doseq [i (range 2 4)]
      ;; TODO - generate prim param/return combos
      )

    (.append sb footer)
    (spit "src/jvm/clojure/lang/FnAdapters.java" (.toString sb))))
