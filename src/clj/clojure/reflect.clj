;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Stuart Halloway"
      :added "1.3"
      :doc "Reflection on Host Types
Alpha - subject to change.

Two main entry points: 

* type-reflect reflects on something that implements TypeReference.
* reflect (for REPL use) reflects on the class of an instance, or
  on a class if passed a class

Key features:

* Exposes the read side of reflection as pure data. Reflecting
  on a type returns a map with keys :bases, :flags, and :members.

* Canonicalizes class names as Clojure symbols. Types can extend
  to the TypeReference protocol to indicate that they can be
  unambiguously resolved as a type name. The canonical format
  requires one non-Java-ish convention: array brackets are <>
  instead of [] so they can be part of a Clojure symbol.

* Pluggable Reflectors for different implementations. The default
  JavaReflector is good when you have a class in hand, or use
  the AsmReflector for \"hands off\" reflection without forcing
  classes to load.

Platform implementers must:

* Create an implementation of Reflector.
* Create one or more implementations of TypeReference.
* def default-reflector to be an instance that satisfies Reflector."}
  clojure.reflect
  (:require [clojure.set :as set]))

(defprotocol Reflector
  "Protocol for reflection implementers."
  (do-reflect [reflector typeref]))

(defprotocol TypeReference
  "A TypeReference can be unambiguously converted to a type name on
   the host platform.

   All typerefs are normalized into symbols. If you need to
   normalize a typeref yourself, call typesym."
  (typename [o] "Returns Java name as returned by ASM getClassName, e.g. byte[], java.lang.String[]"))

(declare default-reflector)

(defn type-reflect
  "Alpha - subject to change.
   Reflect on a typeref, returning a map with :bases, :flags, and
  :members. In the discussion below, names are always Clojure symbols.

   :bases            a set of names of the type's bases
   :flags            a set of keywords naming the boolean attributes
                     of the type.
   :members          a set of the type's members. Each member is a map
                     and can be a constructor, method, or field.

   Keys common to all members:
   :name             name of the type 
   :declaring-class  name of the declarer
   :flags            keyword naming boolean attributes of the member

   Keys specific to constructors:
   :parameter-types  vector of parameter type names
   :exception-types  vector of exception type names

   Key specific to methods:
   :parameter-types  vector of parameter type names
   :exception-types  vector of exception type names
   :return-type      return type name

   Keys specific to fields:
   :type             type name

   Options:

     :ancestors     in addition to the keys described above, also
                    include an :ancestors key with the entire set of
                    ancestors, and add all ancestor members to
                    :members.
     :reflector     implementation to use. Defaults to JavaReflector,
                    AsmReflector is also an option."
  {:added "1.3"}
  [typeref & options]
  (let [{:keys [ancestors reflector]}
        (merge {:reflector default-reflector}
               (apply hash-map options))
        refl (partial do-reflect reflector)
        result (refl typeref)]
    ;; could make simpler loop of two args: names an
    (if ancestors
      (let [make-ancestor-map (fn [names]
                            (zipmap names (map refl names)))]
        (loop [reflections (make-ancestor-map (:bases result))]
          (let [ancestors-visited (set (keys reflections))
                ancestors-to-visit (set/difference (set (mapcat :bases (vals reflections)))
                                               ancestors-visited)]
            (if (seq ancestors-to-visit)
              (recur (merge reflections (make-ancestor-map ancestors-to-visit)))
              (apply merge-with into result {:ancestors ancestors-visited}
                     (map #(select-keys % [:members]) (vals reflections)))))))
      result)))

(defn reflect
  "Alpha - subject to change.
   Reflect on the type of obj (or obj itself if obj is a class).
   Return value and options are the same as for type-reflect. "
  {:added "1.3"}
  [obj & options]
  (apply type-reflect (if (class? obj) obj (class obj)) options))

(load "reflect/java")
