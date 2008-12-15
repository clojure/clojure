;;; enum.clj -- Java enum classes in Clojure

;; by Stuart Sierra, http://www.stuartsierra.com/
;; May 29, 2008

;; Copyright (c) Stuart Sierra, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; This file helps define Java Enums, introduced in Java 1.5.  Use it
;; when you need to define an enum to pass to a Java method.
;;
;; This file depends on genclass.clj in the Clojure distribution.


(ns clojure.contrib.enum)

(defmacro defenum
  "Generates and loads a subclass of java.lang.Enum, then
  defs symbols as enumerated instances of that class.

  Example:  (defenum my.package.MyEnum FOO BAR)
            ;; FOO and BAR are now instances of MyEnum

  Java equivalent:  enum MyEnum { FOO, BAR };

  Caveats:
  1. The generated class has no values() method.  
  2. The generated class returns false for Class.isEnum().
  3. Enum.valueOf(Class, String) will not work.
  4. Redefining an enum is allowed, but enumeration resets
     to zero."
  [class & symbols]
  ;; Can't load a class twice, so check first:
  (try (. Class (forName (str class)))  
       (catch java.lang.ClassNotFoundException e
         (gen-and-load-class (str class) :extends java.lang.Enum)))
  (cons 'do
        (map (fn [sym val]
                 `(def ~sym (new ~class ~(str sym) ~val)))
             symbols (iterate inc 0))))
