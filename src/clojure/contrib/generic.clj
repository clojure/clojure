;; Support code for generic interfaces

(ns clojure.contrib.generic
  "Generic interface support code

   NOTE: This library is VERY experimental. It WILL change significantly
   with future release."
  (:use [clojure.contrib.types :only (defadt)]))

;
; A dispatch function that separates nulary, unary, binary, and
; higher arity calls and also selects on type for unary and binary
; calls.
;
(defn nary-dispatch
  ([] ::nulary)
  ([x] (type x))
  ([x y]
     [(type x) (type y)])
  ([x y & more] ::nary))

;
; We can't use [::binary :default], so we need to define a root type
; of the type hierarcy. The derivation for Object covers all classes,
; but all non-class types will need an explicit derive clause.
; Ultimately, a macro might take care of this.
;
(def root-type ::any)
(derive Object root-type)

;
; Symbols referring to ::nulary and ::n-ary
;
(def nulary-type ::nulary)
(def nary-type ::nary)

