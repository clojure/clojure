;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  File: def.clj
;;
;;  def.clj provides variants of def that make including doc strings and
;;  making private definitions more succinct.
;;
;;  scgilardi (gmail)
;;  17 May 2008

(ns clojure.contrib.def)

(defmacro defvar
  "Defines a var with an optional intializer and doc string"
  ([name]
     (list `def name))
  ([name init]
     (list `def name init))
  ([name init doc]
     (list `def (with-meta name (assoc (meta name) :doc doc)) init)))

(defmacro defunbound
  "Defines an unbound var with optional doc string"
  ([name]
     (list `def name))
  ([name doc]
     (list `def (with-meta name (assoc (meta name) :doc doc)))))

(defmacro defmacro-
  "Same as defmacro but yields a private definition"
  [name & decls]
  (list* `defmacro (with-meta name (assoc (meta name) :private true)) decls))

(defmacro defvar-
  "Same as defvar but yields a private definition"
  [name & decls]
  (list* `defvar (with-meta name (assoc (meta name) :private true)) decls))

(defmacro defunbound-
  "Same as defunbound but yields a private definition"
  [name & decls]
  (list* `defunbound (with-meta name (assoc (meta name) :private true)) decls))

(defmacro defstruct-
  "Same as defstruct but yields a private definition"
  [name & decls]
  (list* `defstruct (with-meta name (assoc (meta name) :private true)) decls))

(defmacro defonce-
  "Same as defonce but yields a private definition"
  ([name expr]
     (list `defonce (with-meta name (assoc (meta name) :private true)) expr))
  ([name expr doc]
     (list `defonce (with-meta name (assoc (meta name) :private true :doc doc)) expr)))

(defmacro defalias
  "Defines an alias for a var: a new var with the same root binding (if
  any) and similar metadata. The metadata of the alias is its initial
  metadata (as provided by def) merged into the metadata of the original."
  ([name orig]
     `(do
        (alter-meta!
         (if (.hasRoot (var ~orig))
           (def ~name (.getRoot (var ~orig)))
           (def ~name))
         conj
         (apply dissoc (meta (var ~orig)) (keys (meta (var ~name)))))
        (var ~name)))
  ([name orig doc]
     (list `defalias (with-meta name (assoc (meta name) :doc doc)) orig)))

; defhinted by Chouser:
(defmacro defhinted
  "Defines a var with a type hint matching the class of the given
  init.  Be careful about using any form of 'def' or 'binding' to a
  value of a different type.  See http://paste.lisp.org/display/73344"
  [sym init]
  `(do
     (def ~sym ~init)
     (alter-meta! (var ~sym) assoc :tag (class ~sym))
     (var ~sym)))

; name-with-attributes by Konrad Hinsen:
(defn name-with-attributes
  "To be used in macro definitions.
   Handles optional docstrings and attribute maps for a name to be defined
   in a list of macro arguments. If the first macro argument is a string,
   it is added as a docstring to name and removed from the macro argument
   list. If afterwards the first macro argument is a map, its entries are
   added to the name's metadata map and the map is removed from the
   macro argument list. The return value is a vector containing the name
   with its extended metadata map and the list of unprocessed macro
   arguments."
  [name macro-args]
  (let [[docstring macro-args] (if (string? (first macro-args))
				 [(first macro-args) (next macro-args)]
				 [nil macro-args])
	[attr macro-args]      (if (map? (first macro-args))
				 [(first macro-args) (next macro-args)]
				 [{} macro-args])
	attr                   (if docstring
				 (assoc attr :doc docstring)
				 attr)
	attr                   (if (meta name)
				 (conj (meta name) attr)
				 attr)]
    [(with-meta name attr) macro-args]))
