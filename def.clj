;;  Copyright (c) Stephen C. Gilardi. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.
;;
;;  File: def.clj
;;
;;  def.clj provides variants of def that make including doc strings and
;;  making private definitions more succinct.
;;
;;  scgilardi (gmail)
;;  17 May 2008

(clojure/in-ns 'def)
(clojure/refer 'clojure)

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

(defmacro defalias
  "Defines an alias for a var: a new var with the same value and metadata
  as another with the exception of :namespace, :name, :file, :line, and
  optionally :doc which are those of new var."
  ([name orig]
     `(let [v# (def ~name ~orig)]
        (. v# (setMeta (merge (meta #'~orig) (meta #'~name))))
        v#))
  ([name orig doc]
     `(let [v# (def ~name ~orig)]
        (. v# (setMeta (merge (meta #'~orig) (assoc (meta #'~name) :doc ~doc))))
        v#)))
