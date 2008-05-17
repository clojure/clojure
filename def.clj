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

(in-ns 'def)
(clojure/refer 'clojure)

(defmacro defvar
  "Defines a var with an optional intializer and doc string"
  ([name]
   (list `def name))
  ([name init]
   (list `def name init))
  ([name init doc]
   (list `def (with-meta name (assoc (meta name) :doc doc)) init)))

(defmacro defmacro-
  "Same as defmacro but yields a private definition"
  [name & decls]
  (list* `defmacro (with-meta name (assoc (meta name) :private true)) decls))

(defmacro defvar-
  "Same as defvar but yields a private definition"
  [name & decls]
  (list* `defvar (with-meta name (assoc (meta name) :private true)) decls))

(defmacro defalias
  "Defines a new var with the same value and metadata as another except
  a fresh namespace, name, and optionally doc."
  ([name orig]
   `(. (def ~name ~orig) setMeta (meta #'~orig)))
  ([name orig doc]
   `(. (def ~name ~orig) setMeta (assoc (meta #'~orig) :doc ~doc))))
