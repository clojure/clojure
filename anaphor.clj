;;; anaphor.clj -- Anaphoric macros for Clojure

;; by Stuart Sierra <mail@stuartsierra.com>
;; April 7, 2008

;; Copyright (c) 2008 Stuart Sierra. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; This file defines some simple anaphoric macros for Clojure.
;;
;; See "On Lisp" for a description of anaphoric macros:
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=onlisp&node=Anaphoric+Macros
;;
;; The macros defined in this file differ slightly from the anaphoric
;; macros in "On Lisp" in that they each take a symbol as their first
;; argument.  That symbol is the "anaphor" (for which "On Lisp" always
;; uses "it").  This follows Clojure's preference for non-capturing
;; macros.


(clojure/in-ns 'anaphor)
(clojure/refer 'clojure)

(defmacro aif
  "DEPRECATED in favor of 'if-let' in boot.clj as of Clojure SVN
  revision 755 on March 17 2008.

  Like 'if', but binds the result of the test to 'symbol' in the
  body."
  ([symbol test then]
   `(let [~symbol ~test]
      (if ~symbol ~then)))
  ([symbol test then else]
   `(let [~symbol ~test]
      (if ~symbol ~then ~else))))

(defmacro awhen
  "DEPRECATED in favor of 'when-let' in boot.clj as of Clojure SVN
  revision 755 on March 17 2008.

  Like 'when', but binds the result of the test to 'symbol' in the
  body."
  [symbol test & body]
  `(aif ~symbol ~test
	(do ~@body)))

(defmacro acond
  "Like 'cond', but binds the result of each test to 'symbol' in the
  expression body."
  [symbol & clauses]
  (when clauses
    (list 'aif symbol (first clauses)
	  (second clauses)
	  (cons 'acond (cons symbol (rest (rest clauses)))))))

