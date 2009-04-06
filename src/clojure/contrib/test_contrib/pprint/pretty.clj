;   Copyright (c) Tom Faulhaber, Feb 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.contrib.test-contrib.pprint.pretty
  (:use [clojure.contrib.test-is :only (deftest are run-tests)]
        clojure.contrib.test-contrib.pprint.helper
        clojure.contrib.pprint))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unit tests for the pretty printer
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(simple-tests xp-fill-test
  (binding [*print-pprint-dispatch* *simple-dispatch*
            *print-right-margin* 38
            *print-miser-width* nil]
    (cl-format nil "(let ~:<~@{~:<~w ~_~w~:>~^ ~:_~}~:>~_ ...)~%"
               '((x 4) (*print-length* nil) (z 2) (list nil))))
  "(let ((x 4) (*print-length* nil)\n      (z 2) (list nil))\n ...)\n"

  (binding [*print-pprint-dispatch* *simple-dispatch*
            *print-right-margin* 22]
    (cl-format nil "(let ~:<~@{~:<~w ~_~w~:>~^ ~:_~}~:>~_ ...)~%"
               '((x 4) (*print-length* nil) (z 2) (list nil))))
  "(let ((x 4)\n      (*print-length*\n       nil)\n      (z 2)\n      (list nil))\n ...)\n")

(simple-tests xp-miser-test
  (binding [*print-pprint-dispatch* *simple-dispatch*
            *print-right-margin* 10, *print-miser-width* 9]
    (cl-format nil "~:<LIST ~@_~W ~@_~W ~@_~W~:>" '(first second third)))
  "(LIST\n first\n second\n third)"

  (binding [*print-pprint-dispatch* *simple-dispatch*
            *print-right-margin* 10, *print-miser-width* 8]
    (cl-format nil "~:<LIST ~@_~W ~@_~W ~@_~W~:>" '(first second third)))
  "(LIST first second third)")


(simple-tests prefix-suffix-test
  (binding [*print-pprint-dispatch* *simple-dispatch*
            *print-right-margin* 10, *print-miser-width* 10]
    (cl-format nil "~<{~;LIST ~@_~W ~@_~W ~@_~W~;}~:>" '(first second third)))
  "{LIST\n first\n second\n third}")

(simple-tests pprint-test
  (binding [*print-pprint-dispatch* *simple-dispatch*]
    (write '(defn foo [x y] 
              (let [result (* x y)] 
                (if (> result 400) 
                  (cl-format true "That number is too big")
                  (cl-format true "The  result of ~d x ~d is ~d" x y result))))
           :stream nil))
  "(defn
 foo
 [x y]
 (let
  [result (* x y)]
  (if
   (> result 400)
   (cl-format true \"That number is too big\")
   (cl-format true \"The  result of ~d x ~d is ~d\" x y result))))"

  (with-pprint-dispatch *code-dispatch*
    (write '(defn foo [x y] 
              (let [result (* x y)] 
                (if (> result 400) 
                  (cl-format true "That number is too big")
                  (cl-format true "The  result of ~d x ~d is ~d" x y result))))
           :stream nil))
  "(defn foo [x y]
  (let [result (* x y)]
    (if (> result 400)
      (cl-format true \"That number is too big\")
      (cl-format true \"The  result of ~d x ~d is ~d\" x y result))))"

  (binding [*print-pprint-dispatch* *simple-dispatch*
            *print-right-margin* 15] 
    (write '(fn (cons (car x) (cdr y))) :stream nil))
  "(fn\n (cons\n  (car x)\n  (cdr y)))"

  (with-pprint-dispatch *code-dispatch*
    (binding [*print-right-margin* 52] 
      (write 
       '(add-to-buffer this (make-buffer-blob (str (char c)) nil))
       :stream nil)))
  "(add-to-buffer\n  this\n  (make-buffer-blob (str (char c)) nil))"
  )



(simple-tests pprint-reader-macro-test
  (with-pprint-dispatch *code-dispatch*
    (write (read-string "(map #(first %) [[1 2 3] [4 5 6] [7]])")
	   :stream nil))
  "(map #(first %) [[1 2 3] [4 5 6] [7]])"

  (with-pprint-dispatch *code-dispatch*
    (write (read-string "^#'first")
	   :stream nil))
  "^#'first"

  (with-pprint-dispatch *code-dispatch*
    (write (read-string "@@(ref (ref 1))")
	   :stream nil))
  "@@(ref (ref 1))"

  (with-pprint-dispatch *code-dispatch*
    (write (read-string "'foo")
	   :stream nil))
  "'foo"
)
