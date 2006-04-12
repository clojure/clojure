;/**
; *   Copyright (c) Rich Hickey. All rights reserved.
; *   The use and distribution terms for this software are covered by the
; *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
; *   which can be found in the file CPL.TXT at the root of this distribution.
; *   By using this software in any fashion, you are agreeing to be bound by
; * 	 the terms of this license.
; *   You must not remove this notice, or any other, from this software.
; **/

(defpackage :clojure
  (:export :load-types :*namespace-separator*))

(in-package :clojure)

(defvar *namespace-separator* nil
 "set to #\/ for JVM, #\. for CLI")

(defun ensure-package (name)
    "find the package or create it if it doesn't exist"
    (or (find-package name)
        (make-package name :use '())))


(defun primitive-name (tn)
  (or (cdr (assoc tn
                   '(("Z" . "boolean")
                     ("B" . "byte")
                     ("C" . "char")
                     ("S" . "short")
                     ("I" . "int")
                     ("J" . "long")
                     ("F" . "float")
                     ("D" . "double")
                     ("V" . "void"))
                   :test #'string-equal))
      tn))

(defun java-array-name? (tn)
  (eql (schar tn 0) #\[))

(defun load-types (type-file)
"generates symbols for types/classes and members in supplied typedump file
 see typedump in the Java/C# side
 uses *namespace-separator*
 note that this interns symbols and pushes plist entries on them, 
 is destructive and not idempotent, so delete-package any packages prior to re-running"
  (unless *namespace-separator*
    (error "*namespace-separator* must be set"))
  (labels
      ((type-name (td)
         (second (assoc :name td)))
       (arity (entry)
         (second (assoc :arity (rest entry))))
       (name (entry)
         (second (assoc :name (rest entry))))
       (simple-name (tn)
         (when tn
           (let ((base-name (if (find *namespace-separator* tn)
                                (subseq tn
                                        (1+ (position *namespace-separator* tn :from-end t))
                                        (position #\; tn :from-end t))
                              (primitive-name (subseq tn (if (java-array-name? tn)
                                                             (1+ (position #\[ tn :from-end t))
                                                           0))))))
             (if (java-array-name? tn)
                 (with-output-to-string (s)
                   (write-string base-name s)
                   (dotimes (x (1+ (position #\[ tn :from-end t)))
                     (write-string "[]" s)))
               base-name))))
         (sig (entry)
              (format nil "<~{~A~^*~}>"
                      (mapcar #'simple-name (rest (assoc :args (rest entry)))))))
    (let ((type-descriptors (with-open-file (f type-file)
                              (read f))))
      (dolist (td type-descriptors)
        (let* ((split (position *namespace-separator* (type-name td) :from-end t))
               (package-name (subseq (type-name td) 0 split))
               (class-name (string-append (subseq (type-name td) (1+ split)) "."))
               (package (ensure-package package-name))
               (class-sym (intern class-name package)))
          (export class-sym package)
          (dolist (entry td)
            (case (first entry)
              (:field
               (let ((field-sym (intern (string-append class-name
                                                       (name entry))
                                        package)))
                 (export field-sym package)
                 (setf (get field-sym 'type-info) entry)))
              (:ctor
               (let* ((ar (arity entry))
                      (overloaded (member-if (lambda (e)
                                               (and (not (equal e entry))
                                                    (eql (first e) :ctor)
                                                    (eql (arity e) ar)))
                                             td))
                      (ctor-sym (intern (concatenate 'string 
                                                     class-name
                                                     "new"
                                                     (when overloaded
                                                       (sig entry)))
                                        package)))
                 (export ctor-sym package)
                 (push entry (get ctor-sym 'type-info))))
              (:method
               (let* ((ar (arity entry))
                      (nm (name entry))
                      (overloaded (member-if (lambda (e)
                                               (and (not (equal e entry))
                                                    (eql (first e) :method)
                                                    (string= (name e) nm)
                                                    (eql (arity e) ar)))
                                             td))
                      (method-sym (intern (concatenate 'string 
                                                       class-name
                                                       nm
                                                       (when overloaded
                                                         (sig entry)))
                                          package)))
                 (export method-sym package)
                 (push entry (get method-sym 'type-info)))))))))
    t))