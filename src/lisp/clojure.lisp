;/**
; *   Copyright (c) Rich Hickey. All rights reserved.
; *   The use and distribution terms for this software are covered by the
; *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
; *   which can be found in the file CPL.TXT at the root of this distribution.
; *   By using this software in any fashion, you are agreeing to be bound by
; * 	 the terms of this license.
; *   You must not remove this notice, or any other, from this software.
; **/

(defpackage :clojure)
(in-package :clojure)

(defvar *package-separator* #\/)

(defun ensure-package (name)
    "find the package or create it if it doesn't exist"
    (or (find-package name)
        (make-package name :use '())))

(defun load-types (type-file)
"generates symbols for types/classes and members in supplied file
see typedump in the Java/C# side"
  (labels ((type-name (td)
             (second (assoc :name td)))
           (arity (entry)
             (second (assoc :arity (rest entry))))
           (name (entry)
             (second (assoc :name (rest entry))))
           (simple-name (tn)
             (if (find *package-separator* tn)
                 (subseq tn (1+ (position *package-separator* tn :from-end t)))
               tn))
           (sig (entry)
             (format nil "<~{~A~^*~}>~@[~A~]"
                     (mapcar #'simple-name (rest (assoc :args (rest entry))))
                     (simple-name (second (assoc :ret (rest entry)))))))
    (let ((type-descriptors (with-open-file (f type-file)
                              (read f))))
      (dolist (td type-descriptors)
        (let* ((split (position *package-separator* (type-name td) :from-end t))
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
                                               (and (not (eql e entry))
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
                                               (and (not (eql e entry))
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
                 (push entry (get method-sym 'type-info)))))))))))