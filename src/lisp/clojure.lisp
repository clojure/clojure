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
             (second (assoc :name td))))
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
                                                       (second (assoc :name (rest entry))))
                                        package)))
                 (export field-sym package)
                 (setf (get field-sym 'type-info) entry))))))))))