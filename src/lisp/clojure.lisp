;/**
; *   Copyright (c) Rich Hickey. All rights reserved.
; *   The use and distribution terms for this software are covered by the
; *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
; *   which can be found in the file CPL.TXT at the root of this distribution.
; *   By using this software in any fashion, you are agreeing to be bound by
; * 	 the terms of this license.
; *   You must not remove this notice, or any other, from this software.
; **/

(defpackage "clojure"
  (:export :load-types :*namespace-separator*
   :newobj :@ :compile-to :*clojure-source-path* :*clojure-target-path*
   "in-module"
   "defn*" "def"
   "if" "and" "or" "not"
   "block" "let" "let*"))

(in-package "clojure")

(defvar *namespace-separator* nil
 "set to #\/ for JVM, #\. for CLI")


(defconstant +MAX-POSITIONAL-ARITY+ 5)

(defvar *host* nil) ; :jvm or :cli
(defvar *clojure-source-path*)
(defvar *clojure-target-path*)
(defvar *symbols*)
(defvar *keywords*)
(defvar *vars*)
(defvar *accessors*)
(defvar *defvars*)
(defvar *defns*)
(defvar *quoted-aggregates*)
(defvar *nested-fn-bindings*)
(defvar *var-env* nil)
(defvar *frame* nil)
(defvar *next-id*)

;dynamic functions
(defvar *reference-var*)

#|
(let ((*clojure-source-path* #p"/dev/clojure/src/lisp/")
      (*clojure-target-path* #p"/dev/clojure/classes/"))
  (compile-to :jvm "org.clojure" "Clojure"
              "test.lisp"))
(let ((*clojure-source-path* #p"/dev/clojure/src/lisp/")
      (*clojure-target-path* #p"/dev/clojure/classes/test/"))
  (compile-to :cli "org.clojure" "Clojure"
              "test.lisp"))
;build the library
(let ((*clojure-source-path* #p"/dev/clojure/")
      (*clojure-target-path* #p"/dev/gen/clojure/"))
  (compile-to :jvm "org.clojure" "Clojure"
              "arrays.lisp"
            "conditions.lisp"
            "conses.lisp"
            "data-and-control-flow.lisp"
            "hash-tables.lisp"
            "numbers.lisp"
            "printer.lisp"
            "sequences.lisp"
            "symbols.lisp"
            "impl.lisp"))

(let ((*clojure-source-path* #p"/dev/")
      (*clojure-target-path* #p"/dev/clojure/"))
  (compile-to :java "org.clojure.user" "TestArrays"
              "test-arrays.lisp"))

(let ((*clojure-source-path* #p"/dev/")
      (*clojure-target-path* #p"/dev/clojure/"))
  (compile-to :java "org.clojure.user" "TestHash"
              "test-hash.lisp"))
|#


; a simple attribute object lib
(defun newobj (&rest attrs)
  (let ((obj (make-hash-table)))
    (do* ((attrs attrs (nthcdr 2 attrs)))
         ((null attrs))
      (let ((attr (first attrs))
            (val (second attrs)))
        (setf (gethash attr obj) val)))
    obj))

(defmacro @ (attr obj)
  `(gethash ',attr ,obj))


(defun file-type ()
  (ecase *host*
    (:jvm "java")
    (:cli "cs")))

;from c.l.l.
(defun lex-string (string &key (whitespace
                                '(#\space #\newline)))
  "Separates a string at whitespace and returns a list of strings"
  (flet ((whitespace? (char

                       ) (member char whitespace :test #'char=)))
    (let ((tokens nil))
      (do* ((token-start
             (position-if-not #'whitespace? string)
             (when token-end
               (position-if-not #'whitespace? string :start (1+ token-end))))
            (token-end
             (when token-start
               (position-if #'whitespace? string :start token-start))
             (when token-start
               (position-if #'whitespace?
                            string :start token-start))))
           ((null token-start) (nreverse tokens))
        (push (subseq string token-start token-end) tokens)))))
 
(defun file-path (package-name)
  (ecase *host*
    (:jvm (lex-string package-name :whitespace '(#\.)))
    (:cli (list ""))))

(defun package-open-format-string ()
  (ecase *host*
    (:jvm "package ~A;~2%")
    (:cli "namespace ~A {~2%")))

(defun package-close-string ()
  (ecase *host*
    (:jvm "")
    (:cli "}")))

(defun package-import-format-string ()
  (ecase *host*
    (:jvm "import ~A.*;~2%")
    (:cli "using ~A;~2%")))

(defun system-import-string ()
  (ecase *host*
    (:jvm "")
    (:cli "using System;~2%")))

(defun var-member-name (symbol)
  (format nil "~A__~A"
          (munge-name (package-name (symbol-package symbol)))
          (munge-name (symbol-name symbol))))

(defun accessor-member-name (symbol)
  (format nil "ACC__~A__~A"
          (munge-name (package-name (symbol-package symbol)))
          (munge-name (symbol-name symbol))))

(defun symbol-member-name (symbol)
  (format nil "SYM__~A"
          (munge-name (symbol-name symbol))))

(defun keyword-member-name (symbol)
  (format nil "KEY__~A"
          (munge-name (symbol-name symbol))))

(defun munge-name (name)
  (setf name (string name))
  (when (digit-char-p (char name 0))
    (setf name (concatenate 'string "NUM__" name)))
  (labels ((rep (c)
             (second (assoc c
                         '((#\-  #\_)
                           (#\.  #\_)
                           (#\+  "PLUS__")
                           (#\>  "GT__")
                           (#\<  "LT__")
                           (#\=  "EQ__")
                           (#\~  "TILDE__")
                           (#\!  "BANG__")
                           (#\@  "AT__")
                           (#\#  "SHARP__")
                           (#\$  "DOLLAR__")
                           (#\%  "PCT__")
                           (#\^  "CARAT__")
                           (#\&  "AMP__")
                           (#\*  "STAR__")
                           (#\{  "LBRACE__")
                           (#\}  "RBRACE__")
                           (#\[  "LBRACKET__")
                           (#\]  "RBRACKET__")
                           (#\/  "SLASH__")
                           (#\\  "BSLASH__")
                           (#\?  "QMARK__")))))
           (translate (c)
             (let ((r (rep c)))
               (or r c))))
    (if (find-if #'rep name)
        (format nil "~{~A~}" (map 'list #'translate name))
      name)))

(defun begin-static-block (class-name)
  (ecase *host*
    (:jvm (format nil "static {~%"))
    (:cli (format nil "static ~A(){~%" class-name))))


(defun compile-to (host package-name class-name &rest files)
  (let* ((*host* host)
         (orig-package *package*)
         (*features* (list* :clojure host *features*))
         (outpath (make-pathname 
                   :name class-name
                   :type (file-type)
                   :defaults (merge-pathnames 
                              (make-pathname :directory
                                             (list* :relative (file-path package-name)))
                              *clojure-target-path*)))
         (*symbols* (list '|t|))
         (*defns* nil)
         (*defvars* nil)
         (*vars* nil)
         (*keywords* nil)
         (*accessors* nil))
    (with-open-file (target outpath :direction :output :if-exists :supersede)
      (format target "/* Generated by Clojure */~2%")
      (format target (package-open-format-string) package-name)
      (format target (system-import-string))
      (format target (package-import-format-string) "org.clojure.runtime")
      (format target "public class ~A{~%" class-name)
      (unwind-protect
          (dolist (file files)
            (with-open-file (source (merge-pathnames file *clojure-source-path*))
              (labels
                  ((process-form (form)
                     (case (first form)
                       (|in-module| (setf *package* (find-package (second form))))
                       ((|block|) (mapc #'process-form (rest form)))
                       ((|defn*| |def| |defparameter| |defmain|)
                        (let* ((target-sym (second form)))
                          (princ target-sym)
                          (terpri)
                          (let ((*standard-output* target))
                            (convert form))))
                       (t
                        (if (macro-function (car form))
                            (process-form (macroexpand-1 form))
                          (error "Unsupported form ~A" form))))))
                (let ((*readtable* (copy-readtable nil)))
                  (setf (readtable-case *readtable*) :preserve)
                  (do ((form (read source nil 'eof) (read source nil 'eof)))
                      ((eql form 'eof))
                    (process-form form))))))
        (setf *package* orig-package))
      (dolist (sym *symbols*)
        (format target "static Symbol ~A = Symbol.intern(~S);~%"
                (symbol-member-name sym)
                (munge-name (symbol-name sym))))
      (dolist (keyword  *keywords*)
        (format target "static Keyword ~A = Keyword.intern(~S);~%"
                (keyword-member-name keyword)
                (munge-name (symbol-name keyword))))
      (dolist (var *vars*)
        (format target "static Var ~A = Namespace.internVar(~S,~S);~%"
                (var-member-name var)
                (munge-name (package-name (symbol-package var)))
                (munge-name (symbol-name var))))
      (dolist (accessor *accessors*)
        (format target "static Accessor ~A = Namespace.internAccessor(~S,~S);~%"
                (accessor-member-name accessor)
                (munge-name (package-name (symbol-package accessor)))
                (munge-name (symbol-name accessor))))
      (format target "~Atry{~%" (begin-static-block class-name))
        ;(format target "~%static public void __load() ~A{~%" (exception-declaration-string lang))
      (dolist (var *defns*)
        (format target "Namespace.internVar(~S,~S).bind(new ~A());~%"
                (munge-name (package-name (symbol-package var)))
                (munge-name (symbol-name var))
                (munge-name var)))
      (dolist (var-and-init *defvars*)
        (let ((var (@ :var var-and-init))
              (init (@ :init var-and-init)))
          (format target "Namespace.internVar(~S,~S).bind((new ~A()).invoke());~%"
                (munge-name (package-name (symbol-package var)))
                (munge-name (symbol-name var))
                (munge-name init))))
      (format target "}catch(Exception e){}~%}~%")
        ;(format target "}~%")
      (format target "public static void __init(){}~%")
      (format target "}~%")
      (format target "~A~%" (package-close-string)))))

(defun convert (form)
  (let ((tree (analyze :top (macroexpand form)))
        (*next-id* 0))
    ;(print tree)
    (format t "/* Generated by Clojure from the following Lisp:~%") 
    (pprint form)
    (format t "~%~%*/~2%")
    (emit :top tree)
    ;tree
    ))

(defun get-next-id ()
  (incf *next-id*))

(defvar *texpr* (newobj :type :t))

(defun analyze (context form)
  "context - one of :top :return :statement :expression :fn"
  (cond
   ((consp form) (analyze-op context (first form) form))
   ((or (null form)(eql '|nil| form)) nil)
   ((eql '|t| form) *texpr*)
   ((symbolp form) (analyze-symbol context form))
   (t (newobj :type :literal :val form))))

(defmacro |defn| (name params &body body)
  `(|defn*| , name (,params ,@body)))

;(defmacro |block| (&body body)
;  `(|let| nil ,@body))

(defun analyze-op (context op form)
  (case op
    (|quote| (analyze-quote context form))
    (|defn*| (analyze-defn* context form))
    (|def| (analyze-def context form))
    (|block| (analyze-block context form))
    (|fn*| (analyze-fn* context form))
    (|if| (analyze-if context form))
    (|not| (analyze-not context form))
    (|and| (analyze-and context form))
    (|or| (analyze-or context form))
    (|set| (analyze-set context form))
    (|let| (analyze-let context form))
    (|let*| (analyze-let* context form))
    (|loop| (analyze-loop context form))
    (|try| (analyze-try context form))
    (t (analyze-invoke context op form))))

(defmacro emit-to-string (&body body)
  `(with-output-to-string (s)
     (let ((*standard-output* s))
       ,@body)))

(defun analyze-invoke (context op form)
  (declare (ignore context))
  ;if we hit this unspecialized method, it is not a special op, presume function invocation
  (newobj :type :invoke
          :fexpr (if (symbolp op)
                     (analyze-symbol :fn op)
                   (analyze :expression op))
          :args (mapcar (lambda (e)
                          (analyze :expression e))
                        (rest form))))

(defun emit-invoke (context expr)
  (ccase context
    (:statement
     (emit :expression expr)
     (format t ";~%"))
    (:return
     (emit-return expr))
    (:expression
     (let* ((fexpr (@ :fexpr expr))
            (global-binding? (eql :global-binding (@ :type fexpr)))
            (static-method? (will-be-static-method fexpr))
            (args (@ :args expr)))
       (when (not (or global-binding? static-method?))
         (format t "((IFn)"))
       (emit :expression fexpr)        
       (if global-binding?
           (format t ".fn")
         (unless static-method?
           (format t ")")))
       (unless static-method?
         (format t ".invoke"))
       (format t "(__tld")
       (when static-method?
         (let ((closes (@ :closes (first (@ :methods (@ :fn fexpr))))))
           (format t "~{, ~A~}"
                   (mapcar (lambda (b)
                             (binding-name b))
                           closes))))
       (format t "~{, ~A~}"
               (mapcar (lambda (e)
                         (emit-to-string
                          (emit :expression e)))
                       (ldiff args (nthcdr +MAX-POSITIONAL-ARITY+ args))))
       (when (nthcdr +MAX-POSITIONAL-ARITY+ args)
         (format t ",new Object[]{~{~A~^,~}}"
                 (mapcar (lambda (e)
                           (emit-to-string
                            (emit :expression e)))
                         (nthcdr +MAX-POSITIONAL-ARITY+ args))))
       (format t ")")))))

(defun emit (context expr)
  (cond
   ((null expr) (emit-nil context))
   (t (ccase (@ :type expr)
        (:defn* (emit-defn* context expr))
        (:binding (emit-binding context expr))
        (:accessor (emit-accessor context expr))
        (:keyword (emit-keyword context expr))
        (:global-binding (emit-global-binding context expr))
        (:block (emit-block context expr))
        (:invoke (emit-invoke context expr))
        (:let (emit-let context expr))))))

(defun emit-return (expr)
  (format t "return ")
  (emit :expression expr)
  (format t ";~%"))


(defun emit-global-binding (context expr)
  (declare (ignore context))
  (format t "~A" (var-member-name (@ :symbol expr))))

(defun emit-accessor (context expr)
  (declare (ignore context))
  (format t "~A" (accessor-member-name (@ :symbol expr))))

(defun emit-keyword (context expr)
  (declare (ignore context))
  (format t "~A" (keyword-member-name (@ :symbol expr))))

(defun emit-new-closure-instance (name-binding-fn)
  (format t "(new ~A(~{~A~^, ~}))"
          (binding-name name-binding-fn)
          (mapcar (lambda (b)
                    (binding-name b))
                  (@ :closes (first (@ :methods (@ :fn name-binding-fn)))))))

(defun emit-binding (context expr)
  (ccase context
    (:statement) ;var statement is a no-op
    (:expression
     (if (and (@ :anonymous-fn? expr) (not (will-be-static-method expr)))
         (emit-new-closure-instance expr)
       (format t "~A~:[~;.val~]" (binding-name expr) (needs-box expr))))
    (:return (emit-return expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;; let ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun normalize-let-bindings (binding-list)
  (mapcar (lambda (b)
            (if (atom b)
                (list b nil)
              b))
          binding-list))

(defun analyze-let (context form)
  (let ((bindings (normalize-let-bindings (second form)))
        (body (rest (rest form))))
    (cond
     ;special case of (let () expr) ==> expr
     ((not (or bindings (> (length body) 1)))
      (analyze context (third form)))
     ((eql context :expression)
      (analyze :expression `((|fn*| (,(mapcar #' first bindings) ,@body))
                             ,@(mapcar #'second bindings))))
     (t (let* ((binding-inits
                ;init exprs are analyzed prior to adding bindings to env
                (mapcar (lambda (b)
                          (newobj :binding (newobj :type :binding :symbol (first b))
                                :init (analyze :expression (second b))))
                        bindings))
               (*var-env* *var-env*))
          (mapc (lambda (binit)
                  (register-local-binding (@ :binding binit))
                  (add-to-var-env (@ :binding binit)))
                binding-inits)
          (newobj :type :let
                  :binding-inits binding-inits
                  :body (analyze-body context body)))))))

(defun emit-let (context expr)
  (let ((binding-inits (@ :binding-inits expr))
        (body (@ :body expr)))
    (dolist (bi binding-inits)
      (unless (will-be-static-method (@ :binding bi))
        (emit :expression (@ :binding bi))
        (format t " = ")
        (emit :expression (@ :init bi))
        (format t ";~%")))
    (emit-body context body)))

(defun analyze-body (context exprs)
  (when exprs
    (case context
      (:statement
       (mapcar (lambda (expr)
                 (analyze :statement expr))
               exprs))
      (:return
       (append (mapcar (lambda (expr)
                         (analyze :statement expr))
                       (butlast exprs))
               (list (analyze :return (first (last exprs)))))))))

(defun emit-body (context body)
  (case context
        (:return 
         (dolist (e (butlast body))
           (emit :statement e))
         (if body
             (emit :return (first (last body)))
           (format t "return null;~%")))
        (:statement
         (dolist (e body)
           (emit :statement e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; block ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-block (context expr)
  (when (@ :body expr)
    (format t "{~%")
    (emit-body context (@ :body expr))
    (format t "}~%")))

(defun analyze-block (context form)
  (cond 
   ((null (rest form))
      (analyze context '|nil|))
   ((null (rest (rest form)))
      (analyze context (second form)))
   (t (ccase context
        (:expression (analyze context `(|fn*| (() ,@(rest form)))))
        ((:statement :return) (newobj :type :block
                                      :body (analyze-body context (rest form))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; defn ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun analyze-defn* (context form)
  (assert (eql context :top))
  (let* ((*quoted-aggregates* nil)
         (*nested-fn-bindings* nil)
         (fn (analyze :top `(|fn*| ,@(rest (rest form))))))
    (setf (@ :quoted-aggregates fn) *quoted-aggregates*)
    (setf (@ :nested-fn-bindings fn) *nested-fn-bindings*)
    (newobj :type :defn*
            :name (second form) 
            :fn fn)))

(defun register-defn (name)
  (push name *defns*))

(defun emit-defn* (context expr)
  (declare (ignore context))
  (let ((name (@ :name expr)))
    (register-defn name)
    (emit-fn-declaration :top (munge-name name) (@ :fn expr) nil)))

(defun emit-nil (context)
  (ccase context
    (:expression (format t "null"))
    (:statement)
    (:return (emit-return nil))))

(defun reference-var (sym)
  (let ((b (first (member sym *var-env* :key (lambda (b)
                                               (@ :symbol b))))))
    (labels
        ((check-closed (b frame)
           (when (and b frame
                      (not (member b (@ :local-bindings frame)))) ;closed over
             (setf (@ :closed? b) t)
             (pushnew b (@ :closes frame))
             (check-closed b (@ :parent frame)))))
      (check-closed b *frame*))
    b))

(defun add-to-var-env (b)
  (push b *var-env*))

(defun analyze-fn* (context form)
  (declare (ignore context))
  (newobj :type :fn*
          :methods (mapcar (lambda (m)
                             (analyze-method (first m) (rest m)))
                           (rest form))))

(defun analyze-method (params body)
  (let* ((*frame* (newobj :parent *frame*))
         (*var-env* *var-env*)
         (state :reqs))
    (flet ((create-param-binding (p)
             (let ((b (newobj :type :binding :symbol p :param? t)))
               (add-to-var-env b)
               (register-local-binding b)
               b)))
      (dolist (p params)
        (case p
          (& (setf state :rest))
          (t (case state
               (:reqs
                (push (create-param-binding p) (@ :reqs *frame*)))
               (:rest
                (setf (@ :rest *frame*) (create-param-binding p)))))))

      (when (> (length (@ :reqs *frame*)) +MAX-POSITIONAL-ARITY+)
        (error "sorry, can't have more than ~S required args" +MAX-POSITIONAL-ARITY+))
      (setf (@ :reqs *frame*) (nreverse (@ :reqs *frame*)))
      (setf (@ :body *frame*) (analyze :return `(|block| ,@body)))

      *frame*)))

(defun analyze-def (context form)
  (assert (eql context :top))
  (destructuring-bind (name init init-provided) (rest form)
    (newobj :type :def
     :name name
     :init-fn (when init-provided
                (analyze :top `(|fn*| (() ,init)))))))

(defun needs-box (binding)
  (and binding (@ :closed? binding) (@ :assigned? binding)))

(defun binding-type-decl (binding)
  (cond
   ((needs-box binding) "Box")
   (t "Object")))

(defun fn-decl-string ()
  (case *host*
    (:jvm "static")
    (:cli "")))

(defun extends-string ()
  (case *host*
    (:jvm "extends")
    (:cli ":")))

(defun overrides-string ()
  (case *host*
    (:jvm "")
    (:cli "override ")))

(defun fn-name (fn)
  (if (@ :rest fn)
      "doInvoke"
    "invoke"))

(defun exception-declaration-string ()
  (case *host*
    (:jvm "throws Exception")
    (:cli "")))

(defun binding-name (b)
  (format nil "~A~@[__~A~]"
            (munge-name (@ :symbol b))
            (@ :id b)))

(defun can-be-static-method (fn)
  (and (= (length (@ :methods fn)) 1)
       (not (@ :rest (first (@ :methods fn))))))

(defun will-be-static-method (b)
  (and (eql (@ :type b) :binding)
       (@ :fn b)
       (not (or (@ :value-taken? b) (@ :closed? b)))
       (can-be-static-method (@ :fn b))))

(defun emit-binding-declaration (b &optional (init nil init-supplied))
  (format t "~A " (binding-type-decl b))
  (format t "~A"
            (binding-name b))
  (cond
   ((needs-box b)
    (format t " = new Box(~A)" (or init "null")))
   (init-supplied (format t " = ~A" (or init "null"))))
  (format t ";~%"))

(defun munge-closed-over-assigned-arg (b)
  (concatenate 'string (munge-name (@ :symbol b)) "__arg"))

(defun fn-base-class (fn)
  (let ((rest-method (find-if (lambda (m)
                                (@ :rest m))
                              (@ :methods fn))))
    (if rest-method
        (format nil "RestFn~A" (length (@ :reqs rest-method)))
      "AFn")))

(defun emit-fn-declaration (context name fn as-static-method?)
  (let* ((methods (@ :methods fn))
         (base (fn-base-class fn))
         (closes-decls (mapcan (lambda (b)
                                 (list (binding-type-decl b) (@ :name b)))
                               ;expecting only one method if closjure
                               (@ :closes (first methods)))))
    (unless as-static-method?
      ;emit a class declaration
      (format t "~@[~A ~]public class ~A ~A ~A{~%"
              (fn-decl-string)
              name (extends-string) base)
      ;and members and a ctor if closure
      (when closes-decls
        (format t "~{~A ~A;~%~}" closes-decls)
        (format t "public ~A (~{~A ~A~^, ~}){~%" name closes-decls)
        (format t "~{this.~A = ~A;~%~}"
                (mapcan
                 (lambda (b)
                   (let ((s (binding-name b)))
                     (list s s)))
                 (@ :closes (first methods))))
        (format t "}~%")))

    (when as-static-method?
            ;function gets the supplied name, prefix params with closed vars
      (format t "static public Object ~A(~{~A ~A~^, ~}"
              name
              closes-decls))

    (dolist (m methods)
      ;if static, we are expecting this to run once
      (unless as-static-method?
        (format t "~Apublic Object ~A(" (overrides-string) (fn-name m)))

        ;params
      (let ((rest (@ :rest m)))
        (format t "ThreadLocalData __tld~{, ~A ~A~@[~A~]~}"
                (mapcan (lambda (b)
                          (list 
                           (binding-type-decl b)
                           (binding-name b)
                           (when (needs-box b)
                             "__arg")))
                        (@ :reqs m)))
        (when rest
          (format t ", Cons ~A~@[~A~]"
                  (binding-name rest)
                  (when (needs-box rest) "__arg"))))

      (format t ") ~A ~%{~%" (exception-declaration-string))

        ;tls
      (when (@ :needs-tls m)
        (format t "if(__tld == null) __tld = ThreadLocalData.get();~%"))

        ;parameter binding declarations,if needed
          ;reqs
      (dolist (b (@ :reqs m))
        (when (needs-box b)
          (emit-binding-declaration b (munge-closed-over-assigned-arg b))))
        
      ;rest
      (let ((rest (@ :rest m)))
        (when (needs-box rest)
          (emit-binding-declaration rest (munge-closed-over-assigned-arg rest))))

          ;non-param local bindings
      (dolist (b (@ :local-bindings m))
            ; fixup the names, numbering all locals
        (unless (@ :param? b)
          (setf (@ :id b) (get-next-id))
          (unless (or (@ :anonymous-lambda? b)
                      (will-be-static-method b))
            (emit-binding-declaration b))))

          ;body
      (emit :return (@ :body m))
          
          ;end of invoke function
      (format t "}~%"))
        

    (unless as-static-method?
      ;these will only be set on toplevel defn
      (dolist (fb (@ :nested-fn-bindings fn))
        (emit-fn-declaration :statement
                             (binding-name fb)
                             (@ :fn fb)
                             (will-be-static-method fb)))
      (dolist (qa (@ :quoted-aggregates fn))
        (with-slots (symbol form) qa
          (format t "static public Object ~A = " (munge-name (@ :symbol qa)))
          (emit :expression (@ :form qa))
          (format t ";~%")))
      (when (eql context :top)       
        ;anonymous lambdas are named w/gensyms
        ;todo - change, this is fragile
        (when (and (symbolp name) (not (symbol-package name)))
          (format t "static public IFn fn = new ~A();~%" name)))
       ;end of class
      (format t "}~%"))))

(defun register-var-reference (sym)
  (pushnew sym *vars*))

(defun register-accessor-reference (sym)
  (pushnew sym *accessors*))

(defun register-keyword-reference (sym)
  (pushnew sym *keywords*))

(defun register-needs-tls ()
  (setf (@ :needs-tls *frame*) t))

(defun register-local-binding (b)
  (push b (@ :local-bindings *frame*)))

(defun host-symbol? (sym)
  (find #\. (string sym) :start 1))

(defun accessor? (sym)
  (eql (char (string sym) 0) #\.))

(defun analyze-symbol (context sym)
  (cond
   ((keywordp sym)
    (register-keyword-reference sym)
    (newobj :type :keyword :symbol sym))
   ((host-symbol? sym) (newobj :type :host-symbol :symbol sym))
   ((accessor? sym)
    (register-accessor-reference sym)
    (newobj :type :accessor :symbol sym))
   (t (or (reference-var sym)
          ;not a local var
          (progn
            (register-var-reference sym)
            (unless (eql context :fn)
              (register-needs-tls))
            (newobj :type :global-binding :symbol sym)
            )))))


;load-types is for typed host references
;current thinking is that bootstrap compiler will only generate
;reflective host calls, so this will not be needed

#|

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
       (static? (entry)
         (second (assoc :static (rest entry))))
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
               (let ((field-sym (intern (concatenate 'string
                                                     (unless (static? entry)
                                                       ".")
                                                     class-name
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
                                                    (eql (arity e) ar)
                                                    (eql (static? e) (static? entry`))))
                                             td))
                      (method-sym (intern (concatenate 'string 
                                                       (unless (static? entry)
                                                       ".")
                                                       class-name
                                                       nm
                                                       (when overloaded
                                                         (sig entry)))
                                          package)))
                 (export method-sym package)
                 (push entry (get method-sym 'type-info)))))))))
    t))
|#