;; Macrolet and symbol-macrolet

;; by Konrad Hinsen
;; last updated May 25, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns
  #^{:author "Konrad Hinsen"
     :doc "Local macros and symbol macros

           Local macros are defined by a macrolet form. They are usable only
           inside its body. Symbol macros can be defined globally
           (defsymbolmacro) or locally (symbol-macrolet). A symbol
           macro defines a form that replaces a symbol during macro
           expansion. Function arguments and symbols bound in let
           forms are not subject to symbol macro expansion.

           Local macros are most useful in the definition of the expansion
           of another macro, they may be used anywhere. Global symbol
           macros can be used only inside a with-symbol-macros form."}
  clojure.contrib.macro-utils
  (:use [clojure.contrib.def :only (defvar-)])
  (:use [clojure.contrib.walk :only (prewalk)]))

; A set of all special forms. Special forms are not macro-expanded, making
; it impossible to shadow them by macro definitions. For most special
; forms, all the arguments are simply macro-expanded, but some forms
; get special treatment.
(defvar- special-forms
  #{'def 'loop* 'recur 'if 'let* 'letfn* 'do 'fn* 'quote 'var '. 'set!
    'try 'catch 'finally 'throw 'monitor-enter 'monitor-exit 'new '&})

; The following three vars are constantly redefined using the binding
; form, imitating dynamic scoping.
;
; Local macros.
(defvar- macro-fns {})
; Local symbol macros.
(defvar- macro-symbols {})
; Symbols defined inside let forms or function arguments.
(defvar- protected-symbols #{})

(defn- reserved?
  [symbol]
  "Return true if symbol is a reserved symbol (starting or ending with a dot)."
  (let [s (str symbol)]
    (or (= "." (subs s 0 1))
	(= "." (subs s (dec (count s)))))))

(defn- expand-symbol
  "Expand symbol macros"
  [symbol]
  (cond (contains? protected-symbols symbol) symbol
	(reserved? symbol)                   symbol
	(contains? macro-symbols symbol)     (get macro-symbols symbol)
	:else (let [v (resolve symbol)
		    m (meta v)]
		(if (:symbol-macro m)
		  (var-get v)
		  symbol))))

(defn- expand-1
  "Perform a single non-recursive macro expansion of form."
  [form]
  (cond
    (seq? form)
      (let [f (first form)]
        (cond (contains? special-forms f) form
	      (contains? macro-fns f)     (apply (get macro-fns f) (rest form))
	      (symbol? f)                 (let [exp (expand-symbol f)]
					    (if (= exp f)
					      (clojure.core/macroexpand-1 form)
					      (cons exp (rest form))))
	      ; handle defmacro macros and Java method special forms
	      :else (clojure.core/macroexpand-1 form)))
    (symbol? form)
      (expand-symbol form)
     :else
       form))

(defn- expand
  "Perform repeated non-recursive macro expansion of form, until it no
   longer changes."
  [form]
  (let [ex (expand-1 form)]
    (if (identical? ex form)
      form
      (recur ex))))

(declare expand-all)

(defn- expand-args
  "Recursively expand the arguments of form, leaving its first
   n elements unchanged."
  ([form]
   (expand-args form 1))
  ([form n]
   (doall (concat (take n form) (map expand-all (drop n form))))))

(defn- expand-bindings
  [bindings exprs]
  (if (empty? bindings)
    (list (doall (map expand-all exprs)))
    (let [[[s b] & bindings] bindings]
      (let [b (expand-all b)]
	(binding [protected-symbols (conj protected-symbols s)]
	  (doall (cons [s b] (expand-bindings bindings exprs))))))))

(defn- expand-with-bindings
  "Handle let* and loop* forms. The symbols defined in them are protected
   from symbol macro expansion, the definitions and the body expressions
   are expanded recursively."
  [form]
  (let [f        (first form)
	bindings (partition 2 (second form))
	exprs    (rest (rest form))
	expanded (expand-bindings bindings exprs)
	bindings (vec (apply concat (butlast expanded)))
	exprs    (first (last expanded))]
    (list f bindings exprs)))

(defn- expand-fn-body
  [[args & exprs]]
  (binding [protected-symbols (reduce conj protected-symbols
				     (filter #(not (= % '&)) args))]
    (cons args (doall (map expand-all exprs)))))

(defn- expand-fn
  "Handle fn* forms. The arguments are protected from symbol macro
   expansion, the bodies are expanded recursively."
  [form]
  (let [[f & bodies] form
	name         (when (symbol? (first bodies)) (first bodies))
	bodies       (if (symbol? (first bodies)) (rest bodies) bodies)
	bodies       (if (vector? (first bodies)) (list bodies) bodies)
	bodies       (doall (map expand-fn-body bodies))]
    (if (nil? name)
      (cons f bodies)
      (cons f (cons name bodies)))))

; Handlers for special forms that require special treatment. The default
; is expand-args.
(defvar- special-form-handlers
  {'quote 	  identity
   'var   	  identity
   'def   	  #(expand-args % 2)
   'new           #(expand-args % 2)
   'let*          expand-with-bindings
   'loop*         expand-with-bindings
   'fn*           expand-fn})

(defn- expand-list
  "Recursively expand a form that is a list or a cons."
  [form]
  (let [f (first form)]
    (if (symbol? f)
      (if (contains? special-forms f)
	((get special-form-handlers f expand-args) form)
	(expand-args form))
      (doall (map expand-all form)))))

(defn- expand-all
  "Expand a form recursively."
  [form]
  (let [exp (expand form)]
    (cond (symbol? exp) exp
	  (seq? exp) (expand-list exp)
	  (vector? exp) (into [] (map expand-all exp))
	  (map? exp) (into {} (map expand-all (seq exp)))
	  :else exp)))

(defmacro macrolet
  "Define local macros that are used in the expansion of exprs. The
   syntax is the same as for letfn forms."
  [fn-bindings & exprs]
  (let [names      (map first fn-bindings)
	name-map   (into {} (map (fn [n] [(list 'quote n) n]) names))
	macro-map  (eval `(letfn ~fn-bindings ~name-map))]
    (binding [macro-fns     (merge macro-fns macro-map)
	      macro-symbols (apply dissoc macro-symbols names)]
      `(do ~@(doall (map expand-all exprs))))))

(defmacro symbol-macrolet
  "Define local symbol macros that are used in the expansion of exprs.
   The syntax is the same as for let forms."
  [symbol-bindings & exprs]
  (let [symbol-map (into {} (map vec (partition 2 symbol-bindings)))
	names      (keys symbol-map)]
    (binding [macro-fns     (apply dissoc macro-fns names)
	      macro-symbols (merge macro-symbols symbol-map)]
      `(do ~@(doall (map expand-all exprs))))))

(defmacro defsymbolmacro
  "Define a symbol macro. Because symbol macros are not part of
   Clojure's built-in macro expansion system, they can be used only
   inside a with-symbol-macros form."
  [symbol expansion]
  (let [meta-map (if (meta symbol) (meta symbol) {})
	meta-map (assoc meta-map :symbol-macro true)]
  `(def ~(with-meta symbol meta-map) (quote ~expansion))))

(defmacro with-symbol-macros
  "Fully expand exprs, including symbol macros."
  [& exprs]
  `(do ~@(doall (map expand-all exprs))))

(defmacro deftemplate
  "Define a macro that expands into forms after replacing the
   symbols in params (a vector) by the corresponding parameters
   given in the macro call."
  [name params & forms]
  (let [param-map (for [p params] (list (list 'quote p) (gensym)))
	template-params (vec (map second param-map))
	param-map (vec (apply concat param-map))
	expansion (list 'list (list 'quote `symbol-macrolet) param-map
			(list 'quote (cons 'do forms)))]
    `(defmacro ~name ~template-params ~expansion)))

(defn mexpand-1
  "Like clojure.core/macroexpand-1, but takes into account symbol macros."
  [form]
  (binding [macro-fns {}
	    macro-symbols {}
	    protected-symbols #{}]
    (expand-1 form)))

(defn mexpand
  "Like clojure.core/macroexpand, but takes into account symbol macros."
  [form]
  (binding [macro-fns {}
	    macro-symbols {}
	    protected-symbols #{}]
    (expand form)))

(defn mexpand-all
  "Perform a full recursive macro expansion of a form."
  [form]
  (binding [macro-fns {}
	    macro-symbols {}
	    protected-symbols #{}]
    (expand-all form)))
