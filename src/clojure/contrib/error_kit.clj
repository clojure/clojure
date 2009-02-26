;   Copyright (c) Chris Houser, Jan 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; == EXPERIMENTAL ==
; System for defining and using custom errors
; Please contact Chouser if you have any suggestions for better names
; or API adjustments.

(ns clojure.contrib.error-kit
  (:use [clojure.contrib.def :only (defvar defvar-)]
        [clojure.contrib.stacktrace :only (root-cause)]))

(defn- make-ctrl-exception [msg data]
  "Create an exception object with associated data, used for passing
  control and data to a dynamically containing handler."
  (proxy [Error clojure.lang.IDeref] [msg]
    (toString [] (str "Error Kit Control Exception: " msg ", " (pr-str data)))
    (deref [] data)))

(defvar- ctrl-exception-class
  (class (make-ctrl-exception nil nil)))

(defvar- *handler-stack* () "Stack of bound handler symbols")

(defvar- *continues* {} "Map of currently available continue forms")


(defmacro throw-msg
  "Returns a function that throws a Java Exception with the given
  name.  Useful to associate a new error-kit error type with a
  particular Java Exception class, via the :unhandled error key."
  [class-name]
  `(fn [x#] (throw (new ~class-name (:msg x#)))))

(defn *error*
  "Base type for all error-kit errors"
  {::args [:msg :unhandled :tag]}
  [details]
  (merge {:tag `*error* :msg "exception via error-kit"
          :unhandled (throw-msg Exception)}
         details))

(defn- qualify-sym [sym]
  (let [v (resolve sym)]
    (assert v)
    (apply symbol (map #(str (% ^v)) [:ns :name]))))

(defmacro deferror
  "Define a new error type"
  {:arglists '([name [parent-error?] doc-string? [args*] & body]
               [name [parent-error?] doc-string? args-destruct-map & body])}
  [err-name pvec & decl]
  (let [pvec (if (empty? pvec) [`*error*] pvec)
        [docstr args & body] (if (string? (first decl)) decl (cons nil decl))
        args (or args [])
        argmap (if (vector? args) `{:keys ~args} args)
        body (or body {})
        qual-err-name (symbol (str *ns*) (name err-name))]
    (assert (== (count pvec) 1)) ; only support single-inheritance for now
    (assert (vector? args)) ; only vector (keyword destruct) args for now
    `(do
       (defn ~err-name [details#]
         (let [basedata# ((resolve (first (parents '~qual-err-name))) details#)
               ~argmap basedata#]
           (merge basedata# {:tag '~qual-err-name} (do ~@body) details#)))
       (alter-meta! (var ~err-name) assoc
                    :doc ~docstr ::args ~(vec (map #(keyword (str %)) args)))
       ~@(for [parent pvec]
           `(derive '~qual-err-name '~(qualify-sym parent)))
       (var ~err-name))))

(defn- throw-to [msg target-map args]
  (throw (make-ctrl-exception msg (assoc target-map :args args))))

(defn raise*
  "Raise the given error object, best if created by an error
  constructor defined with deferror.  See also 'raise' macro."
  [err]
  (let [err-tag (:tag err)]
    (loop [hs *handler-stack*]
      (if (empty? hs)
        ((:unhandled err) err)
        (let [[{:keys [htag] :as handler}] hs]
          (if (and htag (not (isa? err-tag htag)))
            (recur (next hs))
            (let [rtn ((:hfunc handler) err)]
              (if-not (vector? rtn)
                (throw-to "default" handler (list rtn))
                (condp = (rtn 0)
                  ::continue-with (rtn 1)
                  ::continue (if-let [continue (*continues* (rtn 1))]
                               (throw-to "continue" continue (rtn 2))
                               (do (prn *continues*) (throw
                                 (Exception.
                                   (str "Unbound continue name " (rtn 1))))))
                  ::do-not-handle (recur (next hs))
                  (throw-to "do-not-handle" handler (list rtn)))))))))))

(defmacro raise
  "Raise an error of the type err-name, constructed with the given args"
  [err-name & args]
  `(raise* (~err-name ~(zipmap (::args ^(resolve err-name))
                               args))))

; It'd be nice to assert that these are used in a tail position of a handler
(defmacro do-not-handle
  "Use in a tail position of a 'handle' form to indicate 'raise' should
  not consider the error handled, but should continue searching for an
  appropriate 'handle' form.  Allows finer-grain control over catching
  than just the error type."
  []
  `[::do-not-handle])

(defmacro continue-with [value]
  "Use in a tail position of a 'handle' form to cause the currently
  running 'raise' to return the given 'value'."
  `[::continue-with ~value])

(defmacro continue [continue-name & args]
  "Use in a tail position of a 'handle' form to pass control to the
  named 'continue' form, passing in the given args.  The 'continue'
  form with the given name and the smallest dynamic scope surrounding
  the currently running 'raise' will be used."
  `[::continue '~continue-name [~@args]])


(def #^{:doc "Special form to be used inside a 'with-handler'.  When
  any error is 'raised' from withing the dynamic scope of 'body' that
  is of error-name's type or a derived type, the args will be bound
  and the body executed.  If no 'error-name' is given, the body will
  be executed for regardless of the type of error raised.  The body
  may return a value, in which case that will be the return value of
  the entire 'with-handler' form, or it may use any of the special
  return forms, 'do-not-handle', 'continue-with', or 'continue'."
          :arglists '([error-name? [args*] & body]
                      [error-name? args-destruct-map-args & body])}
  handle)

(def #^{:doc "Special form to be used inside a 'with-handler'.
  Control can be passed to this 'continue' form from a 'raise' enclosed
  in this with-handler's dynamic scope, when this 'continue-name' is
  given to a 'continue' form."
        :arglists '([continue-name [args*] & body])}
  bind-continue)

(defn- special-form [form]
  (and (list form)
       (symbol? (first form))
       (#{#'handle #'bind-continue} (resolve (first form)))))


(defmacro with-handler
  "This is error-kit's dynamic scope form.  The body will be executed
  in a dynamic context that includes all of the following 'handle' and
  'bind-continue' forms."
  [& forms]
  (let [[body special-forms] (split-with (complement special-form) forms)]
    (assert (every? special-form special-forms))
    (let [blockid (gensym)
          handlers (for [[type & more] special-forms
                         :when (= (resolve type) #'handle)]
                     (let [[htag args & hbody] (if (symbol? (first more))
                                                 more
                                                 (cons nil more))
                           argmap (if (vector? args) `{:keys ~args} args)]
                       `{:blockid '~blockid
                         :htag ~(when htag (list `quote (qualify-sym htag)))
                         :hfunc (fn [~argmap] ~@hbody)
                         :rfunc identity}))
          continues (into {}
                          (for [[type & more] special-forms
                                :when (= (resolve type) #'bind-continue)]
                            [(list `quote (first more))
                             `{:blockid '~blockid
                               :rfunc (fn ~@(next more))}]))]
      `(try
         (binding [*handler-stack* (list* ~@handlers @#'*handler-stack*)
                   *continues* (merge @#'*continues* ~@continues)]
           ~@body)
         (catch Throwable e#
           (let [root-cause# (root-cause e#)]
             (if-not (instance? @#'ctrl-exception-class root-cause#)
               (throw e#)
               (let [data# @root-cause#]
                 (if (= '~blockid (:blockid data#))
                   (apply (:rfunc data#) (:args data#))
                   (throw e#))))))))))

(defn rebind-fn [func]
  (let [a *handler-stack*, b *continues*]
    (fn [& args]
      (binding [*handler-stack* a
                *continues* b]
        (apply func args)))))

(comment

(alias 'kit 'clojure.contrib.error-kit)

; This defines an error and its action if unhandled.  A good choice of
; unhandled. action is to throw a Java exception so users of your code
; who do not want to use error-kit can still use normal Java try/catch
; forms to handle the error.
(kit/deferror *number-error* [] [n]
  {:msg (str "Number error: " n)
   :unhandled (kit/throw-msg NumberFormatException)})

(kit/deferror *odd-number-error* [*number-error*]
  "Indicates an odd number was given to an operation that is only
  defined for even numbers."
  [n]
  {:msg (str "Can't handle odd number: " n)})

; Raise an error by name with any extra args defined by the deferror
(defn int-half [i]
  (if (even? i)
    (quot i 2)
    (kit/raise *odd-number-error* i)))

; Throws Java NumberFormatException because there's no 'handle' form
(vec (map int-half [2 4 5 8]))

; Throws Java Exception with details provided by 'raise'
(kit/with-handler
  (vec (map int-half [2 4 5 8]))
  (kit/handle *odd-number-error* [n]
    (throw (Exception. (format "Odd number %d in vector." n)))))

; The above is equivalent to the more complicated version below:
(kit/with-handler
  (vec (map int-half [2 4 5 8]))
  (kit/handle {:keys [n tag]}
    (if (isa? tag `*odd-number-error*)
      (throw (Exception. (format "Odd number %d in vector." n)))
      (kit/do-not-handle))))

; Returns "invalid" string instead of a vector when an error is encountered
(kit/with-handler
  (vec (map int-half [2 4 5 8]))
  (kit/handle kit/*error* [n]
    "invalid"))

; Inserts a zero into the returned vector where there was an error, in
; this case [1 2 0 4]
(kit/with-handler
  (vec (map int-half [2 4 5 8]))
  (kit/handle *number-error* [n]
    (kit/continue-with 0)))

; Intermediate continue: [1 2 :oops 5 4]
(defn int-half-vec [s]
  (reduce (fn [v i]
            (kit/with-handler
              (conj v (int-half i))
              (kit/bind-continue instead-of-half [& instead-seq]
                (apply conj v instead-seq))))
    [] s))

(kit/with-handler
  (int-half-vec [2 4 5 8])
  (kit/handle *number-error* [n]
    (kit/continue instead-of-half :oops n)))

; Notes:

; It seems likely you'd want to convert a handle clause to
; bind-continue, since it would allow higher forms to request what you
; used to do by default.  Thus both should appear in the same
; with-handler form

; Should continue-names be namespace qualified, and therefore require
; pre-definition in some namespace?
; (kit/defcontinue skip-thing "docstring")

; Could add 'catch' for Java Exceptions and 'finally' support to
; with-handler forms.

)
