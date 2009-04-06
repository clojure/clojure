;   Copyright (c) Tom Faulhaber, Feb 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure.contrib.pprint)

;;; TODO: optimize/compile format strings in dispatch funcs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementations of specific dispatch table entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *simple-dispatch* (ref []))
(def *code-dispatch* (ref []))

;;; Handle forms that can be "back-translated" to reader macros
;;; Not all reader macros can be dealt with this way or at all. 
;;; Macros that we can't deal with at all are:
;;; ;  - The comment character is aborbed by the reader and never is part of the form
;;; `  - Is fully processed at read time into a lisp expression (which will contain concats
;;;      and regular quotes).
;;; ~@ - Also fully eaten by the processing of ` and can't be used outside.
;;; ,  - is whitespace and is lost (like all other whitespace). Formats can generate commas
;;;      where they deem them to help readability.
;;; #^ - Adding metadata completely disappears at read time and the data appears to be
;;;      completely lost.
;;;
;;; Most other syntax stuff is dealt with directly by the formats (like (), [], {}, and #{})
;;; or directly by printing the objects using Clojure's built-in print functions (like
;;; :keyword, \char, or ""). The notable exception is #() which is special-cased.

(def reader-macros
     {'quote "'", 'clojure.core/meta "^", 'clojure.core/deref "@", 
      'var "#'", 'clojure.core/unquote "~"})
(defn pprint-reader-macro [#^java.io.Writer writer alis]
  (let [#^String macro-char (reader-macros (first alis))]
    (if (and macro-char (= 2 (count alis)))
      (do
        (.write writer macro-char)
        (write (second alis) :stream writer)
        true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatch for the basic data types when interpreted
;; as data (as opposed to code).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: inline these formatter statements into funcs so that we
;;; are a little easier on the stack. (Or, do "real" compilation, a
;;; la Common Lisp)

(def pprint-simple-list (formatter "~:<~@{~w~^ ~_~}~:>"))
(defn pprint-list [writer alis]
  (if-not (pprint-reader-macro writer alis)
    (pprint-simple-list writer alis)))
(dosync (alter *simple-dispatch* conj [list? pprint-list]))
(dosync (alter *simple-dispatch* conj [#(instance? clojure.lang.Cons %) pprint-list]))
(dosync (alter *simple-dispatch* conj [#(instance? clojure.lang.LazySeq %) pprint-list]))

(def pprint-vector (formatter "~<[~;~@{~w~^ ~_~}~;]~:>"))
(dosync (alter *simple-dispatch* conj [vector? pprint-vector]))

(def pprint-array (formatter "~<[~;~@{~w~^, ~:_~}~;]~:>"))
(dosync (alter *simple-dispatch* conj [#(and % (.isArray (class %))) pprint-array]))

(def pprint-map (formatter "~<{~;~@{~<~w~^ ~_~w~:>~^, ~_~}~;}~:>"))
(dosync (alter *simple-dispatch* conj [map? pprint-map]))

(def pprint-set (formatter "~<#{~;~@{~w~^ ~:_~}~;}~:>"))
(dosync (alter *simple-dispatch* conj [set? pprint-set]))

(defn pprint-ref [writer ref]
  (pprint-logical-block writer :prefix "#<Ref " :suffix ">"
    (write @ref)))
(dosync (alter *simple-dispatch* conj [#(instance? clojure.lang.Ref %) pprint-ref]))

(defn pprint-atom [writer ref]
  (pprint-logical-block writer :prefix "#<Atom " :suffix ">"
    (write @ref)))
(dosync (alter *simple-dispatch* conj [#(instance? clojure.lang.Atom %) pprint-atom]))

(defn pprint-agent [writer ref]
  (pprint-logical-block writer :prefix "#<Agent " :suffix ">"
    (write @ref :stream writer)))
(dosync (alter *simple-dispatch* conj [#(instance? clojure.lang.Agent %) pprint-agent]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dispatch for the code table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare pprint-simple-code-list)

(dosync (alter *code-dispatch* conj [vector? pprint-vector]))
(dosync (alter *code-dispatch* conj [#(and % (.isArray (class %))) pprint-array]))
(dosync (alter *code-dispatch* conj [map? pprint-map]))
(dosync (alter *code-dispatch* conj [set? pprint-set]))
(dosync (alter *code-dispatch* conj [#(instance? clojure.lang.Ref %) pprint-ref]))
(dosync (alter *code-dispatch* conj [#(instance? clojure.lang.Atom %) pprint-atom]))
(dosync (alter *code-dispatch* conj [#(instance? clojure.lang.Agent %) pprint-agent]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Format something that looks like a simple def (sans metadata, since the reader
;;; won't give it to us now).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pprint-hold-first (formatter "~:<~w~^ ~@_~w~^ ~_~@{~w~^ ~_~}~:>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Format something that looks like a defn or defmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Format the params and body of a defn with a single arity
(defn- single-defn [alis has-doc-str?]
  (if (seq alis)
    (do
      (if has-doc-str?
        ((formatter " ~_") true)
        ((formatter " ~@_") true))
      ((formatter "~{~w~^ ~_~}") true alis))))

;;; Format the param and body sublists of a defn with multiple arities
(defn- multi-defn [alis has-doc-str?]
  (if (seq alis)
    ((formatter " ~_~{~w~^ ~_~}") true alis)))

;;; TODO: figure out how to support capturing metadata in defns (we might need a 
;;; special reader)
(defn pprint-defn [writer alis]
  (if (next alis) 
    (let [[defn-sym defn-name & stuff] alis
          [doc-str stuff] (if (string? (first stuff))
                            [(first stuff) (next stuff)]
                            [nil stuff])
          [attr-map stuff] (if (map? (first stuff))
                             [(first stuff) (next stuff)]
                             [nil stuff])]
      (pprint-logical-block writer :prefix "(" :suffix ")"
        ((formatter "~w ~1I~@_~w") true defn-sym defn-name)
        (if doc-str
          ((formatter " ~_~w") true doc-str))
        (if attr-map
          ((formatter " ~_~w") true attr-map))
        ;; Note: the multi-defn case will work OK for malformed defns too
        (cond
         (vector? (first stuff)) (single-defn stuff (or doc-str attr-map))
         :else (multi-defn stuff (or doc-str attr-map)))))
    (pprint-simple-code-list writer alis)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Format something with a binding form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pprint-binding-form [writer binding-vec]
  (pprint-logical-block writer :prefix "[" :suffix "]"
    (loop [binding binding-vec]
      (when (seq binding)
        (pprint-logical-block *out* binding
          (write (first binding))
          (when (next binding)
            (.write *out* " ")
            (pprint-newline :miser)
            (write (second binding))))
        (when (next (rest binding))
          (.write *out* " ")
          (pprint-newline :linear)
          (recur (next (rest binding))))))))

(defn pprint-let [writer alis]
  (let [base-sym (first alis)]
    (pprint-logical-block writer :prefix "(" :suffix ")"
      (if (and (next alis) (vector? (second alis)))
        (do
          ((formatter "~w ~1I~@_") true base-sym)
          (pprint-binding-form *out* (second alis))
          ((formatter " ~_~{~w~^ ~_~}") true (next (rest alis))))
        (pprint-simple-code-list *out* alis)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Format something that looks like "if"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pprint-if (formatter "~:<~1I~w~^ ~@_~w~@{ ~_~w~}~:>"))

(defn pprint-cond [writer alis]
  (pprint-logical-block writer :prefix "(" :suffix ")"
    (pprint-indent :block 1)
    (write (first alis))
    (when (next alis)
      (.write *out* " ")
      (pprint-newline :linear)
     (loop [alis (next alis)]
       (when alis
         (pprint-logical-block *out* alis
          (write (first alis))
          (when (next alis)
            (.write *out* " ")
            (pprint-newline :miser)
            (write (second alis))))
         (when (next (rest alis))
           (.write *out* " ")
           (pprint-newline :linear)
           (recur (next (rest alis)))))))))

(defn pprint-condp [writer alis]
  (if (> (count alis) 3) 
    (pprint-logical-block writer :prefix "(" :suffix ")"
      (pprint-indent :block 1)
      (apply (formatter "~w ~@_~w ~@_~w ~_") true alis)
      (loop [alis (seq (drop 3 alis))]
        (when alis
          (pprint-logical-block *out* alis
            (write (first alis))
            (when (next alis)
              (.write *out* " ")
              (pprint-newline :miser)
              (write (second alis))))
          (when (next (rest alis))
            (.write *out* " ")
            (pprint-newline :linear)
            (recur (next (rest alis)))))))
    (pprint-simple-code-list writer alis)))

;;; The map of symbols that are defined in an enclosing #() anonymous function
(def *symbol-map* {})

(defn pprint-anon-func [writer alis]
  (let [args (second alis)
        nlis (first (rest (rest alis)))]
    (if (vector? args)
      (binding [*symbol-map* (if (= 1 (count args)) 
                               {(first args) "%"}
                               (into {} 
                                     (map 
                                      #(vector %1 (str \% %2)) 
                                      args 
                                      (range 1 (inc (count args))))))]
        ((formatter "~<#(~;~@{~w~^ ~_~}~;)~:>") writer nlis))
      (pprint-simple-code-list writer alis))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The master definitions for formatting lists in code (that is, (fn args...) or
;;; special forms).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is the equivalent of (formatter "~:<~1I~@{~w~^ ~_~}~:>"), but is
;;; easier on the stack.

(defn pprint-simple-code-list [writer alis]
  (pprint-logical-block writer :prefix "(" :suffix ")"
    (pprint-indent :block 1)
    (loop [alis (seq alis)]
      (when alis
	(write (first alis))
	(when (next alis)
	  (.write *out* " ")
	  (pprint-newline :linear)
	  (recur (next alis)))))))

;;; Take a map with symbols as keys and add versions with no namespace.
;;; That is, if ns/sym->val is in the map, add sym->val to the result.
(defn two-forms [amap]
  (into {} 
        (mapcat 
         identity 
         (for [x amap] 
           [x [(symbol (name (first x))) (second x)]]))))

(defn add-core-ns [amap]
  (let [core "clojure.core"]
    (into {}
          (map #(let [[s f] %] 
                  (if (not (or (namespace s) (special-symbol? s)))
                    [(symbol core (name s)) f]
                    %))
               amap))))

(def *code-table*
     (two-forms
      (add-core-ns
       {'def pprint-hold-first, 'defonce pprint-hold-first, 
	'defn pprint-defn, 'defn- pprint-defn, 'defmacro pprint-defn, 'fn pprint-defn,
        'let pprint-let, 'loop pprint-let, 'binding pprint-let,
        'with-local-vars pprint-let, 'with-open pprint-let, 'when-let pprint-let,
	'if-let pprint-let, 'doseq pprint-let, 'dotimes pprint-let,
	'when-first pprint-let,
        'if pprint-if, 'if-not pprint-if, 'when pprint-if, 'when-not pprint-if,
        'cond pprint-cond, 'condp pprint-condp,
        'fn* pprint-anon-func,
        '. pprint-hold-first, '.. pprint-hold-first, '-> pprint-hold-first,
        'locking pprint-hold-first, 'struct pprint-hold-first,
        'struct-map pprint-hold-first, 
        })))

(defn pprint-code-list [writer alis]
  (if-not (pprint-reader-macro writer alis) 
    (if-let [special-form (*code-table* (first alis))]
      (special-form writer alis)
      (pprint-simple-code-list writer alis))))

(dosync (alter *code-dispatch* conj [list? pprint-code-list]))
(dosync (alter *code-dispatch* conj [#(instance? clojure.lang.Cons %) pprint-code-list]))
(dosync (alter *code-dispatch* conj [#(instance? clojure.lang.LazySeq %) pprint-code-list]))

(defn pprint-code-symbol [writer sym] 
  (if-let [arg-num (sym *symbol-map*)]
    (print arg-num)
    (if *print-suppress-namespaces* 
      (print (name sym))
      (pr sym))))
(dosync (alter *code-dispatch* conj [symbol? pprint-code-symbol]))

(set-pprint-dispatch *simple-dispatch*)


;;; For testing
(comment

(with-pprint-dispatch *code-dispatch* 
  (pprint 
   '(defn cl-format 
      "An implementation of a Common Lisp compatible format function"
      [stream format-in & args]
      (let [compiled-format (if (string? format-in) (compile-format format-in) format-in)
            navigator (init-navigator args)]
        (execute-format stream compiled-format navigator)))))

(with-pprint-dispatch *code-dispatch* 
  (pprint 
   '(defn cl-format 
      [stream format-in & args]
      (let [compiled-format (if (string? format-in) (compile-format format-in) format-in)
            navigator (init-navigator args)]
        (execute-format stream compiled-format navigator)))))

(with-pprint-dispatch *code-dispatch* 
  (pprint
   '(defn- -write 
      ([this x]
         (condp = (class x)
           String 
           (let [s0 (write-initial-lines this x)
                 s (.replaceFirst s0 "\\s+$" "")
                 white-space (.substring s0 (count s))
                 mode (getf :mode)]
             (if (= mode :writing)
               (dosync
                (write-white-space this)
                (.col-write this s)
                (setf :trailing-white-space white-space))
               (add-to-buffer this (make-buffer-blob s white-space))))

           Integer
           (let [c #^Character x]
             (if (= (getf :mode) :writing)
               (do 
                 (write-white-space this)
                 (.col-write this x))
               (if (= c (int \newline))
                 (write-initial-lines this "\n")
                 (add-to-buffer this (make-buffer-blob (str (char c)) nil))))))))))

(with-pprint-dispatch *code-dispatch* 
  (pprint 
   '(defn pprint-defn [writer alis]
      (if (next alis) 
        (let [[defn-sym defn-name & stuff] alis
              [doc-str stuff] (if (string? (first stuff))
                                [(first stuff) (next stuff)]
                                [nil stuff])
              [attr-map stuff] (if (map? (first stuff))
                                 [(first stuff) (next stuff)]
                                 [nil stuff])]
          (pprint-logical-block writer :prefix "(" :suffix ")"
                                (cl-format true "~w ~1I~@_~w" defn-sym defn-name)
                                (if doc-str
                                  (cl-format true " ~_~w" doc-str))
                                (if attr-map
                                  (cl-format true " ~_~w" attr-map))
                                ;; Note: the multi-defn case will work OK for malformed defns too
                                (cond
                                  (vector? (first stuff)) (single-defn stuff (or doc-str attr-map))
                                  :else (multi-defn stuff (or doc-str attr-map)))))
        (pprint-simple-code-list writer alis)))))
)
nil

