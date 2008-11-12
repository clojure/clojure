;;; test_is.clj: test framework for Clojure

;; by Stuart Sierra, http://stuartsierra.com/
;; September 25, 2008

;; Thanks to Chas Emerick for contributions.
;; Thanks to Allen Rohner for assert-raises.

;; Copyright (c) 2008 Stuart Sierra. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.



;; Inspired by many Common Lisp test frameworks and clojure/test, this
;; file is a Clojure test framework.
;;
;; Define tests as :test metadata on your fns.  Use the "is" macro
;; for assertions.  Examples:
;;
;;     (defn add2
;;       ([x] (+ x 2))
;;       {:test (fn [] (is (= (add2 3) 5))
;;                     (is (= (add2 -4) -2)
;;                     (is (> (add2 50) 50)))})
;;
;; You can also define tests in isolation with the "deftest" macro:
;;
;;     (deftest test-new-fn
;;       (is (= (new-fn) "Awesome")))
;;
;; You can test that a function throws an exception with the "throws"
;; macro:
;;
;;     (defn factorial
;;       ([n] (cond
;;             (zero? n) 1  ; 0!=1 is often defined for convenience
;;             (> n 0) (* n (factorial (dec n)))
;;             :else (throw (IllegalArgumentException. "Negative factorial"))))
;;       {:test (fn [] (is (= (factorial 3) 6))
;;                     (is (= (factorial 6) 720))
;;                     (throws IllegalArgumentException (factorial -2)))}) 
;;
;; Run tests with (run-tests). As in any language with macros, you may
;; need to recompile functions after changing a macro definition.


(ns clojure.contrib.test-is)

(def
 #^{:doc "PrintWriter to which test results are printed; defaults to
 System.err."}
 *test-out* (. System err))


;;; PRIVATE

(defmacro #^{:private true} defcounter [ref-name fn-name]
  `(do (def ~(with-meta ref-name {:private true}) nil)
       (defn ~fn-name []
         (when ~ref-name (sync nil (commute ~ref-name inc))))))

(defcounter *tests* count-test)
(defcounter *assertions* count-assertion)
(defcounter *failures* count-failure)
(defcounter *exceptions* count-exception)

(defmacro failure [reason message]
  `(throw (new java.lang.AssertionError
               (str ~reason (when ~message (str "; " ~message))))))

(defn- assert-true [form message]
  `(do (count-assertion)
       (let [value# ~form]
         (when-not value#
           (failure (str ~(pr-str form) " was false/nil")
                    ~message)))))

;; Multimethod for testing expressions, dispatches on the first symbol
;; in the expression.
(defmulti assert-expr (fn [form message] (first form)))

;; Test for (= actual expected) expressions.
(defmethod assert-expr '= [form message]
  (let [expr1 (second form)
        expr2 (nth form 2)]
    `(do (count-assertion)
         (let [value1# ~expr1
               value2# ~expr2]
           (when-not (= value1# value2#)
             (failure (str ~(pr-str expr1) " is " (pr-str value1#)
                           " but should be " (pr-str value2#))
                      ~message))))))

;; Test for (instance? class object) expressions.
(defmethod assert-expr 'instance? [form message]
  (let [clazz (second form)
        object (nth form 2)]
    `(do (count-assertion)
         (let [value1# ~clazz
               value2# ~object]
           (when-not (instance? value1# value2#)
             (failure (str ~(pr-str object) " has " (class value2#)
                           " but should have " (pr-str value1#))
                      ~message))))))

;; Generic expression test, just check if expression evaluates to
;; logical true.
(defmethod assert-expr :default [form message]
  (assert-true form message))

(defn- always-fail-assert [message]
  `(do (count-assertion)
       (failure ~message nil)))

(defmacro #^{:private true} with-test-counters
  "Creates dynamic bindings for counting the number of tests,
  assertions, failures, and exceptions.  Returns the results in a
  map."
  [& body]
  `(binding [*tests* (ref 0)
             *assertions* (ref 0)
             *failures* (ref 0)
             *exceptions* (ref 0)]
     ~@body
     {:tests @*tests*
      :assertions @*assertions*
      :failures @*failures*
      :exceptions @*exceptions*}))

(defn- run-test-fn
  "Calls the function; reports errors/exceptions."
  [f name]
  (try
   (count-test)
   (f)
   (catch java.lang.AssertionError e
     (count-failure)
     (. *test-out* (println (str "FAIL in " name ": "
                                 (.getMessage e)))))
   (catch java.lang.Exception e
     (count-exception)
     (. *test-out* (println (str "EXCEPTION in " name ":")))
     (.printStackTrace e *test-out*))))

(defn- test-var
  "Finds and calls the fn in a var's :test metadata."
  [v]
  (when-let [f (:test (meta v))]
    (run-test-fn f (str v))))

(defn- test-interns
  "Tests all interned symbols in the namespace."
  [ns]
  (let [ns (if (symbol? ns) (find-ns ns) ns)]
    (. *test-out* (println (str "Testing " ns)))
    (dorun (map test-var (vals (ns-interns ns))))))


;;; PUBLIC

(defmacro is
  "Generic assertion macro.  Throws AssertionError if form evaluates
  logical false.  Optional message will be added to the error.

  form may be one of:
    * an equality test like (= expression expected-value)
    * an instance? test like (instance? class expression)
    * nil, which always fails
    * an arbitrary expression, fails if it returns false/nil"
  ([form] `(is ~form nil))
  ([form message]
     (cond
      (nil? form) (always-fail-assert message)
      (seq? form) (assert-expr form message)
      :else (assert-true form message))))

(defmacro throws
  "Asserts that form throws an exception of the given class (or one of
  its subclasses)."
  ([class form]
     `(throws ~class ~form nil))
  ([class form message]
  `(try
    (count-assertion)
    (let [value# ~form]
      (failure (str "expected " ~(pr-str form) " to throw " ~class
                    ", but returned " value#) ~message))
    (catch ~class e# nil)  ; the correct exception was thrown
    (catch java.lang.Throwable e#  ; some other exception was thrown
      (failure (str "expected " ~(pr-str form) " to throw " ~class
                    ", but threw " e#) ~message)))))

(defn print-results
  "Prints a summary of the results from test-ns to *test-out*."
  [r]
  (. *test-out*
     (println (str "\nRan " (:tests r) " tests with "
                   (:assertions r) " assertions.\n"
                   (:failures r) " failures, "
                   (:exceptions r) " exceptions.")))  )

(defn test-ns
  "Runs tests on all interned symbols in the namespaces 
  (symbols or namespace objects).

  Returns a map with the following keys:
    :tests      => number of tests run
    :assertions => number of assertions checked
    :failures   => number of failed assertions
    :exceptions => number of exceptions raised
  
  If no namespace is given, uses *ns*."
  ([] (test-ns *ns*))
  ([& namespaces]
     (with-test-counters (dorun (map test-interns namespaces)))))

(defn run-tests
  "Runs tests in the given namespaces and prints a summary of
  results.

  If no namespace is given, uses *ns*."
  [& namespaces]
  (print-results (apply test-ns namespaces)))

(defn run-all-tests
  "Runs tests in all namespaces and prints a summary of results."
  []
  (apply run-tests (all-ns)))

(defmacro deftest
  "Defines a Var with no value and with body in its :test fn."
  [name & body]
  `(def ~(with-meta name {:test `(fn [] ~@body)})))
