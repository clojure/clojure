;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;;; test.clj: test framework for Clojure

;; by Stuart Sierra
;; March 28, 2009

;; Thanks to Chas Emerick, Allen Rohner, and Stuart Halloway for
;; contributions and suggestions.

(ns 
  ^{:author "Stuart Sierra, with contributions and suggestions by 
  Chas Emerick, Allen Rohner, and Stuart Halloway",
     :doc "A unit testing framework.

   ASSERTIONS

   The core of the library is the \"is\" macro, which lets you make
   assertions of any arbitrary expression:

   (is (= 4 (+ 2 2)))
   (is (instance? Integer 256))
   (is (.startsWith \"abcde\" \"ab\"))

   You can type an \"is\" expression directly at the REPL, which will
   print a message if it fails.

       user> (is (= 5 (+ 2 2)))

       FAIL in  (:1)
       expected: (= 5 (+ 2 2))
         actual: (not (= 5 4))
       false

   The \"expected:\" line shows you the original expression, and the
   \"actual:\" shows you what actually happened.  In this case, it
   shows that (+ 2 2) returned 4, which is not = to 5.  Finally, the
   \"false\" on the last line is the value returned from the
   expression.  The \"is\" macro always returns the result of the
   inner expression.

   There are two special assertions for testing exceptions.  The
   \"(is (thrown? c ...))\" form tests if an exception of class c is
   thrown:

   (is (thrown? ArithmeticException (/ 1 0))) 

   \"(is (thrown-with-msg? c re ...))\" does the same thing and also
   tests that the message on the exception matches the regular
   expression re:

   (is (thrown-with-msg? ArithmeticException #\"Divide by zero\"
                         (/ 1 0)))

   DOCUMENTING TESTS

   \"is\" takes an optional second argument, a string describing the
   assertion.  This message will be included in the error report.

   (is (= 5 (+ 2 2)) \"Crazy arithmetic\")

   In addition, you can document groups of assertions with the
   \"testing\" macro, which takes a string followed by any number of
   assertions.  The string will be included in failure reports.
   Calls to \"testing\" may be nested, and all of the strings will be
   joined together with spaces in the final report, in a style
   similar to RSpec <http://rspec.info/>

   (testing \"Arithmetic\"
     (testing \"with positive integers\"
       (is (= 4 (+ 2 2)))
       (is (= 7 (+ 3 4))))
     (testing \"with negative integers\"
       (is (= -4 (+ -2 -2)))
       (is (= -1 (+ 3 -4)))))

   Note that, unlike RSpec, the \"testing\" macro may only be used
   INSIDE a \"deftest\" or \"with-test\" form (see below).


   DEFINING TESTS

   There are two ways to define tests.  The \"with-test\" macro takes
   a defn or def form as its first argument, followed by any number
   of assertions.  The tests will be stored as metadata on the
   definition.

   (with-test
       (defn my-function [x y]
         (+ x y))
     (is (= 4 (my-function 2 2)))
     (is (= 7 (my-function 3 4))))

   As of Clojure SVN rev. 1221, this does not work with defmacro.
   See http://code.google.com/p/clojure/issues/detail?id=51

   The other way lets you define tests separately from the rest of
   your code, even in a different namespace:

   (deftest addition
     (is (= 4 (+ 2 2)))
     (is (= 7 (+ 3 4))))

   (deftest subtraction
     (is (= 1 (- 4 3)))
     (is (= 3 (- 7 4))))

   This creates functions named \"addition\" and \"subtraction\", which
   can be called like any other function.  Therefore, tests can be
   grouped and composed, in a style similar to the test framework in
   Peter Seibel's \"Practical Common Lisp\"
   <http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html>

   (deftest arithmetic
     (addition)
     (subtraction))

   The names of the nested tests will be joined in a list, like
   \"(arithmetic addition)\", in failure reports.  You can use nested
   tests to set up a context shared by several tests.


   RUNNING TESTS

   Run tests with the function \"(run-tests namespaces...)\":

   (run-tests 'your.namespace 'some.other.namespace)

   If you don't specify any namespaces, the current namespace is
   used.  To run all tests in all namespaces, use \"(run-all-tests)\".

   By default, these functions will search for all tests defined in
   a namespace and run them in an undefined order.  However, if you
   are composing tests, as in the \"arithmetic\" example above, you
   probably do not want the \"addition\" and \"subtraction\" tests run
   separately.  In that case, you must define a special function
   named \"test-ns-hook\" that runs your tests in the correct order:

   (defn test-ns-hook []
     (arithmetic))

   Note: test-ns-hook prevents execution of fixtures (see below).


   OMITTING TESTS FROM PRODUCTION CODE

   You can bind the variable \"*load-tests*\" to false when loading or
   compiling code in production.  This will prevent any tests from
   being created by \"with-test\" or \"deftest\".


   FIXTURES

   Fixtures allow you to run code before and after tests, to set up
   the context in which tests should be run.

   A fixture is just a function that calls another function passed as
   an argument.  It looks like this:

   (defn my-fixture [f]
      Perform setup, establish bindings, whatever.
     (f)  Then call the function we were passed.
      Tear-down / clean-up code here.
    )

   Fixtures are attached to namespaces in one of two ways.  \"each\"
   fixtures are run repeatedly, once for each test function created
   with \"deftest\" or \"with-test\".  \"each\" fixtures are useful for
   establishing a consistent before/after state for each test, like
   clearing out database tables.

   \"each\" fixtures can be attached to the current namespace like this:
   (use-fixtures :each fixture1 fixture2 ...)
   The fixture1, fixture2 are just functions like the example above.
   They can also be anonymous functions, like this:
   (use-fixtures :each (fn [f] setup... (f) cleanup...))

   The other kind of fixture, a \"once\" fixture, is only run once,
   around ALL the tests in the namespace.  \"once\" fixtures are useful
   for tasks that only need to be performed once, like establishing
   database connections, or for time-consuming tasks.

   Attach \"once\" fixtures to the current namespace like this:
   (use-fixtures :once fixture1 fixture2 ...)

   Note: Fixtures and test-ns-hook are mutually incompatible.  If you
   are using test-ns-hook, fixture functions will *never* be run.


   SAVING TEST OUTPUT TO A FILE

   All the test reporting functions write to the var *test-out*.  By
   default, this is the same as *out*, but you can rebind it to any
   PrintWriter.  For example, it could be a file opened with
   clojure.java.io/writer.


   EXTENDING TEST-IS (ADVANCED)

   You can extend the behavior of the \"is\" macro by defining new
   methods for the \"assert-expr\" multimethod.  These methods are
   called during expansion of the \"is\" macro, so they should return
   quoted forms to be evaluated.

   You can plug in your own test-reporting framework by rebinding
   the \"report\" function: (report event)

   The 'event' argument is a map.  It will always have a :type key,
   whose value will be a keyword signaling the type of event being
   reported.  Standard events with :type value of :pass, :fail, and
   :error are called when an assertion passes, fails, and throws an
   exception, respectively.  In that case, the event will also have
   the following keys:

     :expected   The form that was expected to be true
     :actual     A form representing what actually occurred
     :message    The string message given as an argument to 'is'

   The \"testing\" strings will be a list in \"*testing-contexts*\", and
   the vars being tested will be a list in \"*testing-vars*\".

   Your \"report\" function should wrap any printing calls in the
   \"with-test-out\" macro, which rebinds *out* to the current value
   of *test-out*.

   For additional event types, see the examples in the code.
"}
  clojure.test
  (:require [clojure.template :as temp]
            [clojure.stacktrace :as stack]
            [clojure.string :as str]))

;; Nothing is marked "private" here, so you can rebind things to plug
;; in your own testing or reporting frameworks.


;;; USER-MODIFIABLE GLOBALS

(defonce ^:dynamic
  ^{:doc "True by default.  If set to false, no test functions will
   be created by deftest, set-test, or with-test.  Use this to omit
   tests when compiling or loading production code."
    :added "1.1"}
  *load-tests* true)

(def ^:dynamic
 ^{:doc "The maximum depth of stack traces to print when an Exception
  is thrown during a test.  Defaults to nil, which means print the 
  complete stack trace."
   :added "1.1"}
 *stack-trace-depth* nil)


;;; GLOBALS USED BY THE REPORTING FUNCTIONS

(def ^:dynamic *report-counters* nil)	  ; bound to a ref of a map in test-ns

(def ^:dynamic *initial-report-counters*  ; used to initialize *report-counters*
     {:test 0, :pass 0, :fail 0, :error 0})

(def ^:dynamic *testing-vars* (list))  ; bound to hierarchy of vars being tested

(def ^:dynamic *testing-contexts* (list)) ; bound to hierarchy of "testing" strings

(def ^:dynamic *test-out* *out*)         ; PrintWriter for test reporting output

(defmacro with-test-out
  "Runs body with *out* bound to the value of *test-out*."
  {:added "1.1"}
  [& body]
  `(binding [*out* *test-out*]
     ~@body))

;;; UTILITIES FOR REPORTING FUNCTIONS

(defn file-position
  "Returns a vector [filename line-number] for the nth call up the
  stack.

  Deprecated in 1.2: The information needed for test reporting is
  now on :file and :line keys in the result map."
  {:added "1.1"
   :deprecated "1.2"}
  [n]
  (let [^StackTraceElement s (nth (.getStackTrace (new java.lang.Throwable)) n)]
    [(.getFileName s) (.getLineNumber s)]))

(defn testing-vars-str
  "Returns a string representation of the current test.  Renders names
  in *testing-vars* as a list, then the source file and line of
  current assertion."
  {:added "1.1"}
  [m]
  (let [{:keys [file line]} m]
    (str
     ;; Uncomment to include namespace in failure report:
     ;;(ns-name (:ns (meta (first *testing-vars*)))) "/ "
     (reverse (map #(:name (meta %)) *testing-vars*))
     " (" file ":" line ")")))

(defn testing-contexts-str
  "Returns a string representation of the current test context. Joins
  strings in *testing-contexts* with spaces."
  {:added "1.1"}
  []
  (apply str (interpose " " (reverse *testing-contexts*))))

(defn inc-report-counter
  "Increments the named counter in *report-counters*, a ref to a map.
  Does nothing if *report-counters* is nil."
  {:added "1.1"}
  [name]
  (when *report-counters*
    (dosync (commute *report-counters* update-in [name] (fnil inc 0)))))

;;; TEST RESULT REPORTING

(defmulti
  ^{:doc "Generic reporting function, may be overridden to plug in
   different report formats (e.g., TAP, JUnit).  Assertions such as
   'is' call 'report' to indicate results.  The argument given to
   'report' will be a map with a :type key.  See the documentation at
   the top of test_is.clj for more information on the types of
   arguments for 'report'."
     :dynamic true
     :added "1.1"}
  report :type)

(defn- file-and-line
  {:deprecated "1.8"}
  [^Throwable exception depth]
  (let [stacktrace (.getStackTrace exception)]
    (if (< depth (count stacktrace))
      (let [^StackTraceElement s (nth stacktrace depth)]
        {:file (.getFileName s) :line (.getLineNumber s)})
      {:file nil :line nil})))

(defn- stacktrace-file-and-line
  [stacktrace]
  (if (seq stacktrace)
    (let [^StackTraceElement s (first stacktrace)]
      {:file (.getFileName s) :line (.getLineNumber s)})
    {:file nil :line nil}))

(defn do-report
  "Add file and line information to a test result and call report.
   If you are writing a custom assert-expr method, call this function
   to pass test results to report."
  {:added "1.2"}
  [m]
  (report
   (case
    (:type m)
    :fail (merge (stacktrace-file-and-line (drop-while
                                             #(let [cl-name (.getClassName ^StackTraceElement %)]
                                                (or (str/starts-with? cl-name "java.lang.")
                                                    (str/starts-with? cl-name "clojure.test$")))
                                             (.getStackTrace (Thread/currentThread)))) m)
    :error (merge (stacktrace-file-and-line (.getStackTrace ^Throwable (:actual m))) m)
    m)))

(defmethod report :default [m]
  (with-test-out (prn m)))

(defmethod report :pass [m]
  (with-test-out (inc-report-counter :pass)))

(defmethod report :fail [m]
  (with-test-out
    (inc-report-counter :fail)
    (println "\nFAIL in" (testing-vars-str m))
    (when (seq *testing-contexts*) (println (testing-contexts-str)))
    (when-let [message (:message m)] (println message))
    (println "expected:" (pr-str (:expected m)))
    (println "  actual:" (pr-str (:actual m)))))

(defmethod report :error [m]
  (with-test-out
   (inc-report-counter :error)
   (println "\nERROR in" (testing-vars-str m))
   (when (seq *testing-contexts*) (println (testing-contexts-str)))
   (when-let [message (:message m)] (println message))
   (println "expected:" (pr-str (:expected m)))
   (print "  actual: ")
   (let [actual (:actual m)]
     (if (instance? Throwable actual)
       (stack/print-cause-trace actual *stack-trace-depth*)
       (prn actual)))))

(defmethod report :summary [m]
  (with-test-out
   (println "\nRan" (:test m) "tests containing"
            (+ (:pass m) (:fail m) (:error m)) "assertions.")
   (println (:fail m) "failures," (:error m) "errors.")))

(defmethod report :begin-test-ns [m]
  (with-test-out
   (println "\nTesting" (ns-name (:ns m)))))

;; Ignore these message types:
(defmethod report :end-test-ns [m])
(defmethod report :begin-test-var [m])
(defmethod report :end-test-var [m])



;;; UTILITIES FOR ASSERTIONS

(defn get-possibly-unbound-var
  "Like var-get but returns nil if the var is unbound."
  {:added "1.1"}
  [v]
  (try (var-get v)
       (catch IllegalStateException e
         nil)))

(defn function?
  "Returns true if argument is a function or a symbol that resolves to
  a function (not a macro)."
  {:added "1.1"}
  [x]
  (if (symbol? x)
    (when-let [v (resolve x)]
      (when-let [value (get-possibly-unbound-var v)]
        (and (fn? value)
             (not (:macro (meta v))))))
    (fn? x)))

(defn assert-predicate
  "Returns generic assertion code for any functional predicate.  The
  'expected' argument to 'report' will contains the original form, the
  'actual' argument will contain the form with all its sub-forms
  evaluated.  If the predicate returns false, the 'actual' form will
  be wrapped in (not...)."
  {:added "1.1"}
  [msg form]
  (let [args (rest form)
        pred (first form)]
    `(let [values# (list ~@args)
           result# (apply ~pred values#)]
       (if result#
         (do-report {:type :pass, :message ~msg,
                  :expected '~form, :actual (cons ~pred values#)})
         (do-report {:type :fail, :message ~msg,
                  :expected '~form, :actual (list '~'not (cons '~pred values#))}))
       result#)))

(defn assert-any
  "Returns generic assertion code for any test, including macros, Java
  method calls, or isolated symbols."
  {:added "1.1"}
  [msg form]
  `(let [value# ~form]
     (if value#
       (do-report {:type :pass, :message ~msg,
                :expected '~form, :actual value#})
       (do-report {:type :fail, :message ~msg,
                :expected '~form, :actual value#}))
     value#))



;;; ASSERTION METHODS

;; You don't call these, but you can add methods to extend the 'is'
;; macro.  These define different kinds of tests, based on the first
;; symbol in the test expression.

(defmulti assert-expr 
  (fn [msg form]
    (cond
      (nil? form) :always-fail
      (seq? form) (first form)
      :else :default)))

(defmethod assert-expr :always-fail [msg form]
  ;; nil test: always fail
  `(do-report {:type :fail, :message ~msg}))

(defmethod assert-expr :default [msg form]
  (if (and (sequential? form) (function? (first form)))
    (assert-predicate msg form)
    (assert-any msg form)))

(defmethod assert-expr 'instance? [msg form]
  ;; Test if x is an instance of y.
  `(let [klass# ~(nth form 1)
         object# ~(nth form 2)]
     (let [result# (instance? klass# object#)]
       (if result#
         (do-report {:type :pass, :message ~msg,
                  :expected '~form, :actual (class object#)})
         (do-report {:type :fail, :message ~msg,
                  :expected '~form, :actual (class object#)}))
       result#)))

(defmethod assert-expr 'thrown? [msg form]
  ;; (is (thrown? c expr))
  ;; Asserts that evaluating expr throws an exception of class c.
  ;; Returns the exception thrown.
  (let [klass (second form)
        body (nthnext form 2)]
    `(try ~@body
          (do-report {:type :fail, :message ~msg,
                   :expected '~form, :actual nil})
          (catch ~klass e#
            (do-report {:type :pass, :message ~msg,
                     :expected '~form, :actual e#})
            e#))))

(defmethod assert-expr 'thrown-with-msg? [msg form]
  ;; (is (thrown-with-msg? c re expr))
  ;; Asserts that evaluating expr throws an exception of class c.
  ;; Also asserts that the message string of the exception matches
  ;; (with re-find) the regular expression re.
  (let [klass (nth form 1)
        re (nth form 2)
        body (nthnext form 3)]
    `(try ~@body
          (do-report {:type :fail, :message ~msg, :expected '~form, :actual nil})
          (catch ~klass e#
            (let [m# (.getMessage e#)]
              (if (re-find ~re m#)
                (do-report {:type :pass, :message ~msg,
                         :expected '~form, :actual e#})
                (do-report {:type :fail, :message ~msg,
                         :expected '~form, :actual e#})))
            e#))))


(defmacro try-expr
  "Used by the 'is' macro to catch unexpected exceptions.
  You don't call this."
  {:added "1.1"}
  [msg form]
  `(try ~(assert-expr msg form)
        (catch Throwable t#
          (do-report {:type :error, :message ~msg,
                      :expected '~form, :actual t#}))))



;;; ASSERTION MACROS

;; You use these in your tests.

(defmacro is
  "Generic assertion macro.  'form' is any predicate test.
  'msg' is an optional message to attach to the assertion.
  
  Example: (is (= 4 (+ 2 2)) \"Two plus two should be 4\")

  Special forms:

  (is (thrown? c body)) checks that an instance of c is thrown from
  body, fails if not; then returns the thing thrown.

  (is (thrown-with-msg? c re body)) checks that an instance of c is
  thrown AND that the message on the exception matches (with
  re-find) the regular expression re."
  {:added "1.1"} 
  ([form] `(is ~form nil))
  ([form msg] `(try-expr ~msg ~form)))

(defmacro are
  "Checks multiple assertions with a template expression.
  See clojure.template/do-template for an explanation of
  templates.

  Example: (are [x y] (= x y)  
                2 (+ 1 1)
                4 (* 2 2))
  Expands to: 
           (do (is (= 2 (+ 1 1)))
               (is (= 4 (* 2 2))))

  Note: This breaks some reporting features, such as line numbers."
  {:added "1.1"}
  [argv expr & args]
  (if (or
       ;; (are [] true) is meaningless but ok
       (and (empty? argv) (empty? args))
       ;; Catch wrong number of args
       (and (pos? (count argv))
            (pos? (count args))
            (zero? (mod (count args) (count argv)))))
    `(temp/do-template ~argv (is ~expr) ~@args)
    (throw (IllegalArgumentException. "The number of args doesn't match are's argv."))))

(defmacro testing
  "Adds a new string to the list of testing contexts.  May be nested,
  but must occur inside a test function (deftest)."
  {:added "1.1"}
  [string & body]
  `(binding [*testing-contexts* (conj *testing-contexts* ~string)]
     ~@body))



;;; DEFINING TESTS

(defmacro with-test
  "Takes any definition form (that returns a Var) as the first argument.
  Remaining body goes in the :test metadata function for that Var.

  When *load-tests* is false, only evaluates the definition, ignoring
  the tests."
  {:added "1.1"}
  [definition & body]
  (if *load-tests*
    `(doto ~definition (alter-meta! assoc :test (fn [] ~@body)))
    definition))


(defmacro deftest
  "Defines a test function with no arguments.  Test functions may call
  other tests, so tests may be composed.  If you compose tests, you
  should also define a function named test-ns-hook; run-tests will
  call test-ns-hook instead of testing all vars.

  Note: Actually, the test body goes in the :test metadata on the var,
  and the real function (the value of the var) calls test-var on
  itself.

  When *load-tests* is false, deftest is ignored."
  {:added "1.1"}
  [name & body]
  (when *load-tests*
    `(def ~(vary-meta name assoc :test `(fn [] ~@body))
          (fn [] (test-var (var ~name))))))

(defmacro deftest-
  "Like deftest but creates a private var."
  {:added "1.1"}
  [name & body]
  (when *load-tests*
    `(def ~(vary-meta name assoc :test `(fn [] ~@body) :private true)
          (fn [] (test-var (var ~name))))))


(defmacro set-test
  "Experimental.
  Sets :test metadata of the named var to a fn with the given body.
  The var must already exist.  Does not modify the value of the var.

  When *load-tests* is false, set-test is ignored."
  {:added "1.1"}
  [name & body]
  (when *load-tests*
    `(alter-meta! (var ~name) assoc :test (fn [] ~@body))))



;;; DEFINING FIXTURES

(defn- add-ns-meta
  "Adds elements in coll to the current namespace metadata as the
  value of key."
  {:added "1.1"}
  [key coll]
  (alter-meta! *ns* assoc key coll))

(defmulti use-fixtures
  "Wrap test runs in a fixture function to perform setup and
  teardown. Using a fixture-type of :each wraps every test
  individually, while :once wraps the whole run in a single function."
  {:added "1.1"}
  (fn [fixture-type & args] fixture-type))

(defmethod use-fixtures :each [fixture-type & args]
  (add-ns-meta ::each-fixtures args))

(defmethod use-fixtures :once [fixture-type & args]
  (add-ns-meta ::once-fixtures args))

(defn- default-fixture
  "The default, empty, fixture function.  Just calls its argument."
  {:added "1.1"}
  [f]
  (f))

(defn compose-fixtures
  "Composes two fixture functions, creating a new fixture function
  that combines their behavior."
  {:added "1.1"}
  [f1 f2]
  (fn [g] (f1 (fn [] (f2 g)))))

(defn join-fixtures
  "Composes a collection of fixtures, in order.  Always returns a valid
  fixture function, even if the collection is empty."
  {:added "1.1"}
  [fixtures]
  (reduce compose-fixtures default-fixture fixtures))




;;; RUNNING TESTS: LOW-LEVEL FUNCTIONS

(defn test-var
  "If v has a function in its :test metadata, calls that function,
  with *testing-vars* bound to (conj *testing-vars* v)."
  {:dynamic true, :added "1.1"}
  [v]
  (when-let [t (:test (meta v))]
    (binding [*testing-vars* (conj *testing-vars* v)]
      (do-report {:type :begin-test-var, :var v})
      (inc-report-counter :test)
      (try (t)
           (catch Throwable e
             (do-report {:type :error, :message "Uncaught exception, not in assertion."
                      :expected nil, :actual e})))
      (do-report {:type :end-test-var, :var v}))))

(defn test-vars
  "Groups vars by their namespace and runs test-vars on them with
   appropriate fixtures applied."
  {:added "1.6"}
  [vars]
  (doseq [[ns vars] (group-by (comp :ns meta) vars)]
    (let [once-fixture-fn (join-fixtures (::once-fixtures (meta ns)))
          each-fixture-fn (join-fixtures (::each-fixtures (meta ns)))]
      (once-fixture-fn
       (fn []
         (doseq [v vars]
           (when (:test (meta v))
             (each-fixture-fn (fn [] (test-var v))))))))))

(defn test-all-vars
  "Calls test-vars on every var interned in the namespace, with fixtures."
  {:added "1.1"}
  [ns]
  (test-vars (vals (ns-interns ns))))

(defn test-ns
  "If the namespace defines a function named test-ns-hook, calls that.
  Otherwise, calls test-all-vars on the namespace.  'ns' is a
  namespace object or a symbol.

  Internally binds *report-counters* to a ref initialized to
  *initial-report-counters*.  Returns the final, dereferenced state of
  *report-counters*."
  {:added "1.1"}
  [ns]
  (binding [*report-counters* (ref *initial-report-counters*)]
    (let [ns-obj (the-ns ns)]
      (do-report {:type :begin-test-ns, :ns ns-obj})
      ;; If the namespace has a test-ns-hook function, call that:
      (if-let [v (find-var (symbol (str (ns-name ns-obj)) "test-ns-hook"))]
	((var-get v))
        ;; Otherwise, just test every var in the namespace.
        (test-all-vars ns-obj))
      (do-report {:type :end-test-ns, :ns ns-obj}))
    @*report-counters*))



;;; RUNNING TESTS: HIGH-LEVEL FUNCTIONS

(defn run-tests
  "Runs all tests in the given namespaces; prints results.
  Defaults to current namespace if none given.  Returns a map
  summarizing test results."
  {:added "1.1"}
  ([] (run-tests *ns*))
  ([& namespaces]
     (let [summary (assoc (apply merge-with + (map test-ns namespaces))
                     :type :summary)]
       (do-report summary)
       summary)))

(defn run-all-tests
  "Runs all tests in all namespaces; prints results.
  Optional argument is a regular expression; only namespaces with
  names matching the regular expression (with re-matches) will be
  tested."
  {:added "1.1"}
  ([] (apply run-tests (all-ns)))
  ([re] (apply run-tests (filter #(re-matches re (name (ns-name %))) (all-ns)))))

(defn successful?
  "Returns true if the given test summary indicates all tests
  were successful, false otherwise."
  {:added "1.1"}
  [summary]
  (and (zero? (:fail summary 0))
       (zero? (:error summary 0))))
