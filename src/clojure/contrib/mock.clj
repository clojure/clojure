;;; clojure.contrib.mock.clj: mocking/expectation framework for Clojure

;; by Matt Clark

;; Copyright (c) Matt Clark, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
;; By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.
;;------------------------------------------------------------------------------

(comment
  ;; This is a simple function mocking library I accidentally wrote as a side
  ;; effect of trying to write an opengl library in clojure. This is loosely
  ;; based on various ruby and java mocking frameworks I have used in the past
  ;; such as mockito, easymock, and whatever rspec uses.
  ;;
  ;; expect uses bindings to wrap the functions that are being tested and
  ;; then validates the invocation count at the end. The expect macro is the
  ;; main entry point and it is given a vector of binding pairs.
  ;; The first of each pair names the dependent function you want to override,
  ;; while the second is a hashmap containing the mock description, usually
  ;; created via the simple helper methods described below.
  ;;
  ;; Usage:
  ;;
  ;; there are one or more dependent functions:
  
  (defn dep-fn1 [] "time consuming calculation in 3rd party library")
  (defn dep-fn2 [x] "function with undesirable side effects while testing")
  
  ;; then we have the code under test that calls these other functions:
  
  (defn my-code-under-test [] (dep-fn1) (dep-fn2 "a") (+ 2 2))

  ;; to test this code, we simply surround it with an expect macro within
  ;; the test:
  
  (expect [dep-fn1 (times 1)
           dep-fn2 (times 1 (has-args [#(= "a" %)]))]
    (my-code-under-test))

  ;; When an expectation fails during execution of the function under test,
  ;; an error condition function is called with the name of the function
  ;; being mocked, the expected form and the actual value. These
  ;; error functions can be overridden to allow easy integration into
  ;; test frameworks such as test-is by reporting errors in the function
  ;; overrides.
  
  ) ;; end comment

(ns clojure.contrib.mock
  #^{:author "Matt Clark",
     :doc "function mocking/expectations for Clojure" }
  (:use [clojure.contrib.seq-utils :only (positions)]
        [clojure.contrib.def :only (defmacro-)]))


;;------------------------------------------------------------------------------
;; These are the error condition functions. Override them to integrate into
;; the test framework of your choice, or to simply customize error handling.

(defn report-problem
  ([function expected actual]
    (report-problem function expected actual "Expectation not met."))
  ([function expected actual message]
    (prn (str message " Function name: " function
           " expected: " expected " actual: " actual))))

(defn no-matching-function-signature [function expected actual]
  (report-problem function expected actual
    "No matching real function signature for given argument count."))

(defn unexpected-args [function expected actual i]
  (report-problem function expected actual
    (str "Argument " i " has an unexpected value for function.")))

(defn incorrect-invocation-count [function expected actual]
  (report-problem function expected actual "Unexpected invocation count."))


;;------------------------------------------------------------------------------
;;  Internal Functions - ignore these


(defn- has-arg-count-match?
  "Given the sequence of accepted argument vectors for a function,
returns true if at least one matches the given-count value."
  [arg-lists given-count]
  (some #(let [[ind] (positions #{'&} %)]
           (if ind
             (>= given-count ind)
             (= (count %) given-count)))
        arg-lists))


(defn has-matching-signature?
  "Calls no-matching-function-signature if no match is found for the given
function. If no argslist meta data is available for the function, it is
not called."
  [fn-name args]
  (let [arg-count (count args)
            arg-lists (:arglists (meta (resolve fn-name)))]
        (if (and arg-lists (not (has-arg-count-match? arg-lists arg-count)))
          (no-matching-function-signature fn-name arg-lists args))))


(defn make-arg-checker
  "Creates the argument verifying function for a replaced dependency within
the expectation bound scope. These functions take the additional argument
of the name of the replaced function, then the rest of their args. It is
designed to be called from the mock function generated in the first argument
of the mock info object created by make-mock."
  [arg-preds arg-pred-forms]
  (let [sanitized-preds (map (fn [v] (if (fn? v) v #(= v %))) arg-preds)]
    (fn [fn-name & args]
      (every? true?
        (map (fn [pred arg pred-form i] (if (pred arg) true
                                          (unexpected-args fn-name pred-form arg i)))
          sanitized-preds args arg-pred-forms (iterate inc 0))))))


(defn make-count-checker
  "creates the count checker that is invoked at the end of an expectation, after
the code under test has all been executed. The function returned takes the
name of the associated dependency and the invocation count as arguments."
  [pred pred-form]
  (let [pred-fn (if (integer? pred) #(= pred %) pred)]
    (fn [fn-name v] (if (pred-fn v) true
                      (incorrect-invocation-count fn-name pred-form v)))))

; Borrowed from clojure core. Remove if this ever becomes public there.
(defmacro- assert-args
  [fnname & pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  ~(str fnname " requires " (second pairs)))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args fnname more)))))

(defn make-mock
  "creates a vector containing the following information for the named function:
1. dependent function replacement - verifies signature, calls arg checker,
increases count, returns return value.
2. an atom containing the invocation count
3. the invocation count checker function
4. a symbol of the name of the function being replaced."
  [fn-name expectation-hash]
  (assert-args make-mock
    (map? expectation-hash) "a map of expectations")
  (let [arg-checker (or (expectation-hash :has-args) (fn [& args] true))
        count-atom (atom 0)
        ret-fn (or
                 (expectation-hash :calls)
                 (fn [& args] (expectation-hash :returns)))]
    [(fn [& args]
       (has-matching-signature? fn-name args)
       (apply arg-checker fn-name args)
       (swap! count-atom inc)
       (apply ret-fn args))
     count-atom
     (or (expectation-hash :times) (fn [fn-name v] true))
     fn-name]))


(defn validate-counts
  "given the sequence of all mock data for the expectation, simply calls the
count checker for each dependency."
  [mock-data] (doseq [[mfn i checker fn-name] mock-data] (checker fn-name @i)))

(defn #^{:private true} make-bindings [expect-bindings mock-data-sym]
  `[~@(interleave (map #(first %) (partition 2 expect-bindings))
        (map (fn [i] `(nth (nth ~mock-data-sym ~i) 0))
          (range (quot (count expect-bindings) 2))))])


;;------------------------------------------------------------------------------
;; These are convenience functions to improve the readability and use of this
;; library. Useful in expressions such as:
;; (expect [dep-fn1 (times (more-than 1) (returns 15)) etc)

(defn once [x] (= 1 x))

(defn never [x] (zero? x))

(defn more-than [x] #(< x %))

(defn less-than [x] #(> x %))

(defn between [x y] #(and (< x %) (> y %)))


;;------------------------------------------------------------------------------
;; The following functions can be used to build up the expectation hash.

(defn returns
  "Creates or associates to an existing expectation hash the :returns key with
a value to be returned by the expectation after a successful invocation
matching its expected arguments (if applicable).
Usage:
(returns ret-value expectation-hash?)"

  ([val] (returns val {}))
  ([val expectation-hash] (assoc expectation-hash :returns val)))


(defn calls
  "Creates or associates to an existing expectation hash the :calls key with a
function that will be called with the given arguments. The return value from
this function will be returned returned by the expected function. If both this
and returns are specified, the return value of \"calls\" will have precedence.
Usage:
(calls some-fn expectation-hash?)"

  ([val] (calls val {}))
  ([val expectation-hash] (assoc expectation-hash :calls val)))


(defmacro has-args
  "Creates or associates to an existing expectation hash the :has-args key with
a value corresponding to a function that will either return true if its
argument expectations are met or throw an exception with the details of the
first failed argument it encounters.
Only specify as many predicates as you are interested in verifying. The rest
of the values are safely ignored.
Usage:
(has-args [arg-pred-1 arg-pred-2 ... arg-pred-n] expectation-hash?)"

  ([arg-pred-forms] `(has-args ~arg-pred-forms {}))
  ([arg-pred-forms expect-hash-form]
    (assert-args has-args
      (vector? arg-pred-forms) "a vector of argument predicates")
    `(assoc ~expect-hash-form :has-args
       (make-arg-checker ~arg-pred-forms '~arg-pred-forms))))


(defmacro times
  "Creates or associates to an existing expectation hash the :times key with a
value corresponding to a predicate function which expects an integer value.
This function can either be specified as the first argument to times or can be
the result of calling times with an integer argument, in which case the
predicate will default to being an exact match.  This predicate is called at
the end of an expect expression to validate that an expected dependency
function was called the expected number of times.
Usage:
(times n)
(times #(> n %))
(times n expectation-hash)"
  ([times-fn] `(times ~times-fn {}))
  ([times-fn expectation-hash]
    `(assoc ~expectation-hash :times (make-count-checker ~times-fn '~times-fn))))


;-------------------------------------------------------------------------------
; The main expect macro.
(defmacro expect
 "Use expect to redirect calls to dependent functions that are made within the
code under test. Instead of calling the functions that would normally be used,
temporary stubs are used, which can verify function parameters and call counts.
Return values can also be specified as needed.
Usage:
(expect [dep-fn (has-args [arg-pred1] (times n (returns x)))]
  (function-under-test a b c))"

  [expect-bindings & body]
  (assert-args expect
    (vector? expect-bindings) "a vector of expectation bindings"
    (even? (count expect-bindings))
    "an even number of forms in expectation bindings")
  (let [mock-data (gensym "mock-data_")]
    `(let [~mock-data (map (fn [args#]
                             (apply clojure.contrib.mock/make-mock args#))
                        ~(cons 'list (map (fn [[n m]] (vector (list 'quote n) m))
                                       (partition 2 expect-bindings))))]
       (binding ~(make-bindings expect-bindings mock-data) ~@body)
       (clojure.contrib.mock/validate-counts ~mock-data) true)))
