;;; test-is.clj: test framework for Clojure

;; by Stuart Sierra, http://stuartsierra.com/
;; June 5, 2008

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
;; Run tests with (test-ns) or (test-all). As in any language with
;; macros, you may need to recompile functions after changing a macro
;; definition.



(clojure/in-ns 'test-is)
(clojure/refer 'clojure)

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

(defn- assert-expr [form message]
  `(do (count-assertion)
       (let [value# ~form]
         (when-not value#
           (failure (str ~(pr-str form) " was false/nil")
                    ~message)))))

(defn- assert-equal [form message]
  (let [expr1 (second form)
        expr2 (nth form 2)]
    `(do (count-assertion)
         (let [value1# ~expr1
               value2# ~expr2]
           (when-not (= value1# value2#)
             (failure (str ~(pr-str expr1) " is " (pr-str value1#)
                           " but should be " (pr-str value2#))
                      ~message))))))

(defn- always-fail-assert [message]
  `(do (count-assertion)
       (failure ~message nil)))

(defmacro #^{:private true} with-test-counters [& body]
  `(binding [*tests* (ref 0)
             *assertions* (ref 0)
             *failures* (ref 0)
             *exceptions* (ref 0)]
     ~@body
     (.. System err (println (str "\nRan " @*tests* " tests with "
                                  @*assertions* " assertions.\n"
                                  @*failures* " failures, "
                                  @*exceptions* " exceptions.")))))

(defn- run-test-fn
  "Calls the function; reports errors/exceptions."
  [f name]
  (try
   (count-test)
   (f)
   (catch java.lang.AssertionError e
     (count-failure)
     (.. System err (println (str "FAIL in " name ": "
                                  (.getMessage e)))))
   (catch java.lang.Exception e
     (count-exception)
     (.. System err (println (str "EXCEPTION in " name ": " e))))))

(defn- test-var
  "Finds and calls the fn in a var's :test metadata."
  [v]
  (when-let f (:test (meta v))
    (run-test-fn f (str v))))

(defn- test-interns
  "Tests all interned symbols in the namespace."
  [ns]
  (let [ns (if (symbol? ns) (find-ns ns) ns)]
    (.. System err (println (str "Testing " ns)))
    (dorun (map test-var (vals (ns-interns ns))))))


;;; PUBLIC

(defmacro is
  "Generic assertion macro.  Throws AssertionError if form evaluates
  logical false.  Optional message will be added to the error.

  form may be one of:
    * an equality test like (= expression expected-value)
    * nil, which always fails
    * an arbitrary expression, fails if it returns false/nil"
  ([form] `(is ~form nil))
  ([form message]
     (cond
      (nil? form) (always-fail-assert message)
      (and (seq? form) (= '= (first form))) (assert-equal form message)
      :else (assert-expr form message))))

(defn test-ns
  "Runs tests on all interned symbols in the namespaces 
  (symbols or namespace objects) and reports results."
  ([] (test-ns *ns*))
  ([& namespaces]
     (with-test-counters (dorun (map test-interns namespaces)))))

(defn test-all
  "Runs all tests in all namespaces."
  []
  (apply test-ns (all-ns)))

(defmacro deftest
  "Defs an unbound Var with body in its :test fn."
  [name & body]
  `(def ~(with-meta name {:test `(fn [] ~@body)})))
