;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.spec.test
  (:require
   [clojure.spec :as spec]
   [clojure.spec.gen :as gen]))

;; wrap spec/explain-data until specs always return nil for ok data
(defn- explain-data*
  [spec v]
  (when-not (spec/valid? spec v nil)
    (spec/explain-data spec v)))

;; wrap and unwrap spec failure data in an exception so that
;; quick-check will treat it as a failure.
(defn- wrap-failing
  [explain-data step]
  (ex-info "Wrapper" {::check-call (assoc explain-data :failed-on step)}))

(defn- unwrap-failing
  [ret]
  (let [ret (if-let [explain (-> ret :result ex-data ::check-call)]
              (assoc ret :result explain)
              ret)]
    (if-let [shrunk-explain (-> ret :shrunk :result ex-data ::check-call)]
      (assoc-in ret [:shrunk :result] shrunk-explain)
      ret)))

(defn- check-call
  "Returns true if call passes specs, otherwise *returns* an exception
with explain-data plus a :failed-on key under ::check-call."
  [f specs args]
  (let [cargs (when (:args specs) (spec/conform (:args specs) args))]
    (if (= cargs ::spec/invalid)
      (wrap-failing (explain-data* (:args specs) args) :args)
      (let [ret (apply f args)
            cret (when (:ret specs) (spec/conform (:ret specs) ret))]
        (if (= cret ::spec/invalid)
          (wrap-failing (explain-data* (:ret specs) ret) :ret)
          (if (and (:args specs) (:ret specs) (:fn specs))
            (if (spec/valid? (:fn specs) {:args cargs :ret cret})
              true
              (wrap-failing (explain-data* (:fn specs) {:args cargs :ret cret}) :fn))
            true))))))

(defn check-fn
  "Check a function using provided specs and test.check.
Same options and return as check-var"
  [f specs
   & {:keys [num-tests seed max-size reporter-fn]
      :or {num-tests 100 max-size 200 reporter-fn (constantly nil)}}]
  (let [g (spec/gen (:args specs))
        prop (gen/for-all* [g] #(check-call f specs %))]
    (let [ret (gen/quick-check num-tests prop :seed seed :max-size max-size :reporter-fn reporter-fn)]
      (if-let [[smallest] (-> ret :shrunk :smallest)]
        (unwrap-failing ret)
        ret))))

(defn check-var
  "Checks a var's specs using test.check. Optional args are
passed through to test.check/quick-check:

  num-tests     number of tests to run, default 100
  seed          random seed
  max-size      how large an input to generate, max 200
  reporter-fn   reporting fn

Returns a map as quick-check, with :explain-data added if
:result is false."
  [v & opts]
  (let [specs (spec/get-spec v)]
    (if (:args specs)
      (apply check-fn @v specs opts)
      (throw (IllegalArgumentException. (str  "No :args spec for " v))))))

(defn- run-var-tests
  "Helper for run-tests, run-all-tests."
  [vs]
  (let [reporter-fn println]
    (reduce
     (fn [totals v]
       (let [_  (println "Checking" v)
             ret (check-var v :reporter-fn reporter-fn)]
         (prn ret)
         (cond-> totals
                 true (update :test inc)
                 (true? (:result ret)) (update :pass inc)
                 (::spec/problems (:result ret)) (update :fail inc)
                 (instance? Throwable (:result ret)) (update :error inc))))
     {:test 0, :pass 0, :fail 0, :error 0}
     vs)))

(defn run-tests
  "Like run-all-tests, but scoped to specific namespaces, or to
*ns* if no ns-sym are specified."
  [& ns-syms]
  (if (seq ns-syms)
    (run-var-tests (->> (apply spec/speced-vars ns-syms)
                        (filter (fn [v] (:args (spec/get-spec v))))))
    (run-tests (.name ^clojure.lang.Namespace *ns*))))

(defn run-all-tests
  "Like clojure.test/run-all-tests, but runs test.check tests
for all speced vars. Prints per-test results to *out*, and
returns a map with :test,:pass,:fail, and :error counts."
  []
  (run-var-tests (spec/speced-vars)))

(comment
  (require '[clojure.pprint :as pp]
           '[clojure.spec :as s]
           '[clojure.spec.gen :as gen]
           '[clojure.test :as ctest])

  (require :reload '[clojure.spec.test :as test])

  (load-file "examples/broken_specs.clj")
  (load-file "examples/correct_specs.clj")

  ;; discover speced vars for your own test runner
  (s/speced-vars)

  ;; check a single var
  (test/check-var #'-)
  (test/check-var #'+)
  (test/check-var #'clojure.spec.broken-specs/throwing-fn)

  ;; old style example tests
  (ctest/run-all-tests)

  (s/speced-vars 'clojure.spec.correct-specs)
  ;; new style spec tests return same kind of map
  (test/check-var #'subs)
  (clojure.spec.test/run-tests 'clojure.core)
  (test/run-all-tests)

  )


  


