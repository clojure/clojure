;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.spec.test
  (:refer-clojure :exclude [test])
  (:require
   [clojure.pprint :as pp]
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]))

(in-ns 'clojure.spec.test.check)
(in-ns 'clojure.spec.test)
(alias 'stc 'clojure.spec.test.check)

(defn- explain-test
  [args spec v role]
  (ex-info
   "Specification-based test failed"
   (when-not (s/valid? spec v nil)
     (assoc (s/explain-data* spec [role] [] [] v)
       ::args args
       ::val v))))

(defn- check-call
  "Returns true if call passes specs, otherwise *returns* an exception
with explain-data under ::check-call."
  [f specs args]
  (let [cargs (when (:args specs) (s/conform (:args specs) args))]
    (if (= cargs ::s/invalid)
      (explain-test args (:args specs) args :args)
      (let [ret (apply f args)
            cret (when (:ret specs) (s/conform (:ret specs) ret))]
        (if (= cret ::s/invalid)
          (explain-test args (:ret specs) ret :ret)
          (if (and (:args specs) (:ret specs) (:fn specs))
            (if (s/valid? (:fn specs) {:args cargs :ret cret})
              true
              (explain-test args (:fn specs) {:args cargs :ret cret} :fn))
            true))))))

(defn- throwable?
  [x]
  (instance? Throwable x))

(defn- check-fn
  [f specs {gen :gen opts ::stc/opts}]
  (let [{:keys [num-tests] :or {num-tests 100}} opts
        g (try (s/gen (:args specs) gen) (catch Throwable t t))]
    (if (throwable? g)
      {:result g}
      (let [prop (gen/for-all* [g] #(check-call f specs %))]
        (apply gen/quick-check num-tests prop (mapcat identity opts))))))

(defn- unwrap-return
  "Unwraps exceptions used to flow information through test.check."
  [x]
  (let [data (ex-data x)]
    (if (or (::args data) (::s/args data) (::s/no-gen-for data))
      data
      x)))

(defn- result-type
  [result]
  (let [ret (:result result)]
    (cond
     (true? ret) :pass
     (::s/args ret) :no-argspec
     (::s/no-gen-for ret) :no-gen
     (::args ret) :fail
     :default :error)))

(defn- make-test-result
  "Builds spec result map."
  [test-sym spec test-check-ret]
  (let [result (merge {:spec spec
                       ::stc/ret test-check-ret}
                      (when test-sym
                        {:sym test-sym})
                      (when-let [result (-> test-check-ret :result)]
                        {:result (unwrap-return result)})
                      (when-let [shrunk (-> test-check-ret :shrunk)]
                        {:result (unwrap-return (:result shrunk))}))]
    (assoc result :type (result-type result))))

(defn- test-1
  [{:keys [s f spec]} {:keys [result-callback] :as opts}]
  (cond
   (nil? f)
   {:type :no-fn :sym s :spec spec}
                
   (:args spec)
   (let [tcret (check-fn f spec opts)]
     (make-test-result s spec tcret))
                
   :default
   {:type :no-argspec :sym s :spec spec}))

;; duped from spec to avoid introducing public API
(defn- collectionize
  [x]
  (if (symbol? x)
    (list x)
    x))

(defn- sym-matcher
  "Returns a fn that matches symbols that are either in syms,
or whose namespace is in syms."
  [syms]
  (let [names (into #{} (map str) syms)]
    (fn [s]
      (or (contains? names (namespace s))
          (contains? names (str s))))))

(defn- sym->test-map
  [s]
  (let [v (resolve s)]
    {:s s
     :f (when v @v)
     :spec (when v (s/get-spec v))}))

(defn- validate-opts
  [opts]
  (assert (every? ident? (keys (:gen opts))) "test :gen expects ident keys"))

(defn syms-to-test
  "Returns a coll of registered syms matching ns-or-names (a symbol or
collection of symbols).

A symbol matches ns-or-names if ns-or-names includes either the symbol
itself or the symbol's namespace symbol.

If no ns-or-names specified, returns all registered syms."
  ([] (sequence
       (filter symbol?)
       (keys (s/registry))))
  ([ns-or-names]
     (let [match? (sym-matcher (collectionize ns-or-names))]
       (sequence
        (comp (filter symbol?)
              (filter match?))
        (keys (s/registry))))))

(defn test-fn
  "Runs generative tests for fn f using spec and opts. See
'test' for options and return."
  ([f spec] (test-fn f spec nil))
  ([f spec opts]
     (validate-opts opts)
     (test-1 {:f f :spec spec} opts)))

(defn test
  "Checks specs for vars named by syms using test.check.

The opts map includes the following optional keys, where stc
aliases clojure.spec.test.check: 

::stc/opts  opts to flow through test.check/quick-check
:gen        map from spec names to generator overrides

The ::stc/opts include :num-tests in addition to the keys
documented by test.check. Generator overrides are passed to
spec/gen when generating function args.

Returns a lazy sequence of test result maps with the following
keys

:spec       the spec tested
:type       the type of the test result
:sym        optional symbol naming the var tested
:result     optional test result
::stc/ret   optional value returned by test.check/quick-check

Values for the :result key can be one of

true        passing test
exception   code under test threw
map         with explain-data under :clojure.spec/problems

Values for the :type key can be one of

:pass       test passed
:fail       test failed
:error      test threw
:no-argspec no :args in fn-spec
:no-gen     unable to generate :args
:no-fn      unable to resolve fn to test
"
  ([syms] (test syms nil))
  ([syms opts]
     (validate-opts opts)
     (pmap #(test-1 (sym->test-map %) opts) syms)))

(defn abbrev-result
  "Given a test result, returns an abbreviated version
suitable for summary use."
  [x]
  (if (true? (:result x))
    (dissoc x :spec ::stc/ret :result)
    (update (dissoc x ::stc/ret) :spec s/describe)))

(defn summarize-results
  "Given a collection of test-results, e.g. from 'test',
pretty prints the abbrev-result of each.

Returns a map with :total, the total number of results, plus a
key with a count for each different :type of result."
  [test-results]
  (reduce
   (fn [summary result]
     (pp/pprint (abbrev-result result))
     (-> summary
         (update :total inc)
         (update (:type result) (fnil inc 0))))
   {:total 0}
   test-results))



