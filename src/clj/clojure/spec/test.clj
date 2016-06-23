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
  (let [ret (::return result)]
    (cond
     (true? ret) :pass
     (::s/args ret) :instrument-fail
     (::s/no-gen-for ret) :no-gen
     (::args ret) :fail
     :default :error)))

(defn- make-test-result
  "Builds spec result map."
  [test-sym spec test-check-ret]
  (let [result (merge {::sym test-sym
                       ::spec spec
                       ::stc/ret test-check-ret}
                      (when-let [result (-> test-check-ret :result)]
                        {::return (unwrap-return result)})
                      (when-let [shrunk (-> test-check-ret :shrunk)]
                        {::return (unwrap-return (:result shrunk))}))]
    (assoc result ::result-type (result-type result))))

(defn- abbrev-result
  [x]
  (if (true? (::return x))
    (dissoc x ::spec ::stc/ret ::return)
    (update (dissoc x ::stc/ret) ::spec s/describe)))

(defn- default-result-callback
  [x]
  (pp/pprint (abbrev-result x))
  (flush))

(defn- test-1
  [{:keys [s f spec]}
   {:keys [result-callback] :as opts
    :or {result-callback default-result-callback}}]
  (let [result (cond
                (nil? f)
                {::result-type :no-fn ::sym s ::spec spec}
                
                (:args spec)
                (let [tcret (check-fn f spec opts)]
                  (make-test-result s spec tcret))
                
                :default
                {::result-type :no-args ::sym s ::spec spec})]
    (result-callback result)
    result))

;; duped from spec to avoid introducing public API
(defn- as-seqable
  [x]
  (if (seqable? x) x (list x)))

;; duped from spec to avoid introducing public API
(defn- ns-matcher
  [ns-syms]
  (let [ns-names (into #{} (map str) ns-syms)]
    (fn [s]
      (contains? ns-names (namespace s)))))

(defn- update-result-map
  ([]
     {:test 0 :pass 0 :fail 0 :error 0
      :no-fn 0 :no-args 0 :no-gen 0})
  ([m] m)
  ([results result]
     (-> results
         (update :test inc)
         (update (::result-type result) inc))))

(defn- sym->test-map
  [s]
  (let [v (resolve s)]
    {:s s
     :f (when v @v)
     :spec (when v (s/get-spec v))}))

(defn test-fn
  "Runs generative tests for fn f using spec and opts. See
'test' for options and return."
  ([f spec] (test-fn f spec nil))
  ([f spec opts]
     (update-result-map
      (update-result-map)
      (test-1 {:f f :spec spec} opts))))

(defn test
  "Checks specs for fns named by sym-or-syms using test.check.

The opts map includes the following optional keys:

:clojure.spec.test.check/opts  opts to flow through test.check
:result-callback               callback fn to handle test results
:gen                           overrides map for spec/gen

The c.s.t.c/opts include :num-tests in addition to the keys
documented by test.check.

The result-callback defaults to default-result-callback.

Returns a map with the following keys:

:test     # of syms tested
:pass     # of passing tests
:fail     # of failing tests
:error    # of throwing tests
:no-fn    # of syms with no fn
:no-args  # of syms with no argspec
:no-gen   # of syms for which arg data gen failed"
  ([sym-or-syms] (test sym-or-syms nil))
  ([sym-or-syms opts]
     (transduce
      (comp
       (map sym->test-map)
       (map #(test-1 % opts)))
      update-result-map
      (as-seqable sym-or-syms))))

(defn test-ns
  "Like test, but scoped to specific namespaces, or to
*ns* if no arg specified."
  ([] (test-ns (.name ^clojure.lang.Namespace *ns*)))
  ([ns-or-nses] (test-ns ns-or-nses nil))
  ([ns-or-nses opts]
     (let [ns-match? (ns-matcher (as-seqable ns-or-nses))]
       (transduce
        (comp (filter symbol?)
              (filter ns-match?)
              (map sym->test-map)
              (map #(test-1 % opts)))
        update-result-map
        (keys (s/registry))))))

(defn test-all
  "Like test, but tests all vars named by fn-specs in the spec
registry."
  ([] (test-all nil))
  ([opts]
     (transduce
      (comp (filter symbol?)
            (map sym->test-map)
            (map #(test-1 % opts)))
      update-result-map
      (keys (s/registry)))))



  


