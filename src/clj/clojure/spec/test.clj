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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; instrument ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private ^:dynamic *instrument-enabled*
  "if false, instrumented fns call straight through"
  true)

(defn- fn-spec?
  "Fn-spec must include at least :args or :ret specs."
  [m]
  (or (:args m) (:ret m)))

(defmacro with-instrument-disabled
  "Disables instrument's checking of calls, within a scope."
  [& body]
  `(binding [*instrument-enabled* nil]
     ~@body))

(defn- spec-checking-fn
  [v f fn-spec]
  (let [fn-spec (@#'s/maybe-spec fn-spec)
        conform! (fn [v role spec data args]
                   (let [conformed (s/conform spec data)]
                     (if (= ::s/invalid conformed)
                       (let [ed (assoc (s/explain-data* spec [role] [] [] data)
                                  ::s/args args)]
                         (throw (ex-info
                                 (str "Call to " v " did not conform to spec:\n" (with-out-str (s/explain-out ed)))
                                 ed)))
                       conformed)))]
    (fn
     [& args]
     (if *instrument-enabled*
       (with-instrument-disabled
         (when (:args fn-spec) (conform! v :args (:args fn-spec) args args))
         (binding [*instrument-enabled* true]
           (.applyTo ^clojure.lang.IFn f args)))
       (.applyTo ^clojure.lang.IFn f args)))))


(defn- no-fn-spec
  [v spec]
  (ex-info (str "Fn at " v " is not spec'ed.")
           {:var v :spec spec}))

(def ^:private instrumented-vars
     "Map for instrumented vars to :raw/:wrapped fns"
     (atom {}))

(defn- ->var
  [s-or-v]
  (if (var? s-or-v)
    s-or-v
    (let [v (and (symbol? s-or-v) (resolve s-or-v))]
      (if (var? v)
        v
        (throw (IllegalArgumentException. (str (pr-str s-or-v) " does not name a var")))))))

(defn- instrument-choose-fn
  "Helper for instrument."
  [f spec sym {over :gen :keys [stub replace]}]
  (if (some #{sym} stub)
    (-> spec (s/gen over) gen/generate)
    (get replace sym f)))

(defn- instrument-choose-spec
  "Helper for instrument"
  [spec sym {overrides :spec}]
  (get overrides sym spec))

(defn- collectionize
  [x]
  (if (symbol? x)
    (list x)
    x))

(def ->sym @#'s/->sym)

(defn- instrument-1
  [s opts]
  (when-let [v (resolve s)]
    (let [spec (s/get-spec v)
          {:keys [raw wrapped]} (get @instrumented-vars v)
          current @v
          to-wrap (if (= wrapped current) raw current)
          ospec (or (instrument-choose-spec spec s opts)
                      (throw (no-fn-spec v spec)))
          ofn (instrument-choose-fn to-wrap ospec s opts)
          checked (spec-checking-fn v ofn ospec)]
      (alter-var-root v (constantly checked))
      (swap! instrumented-vars assoc v {:raw to-wrap :wrapped checked}))
    (->sym v)))

(defn- unstrument-1
  [s]
  (when-let [v (resolve s)]
    (when-let [{:keys [raw wrapped]} (get @instrumented-vars v)]
      (let [current @v]
        (when (= wrapped current)
          (alter-var-root v (constantly raw))))
      (swap! instrumented-vars dissoc v))
    (->sym v)))

(defn- opt-syms
  "Returns set of symbols referenced by 'instrument' opts map"
  [opts]
  (reduce into #{} [(:stub opts) (keys (:replace opts)) (keys (:spec opts))]))

(defn- sym-matcher
  "Returns a fn that matches symbols that are either in syms,
or whose namespace is in syms."
  [syms]
  (let [names (into #{} (map str) syms)]
    (fn [s]
      (or (contains? names (namespace s))
          (contains? names (str s))))))

(defn- validate-opts
  [opts]
  (assert (every? ident? (keys (:gen opts))) "instrument :gen expects ident keys"))

(defn instrument
  "Instruments the vars matched by ns-or-names, a symbol or a
collection of symbols. Instruments the current namespace if
ns-or-names not specified. Idempotent.

A var matches ns-or-names if ns-or-names includes either the var's
fully qualified name or the var's namespace.

If a var has an :args fn-spec, sets the var's root binding to a
fn that checks arg conformance (throwing an exception on failure)
before delegating to the original fn.

The opts map can be used to override registered specs, and/or to
replace fn implementations entirely. Opts for symbols not matched
by ns-or-names are ignored. This facilitates sharing a common
options map across many different calls to instrument.

The opts map may have the following keys:

  :spec     a map from var-name symbols to override specs
  :stub     a collection of var-name symbols to be replaced by stubs
  :gen      a map from spec names to generator overrides
  :replace  a map from var-name symbols to replacement fns

:spec overrides registered fn-specs with specs your provide. Use
:spec overrides to provide specs for libraries that do not have
them, or to constrain your own use of a fn to a subset of its
spec'ed contract.

:stub replaces a fn with a stub that checks :args, then uses the
:ret spec to generate a return value.

:gen overrides are used only for :stub generation.

:replace replaces a fn with a fn that checks args conformance, then
invokes the fn you provide, enabling arbitrary stubbing and mocking.

:spec can be used in combination with :stub or :replace.

Returns a collection of syms naming the vars instrumented."
  ([] (instrument (.name ^clojure.lang.Namespace *ns*)))
  ([ns-or-names] (instrument ns-or-names nil))
  ([ns-or-names opts]
     (validate-opts opts)
     (let [match? (sym-matcher (collectionize ns-or-names))]
       (locking instrumented-vars
         (into
          []
          (comp cat
                (filter symbol?)
                (filter match?)
                (distinct)
                (map #(instrument-1 % opts))
                (remove nil?))
          [(keys (s/registry)) (opt-syms opts)])))))

(defn unstrument
  "Undoes instrument on the vars matched by ns-or-names, specified
as in instrument. Returns a collection of syms naming the vars
unstrumented."
  ([] (unstrument (.name ^clojure.lang.Namespace *ns*)))
  ([ns-or-names]
     (let [match? (sym-matcher (collectionize ns-or-names))]
       (locking instrumented-vars
         (into
          []
          (comp (map ->sym)
                 (filter match?)
                 (map unstrument-1)
                 (remove nil?))
           (keys @instrumented-vars))))))

(defn instrument-all
  "Like instrument, but works on all vars."
  ([] (instrument-all nil))
  ([opts]
     (validate-opts opts)
     (locking instrumented-vars
       (into
        []
        (comp cat
              (filter symbol?)
              (distinct)
              (map #(instrument-1 % opts))
              (remove nil?))
        [(keys (s/registry)) (opt-syms opts)]))))

(defn unstrument-all
  "Like unstrument, but works on all vars."
  []
  (locking instrumented-vars
    (into
     []
     (comp (map ->sym)
           (map unstrument-1)
           (remove nil?))
     (keys @instrumented-vars))))

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



