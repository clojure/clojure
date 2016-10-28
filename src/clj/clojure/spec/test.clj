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
   [clojure.spec.gen :as gen]
   [clojure.string :as str]))

(in-ns 'clojure.spec.test.check)
(in-ns 'clojure.spec.test)
(alias 'stc 'clojure.spec.test.check)

(defn- throwable?
  [x]
  (instance? Throwable x))

(defn ->sym
  [x]
  (@#'s/->sym x))

(defn- ->var
  [s-or-v]
  (if (var? s-or-v)
    s-or-v
    (let [v (and (symbol? s-or-v) (resolve s-or-v))]
      (if (var? v)
        v
        (throw (IllegalArgumentException. (str (pr-str s-or-v) " does not name a var")))))))

(defn- collectionize
  [x]
  (if (symbol? x)
    (list x)
    x))

(defn enumerate-namespace
  "Given a symbol naming an ns, or a collection of such symbols,
returns the set of all symbols naming vars in those nses."
  [ns-sym-or-syms]
  (into
   #{}
   (mapcat (fn [ns-sym]
             (map
              (fn [name-sym]
                (symbol (name ns-sym) (name name-sym)))
              (keys (ns-interns ns-sym)))))
   (collectionize ns-sym-or-syms)))

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

(defn- interpret-stack-trace-element
  "Given the vector-of-syms form of a stacktrace element produced
by e.g. Throwable->map, returns a map form that adds some keys
guessing the original Clojure names. Returns a map with

  :class         class name symbol from stack trace
  :method        method symbol from stack trace
  :file          filename from stack trace
  :line          line number from stack trace
  :var-scope     optional Clojure var symbol scoping fn def
  :local-fn      optional local Clojure symbol scoping fn def

For non-Clojure fns, :scope and :local-fn will be absent."
  [[cls method file line]]
  (let [clojure? (contains? '#{invoke invokeStatic} method)
        demunge #(clojure.lang.Compiler/demunge %)
        degensym #(str/replace % #"--.*" "")
        [ns-sym name-sym local] (when clojure?
                                  (->> (str/split (str cls) #"\$" 3)
                                       (map demunge)))]
    (merge {:file file
            :line line
            :method method
            :class cls}
           (when (and ns-sym name-sym)
             {:var-scope (symbol ns-sym name-sym)})
           (when local
             {:local-fn (symbol (degensym local))}))))

(defn- stacktrace-relevant-to-instrument
  "Takes a coll of stack trace elements (as returned by
StackTraceElement->vec) and returns a coll of maps as per
interpret-stack-trace-element that are relevant to a
failure in instrument."
  [elems]
  (let [plumbing? (fn [{:keys [var-scope]}]
                    (contains? '#{clojure.spec.test/spec-checking-fn} var-scope))]
    (sequence (comp (map StackTraceElement->vec)
                    (map interpret-stack-trace-element)
                    (filter :var-scope)
                    (drop-while plumbing?))
              elems)))

(defn- spec-checking-fn
  [v f fn-spec]
  (let [fn-spec (@#'s/maybe-spec fn-spec)
        conform! (fn [v role spec data args]
                   (let [conformed (s/conform spec data)]
                     (if (= ::s/invalid conformed)
                       (let [caller (->> (.getStackTrace (Thread/currentThread))
                                         stacktrace-relevant-to-instrument
                                         first)
                             ed (merge (assoc (s/explain-data* spec [role] [] [] data)
                                         ::s/args args
                                         ::s/failure :instrument)
                                       (when caller
                                         {::caller (dissoc caller :class :method)}))]
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

(defn- no-fspec
  [v spec]
  (ex-info (str "Fn at " v " is not spec'ed.")
           {:var v :spec spec ::s/failure :no-fspec}))

(defonce ^:private instrumented-vars (atom {}))

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

(defn- instrument-1
  [s opts]
  (when-let [v (resolve s)]
    (when-not (-> v meta :macro)
      (let [spec (s/get-spec v)
            {:keys [raw wrapped]} (get @instrumented-vars v)
            current @v
            to-wrap (if (= wrapped current) raw current)
            ospec (or (instrument-choose-spec spec s opts)
                      (throw (no-fspec v spec)))
            ofn (instrument-choose-fn to-wrap ospec s opts)
            checked (spec-checking-fn v ofn ospec)]
        (alter-var-root v (constantly checked))
        (swap! instrumented-vars assoc v {:raw to-wrap :wrapped checked})
        (->sym v)))))

(defn- unstrument-1
  [s]
  (when-let [v (resolve s)]
    (when-let [{:keys [raw wrapped]} (get @instrumented-vars v)]
      (swap! instrumented-vars dissoc v)
      (let [current @v]
        (when (= wrapped current)
          (alter-var-root v (constantly raw))
          (->sym v))))))

(defn- opt-syms
  "Returns set of symbols referenced by 'instrument' opts map"
  [opts]
  (reduce into #{} [(:stub opts) (keys (:replace opts)) (keys (:spec opts))]))

(defn- fn-spec-name?
  [s]
  (and (symbol? s)
       (not (some-> (resolve s) meta :macro))))

(defn instrumentable-syms
  "Given an opts map as per instrument, returns the set of syms
that can be instrumented."
  ([] (instrumentable-syms nil))
  ([opts]
     (assert (every? ident? (keys (:gen opts))) "instrument :gen expects ident keys")
     (reduce into #{} [(filter fn-spec-name? (keys (s/registry)))
                       (keys (:spec opts))
                       (:stub opts)
                       (keys (:replace opts))])))

(defn instrument
  "Instruments the vars named by sym-or-syms, a symbol or collection
of symbols, or all instrumentable vars if sym-or-syms is not
specified.

If a var has an :args fn-spec, sets the var's root binding to a
fn that checks arg conformance (throwing an exception on failure)
before delegating to the original fn.

The opts map can be used to override registered specs, and/or to
replace fn implementations entirely. Opts for symbols not included
in sym-or-syms are ignored. This facilitates sharing a common
options map across many different calls to instrument.

The opts map may have the following keys:

  :spec     a map from var-name symbols to override specs
  :stub     a set of var-name symbols to be replaced by stubs
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
  ([] (instrument (instrumentable-syms)))
  ([sym-or-syms] (instrument sym-or-syms nil))
  ([sym-or-syms opts]
     (locking instrumented-vars
       (into
        []
        (comp (filter (instrumentable-syms opts))
              (distinct)
              (map #(instrument-1 % opts))
              (remove nil?))
        (collectionize sym-or-syms)))))

(defn unstrument
  "Undoes instrument on the vars named by sym-or-syms, specified
as in instrument. With no args, unstruments all instrumented vars.
Returns a collection of syms naming the vars unstrumented."
  ([] (unstrument (map ->sym (keys @instrumented-vars))))
  ([sym-or-syms]
     (locking instrumented-vars
       (into
        []
        (comp (filter symbol?)
              (map unstrument-1)
              (remove nil?))
        (collectionize sym-or-syms)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; testing  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- explain-check
  [args spec v role]
  (ex-info
   "Specification-based check failed"
   (when-not (s/valid? spec v nil)
     (assoc (s/explain-data* spec [role] [] [] v)
       ::args args
       ::val v
       ::s/failure :check-failed))))

(defn- check-call
  "Returns true if call passes specs, otherwise *returns* an exception
with explain-data + ::s/failure."
  [f specs args]
  (let [cargs (when (:args specs) (s/conform (:args specs) args))]
    (if (= cargs ::s/invalid)
      (explain-check args (:args specs) args :args)
      (let [ret (apply f args)
            cret (when (:ret specs) (s/conform (:ret specs) ret))]
        (if (= cret ::s/invalid)
          (explain-check args (:ret specs) ret :ret)
          (if (and (:args specs) (:ret specs) (:fn specs))
            (if (s/valid? (:fn specs) {:args cargs :ret cret})
              true
              (explain-check args (:fn specs) {:args cargs :ret cret} :fn))
            true))))))

(defn- quick-check
  [f specs {gen :gen opts ::stc/opts}]
  (let [{:keys [num-tests] :or {num-tests 1000}} opts
        g (try (s/gen (:args specs) gen) (catch Throwable t t))]
    (if (throwable? g)
      {:result g}
      (let [prop (gen/for-all* [g] #(check-call f specs %))]
        (apply gen/quick-check num-tests prop (mapcat identity opts))))))

(defn- make-check-result
  "Builds spec result map."
  [check-sym spec test-check-ret]
  (merge {:spec spec
          ::stc/ret test-check-ret}
         (when check-sym
           {:sym check-sym})
         (when-let [result (-> test-check-ret :result)]
           (when-not (true? result) {:failure result}))
         (when-let [shrunk (-> test-check-ret :shrunk)]
           {:failure (:result shrunk)})))

(defn- check-1
  [{:keys [s f v spec]} opts]
  (let [re-inst? (and v (seq (unstrument s)) true)
        f (or f (when v @v))
        specd (s/spec spec)]
    (try
     (cond
      (or (nil? f) (some-> v meta :macro))
      {:failure (ex-info "No fn to spec" {::s/failure :no-fn})
       :sym s :spec spec}
    
      (:args specd)
      (let [tcret (quick-check f specd opts)]
        (make-check-result s spec tcret))
    
      :default
      {:failure (ex-info "No :args spec" {::s/failure :no-args-spec})
       :sym s :spec spec})
     (finally
      (when re-inst? (instrument s))))))

(defn- sym->check-map
  [s]
  (let [v (resolve s)]
    {:s s
     :v v
     :spec (when v (s/get-spec v))}))

(defn- validate-check-opts
  [opts]
  (assert (every? ident? (keys (:gen opts))) "check :gen expects ident keys"))

(defn check-fn
  "Runs generative tests for fn f using spec and opts. See
'check' for options and return."
  ([f spec] (check-fn f spec nil))
  ([f spec opts]
     (validate-check-opts opts)
     (check-1 {:f f :spec spec} opts)))

(defn checkable-syms
  "Given an opts map as per check, returns the set of syms that
can be checked."
  ([] (checkable-syms nil))
  ([opts]
     (validate-check-opts opts)
     (reduce into #{} [(filter fn-spec-name? (keys (s/registry)))
                       (keys (:spec opts))])))

(defn check
  "Run generative tests for spec conformance on vars named by
sym-or-syms, a symbol or collection of symbols. If sym-or-syms
is not specified, check all checkable vars.

The opts map includes the following optional keys, where stc
aliases clojure.spec.test.check: 

::stc/opts  opts to flow through test.check/quick-check
:gen        map from spec names to generator overrides

The ::stc/opts include :num-tests in addition to the keys
documented by test.check. Generator overrides are passed to
spec/gen when generating function args.

Returns a lazy sequence of check result maps with the following
keys

:spec       the spec tested
:sym        optional symbol naming the var tested
:failure    optional test failure
::stc/ret   optional value returned by test.check/quick-check

The value for :failure can be any exception. Exceptions thrown by
spec itself will have an ::s/failure value in ex-data:

:check-failed   at least one checked return did not conform
:no-args-spec   no :args spec provided
:no-fn          no fn provided
:no-fspec       no fspec provided
:no-gen         unable to generate :args
:instrument     invalid args detected by instrument
"
  ([] (check (checkable-syms)))
  ([sym-or-syms] (check sym-or-syms nil))
  ([sym-or-syms opts]
     (->> (collectionize sym-or-syms)
          (filter (checkable-syms opts))
          (pmap
           #(check-1 (sym->check-map %) opts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; check reporting  ;;;;;;;;;;;;;;;;;;;;;;;;

(defn- failure-type
  [x]
  (::s/failure (ex-data x)))

(defn- unwrap-failure
  [x]
  (if (failure-type x)
    (ex-data x)
    x))

(defn- result-type
  "Returns the type of the check result. This can be any of the
::s/failure keywords documented in 'check', or:

  :check-passed   all checked fn returns conformed
  :check-threw    checked fn threw an exception"
  [ret]
  (let [failure (:failure ret)]
    (cond
     (nil? failure) :check-passed
     (failure-type failure) (failure-type failure)
     :default :check-threw)))

(defn abbrev-result
  "Given a check result, returns an abbreviated version
suitable for summary use."
  [x]
  (if (:failure x)
    (-> (dissoc x ::stc/ret)
        (update :spec s/describe)
        (update :failure unwrap-failure))
    (dissoc x :spec ::stc/ret)))

(defn summarize-results
  "Given a collection of check-results, e.g. from 'check', pretty
prints the summary-result (default abbrev-result) of each.

Returns a map with :total, the total number of results, plus a
key with a count for each different :type of result."
  ([check-results] (summarize-results check-results abbrev-result))
  ([check-results summary-result]
     (reduce
      (fn [summary result]
        (pp/pprint (summary-result result))
        (-> summary
            (update :total inc)
            (update (result-type result) (fnil inc 0))))
      {:total 0}
       check-results)))



