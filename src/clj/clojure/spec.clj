;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.spec
  (:refer-clojure :exclude [+ * and or cat def keys])
  (:require [clojure.walk :as walk]
            [clojure.spec.gen :as gen]
            [clojure.string :as str]))

(alias 'c 'clojure.core)

(set! *warn-on-reflection* true)

(def ^:dynamic *recursion-limit*
  "A soft limit on how many times a branching spec (or/alt/*/opt-keys/multi-spec)
  can be recursed through during generation. After this a
  non-recursive branch will be chosen."
  4)

(def ^:dynamic *fspec-iterations*
  "The number of times an anonymous fn specified by fspec will be (generatively) tested during conform"
  21)

(def ^:dynamic *coll-check-limit*
  "The number of items validated in a collection spec'ed with 'coll'"
  100)

(def ^:private ^:dynamic *instrument-enabled*
  "if false, instrumented fns call straight through"
  true)

(defprotocol Spec
  (conform* [spec x])
  (explain* [spec path via in x])
  (gen* [spec overrides path rmap])
  (with-gen* [spec gfn])
  (describe* [spec]))

(defonce ^:private registry-ref (atom {}))

(defn- named? [x] (instance? clojure.lang.Named x))

(defn- with-name [spec name]
  (with-meta spec (assoc (meta spec) ::name name)))

(defn- spec-name [spec]
  (when (instance? clojure.lang.IObj spec)
    (-> (meta spec) ::name)))

(defn- reg-resolve
  "returns the spec/regex at end of alias chain starting with k, nil if not found, k if k not Named"
  [k]
  (if (named? k)
    (let [reg @registry-ref]
      (loop [spec k]
        (if (named? spec)
          (recur (get reg spec))
          (when spec
            (with-name spec k)))))
    k))

(defn spec?
  "returns x if x is a spec object, else logical false"
  [x]
  (c/and (extends? Spec (class x)) x))

(defn regex?
  "returns x if x is a (clojure.spec) regex op, else logical false"
  [x]
  (c/and (::op x) x))

(declare spec-impl)
(declare regex-spec-impl)

(defn- maybe-spec
  "spec-or-k must be a spec, regex or resolvable kw/sym, else returns nil."
  [spec-or-k]
  (let [s (c/or (spec? spec-or-k)
                (regex? spec-or-k)
                (c/and (named? spec-or-k) (reg-resolve spec-or-k))
                nil)]
    (if (regex? s)
      (with-name (regex-spec-impl s nil) (spec-name s))
      s)))

(defn- the-spec
  "spec-or-k must be a spec, regex or kw/sym, else returns nil. Throws if unresolvable kw/sym"
  [spec-or-k]
  (c/or (maybe-spec spec-or-k)
        (when (named? spec-or-k)
          (throw (Exception. (str "Unable to resolve spec: " spec-or-k))))))

(defn- specize [s]
  (c/or (the-spec s) (spec-impl ::unknown s nil nil)))

(defn conform
  "Given a spec and a value, returns :clojure.spec/invalid if value does not match spec,
  else the (possibly destructured) value."
  [spec x]
  (conform* (specize spec) x))

(defn form
  "returns the spec as data"
  [spec]
  ;;TODO - incorporate gens
  (describe* (specize spec)))

(defn abbrev [form]
  (cond
   (seq? form)
   (walk/postwalk (fn [form]
                    (cond
                     (c/and (symbol? form) (namespace form))
                     (-> form name symbol)

                     (c/and (seq? form) (= 'fn (first form)) (= '[%] (second form)))
                     (last form)
                     
                     :else form))
                  form)

   (c/and (symbol? form) (namespace form))
   (-> form name symbol)

   :else form))

(defn describe
  "returns an abbreviated description of the spec as data"
  [spec]
  (abbrev (form spec)))

(defn with-gen
  "Takes a spec and a no-arg, generator-returning fn and returns a version of that spec that uses that generator"
  [spec gen-fn]
  (with-gen* (specize spec) gen-fn))

(defn explain-data* [spec path via in x]
  (when-let [probs (explain* (specize spec) path via in x)]
    {::problems probs}))

(defn explain-data
  "Given a spec and a value x which ought to conform, returns nil if x
  conforms, else a map with at least the key ::problems whose value is
  a path->problem-map, where problem-map has at least :pred and :val
  keys describing the predicate and the value that failed at that
  path."
  [spec x]
  (explain-data* spec [] (if-let [name (spec-name spec)] [name] []) [] x))

(defn- explain-out
  "prints an explanation to *out*."
  [ed]
  (if ed
    (do
      ;;(prn {:ed ed})
      (doseq [[path {:keys [pred val reason via in] :as prob}]  (::problems ed)]
        (when-not (empty? in)
          (print "In:" in ""))
        (print "val: ")
        (pr val)
        (print " fails spec: ")
        (print (c/or (last via) "_"))
        (when-not (empty? path)
          (print " at:" path))
        (print " predicate: ")
        (pr pred)
        (when reason (print ", " reason))
        (doseq [[k v] prob]
          (when-not (#{:pred :val :reason :via :in} k)
            (print "\n\t" k " ")
            (pr v)))
        (newline))
      (doseq [[k v] ed]
        (when-not (#{::problems} k)
          (print k " ")
          (pr v)
          (newline))))
    (println "Success!")))

(defn explain
  "Given a spec and a value that fails to conform, prints an explanation to *out*."
  [spec x]
  (explain-out (explain-data spec x)))

(declare valid?)

(defn- gensub
  [spec overrides path rmap form]
  ;;(prn {:spec spec :over overrides :path path :form form})
  (if-let [spec (specize spec)]
    (if-let [g (c/or (get overrides path) (gen* spec overrides path rmap))]
      (gen/such-that #(valid? spec %) g 100)
      (throw (Exception. (str "Unable to construct gen at: " path " for: " (abbrev form)))))
    (throw (Exception. (str "Unable to construct gen at: " path ", " (abbrev form) " can not be made a spec")))))

(defn gen
  "Given a spec, returns the generator for it, or throws if none can
  be constructed. Optionally an overrides map can be provided which
  should map paths (vectors of keywords) to generators. These will be
  used instead of the generators at those paths. Note that parent
  generator (in the spec or overrides map) will supersede those of any
  subtrees. A generator for a regex op must always return a
  sequential collection (i.e. a generator for s/? should return either
  an empty sequence/vector or a sequence/vector with one item in it)"
  ([spec] (gen spec nil))
  ([spec overrides] (gensub spec overrides [] {::recursion-limit *recursion-limit*} spec)))

(defn- ->sym
  "Returns a symbol from a symbol or var"
  [x]
  (if (var? x)
    (let [^clojure.lang.Var v x]
      (symbol (str (.name (.ns v)))
              (str (.sym v))))
    x))

(defn- unfn [expr]
  (if (c/and (seq? expr)
             (symbol? (first expr))
             (= "fn*" (name (first expr))))
    (let [[[s] & form] (rest expr)]
      (conj (walk/postwalk-replace {s '%} form) '[%] 'fn))
    expr))

(defn- res [form]
  (cond
   (keyword? form) form
   (symbol? form) (c/or (-> form resolve ->sym) form)   
   (sequential? form) (walk/postwalk #(if (symbol? %) (res %) %) (unfn form))
   :else form))

(defn ^:skip-wiki def-impl
  "Do not call this directly, use 'def'"
  [k form spec]
  (assert (c/and (named? k) (namespace k)) "k must be namespaced keyword/symbol")
  (let [spec (if (c/or (spec? spec) (regex? spec) (get @registry-ref spec))
               spec
               (spec-impl form spec nil nil))]
    (swap! registry-ref assoc k spec)
    k))

(defmacro def
  "Given a namespace-qualified keyword or symbol k, and a spec, spec-name, predicate or regex-op
  makes an entry in the registry mapping k to the spec"
  [k spec-form]
  `(def-impl ~k '~(res spec-form) ~spec-form))

(defn registry
  "returns the registry map"
  []
  @registry-ref)

(declare map-spec)

(defmacro spec
  "Takes a single predicate form, e.g. can be the name of a predicate,
  like even?, or a fn literal like #(< % 42). Note that it is not
  generally necessary to wrap predicates in spec when using the rest
  of the spec macros, only to attach a unique generator

  Can also be passed the result of one of the regex ops -
  cat, alt, *, +, ?, in which case it will return a regex-conforming
  spec, useful when nesting an independent regex.
  ---

  Optionally takes :gen generator-fn, which must be a fn of no args that
  returns a test.check generator.

  Returns a spec."
  [form & {:keys [gen]}]
  `(spec-impl '~(res form) ~form ~gen nil))

(defmacro multi-spec
  "Takes the name of a spec/predicate-returning multimethod and a
  tag-restoring keyword or fn (retag).  Returns a spec that when
  conforming or explaining data will pass it to the multimethod to get
  an appropriate spec. You can e.g. use multi-spec to dynamically and
  extensibly associate specs with 'tagged' data (i.e. data where one
  of the fields indicates the shape of the rest of the structure).

  The multimethod must use :clojure.spec/invalid as its default value
  and should return nil from that dispatch value:

  (defmulti mspec :tag :default :clojure.spec/invalid)
  (defmethod mspec :clojure.spec/invalid [_] nil)

  The methods should ignore their argument and return a predicate/spec:
  (defmethod mspec :int [_] (s/keys :req-un [::i]))

  retag is used during generation to retag generated values with
  matching tags. retag can either be a keyword, at which key the
  dispatch-tag will be assoc'ed, or a fn of generated value and
  dispatch-tag that should return an appropriately retagged value.

  Note that because the tags themselves comprise an open set,
  the tag key spec cannot enumerate the values, but can e.g.
  test for keyword?.

  Note also that the dispatch values of the multimethod will be
  included in the path, i.e. in reporting and gen overrides, even
  though those values are not evident in the spec.
"
  [mm retag]
  `(multi-spec-impl '~(res mm) (var ~mm) ~retag))

(defmacro keys
  "Creates and returns a map validating spec. :req and :opt are both
  vectors of namespaced-qualified keywords. The validator will ensure
  the :req keys are present. The :opt keys serve as documentation and
  may be used by the generator.

  The :req key vector supports 'and' and 'or' for key groups:

  (s/keys :req [::x ::y (or ::secret (and ::user ::pwd))] :opt [::z])

  There are also -un versions of :req and :opt. These allow
  you to connect unqualified keys to specs.  In each case, fully
  qualfied keywords are passed, which name the specs, but unqualified
  keys (with the same name component) are expected and checked at
  conform-time, and generated during gen:

  (s/keys :req-un [:my.ns/x :my.ns/y])

  The above says keys :x and :y are required, and will be validated
  and generated by specs (if they exist) named :my.ns/x :my.ns/y 
  respectively.

  In addition, the values of *all* namespace-qualified keys will be validated
  (and possibly destructured) by any registered specs. Note: there is
  no support for inline value specification, by design.

  Optionally takes :gen generator-fn, which must be a fn of no args that
  returns a test.check generator."
  [& {:keys [req req-un opt opt-un gen]}]
  (let [unk #(-> % name keyword)
        req-keys (filterv keyword? (flatten req))
        req-un-specs (filterv keyword? (flatten req-un))
        _ (assert (every? #(c/and (keyword? %) (namespace %)) (concat req-keys req-un-specs opt opt-un))
                  "all keys must be namespace-qualified keywords")
        req-specs (into req-keys req-un-specs)
        req-keys (into req-keys (map unk req-un-specs))
        opt-keys (into (vec opt) (map unk opt-un))
        opt-specs (into (vec opt) opt-un)
        parse-req (fn [rk f]
                    (map (fn [x]
                           (if (keyword? x)
                             `#(contains? % ~(f x))
                             (let [gx (gensym)]
                               `(fn* [~gx]
                                     ~(walk/postwalk
                                       (fn [y] (if (keyword? y) `(contains? ~gx ~(f y)) y))
                                       x)))))
                         rk))
        pred-exprs [`map?]
        pred-exprs (into pred-exprs (parse-req req identity))
        pred-exprs (into pred-exprs (parse-req req-un unk))
        pred-forms (walk/postwalk res pred-exprs)]
   ;; `(map-spec-impl ~req-keys '~req ~opt '~pred-forms ~pred-exprs ~gen)
    `(map-spec-impl {:req '~req :opt '~opt :req-un '~req-un :opt-un '~opt-un
                     :req-keys '~req-keys :req-specs '~req-specs
                     :opt-keys '~opt-keys :opt-specs '~opt-specs
                     :pred-forms '~pred-forms
                     :pred-exprs ~pred-exprs
                     :gfn ~gen})))

(defmacro or
  "Takes key+pred pairs, e.g.

  (s/or :even even? :small #(< % 42))

  Returns a destructuring spec that
  returns a vector containing the key of the first matching pred and the
  corresponding value."
  [& key-pred-forms]
  (let [pairs (partition 2 key-pred-forms)
        keys (mapv first pairs)
        pred-forms (mapv second pairs)
        pf (mapv res pred-forms)]
    (assert (c/and (even? (count key-pred-forms)) (every? keyword? keys)) "spec/or expects k1 p1 k2 p2..., where ks are keywords")
    `(or-spec-impl ~keys '~pf ~pred-forms nil)))

(defmacro and
  "Takes predicate/spec-forms, e.g.

  (s/and even? #(< % 42))

  Returns a spec that returns the conformed value. Successive
  conformed values propagate through rest of predicates."
  [& pred-forms]
  `(and-spec-impl '~(mapv res pred-forms) ~(vec pred-forms) nil))

(defmacro *
  "Returns a regex op that matches zero or more values matching
  pred. Produces a vector of matches iff there is at least one match"
  [pred-form]
  `(rep-impl '~(res pred-form) ~pred-form))

(defmacro +
  "Returns a regex op that matches one or more values matching
  pred. Produces a vector of matches"
  [pred-form]
  `(rep+impl '~(res pred-form) ~pred-form))

(defmacro ?
  "Returns a regex op that matches zero or one value matching
  pred. Produces a single value (not a collection) if matched."
  [pred-form]
  `(maybe-impl ~pred-form '~pred-form))

(defmacro alt
  "Takes key+pred pairs, e.g.

  (s/alt :even even? :small #(< % 42))

  Returns a regex op that returns a vector containing the key of the
  first matching pred and the corresponding value."
  [& key-pred-forms]
  (let [pairs (partition 2 key-pred-forms)
        keys (mapv first pairs)
        pred-forms (mapv second pairs)
        pf (mapv res pred-forms)]
    (assert (c/and (even? (count key-pred-forms)) (every? keyword? keys)) "alt expects k1 p1 k2 p2..., where ks are keywords")
    `(alt-impl ~keys ~pred-forms '~pf)))

(defmacro cat
  "Takes key+pred pairs, e.g.

  (s/cat :e even? :o odd?)

  Returns a regex op that matches (all) values in sequence, returning a map
  containing the keys of each pred and the corresponding value."
  [& key-pred-forms]
  (let [pairs (partition 2 key-pred-forms)
        keys (mapv first pairs)
        pred-forms (mapv second pairs)
        pf (mapv res pred-forms)]
    ;;(prn key-pred-forms)
    (assert (c/and (even? (count key-pred-forms)) (every? keyword? keys)) "cat expects k1 p1 k2 p2..., where ks are keywords")
    `(cat-impl ~keys ~pred-forms '~pf)))

(defmacro &
  "takes a regex op re, and predicates. Returns a regex-op that consumes
  input as per re but subjects the resulting value to the
  conjunction of the predicates, and any conforming they might perform."
  [re & preds]
  (let [pv (vec preds)]
    `(amp-impl ~re ~pv '~pv)))

(defmacro conformer
  "takes a predicate function with the semantics of conform i.e. it should return either a
  (possibly converted) value or :clojure.spec/invalid, and returns a
  spec that uses it as a predicate/conformer"
  [f]
  `(spec-impl '~f ~f nil true))

(defmacro fspec
  "takes :args :ret and (optional) :fn kwargs whose values are preds
  and returns a spec whose conform/explain take a fn and validates it
  using generative testing. The conformed value is always the fn itself.

  Optionally takes :gen generator-fn, which must be a fn of no args
  that returns a test.check generator."
  [& {:keys [args ret fn gen]}]
  `(fspec-impl ~args '~(res args) ~ret '~(res ret) ~fn '~(res fn) ~gen))

(defmacro tuple
  "takes one or more preds and returns a spec for a tuple, a vector
  where each element conforms to the corresponding pred. Each element
  will be referred to in paths using its ordinal."
  [& preds]
  (assert (not (empty? preds)))
  `(tuple-impl '~(mapv res preds) ~(vec preds)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; instrument ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- ns-qualify
  "Qualify symbol s by resolving it or using the current *ns*."
  [s]
  (if-let [resolved (resolve s)]
    (->sym resolved)
    (if (namespace s)
      s
      (symbol (str (.name *ns*)) (str s)))))

(defn- fn-spec-sym
  [sym role]
  (symbol (str (ns-qualify sym) "$" (name role))))

(def ^:private fn-spec-roles [:args :ret :fn])

(defn- expect
  "Returns nil if v conforms to spec, else throws ex-info with explain-data."
  [spec v]
  )

(defn- fn-specs?
  "Fn-specs must include at least :args or :ret specs."
  [m]
  (c/or (:args m) (:ret m)))

(defn fn-specs
  "Returns :args/:ret/:fn map of specs for var or symbol v."
  [v]
  (let [s (->sym v)
        reg (registry)]
    (reduce
     (fn [m role]
       (assoc m role (get reg (fn-spec-sym s role))))
     {}
     fn-spec-roles)))

(defmacro with-instrument-disabled
  "Disables instrument's checking of calls, within a scope."
  [& body]
  `(binding [*instrument-enabled* nil]
     ~@body))

(defn- spec-checking-fn
  [v f]
  (let [conform! (fn [v role spec data args]
                   (let [conformed (conform spec data)]
                     (if (= ::invalid conformed)
                       (let [ed (assoc (explain-data* spec [role] [] [] data)
                                  ::args args)]
                         (throw (ex-info
                                 (str "Call to " v " did not conform to spec:\n" (with-out-str (explain-out ed)))
                                 ed)))
                       conformed)))]
    (c/fn
     [& args]
     (if *instrument-enabled*
       (with-instrument-disabled
         (let [specs (fn-specs v)]
           (let [cargs (when (:args specs) (conform! v :args (:args specs) args args))
                 ret (binding [*instrument-enabled* true]
                       (.applyTo ^clojure.lang.IFn f args))
                 cret (when (:ret specs) (conform! v :ret (:ret specs) ret args))]
             (when (c/and (:args specs) (:ret specs) (:fn specs))
               (conform! v :fn (:fn specs) {:args cargs :ret cret} args))
             ret)))
       (.applyTo ^clojure.lang.IFn f args)))))

(defn- macroexpand-check
  [v args]
  (let [specs (fn-specs v)]
    (when-let [arg-spec (:args specs)]
      (when (= ::invalid (conform arg-spec args))
        (let [ed (assoc (explain-data* arg-spec [:args]
                                       (if-let [name (spec-name arg-spec)] [name] []) [] args)
                   ::args args)]
          (throw (IllegalArgumentException.
                   (str
                     "Call to " (->sym v) " did not conform to spec:\n"
                     (with-out-str (explain-out ed))))))))))

(defmacro fdef
  "Takes a symbol naming a function, and one or more of the following:

  :args A regex spec for the function arguments as they were a list to be
    passed to apply - in this way, a single spec can handle functions with
    multiple arities
  :ret A spec for the function's return value
  :fn A spec of the relationship between args and ret - the
    value passed is {:args conformed-args :ret conformed-ret} and is
    expected to contain predicates that relate those values

  Qualifies fn-sym with resolve, or using *ns* if no resolution found.
  Registers specs in the global registry, where they can be retrieved
  by calling fn-specs.

  Once registered, function specs are included in doc, checked by
  instrument, tested by the runner clojure.spec.test/run-tests, and (if
  a macro) used to explain errors during macroexpansion.

  Note that :fn specs require the presence of :args and :ret specs to
  conform values, and so :fn specs will be ignored if :args or :ret
  are missing.

  Returns the qualified fn-sym.

  For example, to register function specs for the symbol function:

  (s/fdef clojure.core/symbol
    :args (s/alt :separate (s/cat :ns string? :n string?)
                 :str string?
                 :sym symbol?)
    :ret symbol?)"
  [fn-sym & {:keys [args ret fn] :as m}]
  (let [qn (ns-qualify fn-sym)]
    `(do ~@(reduce
            (c/fn [defns role]
                  (if (contains? m role)
                    (let [s (fn-spec-sym qn (name role))]
                      (conj defns `(clojure.spec/def '~s ~(get m role))))
                    defns))
            [] [:args :ret :fn])
         '~qn)))

(defn- no-fn-specs
  [v specs]
  (ex-info (str "Fn at " v " is not spec'ed.")
           {:var v :specs specs}))

(def ^:private instrumented-vars
     "Map for instrumented vars to :raw/:wrapped fns"
  (atom {}))

(defn- ->var
  [s-or-v]
  (if (var? s-or-v)
    s-or-v
    (let [v (c/and (symbol? s-or-v) (resolve s-or-v))]
      (if (var? v)
        v
        (throw (IllegalArgumentException. (str (pr-str s-or-v) " does not name a var")))))))

(defn instrument
  "Instruments the var at v, a var or symbol, to check specs
registered with fdef. Wraps the fn at v to check :args/:ret/:fn
specs, if they exist, throwing an ex-info with explain-data if a
check fails. Idempotent."
  [v]
  (let [v (->var v)
        specs (fn-specs v)]
    (if (fn-specs? specs)
      (locking instrumented-vars
        (let [{:keys [raw wrapped]} (get @instrumented-vars v)
              current @v]
          (when-not (= wrapped current)
            (let [checked (spec-checking-fn v current)]
              (alter-var-root v (constantly checked))
              (swap! instrumented-vars assoc v {:raw current :wrapped checked}))))
        v)
      (throw (no-fn-specs v specs)))))

(defn unstrument
  "Undoes instrument on the var at v, a var or symbol. Idempotent."
  [v]
  (let [v (->var v)]
    (locking instrumented-vars
      (when-let [{:keys [raw wrapped]} (get @instrumented-vars v)]
        (let [current @v]
          (when (= wrapped current)
            (alter-var-root v (constantly raw))))
        (swap! instrumented-vars dissoc v))
      v)))

(defn speced-vars
  "Returns the set of vars whose namespace is in ns-syms AND
whose vars have been speced with fdef. If no ns-syms are
specified, return speced vars from all namespaces."
  [& ns-syms]
  (let [ns-match? (if (seq ns-syms)
                    (set (map str ns-syms))
                    (constantly true))]
    (reduce-kv
     (fn [s k _]
       (if (c/and (symbol? k)
                  (re-find  #"\$(args|ret)$" (name k))
                  (ns-match? (namespace k)))
         (if-let [v (resolve (symbol (str/replace (str k) #"\$(args|ret)$" "")))]
           (conj s v)
           s)
         s))
     #{}
     (registry))))

(defn instrument-ns
  "Call instrument for all speced-vars in namespaces named
by ns-syms. Idempotent."
  [& ns-syms]
  (when (seq ns-syms)
    (locking instrumented-vars
      (doseq [v (apply speced-vars ns-syms)]
        (instrument v)))))

(defn unstrument-ns
  "Call unstrument for all speced-vars in namespaces named
by ns-syms. Idempotent."
  [& ns-syms]
  (when (seq ns-syms)
    (locking instrumented-vars
      (doseq [v (apply speced-vars ns-syms)]
        (unstrument v)))))

(defn instrument-all
  "Call instrument for all speced-vars. Idempotent."
  []
  (locking instrumented-vars
    (doseq [v (speced-vars)]
      (instrument v))))

(defn unstrument-all
  "Call unstrument for all speced-vars. Idempotent"
  []
  (locking instrumented-vars
    (doseq [v (speced-vars)]
      (unstrument v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; impl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- recur-limit? [rmap id path k]
  (c/and (> (get rmap id) (::recursion-limit rmap))
         (contains? (set path) k)))

(defn- inck [m k]
  (assoc m k (inc (c/or (get m k) 0))))

(defn- dt
  ([pred x form] (dt pred x form nil))
  ([pred x form cpred?]
     (if pred
       (if-let [spec (the-spec pred)]
         (conform spec x)
         (if (ifn? pred)
           (if cpred?
             (pred x)
             (if (pred x) x ::invalid))
           (throw (Exception. (str (pr-str form) " is not a fn, expected predicate fn")))))
       x)))

(defn valid?
  "Helper function that returns true when x is valid for spec."
  ([spec x]
     (not= ::invalid (dt spec x ::unknown)))
  ([spec x form]
     (not= ::invalid (dt spec x form))))

(defn- explain-1 [form pred path via in v]
  ;;(prn {:form form :pred pred :path path :in in :v v})
  (let [pred (maybe-spec pred)]
    (if (spec? pred)
      (explain* pred path (if-let [name (spec-name pred)] (conj via name) via) in v)
      {path {:pred (abbrev form) :val v :via via :in in}})))

(defn ^:skip-wiki map-spec-impl
  "Do not call this directly, use 'spec' with a map argument"
  [{:keys [req-un opt-un pred-exprs opt-keys req-specs req req-keys opt-specs pred-forms opt gfn]
    :as argm}]
  (let [keys-pred (apply every-pred pred-exprs)
        k->s (zipmap (concat req-keys opt-keys) (concat req-specs opt-specs))
        keys->specs #(c/or (k->s %) %)
        id (java.util.UUID/randomUUID)]
    (reify
     clojure.lang.IFn
     (invoke [this x] (valid? this x))
     Spec
     (conform* [_ m]
               (if (keys-pred m)
                 (let [reg (registry)]
                   (loop [ret m, [k & ks :as keys] (c/keys m)]
                     (if keys
                       (if (contains? reg (keys->specs k))
                         (let [v (get m k)
                               cv (conform (keys->specs k) v)]
                           (if (= cv ::invalid)
                             ::invalid
                             (recur (if (identical? cv v) ret (assoc ret k cv))
                                    ks)))
                         (recur ret ks))
                       ret)))
                 ::invalid))
     (explain* [_ path via in x]
               (if-not (map? x)
                 {path {:pred 'map? :val x :via via :in in}}
                 (let [reg (registry)]
                   (apply merge
                          (when-let [probs (->> (map (fn [pred form] (when-not (pred x) (abbrev form)))
                                                     pred-exprs pred-forms)
                                                (keep identity)
                                                seq)]
                            {path {:pred (vec probs) :val x :via via :in in}})
                          (map (fn [[k v]]
                                 (when-not (c/or (not (contains? reg (keys->specs k)))
                                                 (valid? (keys->specs k) v k))
                                   (explain-1 (keys->specs k) (keys->specs k) (conj path k) via (conj in k) v)))
                               (seq x))))))
     (gen* [_ overrides path rmap]
           (if gfn
             (gfn)
             (let [rmap (inck rmap id)
                   gen (fn [k s] (gensub s overrides (conj path k) rmap k))
                   ogen (fn [k s]
                          (when-not (recur-limit? rmap id path k)
                            [k (gen/delay (gensub s overrides (conj path k) rmap k))]))
                   req-gens (map gen req-keys req-specs)
                   opt-gens (remove nil? (map ogen opt-keys opt-specs))]
               (when (every? identity (concat req-gens opt-gens))
                 (let [reqs (zipmap req-keys req-gens)
                       opts (into {} opt-gens)]
                   (gen/bind (gen/choose 0 (count opts))
                             #(let [args (concat (seq reqs) (when (seq opts) (shuffle (seq opts))))]
                                (->> args
                                     (take (c/+ % (count reqs)))
                                     (apply concat)
                                     (apply gen/hash-map)))))))))
     (with-gen* [_ gfn] (map-spec-impl (assoc argm :gfn gfn)))
     (describe* [_] (cons `keys
                          (cond-> []
                                  req (conj :req req)
                                  opt (conj :opt opt)
                                  req-un (conj :req-un req-un)
                                  opt-un (conj :opt-un opt-un)))))))




(defn ^:skip-wiki spec-impl
  "Do not call this directly, use 'spec'"
  [form pred gfn cpred?]
     (cond
      (spec? pred) (cond-> pred gfn (with-gen gfn))
      (regex? pred) (regex-spec-impl pred gfn)
      (named? pred) (cond-> (the-spec pred) gfn (with-gen gfn))
      :else
      (reify
       clojure.lang.IFn
       (invoke [this x] (valid? this x))
       Spec
       (conform* [_ x] (dt pred x form cpred?))
       (explain* [_ path via in x]
                 (when (= ::invalid (dt pred x form cpred?))
                   {path {:pred (abbrev form) :val x :via via :in in}}))
    (gen* [_ _ _ _] (if gfn
                         (gfn)
                         (gen/gen-for-pred pred)))
    (with-gen* [_ gfn] (spec-impl form pred gfn cpred?))
    (describe* [_] form))))

(defn ^:skip-wiki multi-spec-impl
  "Do not call this directly, use 'multi-spec'"
  ([form mmvar retag] (multi-spec-impl form mmvar retag nil))
  ([form mmvar retag gfn]
     (assert (when-let [dm (-> (methods @mmvar) ::invalid)]
               (nil? (dm nil)))
             (str "Multimethod :" form " does not contain nil-returning default method for :clojure.spec/invalid" ))
     (let [id (java.util.UUID/randomUUID)
           predx #(@mmvar %)
           dval #((.dispatchFn ^clojure.lang.MultiFn @mmvar) %)
           tag (if (keyword? retag)
                 #(assoc %1 retag %2)
                 retag)]
       (reify
        clojure.lang.IFn
        (invoke [this x] (valid? this x))
        Spec
        (conform* [_ x] (if-let [pred (predx x)]
                          (dt pred x form)
                          ::invalid))
        (explain* [_ path via in x]
                  (let [dv (dval x)
                        path (conj path dv)]
                    (if-let [pred (predx x)]
                      (explain-1 form pred path via in x)
                      {path {:pred form :val x :reason "no method" :via via :in in}})))
         (gen* [_ overrides path rmap]
              (if gfn
                (gfn)
                (let [gen (fn [[k f]]
                            (let [p (f nil)]
                              (let [rmap (inck rmap id)]
                                (when-not (recur-limit? rmap id path k)
                                  (gen/delay
                                   (gen/fmap
                                    #(tag % k)
                                    (gensub p overrides (conj path k) rmap (list 'method form k))))))))
                      gs (->> (methods @mmvar)
                              (remove (fn [[k]] (= k ::invalid)))
                              (map gen)
                              (remove nil?))]
                  (when (every? identity gs)
                    (gen/one-of gs)))))
         (with-gen* [_ gfn] (multi-spec-impl form mmvar retag gfn))
        (describe* [_] `(multi-spec ~form))))))

(defn ^:skip-wiki tuple-impl
  "Do not call this directly, use 'tuple'"
  ([forms preds] (tuple-impl forms preds nil))
  ([forms preds gfn]
     (reify
      clojure.lang.IFn
      (invoke [this x] (valid? this x))
      Spec
      (conform* [_ x]
                (if-not (c/and (vector? x)
                               (= (count x) (count preds)))
                  ::invalid
                  (loop [ret x, i 0]
                    (if (= i (count x))
                      ret
                      (let [v (x i)
                            cv (dt (preds i) v (forms i))]
                        (if (= ::invalid cv)
                          ::invalid
                          (recur (if (identical? cv v) ret (assoc ret i cv))
                                 (inc i))))))))
      (explain* [_ path via in x]
                (cond
                 (not (vector? x))
                 {path {:pred 'vector? :val x :via via :in in}}

                 (not= (count x) (count preds))
                 {path {:pred `(= (count ~'%) ~(count preds)) :val x :via via :in in}}

                 :else
                 (apply merge
                        (map (fn [i form pred]
                               (let [v (x i)]
                                 (when-not (valid? pred v)
                                   (explain-1 form pred (conj path i) via (conj in i) v))))
                             (range (count preds)) forms preds))))
      (gen* [_ overrides path rmap]
            (if gfn
              (gfn)
              (let [gen (fn [i p f]
                          (gensub p overrides (conj path i) rmap f))
                    gs (map gen (range (count preds)) preds forms)]
                (when (every? identity gs)
                  (apply gen/tuple gs)))))
      (with-gen* [_ gfn] (tuple-impl forms preds gfn))
      (describe* [_] `(tuple ~@forms)))))


(defn ^:skip-wiki or-spec-impl
  "Do not call this directly, use 'or'"
  [keys forms preds gfn]
     (let [id (java.util.UUID/randomUUID)
           cform (fn [x]
                   (loop [i 0]
                     (if (< i (count preds))
                       (let [pred (preds i)]
                         (let [ret (dt pred x (nth forms i))]
                           (if (= ::invalid ret)
                             (recur (inc i))
                             [(keys i) ret])))
                       ::invalid)))]
       (reify
        clojure.lang.IFn
        (invoke [this x] (valid? this x))
        Spec
        (conform* [_ x] (cform x))
        (explain* [this path via in x]
                  (when-not (valid? this x)
                    (apply merge
                           (map (fn [k form pred]
                                  (when-not (valid? pred x)
                                    (explain-1 form pred (conj path k) via in x)))
                                keys forms preds))))
     (gen* [_ overrides path rmap]
              (if gfn
                (gfn)
                (let [gen (fn [k p f]
                            (let [rmap (inck rmap id)]
                              (when-not (recur-limit? rmap id path k)
                                (gen/delay
                                 (gensub p overrides (conj path k) rmap f)))))
                      gs (remove nil? (map gen keys preds forms))]
                  (when-not (empty? gs)
                    (gen/one-of gs)))))
     (with-gen* [_ gfn] (or-spec-impl keys forms preds gfn))
     (describe* [_] `(or ~@(mapcat vector keys forms))))))

(defn- and-preds [x preds forms]
  (loop [ret x
         [pred & preds] preds
         [form & forms] forms]
    (if pred
      (let [nret (dt pred ret form)]
        (if (= ::invalid nret)
          ::invalid
          ;;propagate conformed values
          (recur nret preds forms)))
      ret)))

(defn- explain-pred-list
  [forms preds path via in x]
  (loop [ret x
         [form & forms] forms
         [pred & preds] preds]
    (when pred
      (let [nret (dt pred ret form)]
        (if (not= ::invalid nret)
          (recur nret forms preds)
          (explain-1 form pred path via in ret))))))

(defn ^:skip-wiki and-spec-impl
  "Do not call this directly, use 'and'"
  [forms preds gfn]
     (reify
      clojure.lang.IFn
      (invoke [this x] (valid? this x))
      Spec
      (conform* [_ x] (and-preds x preds forms))
      (explain* [_ path via in x] (explain-pred-list forms preds path via in x))
   (gen* [_ overrides path rmap] (if gfn (gfn) (gensub (first preds) overrides path rmap (first forms))))
   (with-gen* [_ gfn] (and-spec-impl forms preds gfn))
   (describe* [_] `(and ~@forms))))

;;;;;;;;;;;;;;;;;;;;;;; regex ;;;;;;;;;;;;;;;;;;;
;;See:
;; http://matt.might.net/articles/implementation-of-regular-expression-matching-in-scheme-with-derivatives/
;; http://www.ccs.neu.edu/home/turon/re-deriv.pdf

;;ctors
(defn- accept [x] {::op ::accept :ret x})

(defn- accept? [{:keys [::op]}]
  (= ::accept op))

(defn- pcat* [{[p1 & pr :as ps] :ps,  [k1 & kr :as ks] :ks, [f1 & fr :as forms] :forms, ret :ret, rep+ :rep+}]
  (when (every? identity ps)
    (if (accept? p1)
      (let [rp (:ret p1)
            ret (conj ret (if ks {k1 rp} rp))]
        (if pr
          (pcat* {:ps pr :ks kr :forms fr :ret ret})
          (accept ret)))
      {::op ::pcat, :ps ps, :ret ret, :ks ks, :forms forms :rep+ rep+})))

(defn- pcat [& ps] (pcat* {:ps ps :ret []}))

(defn ^:skip-wiki cat-impl
  "Do not call this directly, use 'cat'"
  [ks ps forms]
  (pcat* {:ks ks, :ps ps, :forms forms, :ret {}}))

(defn- rep* [p1 p2 ret splice form]
  (when p1
    (let [r {::op ::rep, :p2 p2, :splice splice, :forms form :id (java.util.UUID/randomUUID)}]
      (if (accept? p1)
        (assoc r :p1 p2 :ret (conj ret (:ret p1)))
        (assoc r :p1 p1, :ret ret)))))

(defn ^:skip-wiki rep-impl
  "Do not call this directly, use '*'"
  [form p] (rep* p p [] false form))

(defn ^:skip-wiki rep+impl
  "Do not call this directly, use '+'"
  [form p]
  (pcat* {:ps [p (rep* p p [] true form)] :forms `[~form (* ~form)] :ret [] :rep+ form}))

(defn ^:skip-wiki amp-impl
  "Do not call this directly, use '&'"
  [re preds pred-forms]
  {::op ::amp :p1 re :ps preds :forms pred-forms})

(defn- filter-alt [ps ks forms f]
  (if (c/or ks forms)
    (let [pks (->> (map vector ps
                        (c/or (seq ks) (repeat nil))
                        (c/or (seq forms) (repeat nil)))
                   (filter #(-> % first f)))]
      [(seq (map first pks)) (when ks (seq (map second pks))) (when forms (seq (map #(nth % 2) pks)))])
    [(seq (filter f ps)) ks forms]))

(defn- alt* [ps ks forms]
  (let [[[p1 & pr :as ps] [k1 :as ks] forms] (filter-alt ps ks forms identity)]
    (when ps
      (let [ret {::op ::alt, :ps ps, :ks ks :forms forms}]
        (if (nil? pr) 
          (if k1
            (if (accept? p1)
              (accept [k1 (:ret p1)])
              ret)
            p1)
          ret)))))

(defn- alts [& ps] (alt* ps nil nil))
(defn- alt2 [p1 p2] (if (c/and p1 p2) (alts p1 p2) (c/or p1 p2)))

(defn ^:skip-wiki alt-impl
  "Do not call this directly, use 'alt'"
  [ks ps forms] (assoc (alt* ps ks forms) :id (java.util.UUID/randomUUID)))

(defn ^:skip-wiki maybe-impl
  "Do not call this directly, use '?'"
  [p form] (alt* [p (accept ::nil)] nil [form ::nil]))

(defn- noret? [p1 pret]
  (c/or (= pret ::nil)
        (c/and (#{::rep ::pcat} (::op (reg-resolve p1))) ;;hrm, shouldn't know these
               (empty? pret))
        nil))

(declare preturn)

(defn- accept-nil? [p]
  (let [{:keys [::op ps p1 p2 forms] :as p} (reg-resolve p)]
    (case op
          ::accept true
          nil nil
          ::amp (c/and (accept-nil? p1)
                       (c/or (noret? p1 (preturn p1))
                             (let [ret (-> (preturn p1) (and-preds ps (next forms)))]
                               (if (= ret ::invalid)
                                 nil
                                 ret))))
          ::rep (c/or (identical? p1 p2) (accept-nil? p1))
          ::pcat (every? accept-nil? ps)
          ::alt (c/some accept-nil? ps))))

(declare add-ret)

(defn- preturn [p]
  (let [{[p0 & pr :as ps] :ps, [k :as ks] :ks, :keys [::op p1 ret forms] :as p} (reg-resolve p)]
    (case op
          ::accept ret
          nil nil
          ::amp (let [pret (preturn p1)]
                  (if (noret? p1 pret)
                    ::nil
                    (and-preds pret ps forms)))
          ::rep (add-ret p1 ret k)
          ::pcat (add-ret p0 ret k)
          ::alt (let [[[p0] [k0]] (filter-alt ps ks forms accept-nil?)
                      r (if (nil? p0) ::nil (preturn p0))]
                  (if k0 [k0 r] r)))))

(defn- add-ret [p r k]
  (let [{:keys [::op ps splice] :as p} (reg-resolve p)
        prop #(let [ret (preturn p)]
                (if (empty? ret) r ((if splice into conj) r (if k {k ret} ret))))]
    (case op
          nil r
          (::alt ::accept ::amp)
          (let [ret (preturn p)]
            ;;(prn {:ret ret})
            (if (= ret ::nil) r (conj r (if k {k ret} ret))))

          (::rep ::pcat) (prop))))

(defn- deriv
  [p x]
  (let [{[p0 & pr :as ps] :ps, [k0 & kr :as ks] :ks, :keys [::op p1 p2 ret splice forms] :as p} (reg-resolve p)]
    (when p
      (case op
            ::accept nil
            nil (let [ret (dt p x p)]
                  (when-not (= ::invalid ret) (accept ret)))
            ::amp (when-let [p1 (deriv p1 x)]
                    (amp-impl p1 ps forms))
            ::pcat (alt2 (pcat* {:ps (cons (deriv p0 x) pr), :ks ks, :forms forms, :ret ret})
                         (when (accept-nil? p0) (deriv (pcat* {:ps pr, :ks kr, :forms (next forms), :ret (add-ret p0 ret k0)}) x)))
            ::alt (alt* (map #(deriv % x) ps) ks forms)
            ::rep (alt2 (rep* (deriv p1 x) p2 ret splice forms)
                        (when (accept-nil? p1) (deriv (rep* p2 p2 (add-ret p1 ret nil) splice forms) x)))))))

(defn- op-describe [p]  
  (let [{:keys [::op ps ks forms splice p1 rep+] :as p} (reg-resolve p)]
    ;;(prn {:op op :ks ks :forms forms :p p})
    (when p
      (case op
            ::accept nil
            nil p
            ::amp (list* 'clojure.spec/& (op-describe p1) forms)
            ::pcat (if rep+
                     (list `+ rep+)
                     (cons `cat (mapcat vector ks forms)))
            ::alt (cons `alt (mapcat vector ks forms))
            ::rep (list (if splice `+ `*) forms)))))

(defn- op-explain [form p path via in input]
  ;;(prn {:form form :p p :path path :input input})
  (let [[x :as input] input
        via (if-let [name (spec-name p)] (conj via name) via)
        {:keys [::op ps ks forms splice p1 p2] :as p} (reg-resolve p)
        insufficient (fn [path form]
                       {path {:reason "Insufficient input"
                              :pred (abbrev form)
                              :val ()
                              :via via
                              :in in}})]
    (when p
      (case op
            ::accept nil
            nil (if (empty? input)
                  (insufficient path form)
                  (explain-1 form p path via in x))
            ::amp (if (empty? input)
                    (if (accept-nil? p1)
                      (explain-pred-list forms ps path via in (preturn p1))
                      (insufficient path (op-describe p1)))
                    (if-let [p1 (deriv p1 x)]
                      (explain-pred-list forms ps path via in (preturn p1))
                      (op-explain (op-describe p1) p1 path via in input)))
            ::pcat (let [[pred k form] (->> (map vector
                                                 ps
                                                 (c/or (seq ks) (repeat nil))
                                                 (c/or (seq forms) (repeat nil)))
                                            (remove (fn [[p]]
                                                      (accept-nil? p)))
                                            first)
                         path (if k (conj path k) path)
                         form (c/or form (op-describe pred))]
                     (if (c/and (empty? input) (not pred))
                       (insufficient path form)
                       (op-explain form pred path via in input)))
            ::alt (if (empty? input)
                    (insufficient path (op-describe p))
                    (apply merge
                           (map (fn [k form pred]
                                  (op-explain (c/or form (op-describe pred))
                                              pred
                                              (if k (conj path k) path)
                                              via
                                              in
                                              input))
                                (c/or (seq ks) (repeat nil))
                                (c/or (seq forms) (repeat nil))
                                ps)))
            ::rep (op-explain (if (identical? p1 p2)
                                forms
                                (op-describe p1))
                              p1 path via in input)))))

(defn- re-gen [p overrides path rmap f]
  ;;(prn {:op op :ks ks :forms forms})
  (let [{:keys [::op ps ks p1 p2 forms splice ret id] :as p} (reg-resolve p)
        rmap (if id (inck rmap id) rmap)
        ggens (fn [ps ks forms]
                (let [gen (fn [p k f]
                            ;;(prn {:k k :path path :rmap rmap :op op :id id})
                            (when-not (c/and rmap id k (recur-limit? rmap id path k))
                              (if id
                                (gen/delay (re-gen p overrides (if k (conj path k) path) rmap (c/or f p)))
                                (re-gen p overrides (if k (conj path k) path) rmap (c/or f p)))))]
                  (map gen ps (c/or (seq ks) (repeat nil)) (c/or (seq forms) (repeat nil)))))]
    (c/or (when-let [g (get overrides path)]
            (case op
                  (:accept nil) (gen/fmap vector g)
                  g))
          (when p
            (case op
                  ::accept (if (= ret ::nil)
                             (gen/return [])
                             (gen/return [ret]))
                  nil (when-let [g (gensub p overrides path rmap f)]
                        (gen/fmap vector g))
                  ::amp (re-gen p1 overrides path rmap (op-describe p1))
                  ::pcat (let [gens (ggens ps ks forms)]
                           (when (every? identity gens)
                             (apply gen/cat gens)))
                  ::alt (let [gens (remove nil? (ggens ps ks forms))]
                          (when-not (empty? gens)
                            (gen/one-of gens)))
                  ::rep (if (recur-limit? rmap id [id] id)
                          (gen/return [])
                          (when-let [g (re-gen p2 overrides path rmap forms)]
                            (gen/fmap #(apply concat %)
                                      (gen/vector g)))))))))

(defn- re-conform [p [x & xs :as data]]
  ;;(prn {:p p :x x :xs xs})
  (if (empty? data)
    (if (accept-nil? p)
      (let [ret (preturn p)]
        (if (= ret ::nil)
          nil
          ret))
      ::invalid)
    (if-let [dp (deriv p x)]
      (recur dp xs)
      ::invalid)))

(defn- re-explain [path via in re input]
  (loop [p re [x & xs :as data] input i 0]
    ;;(prn {:p p :x x :xs xs}) (prn)
    (if (empty? data)
      (if (accept-nil? p)
        nil ;;success
        (op-explain (op-describe p) p path via in nil))
      (if-let [dp (deriv p x)]
        (recur dp xs (inc i))
        (if (accept? p)
          {path {:reason "Extra input"
                 :pred (abbrev (op-describe re))
                 :val data
                 :via via
                 :in (conj in i)}}
          (c/or (op-explain (op-describe p) p path via (conj in i) (seq data))
                {path {:reason "Extra input"
                       :pred (abbrev (op-describe p))
                       :val data
                       :via via
                       :in (conj in i)}}))))))

(defn ^:skip-wiki regex-spec-impl
  "Do not call this directly, use 'spec' with a regex op argument"
  [re gfn]
  (reify
   clojure.lang.IFn
   (invoke [this x] (valid? this x))
   Spec
   (conform* [_ x]
             (if (c/or (nil? x) (coll? x))
               (re-conform re (seq x))
               ::invalid))
   (explain* [_ path via in x]
             (if (c/or (nil? x) (coll? x))
               (re-explain path via in re (seq x))
               {path {:pred (abbrev (op-describe re)) :val x :via via :in in}}))
   (gen* [_ overrides path rmap]
         (if gfn
           (gfn)
           (re-gen re overrides path rmap (op-describe re))))
   (with-gen* [_ gfn] (regex-spec-impl re gfn))
   (describe* [_] (op-describe re))))

;;;;;;;;;;;;;;;;; HOFs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- call-valid?
  [f specs args]
  (let [cargs (conform (:args specs) args)]
    (when-not (= cargs ::invalid)
      (let [ret (apply f args)
            cret (conform (:ret specs) ret)]
        (c/and (not= cret ::invalid) 
               (if (:fn specs)
                 (valid? (:fn specs) {:args cargs :ret cret})
                 true))))))

(defn- validate-fn
  "returns f if valid, else smallest"
  [f specs iters]
  (let [g (gen (:args specs))
        prop (gen/for-all* [g] #(call-valid? f specs %))]
    (let [ret (gen/quick-check iters prop)]
      (if-let [[smallest] (-> ret :shrunk :smallest)]
        smallest
        f))))

(defn ^:skip-wiki fspec-impl
  "Do not call this directly, use 'fspec'"
  [argspec aform retspec rform fnspec fform gfn]
  (assert (c/and argspec retspec))
  (let [specs {:args argspec :ret retspec :fn fnspec}]
    (reify
     clojure.lang.IFn
     (invoke [this x] (valid? this x))
     Spec
     (conform* [_ f] (if (fn? f)
                       (if (identical? f (validate-fn f specs *fspec-iterations*)) f ::invalid)
                       ::invalid))
     (explain* [_ path via in f]
               (if (fn? f)
                 (let [args (validate-fn f specs 100)]
                   (if (identical? f args) ;;hrm, we might not be able to reproduce
                     nil
                     (let [ret (try (apply f args) (catch Throwable t t))]
                       (if (instance? Throwable ret)
                         ;;TODO add exception data
                         {path {:pred '(apply fn) :val args :reason (.getMessage ^Throwable ret) :via via :in in}}

                         (let [cret (dt retspec ret rform)]
                           (if (= ::invalid cret)
                             (explain-1 rform retspec (conj path :ret) via in ret)
                             (when fnspec
                               (let [cargs (conform argspec args)]
                                 (explain-1 fform fnspec (conj path :fn) via in {:args cargs :ret cret})))))))))
                 {path {:pred 'fn? :val f :via via :in in}}))
     (gen* [_ _ _ _] (if gfn
             (gfn)
             (when-not fnspec
               (gen/return
                (fn [& args]
                  (assert (valid? argspec args) (with-out-str (explain argspec args)))
                  (gen/generate (gen retspec)))))))
     (with-gen* [_ gfn] (fspec-impl argspec aform retspec rform fnspec fform gfn))
     (describe* [_] `(fspec ~aform ~rform ~fform)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; non-primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(clojure.spec/def ::any (spec (constantly true) :gen gen/any))
(clojure.spec/def ::kvs->map (conformer #(zipmap (map ::k %) (map ::v %))))

(defmacro keys*
  "takes the same arguments as spec/keys and returns a regex op that matches sequences of key/values,
  converts them into a map, and conforms that map with a corresponding
  spec/keys call:

  user=> (s/conform (s/keys :req-un [::a ::c]) {:a 1 :c 2})
  {:a 1, :c 2}
  user=> (s/conform (s/keys* :req-un [::a ::c]) [:a 1 :c 2])
  {:a 1, :c 2}

  the resulting regex op can be composed into a larger regex:

  user=> (s/conform (s/cat :i1 integer? :m (s/keys* :req-un [::a ::c]) :i2 integer?) [42 :a 1 :c 2 :d 4 99])
  {:i1 42, :m {:a 1, :c 2, :d 4}, :i2 99}"
  [& kspecs]
  `(clojure.spec/& (* (cat ::k keyword? ::v ::any)) ::kvs->map (keys ~@kspecs)))

(defmacro nilable
  "returns a spec that accepts nil and values satisfiying pred"
  [pred]
  `(and (or ::nil nil? ::pred ~pred) (conformer second)))

(defn exercise
  "generates a number (default 10) of values compatible with spec and maps conform over them,
  returning a sequence of [val conformed-val] tuples. Optionally takes
  a generator overrides map as per gen"
  ([spec] (exercise spec 10))
  ([spec n] (exercise spec n nil))
  ([spec n overrides]
     (map #(vector % (conform spec %)) (gen/sample (gen spec overrides) n))))

(defn coll-checker
  "returns a predicate function that checks *coll-check-limit* items in a collection with pred"
  [pred]
  (let [check? #(valid? pred %)]
    (fn [coll]
      (c/or (nil? coll)
            (c/and
             (coll? coll)
             (every? check? (take *coll-check-limit* coll)))))))

(defn coll-gen
  "returns a function of no args that returns a generator of
  collections of items conforming to pred, with the same shape as
  init-coll"
  [pred init-coll]
  (let [init (empty init-coll)]
    (fn []
      (gen/fmap
       #(if (vector? init) % (into init %))
       (gen/vector (gen pred))))))

(defmacro coll-of
  "Returns a spec for a collection of items satisfying pred. The generator will fill an empty init-coll."
  [pred init-coll]
  `(spec (coll-checker ~pred) :gen (coll-gen ~pred ~init-coll)))

(defmacro map-of
  "Returns a spec for a map whose keys satisfy kpred and vals satisfy vpred."
  [kpred vpred]
  `(and (coll-of (tuple ~kpred ~vpred) {}) map?))


