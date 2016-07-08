;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.spec.gen
    (:refer-clojure :exclude [boolean bytes cat hash-map list map not-empty set vector
                              char double int keyword symbol string uuid delay]))

(alias 'c 'clojure.core)

(defn- dynaload
  [s]
  (let [ns (namespace s)]
    (assert ns)
    (require (c/symbol ns))
    (let [v (resolve s)]
      (if v
        @v
        (throw (RuntimeException. (str "Var " s " is not on the classpath")))))))

(def ^:private quick-check-ref
     (c/delay (dynaload 'clojure.test.check/quick-check)))
(defn quick-check
  [& args]
  (apply @quick-check-ref args))

(def ^:private for-all*-ref
     (c/delay (dynaload 'clojure.test.check.properties/for-all*)))
(defn for-all*
  "Dynamically loaded clojure.test.check.properties/for-all*."
  [& args]
  (apply @for-all*-ref args))

(let [g? (c/delay (dynaload 'clojure.test.check.generators/generator?))
      g (c/delay (dynaload 'clojure.test.check.generators/generate))
      mkg (c/delay (dynaload 'clojure.test.check.generators/->Generator))]
  (defn- generator?
    [x]
    (@g? x))
  (defn- generator
    [gfn]
    (@mkg gfn))
  (defn generate
    "Generate a single value using generator."
    [generator]
    (@g generator)))

(defn ^:skip-wiki delay-impl
  [gfnd]
  ;;N.B. depends on test.check impl details
  (generator (fn [rnd size]
               ((:gen @gfnd) rnd size))))

(defmacro delay
  "given body that returns a generator, returns a
  generator that delegates to that, but delays
  creation until used."
  [& body]
  `(delay-impl (c/delay ~@body)))

(defn gen-for-name
  "Dynamically loads test.check generator named s."
  [s]
  (let [g (dynaload s)]
    (if (generator? g)
      g
      (throw (RuntimeException. (str "Var " s " is not a generator"))))))

(defmacro ^:skip-wiki lazy-combinator
  "Implementation macro, do not call directly."
  [s]
  (let [fqn (c/symbol "clojure.test.check.generators" (name s))
        doc (str "Lazy loaded version of " fqn)]
    `(let [g# (c/delay (dynaload '~fqn))]
       (defn ~s
         ~doc
         [& ~'args]
         (apply @g# ~'args)))))

(defmacro ^:skip-wiki lazy-combinators
  "Implementation macro, do not call directly."
  [& syms]
  `(do
     ~@(c/map
        (fn [s] (c/list 'lazy-combinator s))
        syms)))

(lazy-combinators hash-map list map not-empty set vector vector-distinct fmap elements
                  bind choose fmap one-of such-that tuple sample return
                  large-integer* double*)

(defmacro ^:skip-wiki lazy-prim
  "Implementation macro, do not call directly."
  [s]
  (let [fqn (c/symbol "clojure.test.check.generators" (name s))
        doc (str "Fn returning " fqn)]
    `(let [g# (c/delay (dynaload '~fqn))]
       (defn ~s
         ~doc
         [& ~'args]
         @g#))))

(defmacro ^:skip-wiki lazy-prims
  "Implementation macro, do not call directly."
  [& syms]
  `(do
     ~@(c/map
        (fn [s] (c/list 'lazy-prim s))
        syms)))

(lazy-prims any any-printable boolean bytes char char-alpha char-alphanumeric char-ascii double
            int keyword keyword-ns large-integer ratio simple-type simple-type-printable
            string string-ascii string-alphanumeric symbol symbol-ns uuid)

(defn cat
  "Returns a generator of a sequence catenated from results of
gens, each of which should generate something sequential."
  [& gens]
  (fmap #(apply concat %)
        (apply tuple gens)))

(defn- qualified? [ident] (not (nil? (namespace ident))))

(def ^:private
  gen-builtins
  (c/delay
   (let [simple (simple-type-printable)]
     {any? (one-of [(return nil) (any-printable)])
      some? (such-that some? (any-printable))
      number? (one-of [(large-integer) (double)])
      integer? (large-integer)
      int? (large-integer)
      pos-int? (large-integer* {:min 1})
      neg-int? (large-integer* {:max -1})
      nat-int? (large-integer* {:min 0})
      float? (double)
      double? (double)
      boolean? (boolean)
      string? (string-alphanumeric)
      ident? (one-of [(keyword-ns) (symbol-ns)])
      simple-ident? (one-of [(keyword) (symbol)])
      qualified-ident? (such-that qualified? (one-of [(keyword-ns) (symbol-ns)]))
      keyword? (keyword-ns)
      simple-keyword? (keyword)
      qualified-keyword? (such-that qualified? (keyword-ns))
      symbol? (symbol-ns)
      simple-symbol? (symbol)
      qualified-symbol? (such-that qualified? (symbol-ns))
      uuid? (uuid)
      uri? (fmap #(java.net.URI/create (str "http://" % ".com")) (uuid))
      bigdec? (fmap #(BigDecimal/valueOf %)
                    (double* {:infinite? false :NaN? false}))
      inst? (fmap #(java.util.Date. %)
                  (large-integer))
      seqable? (one-of [(return nil)
                        (list simple)
                        (vector simple)
                        (map simple simple)
                        (set simple)
                        (string-alphanumeric)])
      indexed? (vector simple)
      map? (map simple simple)
      vector? (vector simple)
      list? (list simple)
      seq? (list simple)
      char? (char)
      set? (set simple)
      nil? (return nil)
      false? (return false)
      true? (return true)
      zero? (return 0)
      rational? (one-of [(large-integer) (ratio)])
      coll? (one-of [(map simple simple)
                     (list simple)
                     (vector simple)
                     (set simple)])
      empty? (elements [nil '() [] {} #{}])
      associative? (one-of [(map simple simple) (vector simple)])
      sequential? (one-of [(list simple) (vector simple)])
      ratio? (such-that ratio? (ratio))
      bytes? (bytes)})))

(defn gen-for-pred
  "Given a predicate, returns a built-in generator if one exists."
  [pred]
  (if (set? pred)
    (elements pred)
    (get @gen-builtins pred)))

(comment
  (require :reload 'clojure.spec.gen)
  (in-ns 'clojure.spec.gen)

  ;; combinators, see call to lazy-combinators above for complete list
  (generate (one-of [(gen-for-pred integer?) (gen-for-pred string?)]))
  (generate (such-that #(< 10000 %) (gen-for-pred integer?)))
  (let [reqs {:a (gen-for-pred number?)
              :b (gen-for-pred ratio?)}
        opts {:c (gen-for-pred string?)}]
    (generate (bind (choose 0 (count opts))
                    #(let [args (concat (seq reqs) (shuffle (seq opts)))]
                       (->> args
                            (take (+ % (count reqs)))
                            (mapcat identity)
                            (apply hash-map))))))
  (generate (cat (list (gen-for-pred string?))
                 (list (gen-for-pred ratio?))))
  
  ;; load your own generator
  (gen-for-name 'clojure.test.check.generators/int)
  
  ;; failure modes
  (gen-for-name 'unqualified)
  (gen-for-name 'clojure.core/+)
  (gen-for-name 'clojure.core/name-does-not-exist)
  (gen-for-name 'ns.does.not.exist/f)
  
  )


