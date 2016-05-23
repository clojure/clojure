;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.spec.gen
    (:refer-clojure :exclude [boolean cat hash-map list map not-empty set vector
                              char double int keyword symbol string uuid]))

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
     (delay (dynaload 'clojure.test.check/quick-check)))
(defn quick-check
  [& args]
  (apply @quick-check-ref args))

(def ^:private for-all*-ref
     (delay (dynaload 'clojure.test.check.properties/for-all*)))
(defn for-all*
  "Dynamically loaded clojure.test.check.properties/for-all*."
  [& args]
  (apply @for-all*-ref args))

(let [g? (delay (dynaload 'clojure.test.check.generators/generator?))
      g (delay (dynaload 'clojure.test.check.generators/generate))]
  (defn- generator?
    [x]
    (@g? x))
  (defn generate
    "Generate a single value using generator."
    [generator]
    (@g generator)))

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
    `(let [g# (delay (dynaload '~fqn))]
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

(lazy-combinators hash-map list map not-empty set vector fmap elements
                  bind choose fmap one-of such-that tuple sample return)

(defmacro ^:skip-wiki lazy-prim
  "Implementation macro, do not call directly."
  [s]
  (let [fqn (c/symbol "clojure.test.check.generators" (name s))
        doc (str "Fn returning " fqn)]
    `(let [g# (delay (dynaload '~fqn))]
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

(lazy-prims any any-printable boolean char char-alpha char-alphanumeric char-ascii double
            int keyword keyword-ns large-integer ratio simple-type simple-type-printable
            string string-ascii string-alphanumeric symbol symbol-ns uuid)

(defn cat
  "Returns a generator of a sequence catenated from results of
gens, each of which should generate something sequential."
  [& gens]
  (fmap #(apply concat %)
        (apply tuple gens)))

(def ^:private
  gen-builtins
  (delay
   (let [simple (simple-type-printable)]
     {number? (one-of [(large-integer) (double)])
      integer? (large-integer)
      float? (double)
      string? (string-alphanumeric)
      keyword? (keyword-ns)
      symbol? (symbol-ns)
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
      ratio? (such-that ratio? (ratio))})))

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


