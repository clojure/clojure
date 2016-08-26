(ns ^{:skip-wiki true} clojure.core.specs
  (:require [clojure.spec :as s]))

;;;; destructure

(s/def ::local-name (s/and simple-symbol? #(not= '& %)))

(s/def ::binding-form
  (s/or :sym ::local-name
        :seq ::seq-binding-form
        :map ::map-binding-form))

;; sequential destructuring

(s/def ::seq-binding-form
  (s/cat :elems (s/* ::binding-form)
         :rest (s/? (s/cat :amp #{'&} :form ::binding-form))
         :as (s/? (s/cat :as #{:as} :sym ::local-name))))

;; map destructuring

(s/def ::keys (s/coll-of ident? :kind vector?))
(s/def ::syms (s/coll-of symbol? :kind vector?))
(s/def ::strs (s/coll-of simple-symbol? :kind vector?))
(s/def ::or (s/map-of simple-symbol? any?))
(s/def ::as ::local-name)

(s/def ::map-special-binding
  (s/keys :opt-un [::as ::or ::keys ::syms ::strs]))

(s/def ::map-binding (s/tuple ::binding-form any?))

(s/def ::ns-keys
  (s/tuple
    (s/and qualified-keyword? #(-> % name #{"keys" "syms"}))
    (s/coll-of simple-symbol? :kind vector?)))

(s/def ::map-bindings
  (s/every (s/or :mb ::map-binding
                 :nsk ::ns-keys
                 :msb (s/tuple #{:as :or :keys :syms :strs} any?)) :into {}))

(s/def ::map-binding-form (s/merge ::map-bindings ::map-special-binding))

;; bindings

(s/def ::binding (s/cat :binding ::binding-form :init-expr any?))
(s/def ::bindings (s/and vector? (s/* ::binding)))

;; let, if-let, when-let

(s/fdef clojure.core/let
  :args (s/cat :bindings ::bindings
               :body (s/* any?)))

(s/fdef clojure.core/if-let
  :args (s/cat :bindings (s/and vector? ::binding)
               :then any?
               :else (s/? any?)))

(s/fdef clojure.core/when-let
  :args (s/cat :bindings (s/and vector? ::binding)
               :body (s/* any?)))

;; defn, defn-, fn

(s/def ::arg-list
  (s/and
    vector?
    (s/cat :args (s/* ::binding-form)
           :varargs (s/? (s/cat :amp #{'&} :form ::binding-form)))))

(s/def ::args+body
  (s/cat :args ::arg-list
         :prepost (s/? map?)
         :body (s/* any?)))

(s/def ::defn-args
  (s/cat :name simple-symbol?
         :docstring (s/? string?)
         :meta (s/? map?)
         :bs (s/alt :arity-1 ::args+body
                    :arity-n (s/cat :bodies (s/+ (s/spec ::args+body))
                                    :attr (s/? map?)))))

(s/fdef clojure.core/defn
  :args ::defn-args
  :ret any?)

(s/fdef clojure.core/defn-
  :args ::defn-args
  :ret any?)

(s/fdef clojure.core/fn
  :args (s/cat :name (s/? simple-symbol?)
               :bs (s/alt :arity-1 ::args+body
                          :arity-n (s/+ (s/spec ::args+body))))
  :ret any?)

;;;; ns

(s/def ::exclude (s/coll-of simple-symbol?))
(s/def ::only (s/coll-of simple-symbol?))
(s/def ::rename (s/map-of simple-symbol? simple-symbol?))

(s/def ::ns-refer-clojure
  (s/spec (s/cat :clause #{:refer-clojure}
            :filters (s/keys* :opt-un [::exclude ::only ::rename]))))

(s/def ::refer (s/or :all #{:all}
                 :syms  (s/coll-of simple-symbol?)))

(s/def ::prefix-list
  (s/spec
    (s/cat :prefix simple-symbol?
      :suffix (s/* (s/alt :lib simple-symbol? :prefix-list ::prefix-list))
      :refer (s/keys* :opt-un [::as ::refer]))))

(s/def ::ns-require
  (s/spec (s/cat :clause #{:require}
            :libs (s/* (s/alt :lib simple-symbol?
                         :prefix-list ::prefix-list
                         :flag #{:reload :reload-all :verbose})))))

(s/def ::package-list
  (s/spec
    (s/cat :package simple-symbol?
      :classes (s/* simple-symbol?))))

(s/def ::ns-import
  (s/spec
    (s/cat :clause #{:import}
      :classes (s/* (s/alt :class simple-symbol?
                      :package-list ::package-list)))))

(s/def ::ns-refer
  (s/spec (s/cat :clause #{:refer}
            :lib simple-symbol?
            :filters (s/keys* :opt-un [::exclude ::only ::rename]))))

(s/def ::use-prefix-list
  (s/spec
    (s/cat :prefix simple-symbol?
      :suffix (s/* (s/alt :lib simple-symbol? :prefix-list ::use-prefix-list))
      :filters (s/keys* :opt-un [::exclude ::only ::rename]))))

(s/def ::ns-use
  (s/spec (s/cat :clause #{:use}
            :libs (s/* (s/alt :lib simple-symbol?
                         :prefix-list ::use-prefix-list
                         :flag #{:reload :reload-all :verbose})))))

(s/def ::ns-load
  (s/spec (s/cat :clause #{:load}
            :libs (s/* string?))))

(s/def ::name simple-symbol?)
(s/def ::extends simple-symbol?)
(s/def ::implements (s/coll-of simple-symbol? :kind vector?))
(s/def ::init symbol?)
(s/def ::class-ident (s/or :class simple-symbol? :class-name string?))
(s/def ::signature (s/coll-of ::class-ident :kind vector?))
(s/def ::constructors (s/map-of ::signature ::signature))
(s/def ::post-init symbol?)
(s/def ::method (s/and vector?
                  (s/cat :name simple-symbol?
                    :param-types ::signature
                    :return-type simple-symbol?)))
(s/def ::methods (s/coll-of ::method :kind vector?))
(s/def ::main boolean?)
(s/def ::factory simple-symbol?)
(s/def ::state simple-symbol?)
(s/def ::get simple-symbol?)
(s/def ::set simple-symbol?)
(s/def ::expose (s/keys :opt-un [::get ::set]))
(s/def ::exposes (s/map-of simple-symbol? ::expose))
(s/def ::prefix string?)
(s/def ::impl-ns simple-symbol?)
(s/def ::load-impl-ns boolean?)

(s/def ::ns-gen-class
  (s/spec (s/cat :clause #{:gen-class}
            :options (s/keys* :opt-un [::name ::extends ::implements
                                       ::init ::constructors ::post-init
                                       ::methods ::main ::factory ::state
                                       ::exposes ::prefix ::impl-ns ::load-impl-ns]))))

(s/def ::ns-clauses
  (s/* (s/alt :refer-clojure ::ns-refer-clojure
         :require ::ns-require
         :import ::ns-import
         :use ::ns-use
         :refer ::ns-refer
         :load ::ns-load
         :gen-class ::ns-gen-class)))

(s/fdef clojure.core/ns
  :args (s/cat :name simple-symbol?
          :docstring (s/? string?)
          :attr-map (s/? map?)
          :clauses ::ns-clauses)
  :ret any?)
