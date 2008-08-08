;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  lib
;;
;;  lib provides functions for loading and managing Clojure source code
;;  contained in Java resources. lib tracks what it loads at the
;;  granularity of namespaces and provides convenient functions for
;;  ensuring that namespace definitions have been loaded while avoiding
;;  duplicate loads.
;;
;;  Namespace names are symbols. Every namespace has an associated
;;  'namespace directory' whose classpath-relative path is derived from the
;;  namespace name by replacing any periods with slashes. Clojure
;;  namespaces map one-to-one with Java packages.
;;
;;  A 'lib' is a unit of Clojure code contained in a resource within
;;  classpath. A lib name is a symbol whose name is a concatenation of a
;;  namespace name, a period, and a namespace-relative name. A lib is
;;  contained in a resource whose classpath-relative path is derived from
;;  the lib name by replacing all periods with slashes and then appending
;;  ".clj".
;;
;;  To load a namespace definition, lib loads the 'root lib' for the
;;  namespace. The root lib's name is derived from the namespace name by
;;  repeating its "leaf" portion. For example, the root lib for the
;;  namespace 'clojure.contrib.def is the lib 'clojure.contrib.def.def
;;  contained in the resource <classpath>/clojure/contrib/def/def.clj .
;;
;;  The following code ensures that 'clojure.contrib.def and
;;  'clojure.contrib.sql are loaded:
;;
;;    (require '(clojure.contrib def sql))
;;
;;  In cases where a namespace is defined by more than one lib, all the
;;  libs should be contained within the hierarchy of directories under the
;;  namespace directory. The root lib must (drectly or indirectly) load the
;;  additional libs. For example, a hypothetical namespace named
;;  'clojure.contrib.math-funcs might be defined in multiple libs contained
;;  in resources like these:
;;
;;    <classpath>/clojure/contrib/math_funcs/math_funcs.clj
;;    <classpath>/clojure/contrib/math_funcs/trig.clj
;;    <classpath>/clojure/contrib/math_funcs/logarithms.clj
;;    <classpath>/clojure/contrib/math_funcs/bessel.clj
;;    <classpath>/clojure/contrib/math_funcs/inverses/trig.clj
;;    <classpath>/clojure/contrib/math_funcs/inverses/logarithms.clj
;;
;;  The following code ensures that 'clojure.contrib.math-funcs is loaded:
;;
;;    (require '(clojure.contrib math-funcs))
;;
;;  This is the portion of math_funcs.clj that loads the additional libs:
;;
;;    (clojure/in-ns 'clojure.contrib.math-funcs)
;;
;;    (clojure.contrib.lib/load-libs
;;     '(clojure.contrib.math-funcs
;;       trig logarithms bessel inverses.trig inverses.logarithms))
;;
;;  lib also provides general functions for finding and loading resources
;;  using the class loaders visible to the Clojure runtime.
;;
;;  Here is a brief overview of what's in lib. For detailed docs please see
;;  the doc strings for the individual functions.
;;
;;  Resources
;;
;;    Function: find-resource
;;    Searches available class loaders for a resource, returns URL or nil
;;
;;    Function: load-resource
;;    Loads Clojure source from an absolute path: URI, URL or String
;;
;;  Raw Libs
;;
;;    Function: load-libs
;;    Loads libs by name from arbitrary locations under classpath
;;
;;  Namespaces
;;
;;    Function: load-namespaces
;;    Loads namespace definitions by loading namespace root libs
;;
;;    Function: namespaces
;;    Returns a sorted set of symbols naming the set of namespaces
;;      that lib has loaded and is tracking
;;
;;    Function: require
;;    Loads namespace definitions that are not already loaded
;;
;;    Function: use
;;    Requires namespaces and refers to them using clojure/refer
;;
;;  Examples
;;
;;    (load-namespaces :require '(clojure.contrib sql sql.tests))
;;    (require '(clojure.contrib sql sql.tests))
;;
;;    (load-namespaces :require :use '(clojure.contrib sql ns-utils) :verbose)
;;    (use '(clojure.contrib sql ns-utils) :verbose)
;;
;;    (use :reload-all :verbose
;;      '(clojure.contrib
;;        (sql :exclude (get-connection)
;;             :rename {execute-commands do-commands})
;;        ns-utils))
;;
;;  scgilardi (gmail)
;;  Created 7 April 2008
;;
;;  Thanks to Stuart Sierra for providing many useful ideas, discussions
;;  and code contributions for lib.clj.

(clojure/in-ns 'clojure.contrib.lib)
(clojure/refer 'clojure)

(alias 'set 'clojure.set)

(import '(clojure.lang RT))
(import '(java.io BufferedReader InputStreamReader))
(import '(java.net URI URL))

;; Private

(defmacro init-once
  "Initializes a var exactly once. The var must already exist."
  {:private true}
  [var init]
  `(let [v# (resolve '~var)]
     (when-not (.isBound v#)
       (.bindRoot v# ~init))))

(def
 #^{:private true :doc
    "A ref to a sorted set of symbols representing loaded namespaces"}
 *namespaces*)
(init-once *namespaces* (ref (sorted-set)))

(def
 #^{:private true :doc
    "True while a verbose load is pending"}
 *verbose*)
(init-once *verbose* false)

(def
 #^{:private true :doc
    "A list of keywords that clojure/refer recognizes as filters"}
 *filter-keys*)
(init-once *filter-keys* '(:exclude :only :rename))

(def
 #^{:private true :doc
    "A vector of the available class loaders ordered by the degree to which
  they are controlled by Clojure. The root loader's classpath can be
  extended with clojure/add-classpath"}
 *class-loaders*
 (let [root (.ROOT_CLASSLOADER RT)
       runtime (.getClassLoader (identity RT))
       system (.getSystemClassLoader ClassLoader)]
   (if (= system runtime)
     [root system]
     [root runtime system])))

(defn- format
  "Formats a string using String/format"
  [fmt & args]
  (String/format fmt (to-array args)))

(defn- printf
  "Prints formatted output"
  [fmt & args]
  (print (apply format fmt args)))

(defn- throw-if
  "Throws an exception with a message if pred is true. See
  java.util.Formatter for format string syntax."
  [pred fmt & args]
  (when pred (throw (Exception. (apply format fmt args)))))

(def find-resource)                     ; forward declaration
(def load-resource)                     ; forward declaration

(defn- load-one
  "Loads one lib from a resoure and ensures that namespace ns (if
  not nil) exists. If require is true, also records the load so
  a duplicate load will be skipped."
  [sym url require need-ns]
  (load-resource url)
  (throw-if (and need-ns (not (find-ns sym)))
            "namespace '%s' not found after loading '%s'" sym url)
  (when require
    (dosync
     (commute *namespaces* conj sym))))

(defn- load-all
  "Loads a lib from a resource and forces a load of any libs which it
  directly or indirectly loads via require/use/load-namespaces"
  [sym url require need-ns]
  (dosync
   (commute *namespaces* set/union
            (binding [*namespaces* (ref (sorted-set))]
              (load-one sym url require need-ns)
              @*namespaces*))))

(defn- name-path
  "Returns a classpath-relative path given the name of a symbol"
  [name]
  (.. name
      (replace \- \_)
      (replace \. \/)))

(defn- lib-path
  "Returns the resource path for a lib"
  [lib-sym]
  (str (name-path (name lib-sym)) ".clj"))

(defn- root-lib-path
  "Returns the resource path for a namespace root lib"
  [ns-sym]
  (let [n (name-path (name ns-sym))
        i (inc (.lastIndexOf n (int \/)))
        leaf (.substring n i)]
    (str n \/ leaf ".clj")))

(defn- load-lib
  "Loads a lib with options: sequential keywords and arguments."
  [prefix sym & options]
  (let [sym (symbol (str prefix (when prefix \.) sym))
        opts (apply hash-map options)
        as (:as opts)
        raw (:raw opts)
        reload (:reload opts)
        reload-all (:reload-all opts)
        require (:require opts)
        use (:use opts)
        verbose (:verbose opts)
        loaded (contains? @*namespaces* sym)
        load (cond reload-all
                   load-all
                   (or raw reload (not require) (not loaded))
                   load-one)
        need-ns (or as use)
        path ((if raw lib-path root-lib-path) sym)
        url (find-resource path)
        filter-opts (select-keys opts *filter-keys*)]
    (binding [*verbose* (or *verbose* verbose)]
      (when load
        (throw-if (not url) "'%s' not found in classpath" path)
        (when *verbose*
          (printf "(clojure.contrib.lib/load-resource \"%s\")\n" url)
          (flush))
        (load sym url require need-ns))
      (throw-if (and need-ns (not (find-ns sym)))
                "namespace '%s' not found" sym)
      (when as
        (alias as sym))
      (when use
        (when *verbose*
          (printf "(clojure/in-ns '%s)\n" (ns-name *ns*))
          (printf "(clojure/refer '%s" sym)
          (doseq opt filter-opts
            (printf " %s '%s" (key opt) (print-str (val opt))))
          (printf ")\n"))
        (apply refer sym (mapcat seq filter-opts))))))

;; Resources

(defn find-resource
  "Searches for a resource given a path relative to classpath using
  available ClassLoaders. Returns a URL if the resource is found or nil."
  [rel-path]
  (some #(.findResource % rel-path) *class-loaders*))

(defn load-resource
  "Loads Clojure source from a resource specified by an absolute path. The
  path may be a URI, URL, or String. Accepts any URI scheme supported by
  URLConnection (http and jar), plus file paths."
  [abs-path]
  (let [url (cond (instance? URL abs-path)
                  abs-path
                  (instance? URI abs-path)
                  (.toURL abs-path)
                  (string? abs-path)
                  (URL. abs-path))]
    (throw-if (not url) "Cannot coerce %s to %s" (class abs-path) URL)
    (with-open reader
        (BufferedReader.
         (InputStreamReader.
          (.openStream url)))
      (.load Compiler reader (.getPath url) (.getFile url)))))

;; Raw Libs

(defn load-libs
  "Searches classpath for libs and loads them. Each argument is either a
  libgroupspec that identifies a group of libs to load or a flag that
  modifies how all the identified libs are loaded. Symbols identify paths
  within classpath. Symbol names map to paths by replacing periods with
  slashes.

  A libgroupspec is a list containing a symbol that identifies a prefix
  directory followed by libspecs that identify libs relative to that
  directory.

  A libspec is a symbol.

  A flag is a keyword.
  Recognized flags: :reload-all, :verbose

  :reload-all forces loading of all namespaces that the identified libs
    directly or indirectly load via load-namespaces/require/use
  :verbose triggers printing information about each load and refer"
  [& args]
  (let [libargs (filter (complement keyword?) args)
        flags (filter keyword? args)
        flag-opts (interleave flags (repeat true))]
    (doseq libarg libargs
      (if (symbol? libarg)
        (apply load-lib nil libarg :raw true flag-opts)
        (let [[prefix & libspecs] libarg]
          (throw-if (not prefix) "prefix cannot be nil")
          (doseq libspec libspecs
            (apply load-lib prefix libspec :raw true flag-opts)))))))

;; Namespaces

(defn load-namespaces
  "Searches classpath for namespace definitions and loads them. Each
  argument is either an nsgroupspec that identifies a group of namespaces
  to load or a flag that modifies how all the identified namespaces are
  loaded. Symbols identify paths within classpath. Symbol names map to
  paths by replacing periods with slashes.

  An nsgroupspec is a list containing a symbol that identifies a prefix
  directory followed by nsspecs that identify namespace directories
  relative to that prefix and loading options.

  An nsspec is a symbol or a list containing a symbol and options. The
  recognized options are only effective when the :use flag is set.

  An option is a keyword followed by an argument.
  Recognized options: :exclude, :only, :rename

  The arguments and semantics for :exclude, :only, and :rename are those
  documented for clojure/refer.

  A flag is a keyword.
  Recognized flags: :require, :use, :reload, :reload-all, :verbose

  :require indicates that any identified namespace definitions that are
    already loaded need not be reloaded
  :use triggers referring to each namespace with its options as filters
  :reload forces loading of all the identified namespace definitions even
    if they are already loaded. :reload supersedes :require
  :reload-all implies :reload and also forces loading of all namespace
    definitions that the identified namespace definitions directly or
    indirectly load via load-namespaces/require/use
  :verbose triggers printing information about each load and refer"
  [& args]
  (let [nsargs (filter (complement keyword?) args)
        flags (filter keyword? args)
        flag-opts (interleave flags (repeat true))]
    (doseq nsarg nsargs
      (cond (symbol? nsarg)
            (apply load-lib nil nsarg flag-opts)
            (nil? (second nsarg))
            (apply load-lib nil (first nsarg) flag-opts)
            (keyword? (second nsarg))
            (apply load-lib nil (concat nsarg flag-opts))
            :else
            (let [[prefix & nsspecs] nsarg]
              (throw-if (not prefix) "prefix cannot be nil")
              (doseq nsspec nsspecs
                (if (symbol? nsspec)
                  (apply load-lib prefix nsspec flag-opts)
                  (apply load-lib prefix (concat nsspec flag-opts)))))))))

(defn namespaces
  "Returns a sorted set of symbols naming loaded namespaces"
  []
  @*namespaces*)

(defn require
  "Searches classpath for namespace definitions and loads them if they are
  not already loaded. Each argument is either an nsgroupspec that
  identifies a group of namespaces to load or a flag that modifies how all
  the identified namespaces are loaded. Symbols identify paths within
  classpath. Symbol names map to paths by replacing periods with slashes.

  An nsgroupspec is a list containing a symbol that identifies a prefix
  directory followed by nsspecs that identify namespace directories
  relative to that prefix and loading options.

  In the general case, an nsspec is a symbol or a list containing a symbol
  and options. In the case of require, no options are available so an
  nsspec should be a symbol. (see also load-namespaces and use)

  A flag is a keyword.
  Recognized flags: :reload, :reload-all, :verbose

  :reload forces loading of all the identified namespace definitions even
    if they are already loaded.
  :reload-all implies :reload and also forces loading of all namespace
    definitions that the identified namespace definitions directly or
    indirectly load via load-namespaces/require/use
  :verbose triggers printing information about each load and refer"
  [& args]
  (apply load-namespaces :require args))

(defn use
  "Searches classpath for namespace definitions, loads them if they are not
  already loaded, and refers to them. Each argument is either an
  nsgroupspec that identifies a group of namespaces to load or a flag that
  modifies how all the identified namespaces are loaded. Symbols identify
  paths within classpath. Symbol names map to paths by replacing periods
  with slashes.

  An nsgroupspec is a list containing a symbol that identifies a prefix
  directory followed by nsspecs that identify namespace directories
  relative to that prefix and loading options.

  An nsspec is a symbol or a list containing a symbol and options.

  An option is a keyword followed by an argument.
  Recognized options: :exclude, :only, :rename

  The arguments and semantics for :exclude, :only, and :rename are those
  documented for clojure/refer.

  A flag is a keyword.
  Recognized flags: :reload, :reload-all, :verbose

  :reload forces loading of all the identified namespace definitions even
    if they are already loaded. :reload supersedes :require
  :reload-all implies :reload and also forces loading of all namespace
    definitions that the identified namespace definitions directly or
    indirectly load via load-namespaces/require/use
  :verbose triggers printing information about each load and refer"
  [& args]
  (apply load-namespaces :require :use args))
