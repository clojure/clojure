;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  clojure.contrib.lib (Lib)
;;
;;  Lib provides functions for loading and managing Clojure source code
;;  contained in Java resources.
;;
;;  A 'lib' is a unit of Clojure source code contained in a Java resource
;;  and named by a symbol. The lib's name is used to identify the lib and
;;  to locate the resource that contains it.
;;
;;  Lib provides functions to:
;;
;;    - find a resource given a classpath-relative path
;;    - load code from a resource given an absolute path
;;    - load libs from resources given lib names
;;    - load namespace definitions given namespace specs
;;    - ensure namespace definitions have been loaded while avoiding
;;      duplicate loads, and
;;    - create a namespace and refer 'clojure into it succinctly
;;
;;  Symbols, Namespaces, Packages
;;
;;  Symbols in Clojure are two-part identifiers - with an optional
;;  namespace and a name, both strings. Namespaces are used to distinguish
;;  two symbols that have the same name. Vars are named by symbols that
;;  must have a namespace part, and thus can be considered to be in
;;  namespaces. [From Clojure documentation]
;;
;;  Packages in Java play a role similar to Clojure namespaces - they
;;  partition the global namespace to allow large programs to avoid name
;;  conflicts. Java defines a mapping from package names to directories
;;  within classpath: components of a package name separated by periods
;;  correspond to components of a classpath-relative path separated by
;;  slashes. Lib uses this same mapping to locate libs and namespaces in
;;  classpath.
;;
;;  Loading Libs
;;
;;  A lib's name provides the location of the resource that contains it in
;;  classpath. The resource name is the last component of the lib's path
;;  followed by ".clj". For example, a lib named 'a.b.c is contained in the
;;  resource "<classpath>/a/b/c.clj".
;;
;;  Loading Namespaces
;;
;;  To load a namespace definition, Lib loads the 'root lib' for the
;;  namespace. The root lib's name is derived from the namespace name by
;;  repeating its last component. For example, the root lib for the
;;  namespace 'x.y.z is the lib 'x.y.z.z contained in the resource
;;  "<classpath>/x/y/z/z.clj". The namespace definition may be entirely
;;  within the root lib or it may span multiple libs in the hierarchy of
;;  directories at and under the namespace's directory. In the latter case
;;  the root lib must include commands to (directly or indirectly) load the
;;  remaining libs.
;;
;;  Nsspecs
;;
;;  An nsspec specifies a namespace to load. It is either a namespace name
;;  or a list containing the namespace name and zero or more options
;;  expressed as squential keywords and values. Nsspec examples:
;;    - 'clojure.contrib.sql
;;    - '(clojure.contrib.sql)
;;    - '(clojure.contrib.sql :exclude (get-connection)).
;;    - '(clojure.contrib.sql :as sql)
;;
;;  Prefix Lists
;;
;;  It is common for Clojure code to depend on several libs or namespaces
;;  whose names have one or more initial components in common. When
;;  specifying lib or namespace names for Lib to use, prefix lists can be
;;  used to reduce repetition in the call. A prefix list is a list
;;  containing the shared prefix followed by lib or namespace names and/or
;;  prefix lists with the shared prefix elided. For example, the following
;;  are all equivalent:
;;    (load-libs 'a.b.c 'a.b.d 'a.c.e 'a.c.f) ; all names fully qualified
;;    (load-libs '(a.b c d) '(a.c e f)) ; 'a.b and 'a.c extracted
;;    (load-libs '(a (b c d) (c e f))) ; all common prefixes extracted
;;  Symbols containing explicit periods and the equivalent prefix lists may
;;  be mixed freely.
;;
;;  Lib Functions
;;
;;   Resources
;;
;;    Function: find-resource
;;    Searches available class loaders for a resource, returns URL or nil
;;
;;    Function: load-resource
;;    Loads Clojure source from an absolute path: URI, URL or String
;;
;;   Libs
;;
;;    Function: load-libs
;;    Loads libs by name from arbitrary locations within classpath
;;
;;   Namespaces
;;
;;    Function: load-namespaces
;;    Loads namespace definitions by loading namespace root libs
;;
;;    Function: namespaces
;;    Returns a sorted set symbols naming namespaces that have been loaded
;;      with the :require option - used to track and avoid duplicate loads.
;;
;;    Function: require
;;    Loads namespace definitions that have not yet been loaded
;;
;;    Function: use
;;    Requires namespaces and refers to them using clojure/refer
;;
;;   Examples
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
  (when pred
    (let [message (apply format fmt args)
          exception (Exception. message)
          raw-trace (.getStackTrace exception)
          boring? #(not= (.getMethodName %) "doInvoke")
          trace (into-array (drop 2 (drop-while boring? raw-trace)))]
      (.setStackTrace exception trace)
      (throw exception))))

(defn- nsspec?
  "Returns true if x is an nsspec"
  [x]
  (or (symbol? x)
      (nil? (second x))
      (keyword? (second x))))

(defn- prepend
  "Prepends a symbol or collection to coll"
  [x coll]
  (if (symbol? x)
    (cons x coll)
    (concat x coll)))

(def find-resource)                     ; forward declaration
(def load-resource)                     ; forward declaration

(defn- load-one
  "Loads one lib from a resource at url. If need-ns is 'true' ensures that
  the namespace named by sym exists after loading. If require is 'true'
  also records the namespace named by sym as loaded so any duplicate loads
  can be skipped."
  [url need-ns sym require]
  (load-resource url)
  (throw-if (and need-ns (not (find-ns sym)))
            "namespace '%s' not found after loading '%s'" sym url)
  (when require
    (dosync
     (commute *namespaces* conj sym))))

(defn- load-all
  "Loads a lib from a resource at url and forces a load of any namespaces
  it directly or indirectly loads via require/use/load-namespaces"
  [url need-ns sym require]
  (dosync
   (commute *namespaces* set/union
            (binding [*namespaces* (ref (sorted-set))]
              (load-one url need-ns sym require)
              @*namespaces*))))

(defn- name-path
  "Returns a classpath-relative path given a symbol name"
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
        reload (:reload opts)
        reload-all (:reload-all opts)
        require (:require opts)
        root (:root opts)
        use (:use opts)
        verbose (:verbose opts)
        loaded (contains? @*namespaces* sym)
        load (cond reload-all
                   load-all
                   (or reload (not require) (not loaded))
                   load-one)
        need-ns (or as use)
        path ((if root root-lib-path lib-path) sym)
        url (find-resource path)
        filter-opts (select-keys opts *filter-keys*)]
    (binding [*verbose* (or *verbose* verbose)]
      (when load
        (throw-if (not url) "'%s' not found in classpath" path)
        (when *verbose*
          (printf "(clojure.contrib.lib/load-resource \"%s\")\n" url)
          (flush))
        (load url need-ns sym require))
      (throw-if (and need-ns (not (find-ns sym)))
                "namespace '%s' not found" sym)
      (when (and need-ns *verbose*)
        (printf "(clojure/in-ns '%s)\n" (ns-name *ns*)))
      (when as
        (when *verbose*
          (printf "(clojure/alias '%s '%s)\n" as sym))
        (alias as sym))
      (when use
        (when *verbose*
          (printf "(clojure/refer '%s" sym)
          (doseq opt filter-opts
            (printf " %s '%s" (key opt) (print-str (val opt))))
          (printf ")\n"))
        (apply refer sym (mapcat seq filter-opts))))))

(defn- load-prefix-list
  "Loads libs and handles (nested) prefix lists"
  [load? prefix opts & args]
  (doseq arg args
    (if (load? arg)
      (apply load-lib prefix (prepend arg opts))
      (let [[nested-prefix & nested-args] arg]
        (throw-if (nil? nested-prefix) "prefix cannot be nil")
        (apply load-prefix-list
               load?
               (symbol (str prefix (when prefix \.) nested-prefix))
               opts
               nested-args)))))

;; Resources

(defn find-resource
  "Searches for a resource given a classpath-relative path using available
  ClassLoaders. If the resource is found, returns its URL, otherwise nil."
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

;; Libs

(defn load-libs
  "Loads libs - Clojure source contained in resources in classpath. Each
  argument is a either a symbol that identifies a lib, a prefix list that
  identifies multiple libs with names that share common prefixes, or a flag
  that modifies how all the identified libs are loaded. Symbol names map to
  paths within classpath: components of the symbol name separated by
  periods correspond to components in classpath-relative paths separated by
  slashes. The full resource path is the mapped path with '.clj'
  appended. For example, the resource containing the lib 'a.b.c is
  '<classpath>/a/b/c.clj'.


  Multiple libs whose names share the same period-delimited prefix can be
  identified succinctly using a prefix list: a list containing the shared
  prefix followed by symbols and/or prefix lists with the shared prefix
  elided. For example, the following are all equivalent:
    (load-libs 'a.b.c 'a.b.d 'a.c.e 'a.c.f) ; all names fully qualified
    (load-libs '(a.b c d) '(a.c e f)) ; 'a.b and 'a.c extracted
    (load-libs '(a (b c d) (c e f))) ; all common prefixes extracted
  Symbols containing explicit periods and the equivalent prefix lists may
  be mixed freely.

  Recognized flags: :reload-all, :verbose

  :reload-all forces loading of all namespaces that the identified libs
    directly or indirectly load via load-namespaces/require/use
  :verbose triggers printing information about each load and refer"
  [& args]
  (let [flags (filter keyword? args)
        opts (interleave flags (repeat true))
        args (filter (complement keyword?) args)]
    (apply load-prefix-list symbol? nil opts args)))

;; Namespaces

(defn load-namespaces
  "Loads namespace definitions contained in resources in classpath.
  Each argument is a either an nsspec that identifies a namespace, a prefix
  list that identifies multiple namespaces with names that share common
  prefixes, or a flag that modifies how all the identified namespaces are
  loaded. Namespace names map to paths within classpath: components of the
  namespace name separated by periods correspond to components in
  classpath-relative paths separated by slashes. To load the namespace
  definition, load-namespaces loads the namespace's 'root lib'. The root
  lib's name is derived from the namespace name by repeating its last
  component. The root lib for namespace 'a.b.c is the lib 'a.b.c.c
  contained in the resource '<classpath>/a/b/c/c.clj'. The namespace
  definition need not be completely contained in its root lib. If it's not
  the root lib must contain code to (directly or indirectly) load the
  additional libs using load-libs.

  An nsspec is a symbol or a list containing a symbol and options. Some
  options may have meaning only when specific flags are set.

  An option is a keyword followed by an argument.
  Recognized options: :as :exclude, :only, :rename

  The argument for :as is a symbol to use as an alias for the specified
  namespace.
  The arguments and semantics for :exclude, :only, and :rename are those
  documented for clojure/refer. They are only effective when the :use flag
  is present.

  Multiple namespaces whose names share the same period-delimited prefix
  can be identified succinctly using a prefix list: a list containing the
  shared prefix followed by nsspecs and/or prefix lists with the shared
  prefix elided. For example, the following are all equivalent:
    (load-namespaces 'a.b.c 'a.b.d 'a.c.e 'a.c.f) ; fully qualified names
    (load-namespaces '(a.b c d) '(a.c e f)) ; 'a.b and 'a.c extracted
    (load-namespaces '(a (b c d) (c e f))) ; all common prefixes extracted
  Symbols containing explicit periods and the equivalent prefix lists may be
  mixed freely.

  A flag is a keyword.
  Recognized flags: :require, :use, :reload, :reload-all, :verbose

  :require indicates that any identified namespace definitions that are
    already loaded need not be reloaded
  :use triggers referring to each namespace with optional filters specified
    in its options
  :reload forces loading of all the identified namespace definitions even
    if they are already loaded. :reload supersedes :require
  :reload-all implies :reload and also forces loading of all namespace
    definitions that the identified namespace definitions directly or
    indirectly load via load-namespaces/require/use
  :verbose triggers printing information about each load, alias and refer"
  [& args]
  (let [flags (cons :root (filter keyword? args))
        opts (interleave flags (repeat true))
        args (filter (complement keyword?) args)]
    (apply load-prefix-list nsspec? nil opts args)))

(defn namespaces
  "Returns a sorted set of symbols naming loaded namespaces"
  []
  @*namespaces*)

(defn require
  "Ensures that namespaces are loaded while avoiding duplicate
  loads. Arguments are described in the docs for 'load-namespaces. The
  :require flag is automatically set."
  [& args]
  (apply load-namespaces :require args))

(defn use
  "Ensures that namespaces are loaded while avoiding duplicate loads and
  refers to them in the current namespace. Arguments are described in the
  docs for 'load-namespaces. The :require and :use flags are automatically
  set."
  [& args]
  (apply load-namespaces :require :use args))

(defn init-ns
  "Creates ns (if necessary), makes it current, and uses clojure/refer to
  make the 'clojure namespace available to it. Optional clojure-filters can
  be passed along to clojure/refer to modify how much of and how 'clojure
  is pulled in. init-ns also refers to (all of) 'clojure.contrib.lib.
  Namespace root libs will typically begin with a call to init-ns; any
  subordinate libs can begin with a call to in-ns."
  [ns & clojure-filters]
  (in-ns ns)
  (apply refer 'clojure clojure-filters)
  (refer 'clojure.contrib.lib))
