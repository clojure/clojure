;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  File: lib.clj
;;
;;  lib.clj provides facilities for loading and managing libs. A lib is a
;;  unit of Clojure code contained in a file or other resource within
;;  classpath. Lib names must be valid Clojure symbol names. The name of a
;;  lib's container is the lib name followed by ".clj". lib.clj also
;;  provides general functions for finding and loading resources using the
;;  class loaders visible to the Clojure runtime.
;;
;;  Here is a brief overview of what's in lib.clj. For detailed docs please
;;  see the doc strings for the individual functions and macros.
;;
;;  Resources
;;
;;    Function: find-resource
;;    Searches available class loaders for a resource, returns URL or nil
;;
;;    Function: load-resource
;;    Loads Clojure source from an absolute path: URI, URL or String
;;
;;  Core
;;
;;    Function: load-libs
;;    Loads lib(s) based libspecs and flags
;;
;;    Function: libs
;;    Returns a sorted set of symbols naming the currently loaded libs
;;
;;  Convenience
;;
;;    Macro: require
;;    Loads libs. By default doesn't reload any that are already loaded
;;
;;    Macro: use
;;    Loads libs like require and refers to each lib's namespace
;;
;;  Examples
;;
;;    (load-libs :require 'sql '(sql-test :in "private/unit-tests"))
;;    (require sql (sql-test :in "private/unit-tests"))
;;
;;    (load-libs :require :use 'sql 'ns-utils :verbose)
;;    (use sql ns-utils :verbose)
;;
;;    (use :reload-all :verbose
;;      (sql :exclude '(get-connection)
;;           :rename '{execute-commands do-commands})
;;      ns-utils)
;;
;;    (use (sql))
;;
;;    (load-libs :require :use '(genclass :ns 'clojure))
;;    (use (genclass :ns 'clojure))
;;
;;  scgilardi (gmail)
;;  Created 7 April 2008
;;
;;  Thanks to Stuart Sierra for providing many useful ideas, discussions
;;  and code contributions for lib.clj.

(clojure/in-ns 'lib)
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
    "A ref to a set of symbols representing loaded libs"}
 *libs*)
(init-once *libs* (ref (sorted-set)))

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
  specified) exists"
  [sym url ns]
  (load-resource url)
  (throw-if (and ns (not (find-ns ns)))
            "namespace '%s' not found after loading '%s'" ns url)
  (dosync
   (commute *libs* conj sym)))

(defn- load-all
  "Loads a lib from a resource and forces a load of any libs which it
  directly or indirectly loads via require/use/load-libs"
  [sym url ns]
  (dosync
   (commute *libs* set/union
            (binding [*libs* (ref (sorted-set))]
              (load-one sym url ns)
              @*libs*))))

(defn- load-lib
  "Loads a lib with options: sequential keywords and arguments. The
  arguments to all options are evaluated so literal symbols or lists must
  be quoted"
  [sym & options]
  (let [raw-opts (apply hash-map options)
        opts (zipmap (keys raw-opts) (map eval (vals raw-opts)))
        in (:in opts)
        ns (:ns opts)
        reload (:reload opts)
        reload-all (:reload-all opts)
        require (:require opts)
        use (:use opts)
        verbose (:verbose opts)
        loaded (contains? @*libs* sym)
        load (cond reload-all
                   load-all
                   (or reload (not require) (not loaded))
                   load-one)
        namespace (when use (or ns sym))
        path (str (if in (str in \/)) sym ".clj")
        url (find-resource path)
        filter-opts (select-keys opts *filter-keys*)]
    (binding [*verbose* (or *verbose* verbose)]
      (when load
        (when *verbose*
          (printf "(lib/load-resource \"%s\")\n" url)
          (flush))
        (throw-if (not url) "'%s' not found in classpath" path)
        (load sym url namespace))
      (when namespace
        (when *verbose*
          (printf "(clojure/in-ns '%s)\n" (ns-name *ns*))
          (printf "(clojure/refer '%s" namespace)
          (dorun (map
                  #(printf " %s '%s" (key %) (print-str (val %)))
                  filter-opts))
          (printf ")\n"))
        (apply refer namespace (mapcat seq filter-opts))))))

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

;; Core

(defn load-libs
  "Searches classpath for libs and loads them. 'load-libs' accepts zero or
  more arguments where each argument is either a libspec that identifies a
  lib to load, or a flag that modifies how all the identified libs are
  loaded.

  A libspec is either a symbol or a list containing a symbol followed by
  zero or more options. Since the arguments to 'load-libs' are evaluated
  before the call, any literal libspecs passed in must be quoted.

  The 'require' and 'use' macros offer a simpler interface to the
  capabilities of load-libs and don't evaluate the libspecs they take as
  arguments.

  An option is a keyword followed by an argument.
  Recognized options: :in, :ns, :exclude, :only, :rename

  All arguments to options within libspecs are evaluated so literal lists
  and symbols appearing within libspecs must be quoted.

  The :in option's argument must evaluate to a string specifying the path
  of the lib's parent directory relative to a location in classpath.

  The :ns options's argument must evaluate to a symbol specifying the
  namespace to refer for this lib if the :use flag is present. When the
  :ns option is not present the namespace defaults to the one with the
  same name as the lib.

  The arguments and semantics for :exclude, :only, and :rename are those
  documented for clojure/refer.

  A flag is a keyword.
  Recognized flags: :require, :use, :reload, :reload-all, :verbose

  :require indicates that any identified libs that are already loaded need
    not be reloaded
  :use triggers referring to each lib's namespace after loading
  :reload forces loading of all the identified libs even if they were
    loaded previously. :reload supersedes :require
  :reload-all implies :reload and also forces loading of all libs that the
    identified libs directly or indirectly load via load-libs/require/use
  :verbose triggers printing a message after loading each lib"
  [& args]
  (let [libspecs (filter (complement keyword?) args)
        flags (filter keyword? args)
        flag-opts (interleave flags (repeat true))]
    (doseq libspec libspecs
      (let [combine (if (symbol? libspec) cons concat)]
        (apply load-lib (combine libspec flag-opts))))))

(defn libs
  "Returns a sorted set of symbols naming loaded libs"
  []
  @*libs*)

;; Convenience

(defmacro require
  "Searches classpath for libs and (by default) loads them if they are not
  already loaded. 'require' accepts zero or more arguments where each
  argument is either a libspec that identifies a lib to load, or a flag
  that modifies how the identified libs are loaded.

  A libspec is a symbol or a list containing a symbol followed by zero or
  more options. 'require' does not evaluate its arguments so libspecs and
  flags should not be quoted.

  An option is a keyword followed by an argument.
  Recognized options: :in

  All arguments to options within libspecs are evaluated so literal lists
  and symbols appearing within libspecs must be quoted.

  The :in option's argument must evaluate to a string specifying the path
  of the lib's parent directory relative to a location in classpath.

  A flag is a keyword.
  Recognized flags: :reload, :reload-all, :verbose

  :reload forces loading of all the identified libs even if they were
    loaded previously
  :reload-all implies :reload and also forces loading of all libs that the
    identified libs directly or indirectly load via load-libs/require/use.
  :verbose triggers printing a message after loading each lib"
  [& args]
  `(apply load-libs :require '~args))

(defmacro use
  "Searches classpath for libs and (by default) loads them if they are not
  already loaded and refers to namespaces with the same name. 'use' accepts
  zero or more arguments where each argument is either a libspec that
  identifies a lib to load, or a flag that modifies how the identified libs
  are loaded.

  A libspec is a symbol or a list containing a symbol followed by zero or
  more options. 'use' does not evaluate its arguments so libspecs and flags
  should not be quoted.

  'use' recognizes all the libspecs, options and flags documented for
  'require' plus some additional options in libspecs.

  Additional options: :ns, :exclude, :only, :rename

  All arguments to options within libspecs are evaluated so literal lists
  and symbols appearing within libspecs must be quoted.

  The :ns options's argument must evaluate to a symbol specifying the
  namespace to refer for this lib.

  The arguments and semantics for :exclude, :only, and :rename are those
  documented for clojure/refer."
  [& args]
  `(apply load-libs :require :use '~args))
