;;  Copyright (c) Stephen C. Gilardi. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.
;;
;;  File: lib.clj
;;
;;  lib.clj provides facilities for loading and managing libs. A lib is
;;  is a unit of Clojure code contained in a file or other resource within
;;  classpath. The name of the lib's container is the lib's name followed
;;  by ".clj". A lib's name must be a valid Clojure symbol name.  lib.clj
;;  also provides functions for finding and loading resources via class
;;  loaders visible to the Clojure runtime.
;;
;;  Resources
;;
;;  Function: find-resource
;;
;;  Searches available class loaders for a resource given its path relative
;;  to classpath. Returns a URL if the resource is found or nil.
;;
;;  Function: load-resource
;;
;;  loads Clojure source from an absolute path expressed as a URI, URL
;;  or String
;;
;;  Core
;;
;;  Function: load-libs
;;
;;  load-libs is the core function provided by lib.clj. Its arguments are
;;  libspecs and flags. It uses libspecs to find lib containers and flags
;;  to control how the libs are loaded. In an explicit call to load-libs
;;  libspecs must be quoted. This requirement is relaxed by the 'require'
;;  and 'use' macros described below.
;;
;;  Libspecs
;;
;;  A libspec is either a lib name or a list beginning with the lib name
;;  and followed by zero or more options.
;;
;;  Options
;;
;;  Options in a libspec are keywords followed by values. These are the
;;  options supported by load-libs and the kind of value each expects:
;;
;;    :in          a string specifying the path to the parent directory
;;                 of the lib's container relative to one of the
;;                 directories in classpath
;;    :ns          a symbol specifying a namespace for the lib. If :ns
;;                 is not present, its value defaults to the lib's name.
;;                 See the :use flag below.
;;    :exclude     a list as documented for clojure/refer
;;    :only        a list as documented for clojure/refer
;;    :rename      a map as documented for clojure/refer
;;
;;  Flags
;;
;;  Flags apply to all the libs specified in a single call to load-libs.
;;  These are the flags supported by load-libs:
;;
;;  :require       indicates that already loaded libs need not be reloaded
;;  :use           triggers a call to clojure/refer for each lib's namespace
;;                 after ensuring the lib is loaded. The call to refer
;;                 includes any filter options in the libspec.
;;  :reload        forces reloading of libs that are already loaded
;;  :reload-all    implies :reload and also forces reloading of all libs
;;                 on which each lib directly or indirectly depends
;;  :verbose       triggers printing a message each time a lib is loaded
;;
;;  (The :reload and :reload-all flags supersede :require.)
;;
;;  Function: libs
;;
;;  The libs function returns a sorted set of symbols naming the currently
;;  loaded libs.
;;
;;  Convenience
;;
;;  lib.clj provides two convenience macros to make calls to load-libs
;;  easier to write and read. Libspecs in calls to these macros do not
;;  need to be quoted.
;;
;;  Macro: require
;;
;;  The require macro accepts libspecs and flags and ensures that the
;;  specified libs are loaded. By default, require will not reload a lib
;;  that is already loaded.
;;
;;  Macro: use
;;
;;  The use macro accepts libspecs and flags and first requires the libs
;;  and then refers to each lib's namespace using any filter options present
;;  in its libspec.
;;
;;  Examples
;;
;;  (load-libs :require 'sql '(test :in "private/resources"))
;;  (require sql (test :in "private/resources"))
;;
;;  (load-libs :require :use 'sql 'ns-utils :verbose)
;;  (use sql ns-utils :verbose)
;;
;;  (use :reload-all :verbose
;;    (sql :exclude (get-connection) :rename {execute-commands do-commands})
;;    ns-utils)
;;  (use (sql))
;;
;;  (use (genclass :ns clojure))
;;
;;  scgilardi (gmail)
;;  Created: 7 April 2008
;;
;;  Thanks to Stuart Sierra for providing many useful ideas, discussions
;;  and code contributions for lib.clj.

(clojure/in-ns 'lib)
(clojure/refer 'clojure)

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
    "True while a verbose require is pending"}
 *verbose*)
(init-once *verbose* false)

(def
 #^{:private true :doc
    "A vector of the available class loaders ordered by the
  degree to which they are controlled by Clojure. The root
  loader's classpath can be extended with clojure/add-classpath"}
 *class-loaders*
 (let [root (.ROOT_CLASSLOADER RT)
       runtime (.getClassLoader (identity RT))
       system (.getSystemClassLoader ClassLoader)]
   (if (= system runtime)
     [root system]
     [root runtime system])))

(defn- throw-if
  "Throws an exception with a message if pred is true. See
  java.util.Formatter for format string syntax."
  [pred fmt & args]
  (if pred (throw (Exception. (String/format fmt (to-array args))))))

(def find-resource)                     ; forward declaration
(def load-resource)                     ; forward declaration

(defn- load-lib
  "Loads a lib from <classpath>/in/ and ensures that namespace
  ns (if specified) exists"
  [sym in ns]
  (let [res (str sym ".clj")
        rel-path (if in (str in \/ res) res)
        url (find-resource rel-path)]
    (throw-if (not url) "resource '%s' not found in classpath" rel-path)
    (load-resource url)
    (throw-if (and ns (not (find-ns ns)))
              "namespace '%s' not found after loading resource '%s'"
              ns rel-path))
  (dosync
   (commute *libs* conj sym))
  (when *verbose*
    (println "loaded" sym)))

(defn- load-all
  "Loads a lib from <classpath>/in/ and forces a load of any
  libs on which it directly or indirectly depends"
  [sym in ns]
  (dosync
   (commute *libs* set/union
            (binding [*libs* (ref (sorted-set))]
              (load-lib sym in ns)
              @*libs*))))

(defn- load-with-options
  "Load a lib with options expressed as sequential keywords and
  values"
  [sym & options]
  (let [opts (apply hash-map options)
        in (eval (:in opts))
        ns (eval (:ns opts))
        reload (:reload opts)
        reload-all (:reload-all opts)
        require (:require opts)
        use (:use opts)
        verbose (:verbose opts)
        loaded (contains? @*libs* sym)
        namespace (and use (or ns sym))]
    (binding [*verbose* (or *verbose* verbose)]
      (cond reload-all
            (load-all sym in namespace)
            (or reload (not require) (not loaded))
            (load-lib sym in namespace)))
    (when namespace
      (apply refer namespace options))))

(defn- remove-quote
  "If form is a 'quote' special form, return the unquoted value
  else return form"
  [form]
  (if (and (seq? form)
           (= 'quote (first form))
           (= 2 (count form)))
    (second form) form))

;; Resources

(defn find-resource
  "Searches for a resource given a path relative to classpath via
  available ClassLoaders. Returns a URL if the resource is found or nil."
  [rel-path]
  (some #(.findResource % rel-path) *class-loaders*))

(defn load-resource
  "Loads Clojure source from a resource specified by an absolute path.
  The path may be a URI, URL, or String. Accepts any URI scheme
  supported by URLConnection (http and jar), plus file paths."
  [abs-path]
  (let [url (cond                       ; coerce argument into URL
             (instance? URL abs-path) abs-path
             (instance? URI abs-path) (.toURL abs-path)
             (string? abs-path) (URL. abs-path))]
    (throw-if (not url) "Cannot coerce %s to %s" (class abs-path) URL)
    (with-open reader
        (BufferedReader.
         (InputStreamReader.
          (.openStream url)))
      (.load Compiler reader (.getPath url) (.getFile url)))))

;; Core

(defn load-libs
  "Searches classpath for libs and loads them based on libspecs
  and flags. In addition to the flags documented for 'require'
  load-libs supports :require which indicates that already loaded
  libs need not be reloaded and :use which triggers a call to
  'clojure/refer' for the lib's namespace after ensuring the lib
  is loaded. The :reload and :reload-all flags supersede
  :require."
  [& args]
  (let [libspecs (filter (complement keyword?) args)
        flags (filter keyword? args)
        options (interleave flags (repeat true))]
    (doseq libspec libspecs
      (let [libspec (remove-quote libspec)
            combine (if (symbol? libspec) cons concat)]
        (apply load-with-options (combine libspec options))))))

(defn libs
  "Returns a sorted set of symbols naming loaded libs"
  []
  @*libs*)

;; Convenience

(defmacro require
  "Searches classpath for libs and loads them if they are not
  already loaded. Each argument is either a libspec or a flag.
  A libspec is a name or a seq of the form (name [options*]).
  Each option is a sequential keyword-value pair. 'require'
  supports the :in option whose value is the path of the lib's
  parent directory relative to a location in classpath.

  Flags may include:

    :reload
    :reload-all
    :verbose

  The :reload flag forces reloading of libs that are already
  loaded. The :reload-all flag implies :reload and also forces
  reloading of all libs on which the libs directly or indirectly
  depend. The :verbose flag triggers printing a message each time
  a lib is loaded."
  [& args]
  `(apply load-libs :require '~args))

(defmacro use
  "Requires and refers to the named libs (see clojure/refer).
  Arguments are like those of 'lib/require', except that libspecs
  can contain an :ns option specifying a namespace to refer to
  (ns defaults to the lib's name) and additional options which
  are filters for clojure/refer."
  [& args]
  `(apply load-libs :require :use '~args))
