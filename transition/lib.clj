;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  Libs
;;
;;  A 'lib' is a named set of resources in classpath whose contents define
;;  a library of Clojure code. Lib names are symbols and each lib is
;;  associated with a Clojure namespace and a Java package that share its
;;  name. A lib's name also locates its root directory within classpath
;;  using Java's package name to classpath-relative path mapping. All
;;  resources in a lib should be contained in the directory structure under
;;  its root directory. All definitions a lib makes should be in its
;;  associated namespace.
;;
;;  Clojure loads a lib by loading its root resource. The root resource
;;  path is derived from the root directory path by repeating its last
;;  component and appending '.clj'. For example, the lib 'x.y.z has root
;;  directory <classpath>/x/y/z; root resource <classpath>/x/y/z/z.clj.
;;  The root resource should contain code to create the lib's namespace and
;;  load any additional lib resources.
;;
;;  Clojure provides functions to:
;;
;;    - ensure libs are loaded while avoiding duplicate loads (require)
;;    - require libs and refer to their namespaces (use)
;;    - return a sorted set of symbols naming loaded libs (loaded-libs)
;;    - load Clojure code from resources (load-resources)
;;
;;  Libspecs
;;
;;  A libspec identifies a lib to load. It's either a lib name or a vector
;;  containing a lib name followed by options expressed as squential
;;  keywords and arguments. Libspec examples:
;;
;;    - 'clojure.contrib.sql
;;    - '[clojure.contrib.sql :exclude (get-connection) :as sql]
;;
;;  Prefix Lists
;;
;;  It's common for Clojure code to depend on several libs whose names have
;;  the same prefix. When specifying libs, prefix lists can be used to
;;  reduce repetition. A prefix list contains the shared prefix followed by
;;  libspecs with the shared prefix removed from the lib names. After
;;  removing the prefix, the names that remain must not contain any
;;  periods. For example, the following are equivalent:
;;
;;    - (require 'clojure.contrib.def '[clojure.contrib.sql :as db])
;;    - (require '(clojure.contrib def [sql :as db]))
;;
;;  Examples
;;
;;    (require '(clojure.contrib sql sql.tests))
;;    (use '(clojure.contrib sql ns-utils) :verbose)
;;
;;    (use :reload-all :verbose
;;      '(clojure.contrib
;;        [sql :exclude (get-connection)
;;             :rename {execute-commands do-commands}]
;;        [ns-utils :as nsu]))
;;
;;  scgilardi (gmail)
;;  Created 7 April 2008
;;
;;  Thanks to Stuart Sierra for providing many useful ideas, discussions
;;  and code contributions for lib.clj.

(clojure/in-ns 'clojure)

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
    "A ref to a sorted set of symbols representing loaded libs"}
 *loaded-libs*)
(init-once *loaded-libs* (ref (sorted-set)))

(def
 #^{:private true :doc
    "True while a verbose load is pending"}
 *loading-verbosely*)
(init-once *loading-verbosely* false)

(defn- format
  "Formats a string using String/format, see java.util.Formatter for format
  string syntax"
  [fmt & args]
  (String/format fmt (to-array args)))

(defn- printf
  "Prints formatted output"
  [fmt & args]
  (print (apply format fmt args)))

(defn- throw-if
  "Throws an exception with a message if pred is true"
  [pred fmt & args]
  (when pred
    (let [message (apply format fmt args)
          exception (Exception. message)
          raw-trace (.getStackTrace exception)
          boring? #(not= (.getMethodName %) "doInvoke")
          trace (into-array (drop 2 (drop-while boring? raw-trace)))]
      (.setStackTrace exception trace)
      (throw exception))))

(defn- libspec?
  "Returns true if x is a libspec"
  [x]
  (or (symbol? x)
      (and (vector? x)
           (or
            (nil? (second x))
            (keyword? (second x))))))

(defn- prepend
  "Prepends a symbol or a seq to coll"
  [x coll]
  (if (symbol? x)
    (cons x coll)
    (concat x coll)))

(defn- root-directory
  "Returns the root directory path for a lib"
  [sym]
  (str \/
       (.. (name sym)
           (replace \- \_)
           (replace \. \/))))

(defn- root-resource
  "Returns the root resource path for a lib"
  [sym]
  (let [d (root-directory sym)
        i (inc (.lastIndexOf d (int \/)))
        leaf (.substring d i)]
    (str d \/ leaf ".clj")))

(def load-resources)

(defn- load-one
  "Loads a lib given its name. If need-ns, ensures that the associated
  namespace exists after loading. If require, records the load so any
  duplicate loads can be skipped."
  [sym need-ns require]
  (load-resources (root-resource sym))
  (throw-if (and need-ns (not (find-ns sym)))
            "namespace '%s' not found after loading '%s'"
            sym (root-resource sym))
  (when require
    (dosync
     (commute *loaded-libs* conj sym))))

(defn- load-all
  "Loads a lib given its name and forces a load of any libs it directly or
  indirectly loads. If need-ns, ensures that the associated namespace
  exists after loading. If require, records the load so any duplicate loads
  can be skipped."
  [sym need-ns require]
  (dosync
   (commute *loaded-libs* clojure.set/union
            (binding [*loaded-libs* (ref (sorted-set))]
              (load-one sym need-ns require)
              @*loaded-libs*))))

(defn- load-lib
  "Loads a lib with options"
  [prefix sym & options]
  (throw-if (and prefix (pos? (.indexOf (name sym) (int \.))))
            "lib names inside prefix lists must not contain periods")
  (let [sym (if prefix (symbol (str prefix \. sym)) sym)
        opts (apply hash-map options)
        as (:as opts)
        reload (:reload opts)
        reload-all (:reload-all opts)
        require (:require opts)
        use (:use opts)
        verbose (:verbose opts)
        loaded (contains? @*loaded-libs* sym)
        load (cond reload-all
                   load-all
                   (or reload (not require) (not loaded))
                   load-one)
        need-ns (or as use)
        filter-opts (select-keys opts '(:exclude :only :rename))]
    (binding [*loading-verbosely* (or *loading-verbosely* verbose)]
      (if load
        (load sym need-ns require)
        (throw-if (and need-ns (not (find-ns sym)))
                  "namespace '%s' not found" sym))
      (when (and need-ns *loading-verbosely*)
        (printf "(clojure/in-ns '%s)\n" (ns-name *ns*)))
      (when as
        (when *loading-verbosely*
          (printf "(clojure/alias '%s '%s)\n" as sym))
        (alias as sym))
      (when use
        (when *loading-verbosely*
          (printf "(clojure/refer '%s" sym)
          (doseq opt filter-opts
            (printf " %s '%s" (key opt) (print-str (val opt))))
          (printf ")\n"))
        (apply refer sym (mapcat seq filter-opts))))))

(defn- load-libs
  "Loads libs, interpreting prefix lists and libspecs for forwarding to
  load-lib"
  [& args]
  (let [flags (filter keyword? args)
        opts (interleave flags (repeat true))
        args (filter (complement keyword?) args)]
    (doseq arg args
      (if (libspec? arg)
        (apply load-lib nil (prepend arg opts))
        (let [[prefix & args] arg]
          (throw-if (nil? prefix) "prefix cannot be nil")
          (doseq arg args
            (apply load-lib prefix (prepend arg opts))))))))

;; Public

(defn require
  "Loads libs, skipping any that are already loaded. Each argument is
  either a libspec that identifies a lib, a prefix list that identifies
  multiple libs whose names share a common prefix, or a flag that modifies
  how all the identified libs are loaded.

  Libs

  A 'lib' is a named set of resources in classpath whose contents define a
  library of Clojure code. Lib names are symbols and each lib is associated
  with a Clojure namespace and a Java package that share its name. A lib's
  name also locates its root directory within classpath using Java's
  package name to classpath-relative path mapping. All resources in a lib
  should be contained in the directory structure under its root directory.
  All definitions a lib makes should be in its associated namespace.

  'require loads a lib by loading its root resource. The root resource path
  is derived from the root directory path by repeating its last component
  and appending '.clj'. For example, the lib 'x.y.z has root directory
  <classpath>/x/y/z; root resource <classpath>/x/y/z/z.clj. The root
  resource should contain code to create the lib's namespace and load any
  additional lib resources.

  Libspecs

  A libspec is a lib name or a vector containing a lib name followed by
  options expressed as sequential keywords and arguments.

  Recognized options: :as
  :as takes a symbol as its argument and makes that symbol an alias to the
    lib's namespace in the current namespace.

  Prefix Lists

  It's common for Clojure code to depend on several libs whose names have
  the same prefix. When specifying libs, prefix lists can be used to reduce
  repetition. A prefix list contains the shared prefix followed by libspecs
  with the shared prefix removed from the lib names. After removing the
  prefix, the names that remain must not contain any periods.

  Flags

  A flag is a keyword.
  Recognized flags: :reload, :reload-all, :verbose
  :reload forces loading of all the identified libs even if they are
    already loaded
  :reload-all implies :reload and also forces loading of all libs that the
    identified libs directly or indirectly load via require or use
  :verbose triggers printing information about calls to load-resources,
    alias, and refer"
  [& args]
  (apply load-libs :require args))

(defn use
  "Like 'require, but also refers to each lib's namespace using
  clojure/refer.

  'use accepts additional options in libspecs: :exclude, :only, :rename.
  The arguments and semantics for :exclude, :only, and :rename are the same
  as those documented for clojure/refer."
  [& args]
  (apply load-libs :require :use args))

(defn loaded-libs
  "Returns a sorted set of symbols naming the currently loaded libs"
  []
  @*loaded-libs*)

(defn load-resources
  "Loads Clojure code from resources in classpath. A path is interpreted as
  classpath-relative if it begins with a slash or relative to the root
  directory for the current namespace otherwise."
  [& paths]
  (doseq path paths
    (let [path (if (.startsWith path "/")
                 path
                 (str (root-directory (ns-name *ns*)) \/ path))]
      (when *loading-verbosely*
        (printf "(clojure/load-resources \"%s\")\n" path)
        (flush))
      (.loadResourceScript clojure.lang.RT (.substring path 1)))))
