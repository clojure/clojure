;;  Copyright (c) Stephen C. Gilardi. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.
;;
;;  lib.clj
;;
;;  A 'lib' is a unit of Clojure code contained in a file that follows
;;  these conventions:
;;
;;    - has a basename that is a valid symbol name in Clojure
;;    - has the extension ".clj"
;;
;;  A lib's name is the basename of its implementation file.
;;
;;  A lib will typically contain forms that provide something useful for
;;  a Clojure program to use.  It may also define a namespace which shares
;;  its name.
;;
;;  Libspecs
;;
;;  A lib is specified by a libspec which is either a lib name or a seq
;;  beginning with a lib name and followed by zero or more options
;;  expressed as sequential keywords and values. The options may include
;;  any of the filters documented for clojure/refer or the :in option which
;;  specifies the path to the lib's parent directory relative to one of the
;;  classpath roots.
;;
;;  Flags
;;
;;  Several flags are available to control the loading of libs:
;;
;;  :require       indicates that already loaded libs need not be reloaded
;;  :use           triggers a call to clojure/refer for each lib's namespace
;;                 after ensuring the lib is loaded
;;  :reload        forces reloading of libs that are already loaded
;;  :reload-all    implies :reload and also forces reloading of all libs
;;                 on which each lib directly or indirectly depend
;;  :verbose       triggers printing a message each time a lib is loaded
;;
;;  The :reload and :reload-all flags supersede :require.
;;
;;  Examples
;;
;;  (require sql)
;;  (require (test :in "private/resources"))
;;  (use sql ns-utils :verbose)
;;  (use :reload-all :verbose
;;    (sql :exclude (get-connection) :rename {execute-commands do-commands})
;;    ns-utils)
;;  (use (sql))
;;
;;  lib.clj provides the following functions and macros:
;;
;;  Foundation
;;
;;    'load-libs'             searches classpath for libs and loads them
;;                            based on libspecs and flags
;;
;;    'libs'                  returns a sorted sequence of symbols naming
;;                            loaded libs
;;
;;  Dependency Management
;;
;;    'require'               searches classpath for libs and loads them
;;                            if they are not already loaded (macro)
;;
;;    'use'                   requires libs and refers to their namespaces
;;                            with options (macro)
;;
;;  Loading
;;
;;    'load-uri'              loads Clojure source from a location
;;
;;    'load-system-resource'  loads Clojure source from a resource in
;;                            classpath
;;
;;  scgilardi (gmail)
;;  23 April 2008
;;
;;  Thanks to Stuart Sierra for providing many useful ideas, discussions
;;  and code contributions for lib.clj.

(clojure/in-ns 'lib)
(clojure/refer 'clojure)

(import '(java.io BufferedReader InputStreamReader))

;; Private

(defmacro init-once
  "Initializes a var exactly once.  The var must already exist."
  {:private true}
  [var init]
  `(let [v# (resolve '~var)]
     (when-not (. v# (isBound))
       (. v# (bindRoot ~init)))))

(def
 #^{:private true :doc
    "A ref to a set of symbols representing loaded libs"}
 *libs*)
(init-once *libs* (ref #{}))

(def
 #^{:private true :doc
    "True while a verbose require is pending"}
 *verbose*)
(init-once *verbose* false)

(def load-system-resource)

(defn- load-lib
  "Loads a lib from <classpath>/in/"
  [sym in need-ns]
  (let [res (str sym ".clj")]
    (load-system-resource res in)
    (when (and need-ns (not (find-ns sym)))
      (throw (new Exception
                  (str "namespace '" sym "' not found after "
                       "loading resource '" res "'")))))
  (dosync
   (commute *libs* conj sym))
  (when *verbose*
    (println "loaded" sym)))

(defn- load-all
  "Loads a lib from <classpath>/in/ and forces a load of any
  libs on which it directly or indirectly depends"
  [sym in need-ns]
  (dosync
   (commute *libs* set/union
            (binding [*libs* (ref #{})]
              (load-lib sym in need-ns)
              @*libs*))))

(defn- load-with-options
  "Load a lib with options expressed as sequential keywords and
  values"
  [sym & options]
  (let [opts (apply hash-map options)
        in (:in opts)
        reload (:reload opts)
        reload-all (:reload-all opts)
        require (:require opts)
        use (:use opts)
        verbose (:verbose opts)
        loaded (contains? @*libs* sym)]
    (binding [*verbose* (or *verbose* verbose)]
      (cond reload-all
            (load-all sym in use)
            (or reload (not require) (not loaded))
            (load-lib sym in use)))
    (when use
      (apply refer sym options))))

;; Foundation

(defn load-libs
  "Searches classpath for libs and loads them based on libspecs
  and flags. In addition to the flags documented for 'require'
  load-libs supports :require which indicates that already loaded
  libs need not be reloaded and :use which triggers a call to
  'clojure/refer' for the lib's namespace after ensuring the lib
  is loaded. The :reload and :reload-all flags supersede
  :require."
  [& args]
  (let [libspecs (filter (comp not keyword?) args)
        flags (filter keyword? args)
        options (interleave flags (repeat true))]
    (doseq libspec libspecs
      (let [libspec
            (if (and (seq? libspec) (= 'quote (first libspec)))
              (second libspec) libspec)]
        (apply load-with-options
               ((if (symbol? libspec) cons concat)
                libspec options))))))

(defn libs
  "Returns a sorted sequence of symbols naming loaded libs"
  []
  (sort @*libs*))

;; Dependency Management

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
  can contain additional options which are filters for refer."
  [& args]
  `(apply load-libs :require :use '~args))

;; Loading

(defn load-uri
  "Loads Clojure source from a URI, which may be a java.net.URI
  java.net.URL, or String.  Accepts any URI scheme supported by
  java.net.URLConnection (http and jar), plus file URIs."
  [uri]
  (let [url (cond  ; coerce argument into java.net.URL
             (instance? java.net.URL uri) uri
             (instance? java.net.URI uri) (. uri (toURL))
             (string? uri) (new java.net.URL uri)
             :else (throw (new Exception
                               (str "Cannot coerce "
                                    (class uri)
                                    " into java.net.URL."))))]
    (if (= "file" (. url (getProtocol)))
      (load-file (. url (getFile)))
      (with-open reader
          (new BufferedReader
               (new InputStreamReader
                    (. url (openStream))))
        (load reader)))))

(defn load-system-resource
  "Loads Clojure source from a resource within classpath"
  ([res]
     (let [url (. ClassLoader (getSystemResource res))]
       (when-not url
         (throw (new Exception (str "resource '" res
                                    "' not found in classpath"))))
       (load-uri url)))
  ([res in]
     (load-system-resource (if in (str in \/ res) res))))
