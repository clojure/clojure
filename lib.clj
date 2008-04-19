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
;;  A 'lib' is a Clojure source file that follows these conventions:
;;
;;    - has a basename that is a valid symbol name in Clojure
;;    - has the extension ".clj"
;;
;;  A lib will typically also contain forms that provide something useful
;;  for a Clojure program to use.  It may also define a namespace with the
;;  same name as its basename.
;;
;;  This file is an example of a lib. It follows the naming conventions
;;  and provides these functions in the 'lib' namespace that make it
;;  convenient to use libs from Clojure source code and the Clojure repl:
;;
;;  'require'               searches classpath for libs and loads them if
;;                          they are not already loaded
;;
;;  'use'                   requires libs and refers to their namespaces
;;
;;  'libs'                  returns a sorted list of loaded libs
;;
;;  'load-uri'              loads Clojure source from a location
;;
;;  'load-system-resource'  loads Clojure source from a resource in
;;                          classpath
;;
;;  'require' and 'use' have additional options as described in their docs.
;;
;;  scgilardi (gmail)
;;  8 April 2008
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

(defn- load-one
  "Loads a lib from <classpath>/in/"
  [sym in need-ns]
  (let [res (str sym ".clj")]
    (load-system-resource res in)
    (when (and need-ns (not (find-ns sym)))
      (throw (new Exception (str "namespace '" sym "' not found after "
                                 "loading resource '" res "'")))))
  (dosync
   (commute *libs* conj sym))
  (when *verbose*
    (println "loaded lib" sym)))

(defn- load-all
  "Loads a lib and any libs on which it directly or indirectly
  depends even if already loaded."
  [sym in need-ns]
  (dosync
   (commute *libs* set/union
     (binding [*libs* (ref #{})]
       (load-one sym in need-ns)
       @*libs*))))

(defn- require-one
  "Single-argument version of 'require'"
  [sym & options]
  (let [opts (apply hash-map options)
        in (:in opts)
        need-ns (:need-ns opts)
        reload (:reload opts)
        reload-all (:reload-all opts)
        verbose (:verbose opts)]
    (binding [*verbose* (or *verbose* verbose)]
      (cond reload-all
            (load-all sym in need-ns)
            (or reload (not (contains? @*libs* sym)))
            (load-one sym in need-ns)))))

(defn- use-one
  "Single-argument version of 'use'"
  [sym & options]
  (apply require-one sym :need-ns true options)
  (apply refer sym options))

(defn- ns-vars
  "Returns a sorted list of symbols naming public vars in
  a namespace"
  [ns]
  (sort (map first (ns-publics ns))))

;; Public

(defn require
  "Declares that subsequent code requires the capabilities
  provided by the named libs. Each argument is a quoted
  symbol or a quoted list of the form (symbol & options...).

  If the lib named by the symbol is not yet loaded, searches
  for it and loads it.  The default search is in the locations
  in classpath. Options may include at most one each of the
  following:

    :in string
    :reload boolean
    :reload-all boolean
    :verbose boolean

  An argument to :in specifies the path to the lib's parent
  directory relative to a location in classpath.
  When :reload is true, the lib is reloaded if already loaded.
  When :reload-all is true, the lib and all libs on which
  it directly or indirectly depends are reloaded.
  When :verbose is true, prints a message after each load."
  [& args]
  (doseq arg args
    (if (symbol? arg)
      (require-one arg)
      (apply require-one arg))))

(defn use
  "Requires and 'refer's the named libs. Arguments are like
  those of 'require', with additional options which are filters
  for 'refer'."
  [& args]
  (doseq arg args
    (if (symbol? arg)
      (use-one arg)
      (apply use-one arg))))

(defn libs
  "Returns a sorted sequence of symbols naming loaded libs"
  []
  (sort @*libs*))

(defn vars
  "Returns a sorted sequence of symbols naming public vars in
  a namespace"
  [ns-sym]
  (when-let ns (find-ns ns-sym)
    (ns-vars ns)))

(defn docs
  "Prints documentation for the public names in a namespace"
  [ns-sym]
  (when-let ns (find-ns ns-sym)
    (doseq item (ns-vars ns)
      (print-doc (ns-resolve ns item)))))

(defn dir
  "Prints a sorted directory of public names in a namespace"
  [ns-sym]
  (doseq item (vars ns-sym)
    (println item)))

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
