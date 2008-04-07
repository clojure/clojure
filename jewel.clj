;;  Copyright (c) Stephen C. Gilardi. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.
;;
;;  jewel.clj
;;
;;  A 'jewel' is a Clojure source file that follows these conventions:
;;
;;    - has a basename that is a valid symbol name in Clojure,
;;    - has the extension ".clj", and
;;    - defines a namespace that has the same name as the basename.
;;
;;  A jewel will typically also contain forms that provide something useful
;;  for a Clojure program to use.
;;
;;  This file is an example of a jewel. It follows the naming conventions
;;  and provides two functions that make it convenient to use jewels from
;;  Clojure source code and the Clojure repl:
;;
;;  'jewel/require' takes a symbol that names a jewel and zero or more
;;  optional keyword parameters.
;;  jewel/require:
;;    - ensures that the jewel is either already loaded or loads it from
;;      within classpath. Optionally, the caller may specify a path to
;;      the jewel's parent directory relative to a location in classpath;
;;    - refers to the jewel's namespace. Optionally, the caller may
;;      specify any filters documented for clojure/refer as options to
;;      jewel/require;
;;    - optionally forces a (re)load of either just the jewel itself or of
;;      the jewel and all of the jewels on which it (directly or indirectly)
;;      depends;
;;    - optionally prints a message each time it loads a jewel.
;;
;;  'jewel/jewels' returns a sorted list of loaded jewels.
;;
;;  scgilardi (gmail)
;;  7 April 2008
;;
;;  load-system-resource is adapted from Stuart Sierra's public domain
;;  require.clj.

(clojure/in-ns 'jewel)
(clojure/refer 'clojure)

(import '(java.io BufferedReader InputStreamReader))

;; Private

(def
 #^{:private true :doc
 "A ref to a set of symbols representing loaded jewels"}
 *jewels*)

(def
 #^{:private true :doc
 "True while a verbose require is pending"}
 *verbose*)

(defmacro init-once
  "Initializes a var exactly once.  The var must already exist."
  #^{:private true}
  [var init]
  `(let [v# (resolve '~var)]
	 (when-not (. v# (isBound))
	   (. v# (bindRoot ~init)))))

(init-once *jewels* (ref #{}))
(init-once *verbose* false)

(defn- load-system-resource
  "Loads Clojure source from a resource within classpath"
  ([res]
   (let [url (. ClassLoader (getSystemResource res))]
     (when-not url
       (throw (new Exception (str "resource '" res
                                  "' not found in classpath"))))
     (if (= "file" (. url (getProtocol)))
       (load-file (. url (getFile)))
       (with-open reader
           (new BufferedReader
                (new InputStreamReader
                     (. url (openStream))))
         (load reader)))))
  ([res in]
   (load-system-resource (if in (str in \/ res) res))))

(defn- jewel-load
  "Loads a jewel from <classpath>/in/"
  [sym in]
  (let [res (str sym ".clj")]
    (load-system-resource res in)
    (when-not (find-ns sym)
      (throw (new Exception (str "namespace '" sym "' not found after "
                                 "loading resource '" res "'")))))
  (dosync
   (commute *jewels* conj sym))
  (when *verbose*
    (println "loaded jewel" sym)))

(defn- jewel-load-all
  "Loads a jewel and any jewels on which it (directly or
  indirectly) depends even if already loaded."
  [sym in]
  (dosync
   (commute *jewels* set/union
     (binding [*jewels* (ref #{})]
       (jewel-load sym in)
       @*jewels*))))

;; Public

(defn require
  "Declares that subsequent code requires the capabilities
  provided by the jewel named by sym. If the jewel is not yet
  loaded, searches for it and loads it. The default search
  is in the locations in classpath. Options may include any
  filter arguments documented for clojure/refer and/or at most
  one each of the following:

    :in string
    :reload boolean
    :reload-all boolean
    :verbose boolean

  An argument to :in specifies the path to the jewel's parent
  directory relative to a location in classpath.
  When :reload is true, the jewel is reloaded if already loaded.
  When :reload-all is true, the jewel and all jewels on which
  it directory or indirectly depends are reloaded.
  When :verbose is true, prints a message after each load."
  [sym & options]
  (let [opts (apply hash-map options)
        in (:in opts)
        reload (:reload opts)
        reload-all (:reload-all opts)
        verbose (:verbose opts)]
    (binding [*verbose* (or *verbose* verbose)]
      (cond reload-all
            (jewel-load-all sym in)
            (or reload (not (contains? @*jewels* sym)))
            (jewel-load sym in))))
  (apply refer sym options))

(defn jewels
  "Returns a sorted sequence of symbols representing loaded jewels"
  []
  (sort @*jewels*))
