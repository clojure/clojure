;;  pkg.clj
;;
;;  Clojure package loading and dependency via require/provide.
;;
;;  A 'package' is a named set of capabilities represented by a symbol.
;;  Its capabilities are expected to be defined in an implementation
;;  file somewhere in CLASSPATH whose name is the package name followed
;;  by ".clj".  The implementation file may be in the filesystem, in a
;;  jar file, or at any other valid CLASSPATH URL.
;;
;;  A call to 'require' indicates that subsequent code depends on
;;  capabilities provided by one or more packages and loads any of those
;;  packages that have not already been provided.
;;
;;  A call to 'provide' indicates that a package's capabilities have been
;;  successfully loaded.  A package's implementation file will generally
;;  end with a call to provide.
;;
;;  pkg.clj provides variations of 'require' that:
;;
;;    - require a package and automatically 'refer' to a namespace of the
;;      same name.  This is for the common case of a package defining its
;;      own namespace which dependent code wants to use without namespace
;;      qualifiers. 'require-ns'
;;
;;    - force the loading of all packages and dependencies regardless of
;;      whether they have already been provided. 'require-force'
;;
;;    - both refer to the package's namespace and force a reload of the
;;      the package and its dependencies. 'require-ns-force'
;;
;;  The "force" variations are useful during development.
;;
;;  'provide' is made available as a separate function (rather than being
;;  done automatically after loading an implementation file) to allow
;;  flexibility in providing a package's capabilities by some means other
;;  than loading an implementation file using 'require'.
;;
;;  scgilardi (gmail)
;;  2 April 2008
;;
;;  load-resource adapted from Stuart Sierra's public domain require.clj
;;
;;  Copyright (c) Stephen C. Gilardi. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.

(clojure/in-ns 'pkg)
(clojure/refer 'clojure)

;; Private

(def
 #^{:doc "A ref to a set of symbols representing provided packages"
 :private true}
 *packages* (ref #{}))

(defn- load-resource
  "Sequentially read and evaluate forms from a resource within CLASSPATH"
  [name]
  (let [url (. ClassLoader (getSystemResource name))]
    (when-not url
      (throw (new Exception (str \" name \" " not found in CLASSPATH"))))
    (if (= "file" (. url (getProtocol)))
      (load-file (. url (getFile)))
      (with-open reader (new java.io.BufferedReader
                             (new java.io.InputStreamReader
                                  (. url (openStream))))
        (load reader)))))

;; Public

(defn packages
  "Returns a set of symbols representing provided packages"
  []
  @*packages*)

(defn provided?
  "Returns true if package has been provided, else false"
  [package]
  (contains? @*packages* package))

(defn provide
  "Marks a package as provided"
  [package]
  (dosync
   (commute *packages* conj package)
   nil))

(defn require
  "Indicates that subsequent code depends on capabilities provided by
  the specified packages. Loads any of those packages that have not
  yet been provided."
  [& packages]
  (doseq package packages
    (when-not (provided? package)
      (let [resource (str package ".clj")]
        (load-resource resource)
        (when-not (provided? package)
          (throw (new Exception (str \" resource \"
                                     " did not provide " package))))))))

(defn require-ns
  "Requires a package and then refers to the namespace of the same name
  with filters"
  [package & filters]
  (require package)
  (apply refer package filters))

(defn require-forced
  "Like 'require' but will reload packages (and their dependencies) that
  have already been provided"
  [& packages]
  (let [forced-packages
        (binding [*packages* (ref #{})]
          (apply require packages)
          @*packages*)]
    (dosync
     (commute *packages* set/union forced-packages)))
  nil)

(defn require-ns-forced
  "Like 'require-ns' but will reload packages (and their dependencies)
  that have already been provided"
  [package & filters]
  (require-forced package)
  (apply refer package filters))

(provide 'pkg)
