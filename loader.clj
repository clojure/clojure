;;  Copyright (c) Stephen C. Gilardi. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.
;;
;;  loader.clj
;;
;;  scgilardi (gmail)
;;  5 April 2008
;;
;;  load-system-resource adapted from Stuart Sierra's public domain
;;  require.clj

(clojure/in-ns 'loader)
(clojure/refer 'clojure)

;; Private

(def
 #^{:doc "True when an ensure-ns call has requested a reload of all of its
  dependencies (:reload-all)"
 :private true}
 reloading-all false)

(def
 #^{:doc "True when an ensure-ns call has requested that 'loaded' messages
  be printed after each load (:verbose)"
 :private true}
 loading-verbosely false)

(defn- load-system-resource
  "Sequentially read and evaluate forms from a resource within classpath"
  [name]
  (let [url (. ClassLoader (getSystemResource name))]
    (when-not url
      (throw (new Exception (str "'" name "' not found in classpath"))))
    (if (= "file" (. url (getProtocol)))
      (load-file (. url (getFile)))
      (with-open reader (new java.io.BufferedReader
                             (new java.io.InputStreamReader
                                  (. url (openStream))))
        (load reader)))))

;; Public

(defn ensure-ns
  "Ensures that a namespace is loaded. If it is not yet loaded, searches
  for an implementation file whose name is the namespace name followed by
  '.clj' and loads it. If no :from option is present, the search considers
  only the classpath roots. Options may include one each of:

  :from string
  :reload boolean
  :reload-all boolean
  :verbose boolean

  An argument to :from specifies a path to the implementation file's parent
  directory. An absolute path (beginning with '/') specifies a directory in
  the filesystem. A relative path specifies directories relative to each of
  the classpath roots.
  When :reload is true, the namespace will be reloaded if already loaded.
  When :reload-all is true, all directly and indirectly required namespaces
  are also reloaded.
  When :verbose is true, a 'loaded' message is printed after each load."
  [ns-sym & options]
  (let [opts (apply hash-map options)
        from (:from opts)
        reload (:reload opts)
        reload-all (:reload-all opts)
        verbose (:verbose opts)]
    (binding [reloading-all (or reloading-all reload-all)
              loading-verbosely (or loading-verbosely verbose)]
      (when (or (not (find-ns ns-sym)) reload reloading-all)
        (let [resource (str from (when from \/) ns-sym ".clj")]
          (if (= (first resource) \/)
            (load-file resource)
            (load-system-resource resource))
          (when loading-verbosely
            (println "loaded" resource))
          (when-not (find-ns ns-sym)
            (throw (new Exception (str "namespace '" ns-sym
                                       "' not found in '"
                                       resource "'")))))))))

(defn require
  "Ensures that a namespace is loaded and then refers to it.  Options may
  include options for ensure-ns and/or filters for refer."
  [ns-sym & options]
  (apply ensure-ns ns-sym options)
  (apply refer ns-sym options))
