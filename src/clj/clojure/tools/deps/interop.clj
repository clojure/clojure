;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns clojure.tools.deps.interop
  "Functions for invoking Java processes and invoking tools via the Clojure CLI."
  (:require
   [clojure.java.process :as proc]
   [clojure.edn :as edn]
   [clojure.java.io :as jio]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:private build (atom nil))

(defn- cli-build
  "Return CLI build number (a long) or nil if it can't be determined.
  The build number is cached if found and subsequently read from cache."
  []
  (or @build
    (let [result (try
                   (proc/exec "clojure" "--version")
                   (catch Exception e ""))
          ;; Version string: "Clojure CLI version 1.11.3.1463"
          ;; Match MAJOR.MINOR.PATCH.BUILD and take a capture group just for the last BUILD part
          version (-> (re-find #"[0-9]+\.[0-9]+\.[0-9]+\.([0-9]+)" result) (nth 1))]
      (when version
        (reset! build (parse-long version))))))

(defn- validate-version
  [version]
  (if version
    (when (< version 1347)
      (throw (RuntimeException. "Clojure CLI version is older than minimum required version, 1.11.1.1347. Please update to latest version.")))
    (throw (ex-info "Clojure CLI version unknown, please install the latest version." {}))))

(defn ^:dynamic invoke-tool
  "Invoke tool using Clojure CLI. Args (one of :tool-alias or :tool-name, and :fn
  are required):
    :tool-alias - Tool alias to invoke (keyword)
    :tool-name - Name of installed tool to invoke (string or symbol)
    :fn - Function (symbol)
    :args - map of args to pass to function

  Options:
    :preserve-envelope - if true, return the full invocation envelope, default=false"
  {:added "1.12"}
  [{:keys [tool-name tool-alias fn args preserve-envelope]
    :or {preserve-envelope false}
    :as opts}]
  (when-not (or tool-name tool-alias) (throw (ex-info "Either :tool-alias or :tool-name must be provided" (or opts {}))))
  (when-not (symbol? fn) (throw (ex-info (str ":fn should be a symbol " fn) (or opts {}))))
  (validate-version (cli-build))
  (let [args (conj [fn] (assoc args :clojure.exec/invoke :fn))
        _ (when (:debug opts) (println "args" args))
        command-strs ["clojure" (str "-T" (or tool-alias tool-name)) "-"]
        _ (when (:debug opts) (apply println "Invoking: " command-strs))
        proc (apply proc/start command-strs)
        in (proc/stdin proc)
        out (proc/stdout proc)
        err (proc/stderr proc)]
    (binding [*print-length* nil
              *print-level*  nil
              *print-namespace-maps* false]
      (proc/io-task
        #(with-open [w (jio/writer in)]
           (doseq [a args]
             (.write w (pr-str a))
             (.write w " ")))))
    (if-let [envelope (edn/read-string (slurp out))]
      (if preserve-envelope
        envelope
        (let [{:keys [tag val]} envelope
              parsed-val (edn/read-string val)]
          (if (= :ret tag)
            parsed-val
            (throw (ex-info (:cause parsed-val) (or parsed-val {}))))))
      (let [err-str (slurp err)
            err-msg (if (= "" err-str) "Unknown error invoking Clojure CLI" err-str)]
        (throw (ex-info err-msg
                 {:command (str/join " " command-strs)
                  :in (str/join " " args)}))))))

(comment
  ;; regular invocation, should return {:hi :there}
  (invoke-tool {:tool-alias :deps, :fn 'clojure.core/identity, :args {:hi :there}})

  ;; invocation throws, should return throwable map data
  (try
    (invoke-tool {:tool-alias :deps, :fn 'clojure.core/+, :args {:fail :here}})
    (catch clojure.lang.ExceptionInfo e (ex-data e)))

  ;; capture stdout in returned envelope
  (let [resp (invoke-tool {:tool-alias :deps,
                           :fn 'list
                           :args {:format :edn
                                  :clojure.exec/out :capture}
                           :preserve-envelope true})]
    (edn/read-string (:out resp)))

  )
