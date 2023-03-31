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
   [clojure.edn :as edn]))

(defn ^:dynamic invoke-tool
  "Invoke tool using Clojure CLI. Args (one of :tool-alias or :tool-name, and :fn
  are required):
    :tool-alias - Tool alias to invoke (keyword)
    :tool-name - Name of installed tool to invoke (string or symbol)
    :fn - Function (symbol)
    :args - map of args to pass to function

  Options:
    :command - CLI command, default=\"clojure\"
    :preserve-envelope - if true, return the full invocation envelope, default=false"
  {:added "1.12"}
  [{:keys [tool-name tool-alias fn args command preserve-envelope]
    :or {command "clojure", preserve-envelope false}
    :as opts}]
  (when-not (or tool-name tool-alias) (throw (ex-info "Either :tool-alias or :tool-name must be provided" (or opts {}))))
  (when-not (symbol? fn) (throw (ex-info (str "fn should be a symbol " fn) (or opts {}))))
  (let [args (assoc args :clojure.exec/invoke :fn)
        _ (when (:debug opts) (println "args" args))
        command-strs [command (str "-T" (or tool-alias tool-name)) (pr-str fn) (pr-str args)]
        _ (when (:debug opts) (apply println "Invoking: " command-strs))
        envelope (edn/read-string (apply proc/exec command-strs))]
    (if preserve-envelope
      envelope
      (let [{:keys [tag val]} envelope
            parsed-val (edn/read-string val)]
        (if (= :ret tag)
          parsed-val
          (throw (ex-info (:cause parsed-val) (or parsed-val {}))))))))

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
