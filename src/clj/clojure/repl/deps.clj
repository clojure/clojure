;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns clojure.repl.deps
  "clojure.repl.deps provides facilities for dynamically modifying the available
  libraries in the runtime when running at the REPL, without restarting"
  (:require
   [clojure.java.io :as jio]
   [clojure.java.basis :as basis]
   [clojure.java.basis.impl :as basis-impl]
   [clojure.tools.deps.interop :as tool])
  (:import
    [clojure.lang DynamicClassLoader RT]
    [java.io File]))

(set! *warn-on-reflection* true)

(defn- add-loader-url
  "Add url string or URL to the highest level DynamicClassLoader url set."
  [url]
  (let [u (if (string? url) (RT/toUrl ^String url) url)
        loader (loop [loader (.getContextClassLoader (Thread/currentThread))]
                 (let [parent (.getParent loader)]
                   (if (instance? DynamicClassLoader parent)
                     (recur parent)
                     loader)))]
    (if (instance? DynamicClassLoader loader)
      (.addURL ^DynamicClassLoader loader u)
      (throw (IllegalAccessError. "Context classloader is not a DynamicClassLoader")))))

(defn add-libs
  "Given lib-coords, a map of lib to coord, will resolve all transitive deps for the libs
  together and add them to the repl classpath, unlike separate calls to add-lib."
  {:added "1.12"}
  [lib-coords]
  (when-not *repl* (throw (RuntimeException. "add-libs is only available at the REPL")))
  (let [{:keys [libs] :as basis} (basis/current-basis)
        current-libs (set (keys libs))
        lib-coords (reduce-kv #(if (contains? current-libs %2) %1 (assoc %1 %2 %3))
                     {} lib-coords)]
    (when-not (empty? lib-coords)
      (let [procurer (reduce-kv (fn [m k v] (if (contains? #{"mvn" "git" "local"} (namespace k)) (assoc m k v) m)) {} basis)
            tool-args {:existing libs, :add lib-coords, :procurer procurer}
            {:keys [added] :as _res} (tool/invoke-tool {:tool-alias :deps, :fn 'clojure.tools.deps/resolve-added-libs, :args tool-args})
            ;_ (clojure.pprint/pprint _res)
            paths (mapcat :paths (vals added))
            urls (->> paths (map jio/file) (map #(RT/toUrl ^File %)))]
        (run! add-loader-url urls)
        (basis-impl/update-basis! update :libs merge added)
        ;; reload root *data-readers* from classpath
        (set! *data-readers* (merge (#'clojure.core/load-data-readers) *data-readers*))
        (let [ret (-> added keys sort vec)]
          (when (seq ret) ret))))))

(defn add-lib
  "Given a lib that is not yet on the repl classpath, make it available by
  downloading the library if necessary and adding it to the classloader.
  Libs already on the classpath are not updated. Requires a valid parent
  DynamicClassLoader.

   lib - symbol identifying a library, for Maven: groupId/artifactId
   coord - optional map of location information specific to the procurer,
           or latest if not supplied

  Returns coll of libs loaded, including transitive (or nil if none).

  For info on libs, coords, and versions, see:
   https://clojure.org/reference/deps_and_cli"
  {:added "1.12"}
  ([lib coord]
   (add-libs {lib coord}))
  ([lib]
   (let [procurer (select-keys (basis/current-basis) [:mvn/repos :mvn/local-repo])
         coord (tool/invoke-tool {:tool-alias :deps
                                  :fn 'clojure.tools.deps/find-latest-version
                                  :args {:lib lib, :procurer procurer}})]
     (if coord
       (add-libs {lib coord})
       (throw (ex-info (str "No version found for lib " lib) {}))))))

(defn sync-deps
  "Calls add-libs with any libs present in deps.edn but not yet present on the classpath.

    :aliases - coll of alias keywords to use during the sync"
  {:added "1.12"}
  [& {:as opts}]
  (let [{:keys [aliases]} opts
        basis-config (:basis-config (basis/current-basis))
        new-basis-config (update basis-config :aliases (fnil into []) aliases)
        new-basis (tool/invoke-tool {:tool-alias :deps, :fn 'clojure.tools.deps/create-basis, :args new-basis-config})
        new-libs (:libs new-basis)]
    (add-libs new-libs)))
