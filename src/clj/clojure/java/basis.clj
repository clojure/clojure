;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.java.basis
  "The lib basis includes which libraries and versions were loaded both
  for direct dependencies and transitive dependencies, as well as the
  classpath and possibly other information from the resolution process.
  This basis will be known if the runtime was started by the Clojure CLI.

  The Clojure CLI or tools.deps merge a set of deps maps (often from
  deps.edn files). Additional runtime modifications are supplied via argmap
  keys, provided via alias maps in the merged deps. Deps maps typically have
  :paths, :deps, and :aliases keys.

  The basis is a superset of merged deps.edn files with the following
  additional keys:
    :basis-config - params used to configure basis deps sources, can be
                    string path, deps map, nil, or :default
      :root - default = loaded as a resource from tools.deps)
      :user - default = ~/.clojure/deps.edn)
      :project - default = ./deps.edn)
      :extra - default = nil
      :aliases - coll of keyword aliases to include during dep calculation
    :argmap - effective argmap (after resolving and merging argmaps from aliases)
    :libs - map of lib to coord for all included libraries
    :classpath - classpath map, keys are paths (to directory or .jar), values
                 are maps with source identifier (either :lib-name or :path-key)
    :classpath-roots - vector of paths in classpath order (keys of :classpath)"
  (:require
    [clojure.java.basis.impl :as impl]))

(defn initial-basis
  "Initial runtime basis at launch, nil if unknown (process not started by CLI)"
  {:added "1.12"}
  []
  @impl/init-basis)

(defn current-basis
  "Return the current basis, which may have been modified since runtime launch."
  {:added "1.12"}
  []
  @@impl/the-basis)
