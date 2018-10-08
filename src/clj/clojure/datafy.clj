;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.datafy
  ^{:doc "Functions to turn objects into data. Alpha, subject to change"}
  (:require [clojure.core.protocols :as p]
            [clojure.reflect :as refl]))

(defn datafy
  "Attempts to return x as data. If :clojure.datafy/datafy is present
  as metadata of x, it will be called with x as an argument, else
  datafy will return the value of clojure.protocols/datafy. If the
  value has been transformed and the result supports
  metadata, :clojure.datafy/obj will be set on the metadata to the
  original value of x."
  [x]
  (let [v ((or (-> x meta ::datafy) p/datafy) x)]
    (if (identical? v x)
      v
      (if (instance? clojure.lang.IObj v)
        (vary-meta v assoc ::obj x)
        v))))

(defn nav
  "Returns (possibly transformed) v in the context of coll and k (a
  key/index or nil). Callers should attempt to provide the key/index
  context k for Indexed/Associative/ILookup colls if possible, but not
  to fabricate one e.g. for sequences (pass nil). If :clojure.datafy/nav is
  present as metadata on coll, it will be called with coll, k and v as
  arguments, else nav will call :clojure.protocols/nav."
  [coll k v]
  ((or (-> coll meta ::nav) p/nav) coll k v))

(defn- sortmap [m]
  (into (sorted-map) m))

(extend-protocol p/Datafiable
  clojure.lang.IRef
  (datafy [r] (with-meta [(deref r)] (meta r)))

  clojure.lang.Namespace
  (datafy [n] (with-meta {:publics (-> n ns-publics sortmap)
                          :imports (-> n ns-imports sortmap)
                          :interns (-> n ns-interns sortmap)}
                (meta n)))

  java.lang.Class
  (datafy [c] (let [{:keys [members] :as ret} (refl/reflect c)]
                (assoc ret :members (->> members (group-by :name) sortmap)))))