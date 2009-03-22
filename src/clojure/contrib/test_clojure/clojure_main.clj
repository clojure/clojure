;;  Copyright (c) Shawn Hoover. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.

(ns clojure.contrib.test-clojure.clojure-main
  (:use clojure.contrib.test-is)
  (:require [clojure.main :as main]))

(defn- set-properties
  [settings]
  (doseq [[name val] settings]
    (if val
      (System/setProperty name val)
      (System/clearProperty name))))

(defmacro with-properties
  "setting => property-name value

  Sets the system properties to the supplied values, executes the body, and
  sets the properties back to their original values. Values of nil are
  translated to a clearing of the property."
  [settings & body]
  `(let [settings# ~(apply hash-map settings)
         current# (map (fn [p#] [p# (System/getProperty p#)])
                       (keys settings#))]
     (set-properties settings#)       
     (try
      ~@body
      (finally
       (set-properties current#)))))

(deftest compile-path-respects-java-property
  ;; Bug fixed in r1177; previously was hardwired to the compile-time path.
  (with-properties ["clojure.compile.path" "compile path test"]
    (main/with-bindings
      (is (= "compile path test" *compile-path*)))))
