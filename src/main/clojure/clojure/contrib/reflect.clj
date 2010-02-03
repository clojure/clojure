;   Copyright (c) 2010 Stuart Halloway & Contributors. All rights
;   reserved.  The use and distribution terms for this software are
;   covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;   found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this
;   notice, or any other, from this software.

(ns clojure.contrib.reflect)

(defn call-method
  "Calls a private or protected method.

   params is a vector of classes which correspond to the arguments to
   the method e

   obj is nil for static methods, the instance object otherwise.

   The method-name is given a symbol or a keyword (something Named)."
  [klass method-name params obj & args]
  (-> klass (.getDeclaredMethod (name method-name)
                                (into-array Class params))
      (doto (.setAccessible true))
      (.invoke obj (into-array Object args))))

(defn get-field
  "Access to private or protected field.  field-name is a symbol or
  keyword."
  [klass field-name obj]
  (-> klass (.getDeclaredField (name field-name))
      (doto (.setAccessible true))
      (.get obj)))
