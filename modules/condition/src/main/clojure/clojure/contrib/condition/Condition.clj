;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  Condition.clj
;;
;;  Used by clojure.contrib.condition to implement a "Throwable map"
;;
;;  scgilardi (gmail)
;;  Created 09 June 2009

(ns clojure.contrib.condition.Condition
  (:gen-class :extends Throwable
              :implements [clojure.lang.IMeta]
              :state state
              :init init
              :post-init post-init
              :constructors {[clojure.lang.IPersistentMap]
                             [String Throwable]}))

(defn -init
  "Constructs a Condition object with condition (a map) as its
  metadata. Also initializes the superclass with the values at :message
  and :cause, if any, so they are also available via .getMessage and
  .getCause."
  [condition]
  [[(:message condition) (:cause condition)] (atom condition)])

(defn -post-init
  "Adds :stack-trace to the condition. Drops the bottom 3 frames because
  they are always the same: implementation details of Condition and raise."
  [this condition]
  (swap! (.state this) assoc
         :stack-trace (into-array (drop 3 (.getStackTrace this)))))

(defn -meta
  "Returns this object's metadata, the condition"
  [this]
  @(.state this))
