;   Copyright (c) Tom Faulhaber, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.contrib.pprint.examples.multiply
  (:use clojure.contrib.pprint))

(defn multiplication-table [limit]
  (let [nums (range 1 (inc limit))]
    (cl-format true "~{~{~4d~}~%~}" 
             (map #(map % nums) 
                  (map #(partial * %) nums)))))
