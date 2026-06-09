;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.tap
  (:use clojure.test))

;; "small" (< 1024) number of tapped vals should always succeed
(deftest capture-tapped
  (let [a (atom [])
        tap (fn [val] (swap! a conj val))
        vals (range 100)]
    (add-tap tap)
    (let [rets (doall (for [v vals] (tap> v)))]
      (is (every? true? rets))
      ;; collection in the tap is asynchronous, so loop until we see all of the values
      (loop []
        (if (< (count @a) 100)
          (recur)
          (is (= vals @a))))
      (remove-tap tap))))

;; "small" (< 1024) number of tapped vals should always succeed
(deftest capture-tapped-threaded
  (let [a (atom [])
        tap (fn [val] (swap! a conj val))
        vals (range 100)]
    (add-tap tap)
    (let [rets (doall (for [v vals] (tap-> v)))]
      (is (= vals rets))
      ;; collection in the tap is asynchronous, so loop until we see all of the values
      (loop []
        (if (< (count @a) 100)
          (recur)
          (is (= vals @a))))
      (remove-tap tap))))