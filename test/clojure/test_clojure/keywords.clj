;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.keywords
  (:use clojure.test))

(let [this-ns (str (.name *ns*))]
  (deftest test-find-keyword
    :foo
    ::foo
    (let [absent-keyword-sym (gensym "absent-keyword-sym")]
      (are [result lookup] (= result (find-keyword lookup))
           :foo :foo
           :foo 'foo
           :foo "foo"
           nil absent-keyword-sym
           nil (str absent-keyword-sym))
      (are [result lookup] (= result (find-keyword this-ns lookup))
           ::foo "foo"
           nil (str absent-keyword-sym)))))

(deftest arity-exceptions
  (is (thrown-with-msg? IllegalArgumentException #"Wrong number of args \(0\) passed to: :kw" (:kw)))
  (is (thrown-with-msg? IllegalArgumentException #"Wrong number of args \(20\) passed to: :foo/bar" (apply :foo/bar (range 20))))
  (is (thrown-with-msg? IllegalArgumentException #"Wrong number of args \(21\) passed to: :foo/bar" (apply :foo/bar (range 21))))
  (is (thrown-with-msg? IllegalArgumentException #"Wrong number of args \(22\) passed to: :foo/bar" (apply :foo/bar (range 22)))))
