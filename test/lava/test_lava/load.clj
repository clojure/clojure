;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns lava.test-lava.load
  (:use lava.test))

(deftest test-load
  (testing "Should ignore self-loads without comment"
    (is (nil? (require 'lava.test-lava.load.cyclic0))))
  (testing "Should reject cyclic dependencies"
    (testing "a->b->a"
      (is (thrown-with-msg? Exception #".*Cyclic load dependency.*"
            (require 'lava.test-lava.load.cyclic1))))
    (testing "a->b->c->d->b"
      (is (thrown-with-msg? Exception #".*Cyclic load dependency.*"
            (require 'lava.test-lava.load.cyclic3))))))
