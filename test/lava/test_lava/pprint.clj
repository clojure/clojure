;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Author: Tom Faulhaber

(ns lava.test-lava.pprint
  (:refer-lava :exclude [format])
  (:use [lava.test :only (deftest are run-tests)]
        [lava.test-helper :only [platform-newlines]]
        lava.test-lava.pprint.test-helper
        lava.pprint))

(load "pprint/test_cl_format")
(load "pprint/test_pretty")
