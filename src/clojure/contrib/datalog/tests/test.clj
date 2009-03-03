;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  test.clj
;;
;;  A Clojure implementation of Datalog -- Tests
;;
;;  straszheimjeffrey (gmail)
;;  Created 11 Feburary 2009

(ns clojure.contrib.datalog.tests.test.clj
  (:use clojure.contrib.test-is))

(def tests [:test-util
            :test-database
            :test-literals
            :test-rules
            :test-magic
            :test-softstrat])

(defn test-name
  [test]
  (symbol (str "clojure.contrib.datalog.tests." (name test))))

(doseq [test tests]
  (require (test-name test)))

(apply run-tests (map test-name tests))



;; End of file
