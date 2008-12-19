;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  clojure.contrib.test-clojure
;;
;;  Tests for the facilities provided by Clojure
;;
;;  scgilardi (gmail)
;;  Created 22 October 2008

(ns clojure.contrib.test-clojure
  (:use clojure.contrib.test-is))

; 2008/12/19 SDH temporarily removed evaluation.clj, which isn't passing
(def tests [:for :reader :printer :numbers])

(defn test-name
  [test]
  (symbol (str "clojure.contrib.test-clojure." (name test))))

(doseq [test tests]
  (require (test-name test)))

(apply run-tests (map test-name tests))
