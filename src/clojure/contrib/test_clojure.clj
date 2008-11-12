;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  clojure.contrib.test-clojure
;;
;;  Tests for the facilities provided by Clojure
;;
;;  scgilardi (gmail)
;;  Created 22 October 2008

(ns clojure.contrib.test-clojure
  (:import (java.io File FilenameFilter))
  (:use clojure.contrib.test-is))

(def tests [:reader :printer :numbers])

(defn test-name
  [test]
  (symbol (str "clojure.contrib.test-clojure." (name test))))

(doseq [test tests]
  (require (test-name test)))

(binding [*test-out* (java.io.PrintWriter. *out*)]
  (doseq [test tests]
    (println "\n\n=====>" test)
    (run-tests (test-name test))))
