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
  (:use (clojure.contrib test-is)
        (clojure.contrib.test-clojure reader)))

(binding [*test-out* (java.io.PrintWriter. *out*)]
  (run-tests 'clojure.contrib.test-clojure.reader))
