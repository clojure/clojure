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

(defn- filename-filter-re
  [pattern]
  (proxy [FilenameFilter] []
    (accept
     [dir name]
     (boolean (re-matches pattern name)))))

(doseq file (.listFiles
             (File. "tests")
             (filename-filter-re #".*\.clj"))
  (printf "Loading %s\n" file)
  (load (.getPath file)))

(binding [*test-out* (java.io.PrintWriter. *out*)]
  (run-tests)
  (println))
