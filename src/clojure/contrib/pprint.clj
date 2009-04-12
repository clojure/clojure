;;; pprint.clj -- Pretty printer and Common Lisp compatible format function (cl-format) for Clojure

;; by Tom Faulhaber
;; April 3, 2009

;;   Copyright (c) Tom Faulhaber, April 2009. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; This module comprises two elements:
;; 1) A pretty printer for Clojure data structures, implemented in the function "pprint"
;; 2) A Common Lisp compatible format function, implemented as "cl-format" because
;;    Clojure is using the name "format" for its own format.
;;
;; The most complete documentation can be found at http://github.com/tomfaulhaber/cl-format
;; where I the markdown README is currently displayed. I will be moving it into 
;; clojure.contrib (either to the wiki or some other document structure) RSN.

(ns 
    #^{:doc "This module comprises two elements:
1) A pretty printer for Clojure data structures, implemented in the function \"pprint\"
2) A Common Lisp compatible format function, implemented as \"cl-format\" because
   Clojure is using the name \"format\" for its own format.

The most complete documentation can be found at http://github.com/tomfaulhaber/cl-format
where I the markdown README is currently displayed. I will be moving it into 
clojure.contrib (either to the wiki or some other document structure) RSN.
"}
    clojure.contrib.pprint
  (:use clojure.contrib.pprint.utilities)
  (:import [clojure.contrib.pprint PrettyWriter]))


(load "pprint/pprint_base")
(load "pprint/cl-format")
(load "pprint/dispatch")

nil
