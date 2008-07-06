;;  Copyright (c) Stephen C. Gilardi. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.
;;
;;  string.clj
;;
;;  String functions
;;
;;  scgilardi (gmail)
;;  Created: 6 July 2008

(clojure/in-ns 'string)
(clojure/refer 'clojure)

;; until Clojure supports "..." arguments, calling String/format directly
;; is just ugly enough, and could be commonly used enough to warrant a
;; Clojure wrapper.
;;
;; (let [name "world"] (format "Hello, %s!" name)) ==> "Hello, world!"

(defn format
  "Returns a string using the specified format and arguments. See
  java.util.Formatter for format string syntax."
  [fmt & args]
  (String/format fmt (to-array args)))
