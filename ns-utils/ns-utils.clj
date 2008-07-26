;;  Copyright (c) Stephen C. Gilardi. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.
;;
;;  ns-utils.clj
;;
;;  Namespace Utilities
;;
;;    'ns'              returns the namespace named by a symbol or throws
;;                      if the namespace does not exist
;;
;;    'ns-vars'         returns a sorted seq of symbols naming public vars
;;                      in a namespace
;;
;;    'print-dir'       prints a sorted directory of public vars in a
;;                      namespace
;;
;;    'print-docs'      prints documentation for the public vars in a
;;                      namespace
;;
;;  Convenience
;;
;;    'vars'            returns a sorted seq of symbols naming public vars
;;                      in a namespace (macro)
;;
;;    'dir'             prints a sorted directory of public vars in a
;;                      namespace (macro)
;;
;;    'docs'            prints documentation for the public vars in a
;;                      namespace (macro)
;;
;;  scgilardi (gmail)
;;  23 April 2008

(clojure/in-ns 'clojure-contrib.ns-utils)
(clojure/refer 'clojure)

;; Namespace Utilities

(defn ns
  "Returns the namespace named by ns-sym or throws if the
  namespace does not exist"
  [ns-sym]
  (let [ns (find-ns ns-sym)]
    (when-not ns
      (throw (new Exception (str "Unable to find namespace: "
                                 ns-sym))))
    ns))

(defn ns-vars
  "Returns a sorted seq of symbols naming public vars in
  a namespace"
  [ns]
  (sort (map first (ns-publics ns))))

(defn print-dir
  "Prints a sorted directory of public vars in a namespace"
  [ns]
  (doseq item (ns-vars ns)
    (println item)))

(defn print-docs
  "Prints documentation for the public vars in a namespace"
  [ns]
  (doseq item (ns-vars ns)
    (print-doc (ns-resolve ns item))))

;; Convenience

(defmacro vars
  "Returns a sorted seq of symbols naming public vars in
  a namespace"
  [nsname]
  `(ns-vars (ns '~nsname)))

(defmacro dir
  "Prints a sorted directory of public vars in a namespace"
  [nsname]
  `(print-dir (ns '~nsname)))

(defmacro docs
  "Prints documentation for the public vars in a namespace"
  [nsname]
  `(print-docs (ns '~nsname)))
