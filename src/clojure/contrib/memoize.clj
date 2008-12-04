;;  Copyright (c) Stephen C. Gilardi. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.
;;
;;  File: memoize.clj
;;
;;  scgilardi (gmail)
;;  02 June 2008
;;
;;  Based on Common Lisp code from:
;;  http://asymmetrical-view.com/talks/lisp-presentation/lisp-presentation.pdf

(ns clojure.contrib.memoize)

(defn memoize
  "Returns a memoized version of a referentially transparent function. The
  memoized version of the function keeps a cache of the mapping from arguments
  to results and, when calls with the same arguments are repeated often, has
  higher performance at the expense of higher memory use."
  [function]
  (let [cache (atom {})]
    (fn [& args]
      (or (@cache args)
          (let [result (apply function args)]
            (swap! cache assoc args result)
            result)))))
