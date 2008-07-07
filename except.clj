;;  Copyright (c) Stephen C. Gilardi. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.
;;
;;  except.clj
;;
;;  scgilardi (gmail)
;;  Created 07 July 2008

(clojure/in-ns 'except)
(clojure/refer 'clojure)

(lib/use string)

(defn throw-if
  "Throws an exception with a message if pred is true.  Arguments are:
    pred class? format format-args*
  where class is optional and defaults to Exception, format is a string
  as documented for java.util.Formatter, and format-args are objects
  corresponding to format specifiers in format."
  [pred & args]
  (if pred
    (let [[arg0 arg1 & more] args
          [class fmt fmt-args] (if (instance? Class arg0)
                                 [arg0 arg1 more]
                                 [Exception arg0 (cons arg1 more)])
          ctor (.getConstructor (identity class) (into-array [String]))
          message (apply format fmt fmt-args)
          exception (.newInstance ctor (into-array [message]))
          raw-trace (.getStackTrace exception)
          boring? #(not= (.getMethodName %) "doInvoke")
          trace (drop 2 (drop-while boring? raw-trace))]
      (.setStackTrace exception (into-array trace))
      (throw exception))))
