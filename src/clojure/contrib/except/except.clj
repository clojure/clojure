;;  Copyright (c) Stephen C. Gilardi. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  except.clj
;;
;;  scgilardi (gmail)
;;  Created 07 July 2008

(clojure/in-ns 'clojure.contrib.except)
(clojure/refer 'clojure)

(clojure.contrib.lib/use '(clojure.contrib string))

(defn throw-if
  "Throws an exception with a message if pred is true. Arguments are:

      pred class? format format-args*

  class is optional and defaults to Exception. If present, it must be a
  Class in the tree under Throwable with a constructor that takes a single
  String.

  format is a string as documented for java.util.Formatter.

  format-args are zero or more objects that correspond to the format
  specifiers in format."
  [pred & args]
  (when pred
    (let [class-present (instance? Class (first args))
          args (if class-present args (cons Exception args))
          [class fmt & fmt-args] args
          ctor (.getConstructor (identity class) (into-array [String]))
          message (apply format fmt fmt-args)
          exception (.newInstance ctor (into-array [message]))
          raw-trace (.getStackTrace exception)
          boring? #(not= (.getMethodName %) "doInvoke")
          trace (into-array (drop 2 (drop-while boring? raw-trace)))]
      (.setStackTrace exception trace)
      (throw exception))))
