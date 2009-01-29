;;; apply_macro.clj: make macros behave like functions

;; by Stuart Sierra, http://stuartsierra.com/
;; January 28, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; Don't use this.  I mean it.  It's evil.  How evil?  You can't
;; handle it, that's how evil it is.  That's right.  I did it so you
;; don't have to, ok?  Look but don't touch.  Use this lib and you'll
;; go blind.


(ns apply-macro)

;; Copied from clojure.core/spread, which is private.
(defn- spread
  "Flatten final argument list as in apply."
  [arglist]
  (cond
   (nil? arglist) nil
   (nil? (rest arglist)) (seq (first arglist))
   :else (cons (first arglist) (spread (rest arglist)))))

(defmacro apply-macro
  "This is evil.  Don't ever use it.  It makes a macro behave like a
  function.  Seriously, how messed up is that?

  Evaluates all args, then uses them as arguments to the macro as with
  apply.

  (def things [true true false])
  (apply-macro and things)
  ;; Expands to:  (and true true false)"
  [macro & args]
  (cons macro (spread (map eval args))))
