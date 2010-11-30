;   Copyright (c) Chris Houser, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Utilities meant to be used interactively at the REPL

;; Deprecated in 1.2: source, get-source, and apropos. These are
;; available in clojure.repl as source, source-fn, and apropos, respectively.

(ns 
  ^{:author "Chris Houser, Christophe Grand, Stephen Gilardi, Michel Salim",
     :doc "Utilities meant to be used interactively at the REPL"}
  clojure.contrib.repl-utils
  (:import (java.io File LineNumberReader InputStreamReader PushbackReader)
           (java.lang.reflect Modifier Field Method Constructor)
           (clojure.lang RT Compiler Compiler$C))
  (:use [clojure.contrib.seq :only (indexed)]
        [clojure.java.browse :only (browse-url)]
        [clojure.string :as str :only ()]))

;; ----------------------------------------------------------------------
;; Examine Java classes

(defn- sortable [t]
  (str/replace t #"\d+" #(format "%04d" (Integer/parseInt %))))

(defn- param-str [m]
  (str " (" (str/join
              "," (map (fn [[c i]]
                         (if (> i 3)
                           (str (.getSimpleName c) "*" i)
                           (str/join "," (replicate i (.getSimpleName c)))))
                       (reduce (fn [pairs y] (let [[x i] (peek pairs)]
                                               (if (= x y)
                                                 (conj (pop pairs) [y (inc i)])
                                                 (conj pairs [y 1]))))
                               [] (.getParameterTypes m))))
  ")"))

(defn- member-details [m]
  (let [static? (Modifier/isStatic (.getModifiers m))
        method? (instance? Method m)
        ctor?   (instance? Constructor m)
        text (if ctor?
               (str "<init>" (param-str m))
               (str
                 (when static? "static ")
                 (.getName m) " : "
                 (if method?
                   (str (.getSimpleName (.getReturnType m)) (param-str m))
                   (str (.getSimpleName (.getType m))))))]
    (assoc (bean m)
           :static? static?
           :method? method?
           :field? (instance? Field m)
           :ctor? ctor?
           :sort-val [(not static?) method? (sortable text)]
           :text text
           :member m)))

(defn show
  "With one arg prints all static and instance members of x or (class x).
  Each member is listed with a number which can be given as 'selector'
  to return the member object -- the REPL will print more details for
  that member.

  The selector also may be a string or regex, in which case only
  members whose names match 'selector' as a case-insensitive regex
  will be printed.

  Finally, the selector also may be a predicate, in which case only
  members for which the predicate returns true will be printed.  The
  predicate will be passed a single argument, a map that includes the
  :text that will be printed and the :member object itself, as well as
  all the properies of the member object as translated by 'bean'.

  Examples: (show Integer)  (show [])  (show String 23)  (show String \"case\")"
  ([x] (show x (constantly true)))
  ([x selector]
      (let [c (if (class? x) x (class x))
            members (sort-by :sort-val
                             (map member-details
                                  (concat (.getFields c)
                                          (.getMethods c)
                                          (.getConstructors c))))]
        (if (number? selector)
          (:member (nth members selector))
          (let [pred (if (ifn? selector)
                       selector
                       #(re-find (re-pattern (str "(?i)" selector)) (:name %)))]
            (println "=== " (Modifier/toString (.getModifiers c)) c " ===")
            (doseq [[i m] (indexed members)]
              (when (pred m)
                (printf "[%2d] %s\n" i (:text m)))))))))

;; ----------------------------------------------------------------------
;; Handle Ctrl-C keystrokes

(def ^{:doc "Threads to stop when Ctrl-C is pressed.  See 'add-break-thread!'"}
  break-threads (atom {}))

(let [first-time (atom true)]
  (defn start-handling-break
    "Register INT signal handler.  After calling this, Ctrl-C will cause
    all break-threads to be stopped.  See 'add-break-thread!'"
    []
    (when (= :need-init
             (swap! first-time
                    {:need-init false, false false, true :need-init}))
      (sun.misc.Signal/handle
        (sun.misc.Signal. "INT")
        (proxy [sun.misc.SignalHandler] []
          (handle [sig]
            (let [exc (Exception. (str sig))]
              (doseq [tref (vals @break-threads) :when (.get tref)]
                (.stop (.get tref) exc)))))))))

(defn add-break-thread!
  "Add the given thread to break-threads so that it will be stopped
  any time the user presses Ctrl-C.  Calls start-handling-break for
  you.  Adds the current thread if none is given."
  ([] (add-break-thread! (Thread/currentThread)))
  ([t]
    (start-handling-break)
    (let [tref (java.lang.ref.WeakReference. t)]
      (swap! break-threads assoc (.getId t) tref))))

;; ----------------------------------------------------------------------
;; Compiler hooks

(defn expression-info
  "Uses the Clojure compiler to analyze the given s-expr.  Returns
  a map with keys :class and :primitive? indicating what the compiler
  concluded about the return value of the expression.  Returns nil if
  not type info can be determined at compile-time.
  
  Example: (expression-info '(+ (int 5) (float 10)))
  Returns: {:class float, :primitive? true}"
  [expr]
  (let [fn-ast (Compiler/analyze Compiler$C/EXPRESSION `(fn [] ~expr))
        expr-ast (.body (first (.methods fn-ast)))]
    (when (.hasJavaClass expr-ast)
      {:class (.getJavaClass expr-ast)
       :primitive? (.isPrimitive (.getJavaClass expr-ast))})))

;; ----------------------------------------------------------------------
;; scgilardi at gmail

(defn run*
  "Loads the specified namespace and invokes its \"main\" function with
  optional args."
  [ns-sym & args]
  (require ns-sym :reload-all)
  (apply (ns-resolve ns-sym 'main) args))

(defmacro run
  "Loads the specified namespace and invokes its \"main\" function with
  optional args. ns-name is not evaluated."
  [ns-name & args]
  `(run* '~ns-name ~@args))


(load "repl_utils/javadoc")
