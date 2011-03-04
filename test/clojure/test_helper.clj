;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;

;;  clojure.test-helper
;;
;;  Utility functions shared by various tests in the Clojure
;;  test suite
;;
;;  tomfaulhaber (gmail)
;;  Created 04 November 2010

(ns clojure.test-helper
  (:use clojure.test))

(let [nl (System/getProperty "line.separator")] 
  (defn platform-newlines [s] (.replace s "\n" nl)))

(defn temp-ns
  "Create and return a temporary ns, using clojure.core + uses"
  [& uses]
  (binding [*ns* *ns*]
    (in-ns (gensym))
    (apply clojure.core/use 'clojure.core uses)
    *ns*))

(defmacro eval-in-temp-ns [& forms]
  `(binding [*ns* *ns*]
     (in-ns (gensym))
     (clojure.core/use 'clojure.core)
     (eval
      '(do ~@forms))))

(defn causes
  [^Throwable throwable]
  (loop [causes []
         t throwable]
    (if t (recur (conj causes t) (.getCause t)) causes)))

;; this is how I wish clojure.test/thrown? worked...
;; Does body throw expected exception, anywhere in the .getCause chain?
(defmethod assert-expr 'fails-with-cause?
  [msg [_ exception-class msg-re & body :as form]]
  `(try
   ~@body
   (report {:type :fail, :message ~msg, :expected '~form, :actual nil})
   (catch Throwable t#
     (if (some (fn [cause#]
                 (and
                  (= ~exception-class (class cause#))
                  (re-find ~msg-re (.getMessage cause#))))
               (causes t#))
       (report {:type :pass, :message ~msg,
                :expected '~form, :actual t#})
       (report {:type :fail, :message ~msg,
                :expected '~form, :actual t#})))))


(defn get-field
  "Access to private or protected field.  field-name is a symbol or
  keyword."
  ([klass field-name]
     (get-field klass field-name nil))
  ([klass field-name inst]
     (-> klass (.getDeclaredField (name field-name))
         (doto (.setAccessible true))
         (.get inst))))

(defn set-var-roots
  [maplike]
  (doseq [[var val] maplike]
    (alter-var-root var (fn [_] val))))

(defn with-var-roots*
  "Temporarily set var roots, run block, then put original roots back."
  [root-map f & args]
  (let [originals (doall (map (fn [[var _]] [var @var]) root-map))]
    (set-var-roots root-map)
    (try
     (apply f args)
     (finally
      (set-var-roots originals)))))

(defmacro with-var-roots
  [root-map & body]
  `(with-var-roots* ~root-map (fn [] ~@body)))

(defn exception
  "Use this function to ensure that execution of a program doesn't
  reach certain point."
  []
  (throw (new Exception "Exception which should never occur")))

(defmacro with-err-print-writer
  "Evaluate with err pointing to a temporary PrintWriter, and
   return err contents as a string."
  [& body]
  `(let [s# (java.io.StringWriter.)
         p# (java.io.PrintWriter. s#)]
     (binding [*err* p#]
       ~@body
       (str s#))))

(defmacro with-err-string-writer
  "Evaluate with err pointing to a temporary StringWriter, and
   return err contents as a string."
  [& body]
  `(let [s# (java.io.StringWriter.)]
     (binding [*err* s#]
       ~@body
       (str s#))))

(defmacro should-print-err-message
  "Turn on all warning flags, and test that error message prints
   correctly for all semi-reasonable bindings of *err*."
  [msg-re form]
  `(binding [*warn-on-reflection* true]
    (is (re-matches ~msg-re (with-err-string-writer (eval-in-temp-ns ~form))))
    (is (re-matches ~msg-re (with-err-print-writer (eval-in-temp-ns ~form))))))

(defmacro should-not-reflect
  "Turn on all warning flags, and test that reflection does not occur
   (as identified by messages to *err*)."
  [form]
  `(binding [*warn-on-reflection* true]
     (is (nil? (re-find #"^Reflection warning" (with-err-string-writer (eval-in-temp-ns ~form)))))
     (is (nil? (re-find #"^Reflection warning" (with-err-print-writer (eval-in-temp-ns ~form)))))))
