;; Test routines for macro_utils.clj

;; by Konrad Hinsen
;; last updated May 6, 2009

;; Copyright (c) Konrad Hinsen, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.contrib.test-contrib.macro-utils
  (:use [clojure.contrib.test-is :only (deftest is are run-tests)]
	[clojure.contrib.macro-utils
	 :only (macrolet symbol-macrolet defsymbolmacro with-symbol-macros
		mexpand-1 mexpand mexpand-all)]
	[clojure.contrib.monads
	 :only (with-monad domonad)]))

(deftest macrolet-test
  (is (= (macroexpand-1
	   '(macrolet [(foo [form] `(~form ~form))]  (foo x)))
	 '(do (x x)))))

(deftest symbol-macrolet-test
  (is (= (macroexpand-1
	   '(symbol-macrolet [x xx y yy]
              (exp [a y] (x y))))
	 '(do (exp [a yy] (xx yy)))))
  (is (= (macroexpand-1
	   '(symbol-macrolet [def foo]
              (def def def)))
	 '(do (def def foo))))
  (is (= (macroexpand-1
	   '(symbol-macrolet [x foo z bar]
	      (let [a x b y x b] [a b x z])))
	 '(do (let* [a foo b y x b] [a b x bar]))))
  (is (= (macroexpand-1
	   '(symbol-macrolet [x foo z bar]
	      (fn ([x y] [x y z]) ([x y z] [x y z]))))
	 '(do (fn* ([x y] [x y bar]) ([x y z] [x y z])))))
  (is (= (macroexpand-1
	   '(symbol-macrolet [x foo z bar]
	      (fn f ([x y] [x y z]) ([x y z] [x y z]))))
	 '(do (fn* f ([x y] [x y bar]) ([x y z] [x y z])))))
  (is (= (nth (second (macroexpand-1
		       '(symbol-macrolet [x xx y yy z zz]
			  (domonad m [a x b y x z] [a b x z])))) 2)
	 '(do (m-bind xx (fn* ([a]
	      (m-bind yy (fn* ([b]
	      (m-bind zz (fn* ([x]
	      (m-result [a b x zz]))))))))))))))

(deftest symbol-test
  (defsymbolmacro sum-2-3 (plus 2 3))
  (is (= (macroexpand '(with-symbol-macros (+ 1 sum-2-3)))
	 '(do (+ 1 (plus 2 3)))))
  (is (= (macroexpand '(macrolet [(plus [a b] `(+ ~a ~b))] (+ 1 sum-2-3)))
	 '(do (+ 1 (clojure.core/+ 2 3)))))
  (ns-unmap *ns* 'sum-2-3))
