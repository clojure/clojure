;;  Copyright (c) Frantisek Sodomka. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.

(ns clojure.contrib.test-clojure.java-interop
  (:use clojure.contrib.test-is))

; http://clojure.org/java_interop
; http://clojure.org/compilation

; Member access
; . ..
; doto

(deftest test-dot
  (are (= _1 _2)
      (. Integer MAX_VALUE) Integer/MAX_VALUE
      (. Integer MAX_VALUE) java.lang.Integer/MAX_VALUE ))

(deftest test-double-dot
  (is (= (.. System (getProperties) (get "os.name"))
         (. (. System (getProperties)) (get "os.name")))))

(deftest test-doto
  (let [doto-map (doto (new java.util.HashMap)
                   (.put "a" 1)
                   (.put "b" 2))]
    (is (= (class doto-map) java.util.HashMap))
    (is (= doto-map {"a" 1 "b" 2}))))


; new

; instance?

; set!

; memfn

; bean
; proxy, proxy-super

; bases, supers

; Arrays: alength aget aset make-array to-array into-array to-array-2d aclone
;   float-array, int-array, etc
;   amap, areduce

; Type Hints, *warn-on-reflection*
;   #^ints, #^floats, #^longs, #^doubles

; Coercions: int, long, float, double, char, boolean, short, byte
;   num
;   ints/longs/floats/doubles
