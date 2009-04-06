;   Copyright (c) Tom Faulhaber, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.contrib.pprint.examples.hexdump
  (:use clojure.contrib.pprint)
  (:gen-class (:main true)))

(def *buffer-length* 1024)

(defn zip-array [base-offset arr]
  (let [grouped (partition 16 arr)]
    (first (map-passing-context
            (fn [line offset]
              [[offset 
                (map #(if (neg? %) (+ % 256) %) line)
                (- 16 (count line))
                (map #(if (<= 32 % 126) (char %) \.) line)]
               (+ 16 offset)])
            base-offset grouped))))


(defn hexdump 
  ([in-stream] (hexdump in-stream true 0))
  ([in-stream out-stream] (hexdump [in-stream out-stream 0]))
  ([in-stream out-stream offset] 
     (let [buf (make-array Byte/TYPE *buffer-length*)]
       (loop [offset offset
              count (.read in-stream buf)]
         (if (neg? count)
           nil
           (let [bytes (take count buf)
                 zipped (zip-array offset bytes)]
             (cl-format out-stream 
                        "~:{~8,'0X: ~2{~8@{~#[   ~:;~2,'0X ~]~}  ~}~v@{   ~}~2{~8@{~A~} ~}~%~}" 
                        zipped) 
             (recur (+ offset *buffer-length*) (.read in-stream buf))))))))

(defn hexdump-file 
  ([file-name] (hexdump-file file-name true))
  ([file-name stream] 
     (with-open [s (java.io.FileInputStream. file-name)] 
       (hexdump s))))

;; I don't quite understand how to invoke main funcs w/o AOT yet
(defn -main [& args]
  (hexdump-file (first args)))

