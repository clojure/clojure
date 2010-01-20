;   Copyright (c) Chris Houser, April 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Functions for memory-mapping files, plus some functions that use a
; mmaped file for "normal" activies -- slurp, load-file, etc.

(ns 
  #^{:author "Chris Houser",
     :doc "Functions for memory-mapping files, plus some functions that use a
mmaped file for \"normal\" activies -- slurp, load-file, etc."}
  clojure.contrib.mmap
    (:refer-clojure :exclude (slurp load-file))
    (:import (java.nio ByteBuffer CharBuffer)
             (java.io PushbackReader InputStream InputStreamReader
                      FileInputStream)))

;(set! *warn-on-reflection* true)

(def READ_ONLY #^{:private true}
  (java.nio.channels.FileChannel$MapMode/READ_ONLY))

(defn mmap
  "Memory-map the file named f.  Returns a ByteBuffer."
  [f]
  (let [channel (.getChannel (FileInputStream. f))]
    (.map channel READ_ONLY 0 (.size channel))))

(defn slurp
  "Reads the file named by f and returns it as a string."
  [#^String f]
  (.. java.nio.charset.Charset (forName "UTF-8")
      (newDecoder) (decode (mmap f))))

(defn buffer-stream
  "Returns an InputStream for a ByteBuffer, such as returned by mmap."
  [#^ByteBuffer buf]
  (proxy [InputStream] []
    (available [] (.remaining buf))
    (read
      ([] (if (.hasRemaining buf) (.get buf) -1))
      ([dst offset len] (let [actlen (min (.remaining buf) len)]
                          (.get buf dst offset actlen)
                          (if (< actlen 1) -1 actlen))))))

(defn load-file [f]
  "Like clojure.lang/load-file, but uses mmap internally."
  (with-open [rdr (-> f mmap buffer-stream InputStreamReader. PushbackReader.)]
    (load-reader rdr)))


(comment

(alias 'mmap 'clojure.contrib.mmap)
(alias 'core 'clojure.core)

;---
; zip_filter.clj is 95KB
(def tf "/home/chouser/build/clojure/src/clj/clojure/core.clj")
(println "\nload-file" tf)
(time (dotimes [_ 5] (core/load-file tf))) ; 5420.177813 msecs
(time (dotimes [_ 5] (mmap/load-file tf))) ; 7946.854434 msecs -- not so good

;---
; kern.log.0 is 961KB
(def tf "/var/log/kern.log.0")
(println "\nslurp" tf)
(time (dotimes [_ 10] (.length (core/slurp tf)))) ; 435.767226 msecs
(time (dotimes [_ 10] (.length (mmap/slurp tf)))) ;  93.176858 msecs

;---
; kern.log.0 is 961KB
(def tf "/var/log/kern.log.0")
(println "\nregex slurp large" tf)
(time (dotimes [_ 10] (count (re-seq #"EXT3.*" (core/slurp tf))))) ; 416
(time (dotimes [_ 10] (count (re-seq #"EXT3.*" (mmap/slurp tf))))) ; 101

;---
; mmap.clj is about 3.1KB
(def tf "/home/chouser/proj/clojure-contrib/src/clojure/contrib/mmap.clj")
(println "\nregex slurp small" tf)

(time (dotimes [_ 1000] (count (re-seq #"defn \S*" (core/slurp tf))))) ; 308
(time (dotimes [_ 1000] (count (re-seq #"defn \S*" (mmap/slurp tf))))) ; 198

)
