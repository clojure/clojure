;   Copyright (c) Chris Houser, April 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Functions for memory-mapping files, plus some functions that use a
; mmaped file for "normal" activies -- slurp, load-file, etc.

(clojure/in-ns 'clojure.contrib.mmap)
(clojure/refer 'clojure :exclude '(slurp load-file))

;(set! *warn-on-reflection* true)

(import '(java.nio ByteBuffer CharBuffer)
        '(java.io PushbackReader InputStream InputStreamReader FileInputStream))

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
  (load (PushbackReader. (InputStreamReader. (buffer-stream (mmap f))))))


(comment

(alias 'mmap 'clojure.contrib.mmap)

;---
; zip_filter.clj is 95KB
(def tf "/home/chouser/build/clojure/src/clj/clojure/boot.clj")
(println "\nload-file" tf)
(time (dotimes _ 5 (clojure/load-file tf))) ; 2595.672561 msecs
(time (dotimes _ 5 (mmap/load-file tf)))    ; 2323.142295 msecs

;---
; kern.log.0 is 961KB
(def tf "/var/log/kern.log.0")
(println "\nslurp" tf)
(time (dotimes _ 10 (.length (clojure/slurp tf)))) ; 815.068747 msecs
(time (dotimes _ 10 (.length (mmap/slurp tf))))    ; 141.090915 msecs

;---
; kern.log.0 is 961KB
(def tf "/var/log/kern.log.0")
(println "\nregex slurp large" tf)
(time (dotimes _ 10 (count (re-seq #"EXT3.*" (clojure/slurp tf))))) ; 889
(time (dotimes _ 10 (count (re-seq #"EXT3.*" (mmap/slurp tf)))))    ; 140

;---
; mmap.clj is about 3.1KB
(def tf "/home/chouser/proj/clojure-contrib/src/clojure/contrib/mmap/mmap.clj")
(println "\nregex slurp small" tf)

(time (dotimes _ 1000 (count (re-seq #"defn \\S*" (clojure/slurp tf))))) ; 318
(time (dotimes _ 1000 (count (re-seq #"defn \\S*" (mmap/slurp tf)))))    ; 118

)
