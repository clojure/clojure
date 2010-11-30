;;; base64.clj: Experimental Base-64 encoding and decoding

;; by Stuart Sierra, http://stuartsierra.com/ - encode
;; August 19, 2009
;; by Teemu Antti-Poika (anttipoi@gmail.com) - decode
;; May 12, 2010

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns ^{:doc "Base-64 encoding and decoding.  

  This is mainly here as an example.  It is much slower than the
  Apache Commons Codec implementation or sun.misc.BASE64Encoder."
       :author "Stuart Sierra"}
    clojure.contrib.base64
  (:import (java.io InputStream Writer ByteArrayInputStream 
                    ByteArrayOutputStream StringReader StringWriter)))

(def *base64-alphabet*
     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")

(defn- char-to-index-map 
  []
  (into {} 
        (map #(vec [(int %1) %2])
             *base64-alphabet* 
             (iterate inc 0))))

(defn encode
  "Encodes bytes of input, writing Base 64 text on output.  alphabet
  is a 65-character String containing the 64 characters to use in the
  encoding; the 65th character is the pad character.  line-length is
  the maximum number of characters per line, nil for no line breaks."
  [^InputStream input ^Writer output ^String alphabet line-length]
  (let [buffer (make-array Byte/TYPE 3)]
    (loop [line 0]
      (let [len (.read input buffer)]
        (when (pos? len)
          ;; Pre-boxing the bytes as Integers is more efficient for
          ;; Clojure's bit operations.
          (let [b0 (Integer/valueOf (int (aget buffer 0)))
                b1 (Integer/valueOf (int (aget buffer 1)))
                b2 (Integer/valueOf (int (aget buffer 2)))]
            (cond (= len 3)
                  (let [s0 (bit-and 0x3F (bit-shift-right b0 2))
                        s1 (bit-and 0x3F
                                    (bit-or (bit-shift-left b0 4)
                                            (bit-shift-right b1 4)))
                        s2 (bit-and 0x3F
                                    (bit-or (bit-shift-left b1 2)
                                            (bit-shift-right b2 6)))
                        s3 (bit-and 0x3F b2)]
                    (.append output (.charAt alphabet s0))
                    (.append output (.charAt alphabet s1))
                    (.append output (.charAt alphabet s2))
                    (.append output (.charAt alphabet s3)))

                  (= len 2)
                  (let [s0 (bit-and 0x3F (bit-shift-right b0 2))
                        s1 (bit-and 0x3F
                                    (bit-or (bit-shift-left b0 4)
                                            (bit-shift-right b1 4)))
                        s2 (bit-and 0x3F (bit-shift-left b1 2))]
                    (.append output (.charAt alphabet s0))
                    (.append output (.charAt alphabet s1))
                    (.append output (.charAt alphabet s2))
                    (.append output (.charAt alphabet 64)))

                  (= len 1)
                  (let [s0 (bit-and 0x3F (bit-shift-right b0 2))
                        s1 (bit-and 0x3F (bit-shift-left b0 4))]
                    (.append output (.charAt alphabet s0))
                    (.append output (.charAt alphabet s1))
                    (.append output (.charAt alphabet 64))
                    (.append output (.charAt alphabet 64)))))
            (if (and line-length (> (+ line 4) line-length))
              (do (.append output \newline)
                  (recur 0))
              (recur (+ line 4))))))))

(defn encode-str
  "Encodes String in base 64; returns a String.  If not specified,
  encoding is UTF-8 and line-length is nil."
  ([s] (encode-str s "UTF-8" nil))
  ([^String s ^String encoding line-length]
     (let [output (StringWriter.)]
       (encode (ByteArrayInputStream. (.getBytes s encoding))
               output *base64-alphabet* line-length)
       (.toString output))))

(defn- get-next-char
  "Consume and return next character from reader. Ignore and eat end-of-lines characters. Return -1 on end."
  [reader]
  (let [c (.read reader)]
    (if (or (= c 10) (= c 13))
      (recur reader)
      c)))

(defn decode
  "Decodes base64-encoded content from str-reader. Writes resulting bytes to out."
  [^StringReader str-reader ^ByteArrayOutputStream out]
  (let [next-char (get-next-char str-reader)]
    (when (not (= next-char -1))
      (let [c-to-int (char-to-index-map)
            content-char? (fn [i] (not (= i 64))) ; 64 is index for the pad character =
            sb0 (c-to-int next-char)
            sb1 (c-to-int (get-next-char str-reader))
            sb2 (c-to-int (get-next-char str-reader))
            sb3 (c-to-int (get-next-char str-reader))
            _ (when (not (and sb0 sb1 sb2 sb3)) 
                (throw (IllegalArgumentException. "Illegal Base64-encoded input: illegal characters or missing padding")))
            _ (when (not (and (content-char? sb0) (content-char? sb1)))
                (throw (IllegalArgumentException. "Illegal Base64-encoded input: padding char at illegl position")))
            b0 (bit-or
                (bit-shift-left sb0 2)
                (bit-shift-right (bit-and 0x30 sb1) 4))]
        (.write out b0)
        (when (content-char? sb2)
          (let [b1 (bit-or
                    (bit-shift-left (bit-and 0xF sb1) 4)
                    (bit-shift-right (bit-and 0x3C sb2) 2))]
            (.write out b1)
            (when (content-char? sb3)
              (let [b2 (bit-or
                        (bit-shift-left (bit-and 0x3 sb2) 6)
                        sb3)]
                (.write out b2)
                (recur str-reader out)))))))))
  

(defn decode-str
  "Decodes base64-encoded String using encoding. Encoding defaults to UTF-8."
  ([s] (decode-str s "UTF-8"))
  ([^String s ^String encoding]
     (when s
       (let [baos (ByteArrayOutputStream.)
             str-reader (StringReader. s)]
         (decode str-reader baos)
         (String. (.toByteArray baos) encoding)))))