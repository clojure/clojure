;;; base64.clj: Experimental Base-64 encoding and (later) decoding

;; by Stuart Sierra, http://stuartsierra.com/
;; August 19, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns #^{:doc "Base-64 encoding and (maybe later) decoding.  

  This is mainly here as an example.  It is much slower than the
  Apache Commons Codec implementation or sun.misc.BASE64Encoder."
       :author "Stuart Sierra"}
    clojure.contrib.base64
  (:import (java.io InputStream Writer ByteArrayInputStream
                    StringWriter)))

(def *base64-alphabet*
     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")

(defn encode
  "Encodes bytes of input, writing Base 64 text on output.  alphabet
  is a 65-character String containing the 64 characters to use in the
  encoding; the 65th character is the pad character.  line-length is
  the maximum number of characters per line, nil for no line breaks."
  [#^InputStream input #^Writer output #^String alphabet line-length]
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
  ([#^String s #^String encoding line-length]
     (let [output (StringWriter.)]
       (encode (ByteArrayInputStream. (.getBytes s encoding))
               output *base64-alphabet* line-length)
       (.toString output))))


;;; tests 

;; (deftest t-encode-str
;;   (is (= (encode-str "") ""))
;;   (is (= (encode-str "f") "Zg=="))
;;   (is (= (encode-str "fo") "Zm8="))
;;   (is (= (encode-str "foo") "Zm9v"))
;;   (is (= (encode-str "foob") "Zm9vYg=="))
;;   (is (= (encode-str "fooba") "Zm9vYmE="))
;;   (is (= (encode-str "foobar") "Zm9vYmFy")))
