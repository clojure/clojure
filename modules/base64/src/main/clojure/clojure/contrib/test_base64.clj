;   Copyright (c) Teemu Antti-Poika, May 2010. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; test namespace for clojure.contrib.base64


(ns clojure.contrib.test-base64
  (:use [clojure.test]
        [clojure.contrib.base64]))


(deftest t-encode-str
  (is (= (encode-str "") ""))
  (is (= (encode-str "f") "Zg=="))
  (is (= (encode-str "fo") "Zm8="))
  (is (= (encode-str "foo") "Zm9v"))
  (is (= (encode-str "foob") "Zm9vYg=="))
  (is (= (encode-str "fooba") "Zm9vYmE="))
  (is (= (encode-str "foobar") "Zm9vYmFy")))

(deftest t-encode-multiline-str
  (is (= (encode-str "This fits on one line" "UTF-8" 72) "VGhpcyBmaXRzIG9uIG9uZSBsaW5l"))
  (is (= (encode-str "This is written on multiple lines" "UTF-8" 10) "VGhpcyBpcyB3\ncml0dGVuIG9u\nIG11bHRpcGxl\nIGxpbmVz")))

(deftest t-decode-str
  (is (nil? (decode-str nil)))
  (is (= (decode-str "") ""))
  (is (= (decode-str "Zg==") "f"))
  (is (= (decode-str "Zm8=") "fo"))
  (is (= (decode-str "Zm9v") "foo"))
  (is (= (decode-str "Zm9vYg==") "foob"))
  (is (= (decode-str "Zm9vYmE=") "fooba"))
  (is (= (decode-str "Zm9vYmFy") "foobar")))

(deftest t-decode-multiline-str
  (is (= (decode-str "VGhpcyBpcyB3\ncml0dGVuIG9u\nIG11bHRpcGxl\nIGxpbmVz") "This is written on multiple lines"))
  (is (= (decode-str "VGhpcyBpcyB3\r\ncml0dGVuIG9u\r\nIG11bHRpcGxl\r\nIGxpbmVz") "This is written on multiple lines")))

(deftest t-decode-str-fails-on-incorrect-charcters-in-input
  (is (thrown? IllegalArgumentException (decode-str ";AB=")))
  (is (thrown? IllegalArgumentException (decode-str "A;B=")))
  (is (thrown? IllegalArgumentException (decode-str "AB;=")))
  (is (thrown? IllegalArgumentException (decode-str "ABC;"))))

(deftest t-decode-str-fails-when-padding-character-occurs-at-illegal-positions
  (is (thrown? IllegalArgumentException (decode-str "=ABC")))
  (is (thrown? IllegalArgumentException (decode-str "A=BC"))))

(deftest t-decode-str-fails-when-input-length-is-not-divisble-with-four
  (is (thrown? IllegalArgumentException (decode-str "ABCDE"))))