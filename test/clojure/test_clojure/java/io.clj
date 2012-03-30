;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test-clojure.java.io
  (:use clojure.test clojure.java.io
        [clojure.test-helper :only [platform-newlines]])
  (:import (java.io File BufferedInputStream
                    FileInputStream InputStreamReader InputStream
                    FileOutputStream OutputStreamWriter OutputStream
                    ByteArrayInputStream ByteArrayOutputStream)
           (java.net URL URI Socket ServerSocket)))

(defn temp-file
  [prefix suffix]
  (doto (File/createTempFile prefix suffix)
    (.deleteOnExit)))

(deftest test-spit-and-slurp
  (let [f (temp-file "clojure.java.io" "test")
        content (apply str (concat "a" (repeat 500 "\u226a\ud83d\ude03")))]
    (spit f content)
    (is (= content (slurp f)))
    ;; UTF-16 must be last for the following test
    (doseq [enc [ "UTF-8" "UTF-16BE" "UTF-16LE" "UTF-16" ]]
      (spit f content :encoding enc)
      (is (= content (slurp f :encoding enc))))
    (testing "deprecated arity"
      (is (=
           (platform-newlines "WARNING: (slurp f enc) is deprecated, use (slurp f :encoding enc).\n")
           (with-out-str
             (is (= content (slurp f "UTF-16")))))))))

(deftest test-streams-defaults
  (let [f (temp-file "clojure.java.io" "test-reader-writer")
        content "testing"]
    (try
      (is (thrown? Exception (reader (Object.))))
      (is (thrown? Exception (writer (Object.))))

      (are [write-to read-from] (= content (do
                                             (spit write-to content :encoding "UTF-8")
                                             (slurp read-from :encoding "UTF-8")))
           f f
           (.getAbsolutePath f) (.getAbsolutePath f)
           (.toURL f) (.toURL f)
           (.toURI f) (.toURI f)
           (FileOutputStream. f) (FileInputStream. f)
           (OutputStreamWriter. (FileOutputStream. f) "UTF-8") (reader f :encoding "UTF-8")
           f (FileInputStream. f)
           (writer f :encoding "UTF-8") (InputStreamReader. (FileInputStream. f) "UTF-8"))

      (is (= content (slurp (.getBytes content "UTF-8"))))
      (is (= content (slurp (.toCharArray content))))
      (finally
       (.delete f)))))

(defn bytes-should-equal [byte-array-1 byte-array-2 msg]
  (is (= @#'clojure.java.io/byte-array-type (class byte-array-1) (class byte-array-2)) msg)
  (is (= (into []  byte-array-1) (into []  byte-array-2)) msg))

(defn data-fixture
  "in memory fixture data for tests"
  [encoding]
  (let [s (apply str (concat "a" (repeat 500 "\u226a\ud83d\ude03")))
        bs (.getBytes s encoding)
        cs (.toCharArray s)
        i (ByteArrayInputStream. bs)
        ;; Make UTF-8 encoding explicit for the InputStreamReader and
        ;; OutputStreamWriter, since some JVMs use a different default
        ;; encoding.
        r (InputStreamReader. i "UTF-8")
        o (ByteArrayOutputStream.)
        w (OutputStreamWriter. o "UTF-8")]
    {:bs bs
     :i i
     :r r
     :o o
     :s s
     :cs cs
     :w w}))

(deftest test-copy
  (dorun
   (for [{:keys [in out flush] :as test}
         [{:in :i :out :o}
          {:in :i :out :w}
          {:in :r :out :o}
          {:in :r :out :w}
          {:in :cs :out :o}
          {:in :cs :out :w}
          {:in :bs :out :o}
          {:in :bs :out :w}]
         
         opts
         [{} {:buffer-size 16} {:buffer-size 256}]]
     (let [{:keys [s o] :as d} (data-fixture "UTF-8")]
       (apply copy (in d) (out d) (flatten (vec opts)))
       #_(when (= out :w) (.flush (:w d)))
       (.flush (out d))
       (bytes-should-equal (.getBytes s "UTF-8")
                           (.toByteArray o)
                           (str "combination " test opts))))))

(deftest test-copy-encodings
  (doseq [enc [ "UTF-8" "UTF-16" "UTF-16BE" "UTF-16LE" ]]
    (testing (str "from inputstream " enc " to writer UTF-8")
      (let [{:keys [i s o w bs]} (data-fixture enc)]
        (copy i w :encoding enc :buffer-size 16)
        (.flush w)
        (bytes-should-equal (.getBytes s "UTF-8") (.toByteArray o) "")))
    (testing (str "from reader UTF-8 to output-stream " enc)
      (let [{:keys [r o s]} (data-fixture "UTF-8")]
        (copy r o :encoding enc :buffer-size 16)
        (bytes-should-equal (.getBytes s enc) (.toByteArray o) "")))))

(deftest test-as-file
  (are [result input] (= result (as-file input))
       (File. "foo") "foo"
       (File. "bar") (File. "bar")
       (File. "baz") (URL. "file:baz")
       (File. "bar+baz") (URL. "file:bar+baz")
       (File. "bar baz qux") (URL. "file:bar%20baz%20qux")
       (File. "quux") (URI. "file:quux")
       nil nil))

(deftest test-resources-with-spaces
  (let [file-with-spaces (temp-file "test resource 2" "txt")
        url (as-url (.getParentFile file-with-spaces))
        loader (java.net.URLClassLoader. (into-array [url]))
        r (resource (.getName file-with-spaces) loader)]
    (is (= r (as-url file-with-spaces)))
    (spit r "foobar")
    (is (= "foobar" (slurp r)))))

(deftest test-file
  (are [result args] (= (File. result) (apply file args))
       "foo" ["foo"]
       "foo/bar" ["foo" "bar"]
       "foo/bar/baz" ["foo" "bar" "baz"]))
(deftest test-as-url
  (are [file-part input] (= (URL. (str "file:" file-part)) (as-url input))
       "foo" "file:foo"
       "baz" (URL. "file:baz")
       "quux" (URI. "file:quux"))
  (is (nil? (as-url nil))))

(deftest test-delete-file
  (let [file (temp-file "test" "deletion")
        not-file (File. (str (java.util.UUID/randomUUID)))]
    (delete-file (.getAbsolutePath file))
    (is (not (.exists file)))
    (is (thrown? java.io.IOException (delete-file not-file)))
    (is (= :silently (delete-file not-file :silently)))))

(deftest test-as-relative-path
  (testing "strings"
    (is (= "foo" (as-relative-path "foo"))))
  (testing "absolute path strings are forbidden"
    (is (thrown? IllegalArgumentException (as-relative-path (.getAbsolutePath (File. "baz"))))))
  (testing "relative File paths"
    (is (= "bar" (as-relative-path (File. "bar")))))
  (testing "absolute File paths are forbidden"
    (is (thrown? IllegalArgumentException (as-relative-path (File. (.getAbsolutePath (File. "quux"))))))))

(defn stream-should-have [stream expected-bytes msg]
  (let [actual-bytes (byte-array (alength expected-bytes))]
    (.read stream actual-bytes)
    (is (= -1 (.read stream)) (str msg " : should be end of stream"))
    (is (= (seq expected-bytes) (seq actual-bytes)) (str msg " : byte arrays should match"))))

(deftest test-input-stream
  (let [file (temp-file "test-input-stream" "txt")
        content (apply str (concat "a" (repeat 500 "\u226a\ud83d\ude03")))
        bytes (.getBytes content "UTF-8")]
    (spit file content)
    (doseq [[expr msg]
            [[file File]
             [(FileInputStream. file) FileInputStream]
             [(BufferedInputStream. (FileInputStream. file)) BufferedInputStream]
             [(.. file toURI) URI]
             [(.. file toURI toURL) URL]
             [(.. file toURI toURL toString) "URL as String"]
             [(.. file toString) "File as String"]]]
      (with-open [s (input-stream expr)]
        (stream-should-have s bytes msg)))))

(deftest test-streams-buffering
  (let [data (.getBytes "")]
    (is (instance? java.io.BufferedReader (reader data)))
    (is (instance? java.io.BufferedWriter (writer (java.io.ByteArrayOutputStream.))))
    (is (instance? java.io.BufferedInputStream (input-stream data)))
    (is (instance? java.io.BufferedOutputStream (output-stream (java.io.ByteArrayOutputStream.))))))

(deftest test-resource
  (is (nil? (resource "non/existent/resource")))
  (is (instance? URL (resource "clojure/core.clj")))
  (let [file (temp-file "test-resource" "txt")
        url (as-url (.getParentFile file))
        loader (java.net.URLClassLoader. (into-array [url]))]
    (is (nil? (resource "non/existent/resource" loader)))
    (is (instance? URL (resource (.getName file) loader)))))

(deftest test-make-parents
  (let [tmp (System/getProperty "java.io.tmpdir")]
    (delete-file (file tmp "test-make-parents" "child" "grandchild") :silently)
    (delete-file (file tmp "test-make-parents" "child") :silently)
    (delete-file (file tmp "test-make-parents") :silently)
    (make-parents tmp "test-make-parents" "child" "grandchild")
    (is (.isDirectory (file tmp "test-make-parents" "child")))
    (is (not (.isDirectory (file tmp "test-make-parents" "child" "grandchild"))))
    (delete-file (file tmp "test-make-parents" "child"))
    (delete-file (file tmp "test-make-parents"))))

(deftest test-socket-iofactory
  (let [port 65321
        server-socket (ServerSocket. port)
        client-socket (Socket. "localhost" port)]
    (try
      (is (instance? InputStream (input-stream client-socket)))
      (is (instance? OutputStream (output-stream client-socket)))
      (finally (.close server-socket)
               (.close client-socket)))))
