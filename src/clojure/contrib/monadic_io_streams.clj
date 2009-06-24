;; Monadic I/O

;; by Konrad Hinsen
;; last updated June 24, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns
  #^{:author "Konrad Hinsen"
     :doc "Monadic I/O with Java input/output streams
           Defines monadic I/O statements to be used in a state monad
           with an input or output stream as the state. The macro
           monadic-io creates a stream, runs a monadic I/O statement
           on it, and closes the stream. This structure permits the
           definition of purely functional compound I/O statements
           which are applied to streams that can never escape from the
           monadic statement sequence."}
  clojure.contrib.monadic-io-streams
  (:refer-clojure :exclude (read-line print println flush))
  (:use [clojure.contrib.monads
	 :only (with-monad domonad state-m state-m-until)])
  (:use [clojure.contrib.generic.functor :only (fmap)])
  (:use [clojure.contrib.duck-streams :only (reader writer)]))

;
; Wrap the state into a closure to make sure that "evil" code
; can't obtain the stream using fetch-state and manipulate it.
;
(let [key (Object.)
      lock (fn [state] (fn [x] (if (identical? x key) state nil)))
      unlock (fn [state] (state key))]

  ;
  ; Basic stream I/O statements as provided by Java
  ;
  (defn read-char
    "Read a single character"
    []
    (fn [s] [(.read (unlock s)) s]))

  (defn read-line
    "Read a single line"
    []
    (fn [s] [(.readLine (unlock s)) s]))

  (defn skip-chars
    "Skip n characters"
    [n]
    (fn [s] [(.skip (unlock s) n) s]))

  (defn write
    "Write text (a string)"
    [#^String text]
    (fn [s] [(.write (unlock s) text) s]))

  (defn flush
    "Flush"
    []
    (fn [s] [(.flush (unlock s)) s]))

  (defn print
    "Print obj"
    [obj]
    (fn [s] [(.print (unlock s) obj) s]))

  (defn println
    "Print obj followed by a newline"
    ([]
     (fn [s] [(.println (unlock s)) s]))
    ([obj]
     (fn [s] [(.println (unlock s) obj) s])))

  ;
  ; Inject I/O streams into monadic I/O statements
  ;
  (defn with-reader
    "Create a reader from reader-spec, run the monadic I/O statement
     on it, and close the reader. reader-spec can be any object accepted
     by clojure.contrib.duck-streams/reader."
    [reader-spec statement]
    (with-open [r (reader reader-spec)]
      (first (statement (lock r)))))

  (defn with-writer
    "Create a writer from writer-spec, run the monadic I/O statement
     on it, and close the writer. writer-spec can be any object accepted
     by clojure.contrib.duck-streams/writer."
    [writer-spec statement]
    (with-open [w (writer writer-spec)]
      (first (statement (lock w)))))

  (defn with-io-streams
    "Open one or more streams as specified by io-spec, run a monadic
     I/O statement on them, and close the streams. io-spec is
     a binding-like vector in which each stream is specified by
     three element: a keyword by which the stream can be referred to,
     the stream mode (:read or :write), and a stream specification as
     accepted by clojure.contrib.duck-streams/reader (mode :read) or
     clojure.contrib.duck-streams/writer (mode :write). The statement
     is run on a state which is a map from keywords to corresponding
     streams. Single-stream monadic I/O statements must be wrapped
     with clojure.contrib.monads/with-state-field."
    [io-specs statement]
    (letfn [(run-io [io-specs state statement]
	      (if (zero? (count io-specs))
		(first (statement state))
		(let [[[key mode stream-spec] & r] io-specs
		      opener (cond (= mode :read) reader
				   (= mode :write) writer
				   :else (throw
					  (Exception.
					   "Mode must be :read or :write")))]
		  (with-open [stream (opener stream-spec)]
		    (run-io r (assoc state key (lock stream)) statement)))))]
      (run-io (partition 3 io-specs) {} statement))))

;
; Compound I/O statements
;
(with-monad state-m

  (defn- add-line
    "Read one line and add it to the end of the vector lines. Return
     [lines eof], where eof is an end-of-file flag. The input eof argument
     is not used."
    [[lines eof]]
    (domonad
      [line (read-line)]
      (if (nil? line)
        [lines true]
        [(conj lines line) false])))

  (defn read-lines
    "Read all lines and return them in a vector"
    []
    (domonad
      [[lines eof] (state-m-until second add-line [[] false])]
      lines)))

