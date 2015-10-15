;;; pretty_writer.clj -- part of the pretty printer for Clojure

;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Author: Tom Faulhaber
;; April 3, 2009
;; Revised to use proxy instead of gen-class April 2010

;; This module implements a wrapper around a java.io.Writer which implements the
;; core of the XP algorithm.

(in-ns 'clojure.pprint)

(import [clojure.lang IDeref]
        [java.io Writer])

;; TODO: Support for tab directives


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forward declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare get-miser-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros to simplify dealing with types and classes. These are
;;; really utilities, but I'm experimenting with them here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ^{:private true} 
  getf 
  "Get the value of the field named by the argument (which should be a keyword)."
  [sym]
  `(~sym @@~'this))

(defmacro ^{:private true} 
  setf [sym new-val] 
  "Set the value of the field SYM to NEW-VAL"
  `(alter @~'this assoc ~sym ~new-val))

(defmacro ^{:private true} 
  deftype [type-name & fields]
  (let [name-str (name type-name)]
    `(do
       (defstruct ~type-name :type-tag ~@fields)
       (alter-meta! #'~type-name assoc :private true)
       (defn- ~(symbol (str "make-" name-str)) 
         [& vals#] (apply struct ~type-name ~(keyword name-str) vals#))
       (defn- ~(symbol (str name-str "?")) [x#] (= (:type-tag x#) ~(keyword name-str))))))

(defmacro ^{:private true}
  write-to-base
  "Call .write on Writer (getf :base) with proper type-hinting to
  avoid reflection."
  [& args]
  `(let [^Writer w# (getf :base)]
     (.write w# ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The data structures used by pretty-writer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct ^{:private true} logical-block
           :parent :section :start-col :indent
           :done-nl :intra-block-nl
           :prefix :per-line-prefix :suffix
           :logical-block-callback)

(defn- ancestor? [parent child]
  (loop [child (:parent child)]
    (cond 
     (nil? child) false
     (identical? parent child) true
     :else (recur (:parent child)))))

(defstruct ^{:private true} section :parent)

(defn- buffer-length [l] 
  (let [l (seq l)]
    (if l 
      (- (:end-pos (last l)) (:start-pos (first l)))
      0)))

; A blob of characters (aka a string)
(deftype buffer-blob :data :trailing-white-space :start-pos :end-pos)

; A newline
(deftype nl-t :type :logical-block :start-pos :end-pos)

(deftype start-block-t :logical-block :start-pos :end-pos)

(deftype end-block-t :logical-block :start-pos :end-pos)

(deftype indent-t :logical-block :relative-to :offset :start-pos :end-pos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to write tokens in the output buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private pp-newline (memoize #(System/getProperty "line.separator")))

(declare emit-nl)

(defmulti ^{:private true} write-token #(:type-tag %2))
(defmethod write-token :start-block-t [^Writer this token]
   (when-let [cb (getf :logical-block-callback)] (cb :start))
   (let [lb (:logical-block token)]
    (dosync
     (when-let [^String prefix (:prefix lb)] 
       (write-to-base prefix))
     (let [col (get-column (getf :base))]
       (ref-set (:start-col lb) col)
       (ref-set (:indent lb) col)))))

(defmethod write-token :end-block-t [^Writer this token]
  (when-let [cb (getf :logical-block-callback)] (cb :end))
  (when-let [^String suffix (:suffix (:logical-block token))] 
    (write-to-base suffix)))

(defmethod write-token :indent-t [^Writer this token]
  (let [lb (:logical-block token)]
    (ref-set (:indent lb) 
             (+ (:offset token)
                (condp = (:relative-to token)
		  :block @(:start-col lb)
		  :current (get-column (getf :base)))))))

(defmethod write-token :buffer-blob [^Writer this token]
  (write-to-base ^String (:data token)))

(defmethod write-token :nl-t [^Writer this token]
;  (prlabel wt @(:done-nl (:logical-block token)))
;  (prlabel wt (:type token) (= (:type token) :mandatory))
  (if (or (= (:type token) :mandatory)
           (and (not (= (:type token) :fill))
                @(:done-nl (:logical-block token))))
    (emit-nl this token)
    (if-let [^String tws (getf :trailing-white-space)]
      (write-to-base tws)))
  (dosync (setf :trailing-white-space nil)))

(defn- write-tokens [^Writer this tokens force-trailing-whitespace]
  (doseq [token tokens]
    (if-not (= (:type-tag token) :nl-t)
      (if-let [^String tws (getf :trailing-white-space)]
	(write-to-base tws)))
    (write-token this token)
    (setf :trailing-white-space (:trailing-white-space token)))
  (let [^String tws (getf :trailing-white-space)] 
    (when (and force-trailing-whitespace tws)
      (write-to-base tws)
      (setf :trailing-white-space nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emit-nl? method defs for each type of new line. This makes
;;; the decision about whether to print this type of new line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn- tokens-fit? [^Writer this tokens]
;;;  (prlabel tf? (get-column (getf :base) (buffer-length tokens))
  (let [maxcol (get-max-column (getf :base))]
    (or 
     (nil? maxcol) 
     (< (+ (get-column (getf :base)) (buffer-length tokens)) maxcol))))

(defn- linear-nl? [this lb section]
;  (prlabel lnl? @(:done-nl lb) (tokens-fit? this section))
  (or @(:done-nl lb)
      (not (tokens-fit? this section))))

(defn- miser-nl? [^Writer this lb section]
  (let [miser-width (get-miser-width this)
        maxcol (get-max-column (getf :base))]
    (and miser-width maxcol
         (>= @(:start-col lb) (- maxcol miser-width))
         (linear-nl? this lb section))))

(defmulti ^{:private true} emit-nl? (fn [t _ _ _] (:type t)))

(defmethod emit-nl? :linear [newl this section _]
  (let [lb (:logical-block newl)]
    (linear-nl? this lb section)))

(defmethod emit-nl? :miser [newl this section _]
  (let [lb (:logical-block newl)]
    (miser-nl? this lb section)))

(defmethod emit-nl? :fill [newl this section subsection]
  (let [lb (:logical-block newl)]
    (or @(:intra-block-nl lb)
        (not (tokens-fit? this subsection))
        (miser-nl? this lb section))))

(defmethod emit-nl? :mandatory [_ _ _ _]
  true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various support functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn- get-section [buffer]
  (let [nl (first buffer) 
        lb (:logical-block nl)
        section (seq (take-while #(not (and (nl-t? %) (ancestor? (:logical-block %) lb)))
                                 (next buffer)))]
    [section (seq (drop (inc (count section)) buffer))])) 

(defn- get-sub-section [buffer]
  (let [nl (first buffer) 
        lb (:logical-block nl)
        section (seq (take-while #(let [nl-lb (:logical-block %)]
                                    (not (and (nl-t? %) (or (= nl-lb lb) (ancestor? nl-lb lb)))))
                            (next buffer)))]
    section)) 

(defn- update-nl-state [lb]
  (dosync
   (ref-set (:intra-block-nl lb) false)
   (ref-set (:done-nl lb) true)
   (loop [lb (:parent lb)]
     (if lb
       (do (ref-set (:done-nl lb) true)
           (ref-set (:intra-block-nl lb) true)
           (recur (:parent lb)))))))

(defn- emit-nl [^Writer this nl]
  (write-to-base ^String (pp-newline))
  (dosync (setf :trailing-white-space nil))
  (let [lb (:logical-block nl)
        ^String prefix (:per-line-prefix lb)] 
    (if prefix 
      (write-to-base prefix))
    (let [^String istr (apply str (repeat (- @(:indent lb) (count prefix))
					  \space))] 
      (write-to-base istr))
    (update-nl-state lb)))

(defn- split-at-newline [tokens]
  (let [pre (seq (take-while #(not (nl-t? %)) tokens))]
    [pre (seq (drop (count pre) tokens))]))

;;; Methods for showing token strings for debugging

(defmulti ^{:private true} tok :type-tag)
(defmethod tok :nl-t [token]
  (:type token))
(defmethod tok :buffer-blob [token]
  (str \" (:data token) (:trailing-white-space token) \"))
(defmethod tok :default [token]
  (:type-tag token))
(defn- toks [toks] (map tok toks))

;;; write-token-string is called when the set of tokens in the buffer
;;; is longer than the available space on the line

(defn- write-token-string [this tokens]
  (let [[a b] (split-at-newline tokens)]
;;    (prlabel wts (toks a) (toks b))
    (if a (write-tokens this a false))
    (if b
      (let [[section remainder] (get-section b)
            newl (first b)]
;;         (prlabel wts (toks section)) (prlabel wts (:type newl)) (prlabel wts (toks remainder)) 
        (let [do-nl (emit-nl? newl this section (get-sub-section b))
              result (if do-nl 
                       (do
;;                          (prlabel emit-nl (:type newl))
                         (emit-nl this newl)
                         (next b))
                       b)
              long-section (not (tokens-fit? this result))
              result (if long-section
                       (let [rem2 (write-token-string this section)]
;;;                              (prlabel recurse (toks rem2))
                         (if (= rem2 section)
                           (do ; If that didn't produce any output, it has no nls
                                        ; so we'll force it
                             (write-tokens this section false)
                             remainder)
                           (into [] (concat rem2 remainder))))
                       result)
;;              ff (prlabel wts (toks result))
              ] 
          result)))))

(defn- write-line [^Writer this]
  (dosync
   (loop [buffer (getf :buffer)]
;;     (prlabel wl1 (toks buffer))
     (setf :buffer (into [] buffer))
     (if (not (tokens-fit? this buffer))
       (let [new-buffer (write-token-string this buffer)]
;;          (prlabel wl new-buffer)
         (if-not (identical? buffer new-buffer)
                 (recur new-buffer)))))))

;;; Add a buffer token to the buffer and see if it's time to start
;;; writing
(defn- add-to-buffer [^Writer this token]
;  (prlabel a2b token)
  (dosync
   (setf :buffer (conj (getf :buffer) token))
   (if (not (tokens-fit? this (getf :buffer)))
     (write-line this))))

;;; Write all the tokens that have been buffered
(defn- write-buffered-output [^Writer this]
  (write-line this)
  (if-let [buf (getf :buffer)]
    (do
      (write-tokens this buf true)
      (setf :buffer []))))

(defn- write-white-space [^Writer this]
  (when-let [^String tws (getf :trailing-white-space)]
    ; (prlabel wws (str "*" tws "*"))
    (write-to-base tws)
    (dosync
     (setf :trailing-white-space nil))))

;;; If there are newlines in the string, print the lines up until the last newline, 
;;; making the appropriate adjustments. Return the remainder of the string
(defn- write-initial-lines 
  [^Writer this ^String s] 
  (let [lines (.split s "\n" -1)]
    (if (= (count lines) 1)
      s
      (dosync 
       (let [^String prefix (:per-line-prefix (first (getf :logical-blocks)))
             ^String l (first lines)] 
         (if (= :buffering (getf :mode))
           (let [oldpos (getf :pos)
                 newpos (+ oldpos (count l))]
             (setf :pos newpos)
             (add-to-buffer this (make-buffer-blob l nil oldpos newpos))
             (write-buffered-output this))
           (do
             (write-white-space this)
             (write-to-base l)))
         (write-to-base (int \newline))
         (doseq [^String l (next (butlast lines))]
           (write-to-base l)
           (write-to-base ^String (pp-newline))
           (if prefix
             (write-to-base prefix)))
         (setf :buffering :writing)
         (last lines))))))


(defn- p-write-char [^Writer this ^Integer c]
  (if (= (getf :mode) :writing)
    (do 
      (write-white-space this)
      (write-to-base c))
    (if (= c \newline)
      (write-initial-lines this "\n")
      (let [oldpos (getf :pos)
            newpos (inc oldpos)]
        (dosync
         (setf :pos newpos)
         (add-to-buffer this (make-buffer-blob (str (char c)) nil oldpos newpos)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize the pretty-writer instance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn- pretty-writer [writer max-columns miser-width]
  (let [lb (struct logical-block nil nil (ref 0) (ref 0) (ref false) (ref false))
        fields (ref {:pretty-writer true
                     :base (column-writer writer max-columns)
                     :logical-blocks lb 
                     :sections nil
                     :mode :writing
                     :buffer []
                     :buffer-block lb
                     :buffer-level 1
                     :miser-width miser-width
                     :trailing-white-space nil
                     :pos 0})]
    (proxy [Writer IDeref PrettyFlush] []
      (deref [] fields)

      (write 
       ([x]
          ;;     (prlabel write x (getf :mode))
          (condp = (class x)
            String 
            (let [^String s0 (write-initial-lines this x)
                  ^String s (.replaceFirst s0 "\\s+$" "")
                  white-space (.substring s0 (count s))
                  mode (getf :mode)]
              (dosync
               (if (= mode :writing)
                 (do
                   (write-white-space this)
                   (write-to-base s)
                   (setf :trailing-white-space white-space))
                 (let [oldpos (getf :pos)
                       newpos (+ oldpos (count s0))]
                   (setf :pos newpos)
                   (add-to-buffer this (make-buffer-blob s white-space oldpos newpos))))))

            Integer
            (p-write-char this x)
            Long
            (p-write-char this x)))
        ([x off len]
           (.write ^Writer this (subs (str x) off (+ off len)))))

      (ppflush []
             (if (= (getf :mode) :buffering)
               (dosync
                (write-tokens this (getf :buffer) true)
                (setf :buffer []))
               (write-white-space this)))

      (flush []
             (.ppflush ^PrettyFlush this)
             (let [^Writer w (getf :base)]
               (.flush w)))

      (close []
             (.flush ^Writer this)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods for pretty-writer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- start-block 
  [^Writer this 
   ^String prefix ^String per-line-prefix ^String suffix]
  (dosync 
   (let [lb (struct logical-block (getf :logical-blocks) nil (ref 0) (ref 0)
                    (ref false) (ref false)
                    prefix per-line-prefix suffix)]
     (setf :logical-blocks lb)
     (if (= (getf :mode) :writing)
       (do
         (write-white-space this)
          (when-let [cb (getf :logical-block-callback)] (cb :start))
          (if prefix 
           (write-to-base prefix))
         (let [col (get-column (getf :base))]
           (ref-set (:start-col lb) col)
           (ref-set (:indent lb) col)))
       (let [oldpos (getf :pos)
             newpos (+ oldpos (if prefix (count prefix) 0))]
         (setf :pos newpos)
         (add-to-buffer this (make-start-block-t lb oldpos newpos)))))))

(defn- end-block [^Writer this]
  (dosync
   (let [lb (getf :logical-blocks)
         ^String suffix (:suffix lb)]
     (if (= (getf :mode) :writing)
       (do
         (write-white-space this)
         (if suffix
           (write-to-base suffix))
         (when-let [cb (getf :logical-block-callback)] (cb :end)))
       (let [oldpos (getf :pos)
             newpos (+ oldpos (if suffix (count suffix) 0))]
         (setf :pos newpos)
         (add-to-buffer this (make-end-block-t lb oldpos newpos))))
     (setf :logical-blocks (:parent lb)))))

(defn- nl [^Writer this type]
  (dosync 
   (setf :mode :buffering)
   (let [pos (getf :pos)]
     (add-to-buffer this (make-nl-t type (getf :logical-blocks) pos pos)))))

(defn- indent [^Writer this relative-to offset]
  (dosync 
   (let [lb (getf :logical-blocks)]
     (if (= (getf :mode) :writing)
       (do
         (write-white-space this)
         (ref-set (:indent lb) 
                  (+ offset (condp = relative-to
			      :block @(:start-col lb)
			      :current (get-column (getf :base))))))
       (let [pos (getf :pos)]
         (add-to-buffer this (make-indent-t lb relative-to offset pos pos)))))))

(defn- get-miser-width [^Writer this]
  (getf :miser-width))

(defn- set-miser-width [^Writer this new-miser-width]
  (dosync (setf :miser-width new-miser-width)))

(defn- set-logical-block-callback [^Writer this f]
  (dosync (setf :logical-block-callback f)))
