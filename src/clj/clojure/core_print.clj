;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; printing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import '(java.io Writer))

(set! *warn-on-reflection* true)
(def ^:dynamic
 ^{:doc "*print-length* controls how many items of each collection the
  printer will print. If it is bound to logical false, there is no
  limit. Otherwise, it must be bound to an integer indicating the maximum
  number of items of each collection to print. If a collection contains
  more items, the printer will print items up to the limit followed by
  '...' to represent the remaining items. The root binding is nil
  indicating no limit."
   :added "1.0"}
 *print-length* nil)

(def ^:dynamic
 ^{:doc "*print-level* controls how many levels deep the printer will
  print nested objects. If it is bound to logical false, there is no
  limit. Otherwise, it must be bound to an integer indicating the maximum
  level to print. Each argument to print is at level 0; if an argument is a
  collection, its items are at level 1; and so on. If an object is a
  collection and is at a level greater than or equal to the value bound to
  *print-level*, the printer prints '#' to represent it. The root binding
  is nil indicating no limit."
   :added "1.0"}
 *print-level* nil)

(def ^:dynamic *verbose-defrecords* false)

(defn- print-sequential [^String begin, print-one, ^String sep, ^String end, sequence, ^Writer w]
  (binding [*print-level* (and (not *print-dup*) *print-level* (dec *print-level*))]
    (if (and *print-level* (neg? *print-level*))
      (.write w "#")
      (do
        (.write w begin)
        (when-let [xs (seq sequence)]
          (if (and (not *print-dup*) *print-length*)
            (loop [[x & xs] xs
                   print-length *print-length*]
              (if (zero? print-length)
                (.write w "...")
                (do
                  (print-one x w)
                  (when xs
                    (.write w sep)
                    (recur xs (dec print-length))))))
            (loop [[x & xs] xs]
              (print-one x w)
              (when xs
                (.write w sep)
                (recur xs)))))
        (.write w end)))))

(defn- print-meta [o, ^Writer w]
  (when-let [m (meta o)]
    (when (and (pos? (count m))
               (or *print-dup*
                   (and *print-meta* *print-readably*)))
      (.write w "^")
      (if (and (= (count m) 1) (:tag m))
          (pr-on (:tag m) w)
          (pr-on m w))
      (.write w " "))))

(defn print-simple [o, ^Writer w]
  (print-meta o w)
  (.write w (str o)))

(defmethod print-method :default [o, ^Writer w]
  (if (instance? clojure.lang.IObj o)
    (print-method (vary-meta o #(dissoc % :type)) w)
    (print-simple o w)))

(defmethod print-method nil [o, ^Writer w]
  (.write w "nil"))

(defmethod print-dup nil [o w] (print-method o w))

(defn print-ctor [o print-args ^Writer w]
  (.write w "#=(")
  (.write w (.getName ^Class (class o)))
  (.write w ". ")
  (print-args o w)
  (.write w ")"))

(defn- print-tagged-object [o rep ^Writer w]
  (when (instance? clojure.lang.IMeta o)
    (print-meta o w))
  (.write w "#object[")
  (let [c (class o)]
    (if (.isArray c)
      (print-method (.getName c) w)
      (.write w (.getName c))))
  (.write w " ")
  (.write w (format "0x%x " (System/identityHashCode o)))
  (print-method rep w)
  (.write w "]"))

(defn- print-object [o, ^Writer w]
  (print-tagged-object o (str o) w))

(defmethod print-method Object [o, ^Writer w]
  (print-object o w))

(defmethod print-method clojure.lang.Keyword [o, ^Writer w]
  (.write w (str o)))

(defmethod print-dup clojure.lang.Keyword [o w] (print-method o w))

(defmethod print-method Number [o, ^Writer w]
  (.write w (str o)))

(defmethod print-dup Number [o, ^Writer w]
  (print-ctor o
              (fn [o w]
                  (print-dup (str o) w))
              w))

(defmethod print-dup clojure.lang.Fn [o, ^Writer w]
  (print-ctor o (fn [o w]) w))

(prefer-method print-dup clojure.lang.IPersistentCollection clojure.lang.Fn)
(prefer-method print-dup java.util.Map clojure.lang.Fn)
(prefer-method print-dup java.util.Collection clojure.lang.Fn)

(defmethod print-method Boolean [o, ^Writer w]
  (.write w (str o)))

(defmethod print-dup Boolean [o w] (print-method o w))

(defmethod print-method clojure.lang.Symbol [o, ^Writer w]
  (print-simple o w))

(defmethod print-dup clojure.lang.Symbol [o w] (print-method o w))

(defmethod print-method clojure.lang.Var [o, ^Writer w]
  (print-simple o w))

(defmethod print-dup clojure.lang.Var [^clojure.lang.Var o, ^Writer w]
  (.write w (str "#=(var " (.name (.ns o)) "/" (.sym o) ")")))

(defmethod print-method clojure.lang.ISeq [o, ^Writer w]
  (print-meta o w)
  (print-sequential "(" pr-on " " ")" o w))

(defmethod print-dup clojure.lang.ISeq [o w] (print-method o w))
(defmethod print-dup clojure.lang.IPersistentList [o w] (print-method o w))
(prefer-method print-method clojure.lang.ISeq clojure.lang.IPersistentCollection)
(prefer-method print-dup clojure.lang.ISeq clojure.lang.IPersistentCollection)
(prefer-method print-method clojure.lang.ISeq java.util.Collection)
(prefer-method print-dup clojure.lang.ISeq java.util.Collection)



(defmethod print-dup java.util.Collection [o, ^Writer w]
 (print-ctor o #(print-sequential "[" print-dup " " "]" %1 %2) w))

(defmethod print-dup clojure.lang.IPersistentCollection [o, ^Writer w]
  (print-meta o w)
  (.write w "#=(")
  (.write w (.getName ^Class (class o)))
  (.write w "/create ")
  (print-sequential "[" print-dup " " "]" o w)
  (.write w ")"))

(prefer-method print-dup clojure.lang.IPersistentCollection java.util.Collection)

(def ^{:tag String 
       :doc "Returns escape string for char or nil if none"
       :added "1.0"}
  char-escape-string
    {\newline "\\n"
     \tab  "\\t"
     \return "\\r"
     \" "\\\""
     \\  "\\\\"
     \formfeed "\\f"
     \backspace "\\b"})

(defmethod print-method String [^String s, ^Writer w]
  (if (or *print-dup* *print-readably*)
    (do (.append w \")
      (dotimes [n (count s)]
        (let [c (.charAt s n)
              e (char-escape-string c)]
          (if e (.write w e) (.append w c))))
      (.append w \"))
    (.write w s))
  nil)

(defmethod print-dup String [s w] (print-method s w))

(defmethod print-method clojure.lang.IPersistentVector [v, ^Writer w]
  (print-meta v w)
  (print-sequential "[" pr-on " " "]" v w))

(defn- print-map [m print-one w]
  (print-sequential 
   "{"
   (fn [e  ^Writer w]
     (do (print-one (key e) w) (.append w \space) (print-one (val e) w)))
   ", "
   "}"
   (seq m) w))

(defmethod print-method clojure.lang.IPersistentMap [m, ^Writer w]
  (print-meta m w)
  (print-map m pr-on w))

(defmethod print-dup java.util.Map [m, ^Writer w]
  (print-ctor m #(print-map (seq %1) print-dup %2) w))

(defmethod print-dup clojure.lang.IPersistentMap [m, ^Writer w]
  (print-meta m w)
  (.write w "#=(")
  (.write w (.getName (class m)))
  (.write w "/create ")
  (print-map m print-dup w)
  (.write w ")"))

;; java.util
(prefer-method print-method clojure.lang.IPersistentCollection java.util.Collection)
(prefer-method print-method clojure.lang.IPersistentCollection java.util.RandomAccess)
(prefer-method print-method java.util.RandomAccess java.util.List)
(prefer-method print-method clojure.lang.IPersistentCollection java.util.Map)

(defmethod print-method java.util.List [c, ^Writer w]
  (if *print-readably*
    (do
      (print-meta c w)
      (print-sequential "(" pr-on " " ")" c w))
    (print-object c w)))

(defmethod print-method java.util.RandomAccess [v, ^Writer w]
  (if *print-readably*
    (do
      (print-meta v w)
      (print-sequential "[" pr-on " " "]" v w))
    (print-object v w)))

(defmethod print-method java.util.Map [m, ^Writer w]
  (if *print-readably*
    (do
      (print-meta m w)
      (print-map m pr-on w))
    (print-object m w)))

(defmethod print-method java.util.Set [s, ^Writer w]
  (if *print-readably*
    (do
      (print-meta s w)
      (print-sequential "#{" pr-on " " "}" (seq s) w))
    (print-object s w)))

;; Records

(defmethod print-method clojure.lang.IRecord [r, ^Writer w]
  (print-meta r w)
  (.write w "#")
  (.write w (.getName (class r)))
  (print-map r pr-on w))

(defmethod print-dup clojure.lang.IRecord [r, ^Writer w]
  (print-meta r w)
  (.write w "#")
  (.write w (.getName (class r)))
  (if *verbose-defrecords*
    (print-map r print-dup w)
    (print-sequential "[" pr-on ", " "]" (vals r) w)))

(prefer-method print-method clojure.lang.IRecord java.util.Map)
(prefer-method print-method clojure.lang.IRecord clojure.lang.IPersistentMap)
(prefer-method print-dup clojure.lang.IRecord clojure.lang.IPersistentMap)
(prefer-method print-dup clojure.lang.IPersistentCollection java.util.Map)
(prefer-method print-dup clojure.lang.IRecord clojure.lang.IPersistentCollection)
(prefer-method print-dup clojure.lang.IRecord java.util.Map)

(defmethod print-method clojure.lang.IPersistentSet [s, ^Writer w]
  (print-meta s w)
  (print-sequential "#{" pr-on " " "}" (seq s) w))

(def ^{:tag String
       :doc "Returns name string for char or nil if none"
       :added "1.0"} 
 char-name-string
   {\newline "newline"
    \tab "tab"
    \space "space"
    \backspace "backspace"
    \formfeed "formfeed"
    \return "return"})

(defmethod print-method java.lang.Character [^Character c, ^Writer w]
  (if (or *print-dup* *print-readably*)
    (do (.append w \\)
        (let [n (char-name-string c)]
          (if n (.write w n) (.append w c))))
    (.append w c))
  nil)

(defmethod print-dup java.lang.Character [c w] (print-method c w))
(defmethod print-dup java.lang.Long [o w] (print-method o w))
(defmethod print-dup java.lang.Double [o w] (print-method o w))
(defmethod print-dup clojure.lang.Ratio [o w] (print-method o w))
(defmethod print-dup java.math.BigDecimal [o w] (print-method o w))
(defmethod print-dup clojure.lang.BigInt [o w] (print-method o w))
(defmethod print-dup clojure.lang.PersistentHashMap [o w] (print-method o w))
(defmethod print-dup clojure.lang.PersistentHashSet [o w] (print-method o w))
(defmethod print-dup clojure.lang.PersistentVector [o w] (print-method o w))
(defmethod print-dup clojure.lang.LazilyPersistentVector [o w] (print-method o w))

(def primitives-classnames
  {Float/TYPE "Float/TYPE"
   Integer/TYPE "Integer/TYPE"
   Long/TYPE "Long/TYPE"
   Boolean/TYPE "Boolean/TYPE"
   Character/TYPE "Character/TYPE"
   Double/TYPE "Double/TYPE"
   Byte/TYPE "Byte/TYPE"
   Short/TYPE "Short/TYPE"})

(defmethod print-method Class [^Class c, ^Writer w]
  (.write w (.getName c)))

(defmethod print-dup Class [^Class c, ^Writer w]
  (cond
    (.isPrimitive c) (do
                       (.write w "#=(identity ")
                       (.write w ^String (primitives-classnames c))
                       (.write w ")"))
    (.isArray c) (do
                   (.write w "#=(java.lang.Class/forName \"")
                   (.write w (.getName c))
                   (.write w "\")"))
    :else (do
            (.write w "#=")
            (.write w (.getName c)))))

(defmethod print-method java.math.BigDecimal [b, ^Writer w]
  (.write w (str b))
  (.write w "M"))

(defmethod print-method clojure.lang.BigInt [b, ^Writer w]
  (.write w (str b))
  (.write w "N"))

(defmethod print-method java.util.regex.Pattern [p ^Writer w]
  (.write w "#\"")
  (loop [[^Character c & r :as s] (seq (.pattern ^java.util.regex.Pattern p))
         qmode false]
    (when s
      (cond
        (= c \\) (let [[^Character c2 & r2] r]
                   (.append w \\)
                   (.append w c2)
                   (if qmode
                      (recur r2 (not= c2 \E))
                      (recur r2 (= c2 \Q))))
        (= c \") (do
                   (if qmode
                     (.write w "\\E\\\"\\Q")
                     (.write w "\\\""))
                   (recur r qmode))
        :else    (do
                   (.append w c)
                   (recur r qmode)))))
  (.append w \"))

(defmethod print-dup java.util.regex.Pattern [p ^Writer w] (print-method p w))

(defmethod print-dup clojure.lang.Namespace [^clojure.lang.Namespace n ^Writer w]
  (.write w "#=(find-ns ")
  (print-dup (.name n) w)
  (.write w ")"))

(defn- deref-as-map [^clojure.lang.IDeref o]
  (let [pending (and (instance? clojure.lang.IPending o)
                     (not (.isRealized ^clojure.lang.IPending o)))
        [ex val]
        (when-not pending
          (try [false (deref o)]
               (catch Throwable e
                 [true e])))]
    {:status
     (cond
      (or ex
          (and (instance? clojure.lang.Agent o)
               (agent-error o)))
      :failed

      pending
      :pending

      :else
      :ready)

     :val val}))

(defmethod print-method clojure.lang.IDeref [o ^Writer w]
  (print-tagged-object o (deref-as-map o) w))

(defmethod print-method StackTraceElement [^StackTraceElement o ^Writer w]
  (print-method [(symbol (.getClassName o)) (symbol (.getMethodName o)) (.getFileName o) (.getLineNumber o)] w))

(defn Throwable->map
  "Constructs a data representation for a Throwable."
  {:added "1.7"}
  [^Throwable o]
  (let [base (fn [^Throwable t]
               (let [m {:type (class t)
                        :message (.getLocalizedMessage t)
                        :at (get (.getStackTrace t) 0)}
                     data (ex-data t)]
                 (if data
                   (assoc m :data data)
                   m)))
        via (loop [via [], ^Throwable t o]
              (if t
                (recur (conj via t) (.getCause t))
                via))
        ^Throwable root (peek via)
        m {:cause (.getLocalizedMessage root)
           :via (vec (map base via))
           :trace (vec (.getStackTrace ^Throwable (or root o)))}
        data (ex-data root)]
    (if data
      (assoc m :data data)
      m)))

(defn- print-throwable [^Throwable o ^Writer w]
  (.write w "#error {\n :cause ")
  (let [{:keys [cause data via trace]} (Throwable->map o)
        print-via #(do (.write w "{:type ")
		               (print-method (:type %) w)
					   (.write w "\n   :message ")
					   (print-method (:message %) w)
             (when-let [data (:data %)]
               (.write w "\n   :data ")
               (print-method data w))
					   (.write w "\n   :at ")
					   (print-method (:at %) w)
					   (.write w "}"))]
    (print-method cause w)
    (when data
      (.write w "\n :data ")
      (print-method data w))
    (when via
      (.write w "\n :via\n [")
      (when-let [fv (first via)]
	    (print-via fv)
        (doseq [v (rest via)]
          (.write w "\n  ")
		  (print-via v)))
      (.write w "]"))
    (when trace
      (.write w "\n :trace\n [")
      (when-let [ft (first trace)]
        (print-method ft w)
        (doseq [t (rest trace)]
          (.write w "\n  ")
          (print-method t w)))
      (.write w "]")))
  (.write w "}"))

(defmethod print-method Throwable [^Throwable o ^Writer w]
  (print-throwable o w))

(defmethod print-method clojure.lang.TaggedLiteral [o ^Writer w]
  (.write w "#")
  (print-method (:tag o) w)
  (.write w " ")
  (print-method (:form o) w))

(defmethod print-method clojure.lang.ReaderConditional [o ^Writer w]
  (.write w "#?")
  (when (:splicing? o) (.write w "@"))
  (print-method (:form o) w))

(def ^{:private true} print-initialized true)
