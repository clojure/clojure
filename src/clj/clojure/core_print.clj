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

(defmethod print-method :default [o, ^Writer w]
  (print-method (vary-meta o #(dissoc % :type)) w))

(defmethod print-method nil [o, ^Writer w]
  (.write w "nil"))

(defmethod print-dup nil [o w] (print-method o w))

(defn print-ctor [o print-args ^Writer w]
  (.write w "#=(")
  (.write w (.getName ^Class (class o)))
  (.write w ". ")
  (print-args o w)
  (.write w ")"))

(defmethod print-method Object [o, ^Writer w]
  (.write w "#<")
  (.write w (.getSimpleName (class o)))
  (.write w " ")
  (.write w (str o))
  (.write w ">"))

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

(defn print-simple [o, ^Writer w]
  (print-meta o w)
  (.write w (str o)))

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

(prefer-method print-dup clojure.lang.IPersistentCollection java.util.Map)

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
(defmethod print-dup java.lang.Integer [o w] (print-method o w))
(defmethod print-dup java.lang.Double [o w] (print-method o w))
(defmethod print-dup clojure.lang.Ratio [o w] (print-method o w))
(defmethod print-dup java.math.BigDecimal [o w] (print-method o w))
(defmethod print-dup clojure.lang.BigInt [o w] (print-method o w))
(defmethod print-dup java.math.BigInteger [o w] (print-method o w))
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

(defmethod print-method java.math.BigInteger [b, ^Writer w]
  (.write w (str b))
  (.write w "BIGINT"))

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

(defmethod print-method clojure.lang.IDeref [o ^Writer w]
  (print-sequential (format "#<%s@%x%s: "
                            (.getSimpleName (class o))
                            (System/identityHashCode o)
                            (if (and (instance? clojure.lang.Agent o)
                                     (agent-error o))
                              " FAILED"
                              ""))
                    pr-on, "", ">", (list (if (and (instance? clojure.lang.IPending o) (not (.isRealized o)))
                                            :pending
                                            @o)), w))

(def ^{:private true} print-initialized true)
