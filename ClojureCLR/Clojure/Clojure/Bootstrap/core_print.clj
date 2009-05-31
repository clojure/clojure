;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; printing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import '(System.IO.System.IO.TextWriter))   ;;; was (import '(java.io Writer))    (I have replaced #^Writer with #^System.IO.TextWriter throughout
;; Other global replaces:  .write => .Write, .append => .Write, #^Class => #^Type, #^Character => #^Char
(def
 #^{:doc "*print-length* controls how many items of each collection the
  printer will print. If it is bound to logical false, there is no
  limit. Otherwise, it must be bound to an integer indicating the maximum
  number of items of each collection to print. If a collection contains
  more items, the printer will print items up to the limit followed by
  '...' to represent the remaining items. The root binding is nil
  indicating no limit."}
 *print-length* nil)

(def
 #^{:doc "*print-level* controls how many levels deep the printer will
  print nested objects. If it is bound to logical false, there is no
  limit. Otherwise, it must be bound to an integer indicating the maximum
  level to print. Each argument to print is at level 0; if an argument is a
  collection, its items are at level 1; and so on. If an object is a
  collection and is at a level greater than or equal to the value bound to
  *print-level*, the printer prints '#' to represent it. The root binding
  is nil indicating no limit."}
*print-level* nil)

(defn- print-sequential [#^String begin, print-one, #^String sep, #^String end, sequence, #^System.IO.TextWriter w]
  (binding [*print-level* (and (not *print-dup*) *print-level* (dec *print-level*))]
    (if (and *print-level* (neg? *print-level*))
      (.Write w "#")
      (do
        (.Write w begin)
        (when-let [xs (seq sequence)]
          (if (and (not *print-dup*) *print-length*)
            (loop [[x & xs] xs
                   print-length *print-length*]
              (if (zero? print-length)
                (.Write w "...")
                (do
                  (print-one x w)
                  (when xs
                    (.Write w sep)
                    (recur xs (dec print-length))))))
            (loop [[x & xs] xs]
              (print-one x w)
              (when xs
                (.Write w sep)
                (recur xs)))))
        (.Write w end)))))

(defn- print-meta [o, #^System.IO.TextWriter w]
  (when-let [m (meta o)]
    (when (and (pos? (count m))
               (or *print-dup*
                   (and *print-meta* *print-readably*)))
      (.Write w "#^")
      (if (and (= (count m) 1) (:tag m))
          (pr-on (:tag m) w)
          (pr-on m w))
      (.Write w " "))))

(defmethod print-method :default [o, #^System.IO.TextWriter w]
  (print-method (vary-meta o #(dissoc % :type)) w))

(defmethod print-method nil [o, #^System.IO.TextWriter w]
  (.Write w "nil"))

(defmethod print-dup nil [o w] (print-method o w))

(defn print-ctor [o print-args #^System.IO.TextWriter w]
  (.Write w "#=(")
  (.Write w (.FullName #^Type (class o)))   ;;; .getName  => .FullName
  (.Write w ". ")
  (print-args o w)
  (.Write w ")"))

(defmethod print-method Object [o, #^System.IO.TextWriter w]
  (.Write w "#<")
  (.Write w (.Name (class o)))     ;;; .getSimpleName => .Name
  (.Write w " ")
  (.Write w (str o))
  (.Write w ">"))

(defmethod print-method clojure.lang.Keyword [o, #^System.IO.TextWriter w]
  (.Write w (str o)))

(defmethod print-dup clojure.lang.Keyword [o w] (print-method o w))
;;; MAJOR PROBLEM: no Number type in CLR.  We will just ask every ValueType to print itself.  Need to deal with BigDecimal and BigInteger later.
(defmethod print-method ValueType [o, #^System.IO.TextWriter w]   ;; Number => ValueType
  (.Write w (str o)))

(defmethod print-dup ValueType [o, #^System.IO.TextWriter w]      ;;; Number => ValueType
  (print-ctor o
              (fn [o w]
                  (print-dup (str o) w))
              w))

(defmethod print-dup clojure.lang.Fn [o, #^System.IO.TextWriter w]
  (print-ctor o (fn [o w]) w))

(prefer-method print-dup clojure.lang.IPersistentCollection clojure.lang.Fn)
(prefer-method print-dup java.util.Map clojure.lang.Fn)
(prefer-method print-dup java.util.Collection clojure.lang.Fn)

(defmethod print-method Boolean [o, #^System.IO.TextWriter w]
  (.Write w (str o)))

(defmethod print-dup Boolean [o w] (print-method o w))

(defn print-simple [o, #^System.IO.TextWriter w]
  (print-meta o w)
  (.Write w (str o)))

(defmethod print-method clojure.lang.Symbol [o, #^System.IO.TextWriter w]
  (print-simple o w))

(defmethod print-dup clojure.lang.Symbol [o w] (print-method o w))

(defmethod print-method clojure.lang.Var [o, #^System.IO.TextWriter w]
  (print-simple o w))

(defmethod print-dup clojure.lang.Var [#^clojure.lang.Var o, #^System.IO.TextWriter w]
  (.Write w (str "#=(var " (.Name (.ns o)) "/" (.Symbol o) ")")))   ;;; .name => .Name, .sym => .Symbol

(defmethod print-method clojure.lang.ISeq [o, #^System.IO.TextWriter w]
  (print-meta o w)
  (print-sequential "(" pr-on " " ")" o w))

(defmethod print-dup clojure.lang.ISeq [o w] (print-method o w))
(defmethod print-dup clojure.lang.IPersistentList [o w] (print-method o w))
(prefer-method print-method clojure.lang.IPersistentList clojure.lang.ISeq)
(prefer-method print-dup clojure.lang.IPersistentList clojure.lang.ISeq)
(prefer-method print-method clojure.lang.ISeq clojure.lang.IPersistentCollection)
(prefer-method print-dup clojure.lang.ISeq clojure.lang.IPersistentCollection)
(prefer-method print-method clojure.lang.ISeq System.Collections.ICollection)  ;;  java: java.util.Collection
(prefer-method print-dup clojure.lang.ISeq System.Collections.ICollection)  ;;  java: java.util.Collection

(defmethod print-method clojure.lang.IPersistentList [o, #^System.IO.TextWriter w]
  (print-meta o w)
  (print-sequential "(" print-method " " ")" o w))


(defmethod print-dup System.Collections.ICollection [o, #^System.IO.TextWriter w]                     ;; java.util.Collection => System.Collections.ICollection
 (print-ctor o #(print-sequential "[" print-method " " "]" %1 %2) w))

(defmethod print-dup clojure.lang.IPersistentCollection [o, #^System.IO.TextWriter w]
  (print-meta o w)
  (.Write w "#=(")
  (.Write w (.FullName #^Type (class o)))   ;; .getName => .FullName
  (.Write w "/create ")
  (print-sequential "[" print-dup " " "]" o w)
  (.Write w ")"))

(prefer-method print-dup clojure.lang.IPersistentCollection System.Collections.ICollection)                ;; java.util.Collection => System.Collections.ICollection

(def #^{:tag String 
        :doc "Returns escape string for char or nil if none"}
  char-escape-string
    {\newline "\\n"
     \tab  "\\t"
     \return "\\r"
     \" "\\\""
     \\  "\\\\"
     \formfeed "\\f"
     \backspace "\\b"})  

(defmethod print-method String [#^String s, #^System.IO.TextWriter w]
  (if (or *print-dup* *print-readably*)
    (do (.Write w \")                          
      (dotimes [n (count s)]
        (let [c (.get_Chars s n)                    ;; .charAt => .get_Chars
              e (char-escape-string c)]
          (if e (.Write w e) (.Write w c))))   
      (.Write w \"))                           
    (.Write w s))                                 
  nil)

(defmethod print-dup String [s w] (print-method s w))

(defmethod print-method clojure.lang.IPersistentVector [v, #^System.IO.TextWriter w]
  (print-meta v w)
  (print-sequential "[" pr-on " " "]" v w))

(defn- print-map [m print-one w]
  (print-sequential 
   "{"
   (fn [e  #^System.IO.TextWriter w] 
     (do (print-one (key e) w) (.Write w \space) (print-one (val e) w)))
   ", "
   "}"
   (seq m) w))

(defmethod print-method clojure.lang.IPersistentMap [m, #^System.IO.TextWriter w]
  (print-meta m w)
  (print-map m pr-on w))

(defmethod print-dup java.util.Map [m, #^System.IO.TextWriter w]
  (print-ctor m #(print-map (seq %1) print-method %2) w))

(defmethod print-dup clojure.lang.IPersistentMap [m, #^System.IO.TextWriter w]
  (print-meta m w)
  (.Write w "#=(")
  (.Write w (.FullName (class m)))   ;; .getName => .FullName
  (.Write w "/create ")
  (print-map m print-dup w)
  (.Write w ")"))
  
(prefer-method print-dup clojure.lang.IPersistentMap System.Collections.IDictionary)    ;; java.util.Map  -> System.Collections.IDictionary

(defmethod print-method clojure.lang.IPersistentSet [s, #^System.IO.TextWriter w]
  (print-meta s w)
  (print-sequential "#{" pr-on " " "}" (seq s) w))

(def #^{:tag String 
        :doc "Returns name string for char or nil if none"} 
 char-name-string
   {\newline "newline"
    \tab "tab"
    \space "space"
    \backspace "backspace"
    \formfeed "formfeed"
    \return "return"})

(defmethod print-method Char [#^Char c, #^System.IO.TextWriter w]
  (if (or *print-dup* *print-readably*)
    (do (.Write w \\)
        (let [n (char-name-string c)]
          (if n (.Write w n) (.Write w c))))
    (.Write w c))
  nil)

(defmethod print-dup Char   [c w] (print-method c w))             ;;; java.lang.Character
(defmethod print-dup Int32  [o w] (print-method o w))               ;;; java.lang.Integer
(defmethod print-dup Double [o w] (print-method o w))                ;;; java.lang.Double
(defmethod print-dup clojure.lang.Ratio [o w] (print-method o w))
(defmethod print-dup java.math.BigDecimal [o w] (print-method o w))
(defmethod print-dup clojure.lang.PersistentHashMap [o w] (print-method o w))
(defmethod print-dup clojure.lang.PersistentHashSet [o w] (print-method o w)) 
(defmethod print-dup clojure.lang.PersistentVector [o w] (print-method o w))
(defmethod print-dup clojure.lang.LazilyPersistentVector [o w] (print-method o w))

(def primitives-classnames    ;; not clear what the equiv should be
  {Single  "Single"   ;;{Float/TYPE "Float/TYPE"
   Int32   "Int32"    ;; Integer/TYPE "Integer/TYPE"
   Int64   "Int64"    ;; Long/TYPE "Long/TYPE"
   Boolean "Boolean"  ;; Boolean/TYPE "Boolean/TYPE"
   Char    "Char"     ;; Character/TYPE "Character/TYPE"
   Double  "Double"   ;; Double/TYPE "Double/TYPE"
   Byte    "Byte"     ;; Byte/TYPE "Byte/TYPE"
   Int16   "Int16"})  ;; Short/TYPE "Short/TYPE"})
  
(defmethod print-method Type [#^Type c, #^System.IO.TextWriter w]
  (.Write w (.FullName c)))   ;;; .getName => .FullName

(defmethod print-dup Type [#^Type c, #^System.IO.TextWriter w]
  (cond
    (.IsPrimitive c) (do                                             ;; .isPrimitive
                       (.Write w "#=(identity ")
                       (.Write w #^String (primitives-classnames c))
                       (.Write w ")"))
    (.IsArray c) (do                                                 ;; .isArray ,  java.lang.Class/forName =>
                   (.Write w "#=(clojure.lang.RT/classForName \"")
                   (.Write w (.FullName c))                           ;; .getName => .FullName
                   (.Write w "\")"))
    :else (do
            (.Write w "#=")
            (.Write w (.FullName c)))))    ;;; .getName => .FullName

(defmethod print-method java.math.BigDecimal [b, #^System.IO.TextWriter w]
  (.Write w (str b))
  (.Write w "M"))

(defmethod print-method System.Text.RegularExpressions.Regex [p #^System.IO.TextWriter w]         ;;; java.util.regex.Pattern =>
  (.Write w "#\"")
  (loop [[#^Char c & r :as s] (seq (.ToString #^System.Text.RegularExpressions.Regex p))   ;;; .pattern => .ToString
         qmode false]
    (when s
      (cond
        (= c \\) (let [[#^Char c2 & r2] r]
                   (.Write w \\)
                   (.Write w c2)
                   (if qmode
                      (recur r2 (not= c2 \E))
                      (recur r2 (= c2 \Q))))
        (= c \") (do
                   (if qmode
                     (.Write w "\\E\\\"\\Q")
                     (.Write w "\\\""))
                   (recur r qmode))
        :else    (do
                   (.Write w c)
                   (recur r qmode)))))
  (.Write w \"))

(defmethod print-dup System.Text.RegularExpressions.Regex [p #^System.IO.TextWriter w] (print-method p w))  ;;; java.util.regex.Pattern =>
  
(defmethod print-dup clojure.lang.Namespace [#^clojure.lang.Namespace n #^System.IO.TextWriter w]
  (.Write w "#=(find-ns ")
  (print-dup (.Name n) w)    ;; .name
  (.Write w ")"))

(defmethod print-method clojure.lang.IDeref [o #^System.IO.TextWriter w]
  (print-sequential (format "#<%s@%x: "
                            (.Name (class o))     ;;; .getSimpleName => .Name
                            (.GetHashCode o))    ;;; No easy equivelent in CLR: (System/identityHashCode o)))
                    pr-on, "", ">", (list @o), w))

(def #^{:private true} print-initialized true)  