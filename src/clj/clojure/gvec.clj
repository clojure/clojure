;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;;; a generic vector implementation for vectors of primitives

(in-ns 'clojure.core)

;(set! *warn-on-reflection* true)

(deftype VecNode [edit arr])

(def EMPTY-NODE (VecNode nil (object-array 32)))

(definterface IVecImpl
  (#^int tailoff [])
  (arrayFor [#^int i])
  (pushTail [#^int level parent tailnode])
  (popTail [#^int level node])
  (newPath [edit #^int level node])
  (doAssoc [#^int level node #^int i val]))

(definterface ArrayManager
  (array [#^int size])
  (#^int alength [arr])
  (aclone [arr])
  (aget [arr #^int i])
  (aset [arr #^int i val]))

(deftype ArrayChunk [#^clojure.core.ArrayManager am arr #^int off #^int end]
  
  clojure.lang.Indexed
  (nth [i] (.aget am arr (+ off i)))
  
  (count [] (- end off))

  clojure.lang.IChunk
  (dropFirst []
    (if (= off end)
      (throw (IllegalStateException. "dropFirst of empty chunk"))
      (new ArrayChunk am arr (inc off) end)))
  
  (reduce [f init]
    (loop [ret init i off]
      (if (< i end)
        (recur (f ret (.aget am arr i)) (inc i))
        ret)))
  )

(deftype VecSeq [#^clojure.core.ArrayManager am #^clojure.core.IVecImpl vec anode #^int i #^int offset] 
  :as this
  :no-print true

  clojure.lang.ISeq
  (first [] (.aget am anode offset))
  (next [] 
    (if (< (inc offset) (.alength am anode))
      (new VecSeq am vec anode i (inc offset))
      (.chunkedNext this)))
  (more []
    (let [s (.next this)]
      (or s (clojure.lang.PersistentList/EMPTY))))

  clojure.lang.Seqable
  (seq [] this)

  clojure.lang.IChunkedSeq
  (chunkedFirst [] (ArrayChunk am anode offset (.alength am anode)))
  (chunkedNext [] 
   (let [nexti (+ i (.alength am anode))]
     (when (< nexti (count vec))
       (new VecSeq am vec (.arrayFor vec nexti) nexti 0))))
  (chunkedMore []
    (let [s (.chunkedNext this)]
      (or s (clojure.lang.PersistentList/EMPTY)))))

(defmethod print-method ::VecSeq [v w]
  ((get (methods print-method) clojure.lang.ISeq) v w))

(deftype Vec [#^clojure.core.ArrayManager am #^int cnt #^int shift root tail]
  :as this
  :no-print true

  Object
  (equals [o]
    (cond 
     (or (instance? clojure.lang.IPersistentVector o) (instance? java.util.RandomAccess o))
       (and (= cnt (count o))
            (loop [i (int 0)]
              (cond
               (= i cnt) true
               (.equals (.nth this i) (nth o i)) (recur (inc i))
               :else false)))
     (or (instance? clojure.lang.Sequential o) (instance? java.util.List o))
       (.equals (seq this) (seq o))
     :else false))

  ;todo - cache
  (hashCode []
    (loop [hash (int 1) i (int 0)]
      (if (= i cnt)
        hash
        (let [val (.nth this i)]
          (recur (unchecked-add (unchecked-multiply (int 31) hash) 
                                (clojure.lang.Util/hash val)) 
                 (inc i))))))

  clojure.lang.Counted
  (count [] cnt)

  clojure.lang.Indexed
  (nth [i]
    (let [a (.arrayFor this i)]
      (.aget am a (bit-and i (int 0x1f)))))

  clojure.lang.IPersistentCollection
  (cons [val]
     (if (< (- cnt (.tailoff this)) (int 32))
      (let [new-tail (.array am (inc (.alength am tail)))]
        (System/arraycopy tail 0 new-tail 0 (.alength am tail))
        (.aset am new-tail (.alength am tail) val)
        (new Vec am (inc cnt) shift root new-tail (meta this) nil))
      (let [tail-node (VecNode (:edit root) tail)] 
        (if (> (bit-shift-right cnt (int 5)) (bit-shift-left (int 1) shift)) ;overflow root?
          (let [new-root (VecNode (:edit root) (object-array 32))]
            (doto #^objects (:arr new-root)
              (aset 0 root)
              (aset 1 (.newPath this (:edit root) shift tail-node)))
            (new Vec am (inc cnt) (+ shift (int 5)) new-root (let [tl (.array am 1)] (.aset am  tl 0 val) tl) (meta this) nil))
          (new Vec am (inc cnt) shift (.pushTail this shift root tail-node) 
                 (let [tl (.array am 1)] (.aset am  tl 0 val) tl) (meta this) nil)))))

  (empty [] (new Vec am 0 5 EMPTY-NODE (.array am 0)))                             
  (equiv [o]
    (cond 
     (or (instance? clojure.lang.IPersistentVector o) (instance? java.util.RandomAccess o))
       (and (= cnt (count o))
            (loop [i (int 0)]
              (cond
               (= i cnt) true
               (= (.nth this i) (nth o i)) (recur (inc i))
               :else false)))
     (or (instance? clojure.lang.Sequential o) (instance? java.util.List o))
       (= (seq this) (seq o))
     :else false))
  
  clojure.lang.IPersistentStack
  (peek []
    (when (> cnt (int 0)) 
      (.nth this (dec cnt))))

  (pop []
   (cond
    (zero? cnt) 
      (throw (IllegalStateException. "Can't pop empty vector"))
    (= 1 cnt) 
      (new Vec am 0 5 EMPTY-NODE (.array am 0) (meta this) nil)
    (> (- cnt (.tailoff this)) 1)
      (let [new-tail (.array am (dec (.alength am tail)))]
        (System/arraycopy tail 0 new-tail 0 (.alength am new-tail))
        (new Vec am (dec cnt) shift root new-tail (meta this) nil))
    :else
      (let [new-tail (.arrayFor this (- cnt 2))
            new-root (.popTail this shift root)]
        (cond
         (nil? new-root) 
           (new Vec am (dec cnt) shift EMPTY-NODE new-tail (meta this) nil)
         (and (> shift 5) (nil? (aget #^objects (:arr new-root) 1)))
           (new Vec am (dec cnt) (- shift 5) (aget #^objects (:arr new-root) 0) new-tail (meta this) nil)
         :else
           (new Vec am (dec cnt) shift new-root new-tail (meta this) nil)))))

  clojure.lang.IPersistentVector
  (assocN [i val]
    (cond 
     (and (<= (int 0) i) (< i cnt))
       (if (>= i (.tailoff this))
         (let [new-tail (.array am (.alength am tail))]
           (System/arraycopy tail 0 new-tail 0 (.alength am tail))
           (.aset am new-tail (bit-and i (int 0x1f)) val)
           (new Vec am cnt shift root new-tail (meta this) nil))
         (new Vec am cnt shift (.doAssoc this shift root i val) tail (meta this) nil))
     (= i cnt) (.cons this val)
     :else (throw (IndexOutOfBoundsException.))))

  clojure.lang.Associative
  (assoc [k v]
    (if (clojure.lang.Util/isInteger k)
      (.assocN this k v)
      (throw (IllegalArgumentException. "Key must be integer"))))

  clojure.lang.ILookup
  (valAt [k not-found]
    (if (clojure.lang.Util/isInteger k)
      (let [i (int k)]
        (if (and (>= i 0) (< i cnt))
          (.nth this i)
          not-found))
      not-found))

  (valAt [k] (.valAt this k nil))

  clojure.lang.IFn
  (invoke [k]
    (if (clojure.lang.Util/isInteger k)
      (let [i (int k)]
        (if (and (>= i 0) (< i cnt))
          (.nth this i)
          (throw (IndexOutOfBoundsException.))))
      (throw (IllegalArgumentException. "Key must be integer"))))

  
  clojure.lang.Seqable
  (seq [] 
    (if (zero? cnt) 
      nil
      (VecSeq am this (.arrayFor this 0) 0 0)))

  clojure.lang.Sequential ;marker, no methods

  clojure.core.IVecImpl
  (tailoff [] 
    (- cnt (alength tail)))

  (arrayFor [i]
    (if (and  (<= (int 0) i) (< i cnt))
      (if (>= i (.tailoff this))
        tail
        (loop [node root level shift]
          (if (zero? level)
            (:arr node)
            (recur (aget #^objects (:arr node) (bit-and (bit-shift-right i level) (int 0x1f))) 
                   (- level (int 5))))))
      (throw (IndexOutOfBoundsException.))))

  (pushTail [level parent tailnode]
    (let [subidx (bit-and (bit-shift-right (dec cnt) level) (int 0x1f))
          ret (VecNode (:edit parent) (aclone #^objects (:arr parent)))
          node-to-insert (if (= level (int 5))
                           tailnode
                           (let [child (aget #^objects (:arr parent) subidx)]
                             (if child
                               (.pushTail this (- level (int 5)) child tailnode)
                               (.newPath this (:edit root) (- level (int 5)) tailnode))))]
      (aset #^objects (:arr ret) subidx node-to-insert)
      ret))

  (popTail [level node]
    (let [subidx (bit-and (bit-shift-right (- cnt 2) level) (int 0x1f))]
      (cond
       (> level 5) 
         (let [new-child (.popTail this (- level 5) (aget #^objects (:arr node) subidx))]
           (if (and (nil? new-child) (zero? subidx))
             nil
             (let [arr (aclone #^objects (:arr node))]
               (aset arr subidx new-child)
               (VecNode (:edit root) arr))))
       (zero? subidx) nil
       :else (let [arr (aclone #^objects (:arr node))]
               (aset arr subidx nil)
               (VecNode (:edit root) arr)))))

  (newPath [edit #^int level node]
    (if (zero? level)
      node
      (let [ret (VecNode edit (object-array 32))]
        (aset #^objects (:arr ret) 0 (.newPath this edit (- level (int 5)) node))
        ret)))

  (doAssoc [level node i val] 
    (if (zero? level)
      ;on this branch, array will need val type
      (let [arr (.aclone am (:arr node))]
        (.aset am arr (bit-and i (int 0x1f)) val)
        (VecNode (:edit node) arr))
      (let [arr (aclone #^objects (:arr node))
            subidx (bit-and (bit-shift-right i level) (int 0x1f))]
        (aset arr subidx (.doAssoc this (- level (int 5)) (aget arr subidx) i val))
        (VecNode (:edit node) arr))))
)

(defmethod print-method ::Vec [v w]
  ((get (methods print-method) clojure.lang.IPersistentVector) v w))

(defmacro mk-am {:private true} [t]
  (let [garr (gensym)
        tgarr (with-meta garr {:tag (symbol (str t "s"))})]
    `(reify clojure.core.ArrayManager
            (array [size#] (~(symbol (str t "-array")) size#))
            (alength [~garr] (alength ~tgarr))
            (aclone [~garr] (aclone ~tgarr))
            (aget [~garr i#] (aget ~tgarr i#))
            (aset [~garr i# val#] (aset ~tgarr i# (~t val#))))))

(def #^{:private true} ams
     {:int (mk-am int)
      :long (mk-am long)
      :float (mk-am float)
      :double (mk-am double)
      :byte (mk-am byte)
      :short (mk-am short)
      :char (mk-am char)
      :boolean (mk-am boolean)})

(defn vector-of 
  "Creates a new vector of a single primitive type t, where t is one
  of :int :long :float :double :byte :short :char or :boolean. The
  resulting vector complies with the interface of vectors in general,
  but stores the values unboxed internally."  
  [t]
  (let [am #^clojure.core.ArrayManager (ams t)]
    (Vec am 0 5 EMPTY-NODE (.array am 0))))
