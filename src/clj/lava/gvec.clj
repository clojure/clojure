;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;;; a generic vector implementation for vectors of primitives

(in-ns 'lava.core)

;(set! *warn-on-reflection* true)

(deftype VecNode [edit arr])

(def EMPTY-NODE (VecNode. nil (object-array 32)))

(definterface IVecImpl
  (^int tailoff [])
  (arrayFor [^int i])
  (pushTail [^int level ^lava.core.VecNode parent ^lava.core.VecNode tailnode])
  (popTail [^int level node])
  (newPath [edit ^int level node])
  (doAssoc [^int level node ^int i val]))

(definterface ArrayManager
  (array [^int size])
  (^int alength [arr])
  (aclone [arr])
  (aget [arr ^int i])
  (aset [arr ^int i val]))

(deftype ArrayChunk [^lava.core.ArrayManager am arr ^int off ^int end]
  
  lava.lang.Indexed
  (nth [_ i] (.aget am arr (+ off i)))
  
  (count [_] (- end off))

  lava.lang.IChunk
  (dropFirst [_]
    (if (= off end)
      (throw (IllegalStateException. "dropFirst of empty chunk"))
      (new ArrayChunk am arr (inc off) end)))
  
  (reduce [_ f init]
    (loop [ret init i off]
      (if (< i end)
        (recur (f ret (.aget am arr i)) (inc i))
        ret)))
  )

(deftype VecSeq [^lava.core.ArrayManager am ^lava.core.IVecImpl vec anode ^int i ^int offset] 
  :no-print true

  lava.core.protocols.InternalReduce
  (internal-reduce
   [_ f val]
   (loop [result val
          aidx offset]
     (if (< aidx (count vec))
       (let [node (.arrayFor vec aidx)
             result (loop [result result
                           node-idx (bit-and 0x1f aidx)]
                      (if (< node-idx (.alength am node))
                        (recur (f result (.aget am node node-idx)) (inc node-idx))
                        result))]
         (recur result (bit-and 0xffe0 (+ aidx 32))))
       result)))
  
  lava.lang.ISeq
  (first [_] (.aget am anode offset))
  (next [this] 
    (if (< (inc offset) (.alength am anode))
      (new VecSeq am vec anode i (inc offset))
      (.chunkedNext this)))
  (more [this]
    (let [s (.next this)]
      (or s (lava.lang.PersistentList/EMPTY))))
  (cons [this o]
    (lava.lang.Cons. o this))
  (count [this]
    (loop [i 1
           s (next this)]
      (if s
        (if (instance? lava.lang.Counted s)
          (+ i (.count s))
          (recur (inc i) (next s)))
        i)))
  (equiv [this o]
    (cond
     (identical? this o) true
     (or (instance? lava.lang.Sequential o) (instance? java.util.List o))
     (loop [me this
            you (seq o)]
       (if (nil? me)
         (nil? you)
         (and (lava.lang.Util/equiv (first me) (first you))
              (recur (next me) (next you)))))
     :else false))
  (empty [_]
    lava.lang.PersistentList/EMPTY)


  lava.lang.Seqable
  (seq [this] this)

  lava.lang.IChunkedSeq
  (chunkedFirst [_] (ArrayChunk. am anode offset (.alength am anode)))
  (chunkedNext [_] 
   (let [nexti (+ i (.alength am anode))]
     (when (< nexti (count vec))
       (new VecSeq am vec (.arrayFor vec nexti) nexti 0))))
  (chunkedMore [this]
    (let [s (.chunkedNext this)]
      (or s (lava.lang.PersistentList/EMPTY)))))

(defmethod print-method ::VecSeq [v w]
  ((get (methods print-method) lava.lang.ISeq) v w))

(deftype Vec [^lava.core.ArrayManager am ^int cnt ^int shift ^lava.core.VecNode root tail _meta]
  Object
  (equals [this o]
    (cond 
     (identical? this o) true
     (or (instance? lava.lang.IPersistentVector o) (instance? java.util.RandomAccess o))
       (and (= cnt (count o))
            (loop [i (int 0)]
              (cond
               (= i cnt) true
               (.equals (.nth this i) (nth o i)) (recur (inc i))
               :else false)))
     (or (instance? lava.lang.Sequential o) (instance? java.util.List o))
       (.equals (seq this) (seq o))
     :else false))

  ;todo - cache
  (hashCode [this]
    (loop [hash (int 1) i (int 0)]
      (if (= i cnt)
        hash
        (let [val (.nth this i)]
          (recur (unchecked-add-int (unchecked-multiply-int 31 hash) 
                                (lava.lang.Util/hash val)) 
                 (inc i))))))

  lava.lang.Counted
  (count [_] cnt)

  lava.lang.IMeta
  (meta [_] _meta)

  lava.lang.IObj
  (withMeta [_ m] (new Vec am cnt shift root tail m))

  lava.lang.Indexed
  (nth [this i]
    (let [a (.arrayFor this i)]
      (.aget am a (bit-and i (int 0x1f)))))
  (nth [this i not-found]
       (let [z (int 0)]
         (if (and (>= i z) (< i (.count this)))
           (.nth this i)
           not-found)))

  lava.lang.IPersistentCollection
  (cons [this val]
     (if (< (- cnt (.tailoff this)) (int 32))
      (let [new-tail (.array am (inc (.alength am tail)))]
        (System/arraycopy tail 0 new-tail 0 (.alength am tail))
        (.aset am new-tail (.alength am tail) val)
        (new Vec am (inc cnt) shift root new-tail (meta this)))
      (let [tail-node (VecNode. (.edit root) tail)] 
        (if (> (bit-shift-right cnt (int 5)) (bit-shift-left (int 1) shift)) ;overflow root?
          (let [new-root (VecNode. (.edit root) (object-array 32))]
            (doto ^objects (.arr new-root)
              (aset 0 root)
              (aset 1 (.newPath this (.edit root) shift tail-node)))
            (new Vec am (inc cnt) (+ shift (int 5)) new-root (let [tl (.array am 1)] (.aset am  tl 0 val) tl) (meta this)))
          (new Vec am (inc cnt) shift (.pushTail this shift root tail-node) 
                 (let [tl (.array am 1)] (.aset am  tl 0 val) tl) (meta this))))))

  (empty [_] (new Vec am 0 5 EMPTY-NODE (.array am 0) nil))                             
  (equiv [this o]
    (cond 
     (or (instance? lava.lang.IPersistentVector o) (instance? java.util.RandomAccess o))
       (and (= cnt (count o))
            (loop [i (int 0)]
              (cond
               (= i cnt) true
               (= (.nth this i) (nth o i)) (recur (inc i))
               :else false)))
     (or (instance? lava.lang.Sequential o) (instance? java.util.List o))
       (lava.lang.Util/equiv (seq this) (seq o))
     :else false))

  lava.lang.IPersistentStack
  (peek [this]
    (when (> cnt (int 0)) 
      (.nth this (dec cnt))))

  (pop [this]
   (cond
    (zero? cnt) 
      (throw (IllegalStateException. "Can't pop empty vector"))
    (= 1 cnt) 
      (new Vec am 0 5 EMPTY-NODE (.array am 0) (meta this))
    (> (- cnt (.tailoff this)) 1)
      (let [new-tail (.array am (dec (.alength am tail)))]
        (System/arraycopy tail 0 new-tail 0 (.alength am new-tail))
        (new Vec am (dec cnt) shift root new-tail (meta this)))
    :else
      (let [new-tail (.arrayFor this (- cnt 2))
            new-root ^lava.core.VecNode (.popTail this shift root)]
        (cond
         (nil? new-root) 
           (new Vec am (dec cnt) shift EMPTY-NODE new-tail (meta this))
         (and (> shift 5) (nil? (aget ^objects (.arr new-root) 1)))
           (new Vec am (dec cnt) (- shift 5) (aget ^objects (.arr new-root) 0) new-tail (meta this))
         :else
           (new Vec am (dec cnt) shift new-root new-tail (meta this))))))

  lava.lang.IPersistentVector
  (assocN [this i val]
    (cond 
     (and (<= (int 0) i) (< i cnt))
       (if (>= i (.tailoff this))
         (let [new-tail (.array am (.alength am tail))]
           (System/arraycopy tail 0 new-tail 0 (.alength am tail))
           (.aset am new-tail (bit-and i (int 0x1f)) val)
           (new Vec am cnt shift root new-tail (meta this)))
         (new Vec am cnt shift (.doAssoc this shift root i val) tail (meta this)))
     (= i cnt) (.cons this val)
     :else (throw (IndexOutOfBoundsException.))))
  
  lava.lang.Reversible
  (rseq [this]
        (if (> (.count this) 0)
          (lava.lang.APersistentVector$RSeq. this (dec (.count this)))
          nil))
  
  lava.lang.Associative
  (assoc [this k v]
    (if (lava.lang.Util/isInteger k)
      (.assocN this k v)
      (throw (IllegalArgumentException. "Key must be integer"))))
  (containsKey [this k]
    (and (lava.lang.Util/isInteger k)
         (<= 0 (int k))
         (< (int k) cnt)))
  (entryAt [this k]
    (if (.containsKey this k)
      (lava.lang.MapEntry. k (.nth this (int k)))
      nil))

  lava.lang.ILookup
  (valAt [this k not-found]
    (if (lava.lang.Util/isInteger k)
      (let [i (int k)]
        (if (and (>= i 0) (< i cnt))
          (.nth this i)
          not-found))
      not-found))

  (valAt [this k] (.valAt this k nil))

  lava.lang.IFn
  (invoke [this k]
    (if (lava.lang.Util/isInteger k)
      (let [i (int k)]
        (if (and (>= i 0) (< i cnt))
          (.nth this i)
          (throw (IndexOutOfBoundsException.))))
      (throw (IllegalArgumentException. "Key must be integer"))))

  
  lava.lang.Seqable
  (seq [this] 
    (if (zero? cnt) 
      nil
      (VecSeq. am this (.arrayFor this 0) 0 0)))

  lava.lang.Sequential ;marker, no methods

  lava.core.IVecImpl
  (tailoff [_] 
    (- cnt (.alength am tail)))

  (arrayFor [this i]
    (if (and  (<= (int 0) i) (< i cnt))
      (if (>= i (.tailoff this))
        tail
        (loop [node root level shift]
          (if (zero? level)
            (.arr node)
            (recur (aget ^objects (.arr node) (bit-and (bit-shift-right i level) (int 0x1f))) 
                   (- level (int 5))))))
      (throw (IndexOutOfBoundsException.))))

  (pushTail [this level parent tailnode]
    (let [subidx (bit-and (bit-shift-right (dec cnt) level) (int 0x1f))
          parent ^lava.core.VecNode parent
          ret (VecNode. (.edit parent) (aclone ^objects (.arr parent)))
          node-to-insert (if (= level (int 5))
                           tailnode
                           (let [child (aget ^objects (.arr parent) subidx)]
                             (if child
                               (.pushTail this (- level (int 5)) child tailnode)
                               (.newPath this (.edit root) (- level (int 5)) tailnode))))]
      (aset ^objects (.arr ret) subidx node-to-insert)
      ret))

  (popTail [this level node]
    (let [node ^lava.core.VecNode node
          subidx (bit-and (bit-shift-right (- cnt (int 2)) level) (int 0x1f))]
      (cond
       (> level 5) 
         (let [new-child (.popTail this (- level 5) (aget ^objects (.arr node) subidx))]
           (if (and (nil? new-child) (zero? subidx))
             nil
             (let [arr (aclone ^objects (.arr node))]
               (aset arr subidx new-child)
               (VecNode. (.edit root) arr))))
       (zero? subidx) nil
       :else (let [arr (aclone ^objects (.arr node))]
               (aset arr subidx nil)
               (VecNode. (.edit root) arr)))))

  (newPath [this edit ^int level node]
    (if (zero? level)
      node
      (let [ret (VecNode. edit (object-array 32))]
        (aset ^objects (.arr ret) 0 (.newPath this edit (- level (int 5)) node))
        ret)))

  (doAssoc [this level node i val]
    (let [node ^lava.core.VecNode node]       
      (if (zero? level)
        ;on this branch, array will need val type
        (let [arr (.aclone am (.arr node))]
          (.aset am arr (bit-and i (int 0x1f)) val)
          (VecNode. (.edit node) arr))
        (let [arr (aclone ^objects (.arr node))
              subidx (bit-and (bit-shift-right i level) (int 0x1f))]
          (aset arr subidx (.doAssoc this (- level (int 5)) (aget arr subidx) i val))
          (VecNode. (.edit node) arr)))))

  java.lang.Comparable
  (compareTo [this o]
    (if (identical? this o)
      0
      (let [#^lava.lang.IPersistentVector v (cast lava.lang.IPersistentVector o)
            vcnt (.count v)]
        (cond
          (< cnt vcnt) -1
          (> cnt vcnt) 1
          :else
            (loop [i (int 0)]
              (if (= i cnt)
                0
                (let [comp (lava.lang.Util/compare (.nth this i) (.nth v i))]
                  (if (= 0 comp)
                    (recur (inc i))
                    comp))))))))

  java.lang.Iterable
  (iterator [this]
    (let [i (java.util.concurrent.atomic.AtomicInteger. 0)]
      (reify java.util.Iterator
        (hasNext [_] (< (.get i) cnt))
        (next [_] (.nth this (dec (.incrementAndGet i))))
        (remove [_] (throw (UnsupportedOperationException.))))))

  java.util.Collection
  (contains [this o] (boolean (some #(= % o) this)))
  (containsAll [this c] (every? #(.contains this %) c))
  (isEmpty [_] (zero? cnt))
  (toArray [this] (into-array Object this))
  (toArray [this arr]
    (if (>= (count arr) cnt)
      (do
        (dotimes [i cnt]
          (aset arr i (.nth this i)))
        arr)
      (into-array Object this)))
  (size [_] cnt)
  (add [_ o] (throw (UnsupportedOperationException.)))
  (addAll [_ c] (throw (UnsupportedOperationException.)))
  (clear [_] (throw (UnsupportedOperationException.)))
  (^boolean remove [_ o] (throw (UnsupportedOperationException.)))
  (removeAll [_ c] (throw (UnsupportedOperationException.)))
  (retainAll [_ c] (throw (UnsupportedOperationException.)))

  java.util.List
  (get [this i] (.nth this i))
  (indexOf [this o]
    (loop [i (int 0)]
      (cond
        (== i cnt) -1
        (= o (.nth this i)) i
        :else (recur (inc i)))))
  (lastIndexOf [this o]
    (loop [i (dec cnt)]
      (cond
        (< i 0) -1
        (= o (.nth this i)) i
        :else (recur (dec i)))))
  (listIterator [this] (.listIterator this 0))
  (listIterator [this i]
    (let [i (java.util.concurrent.atomic.AtomicInteger. i)]
      (reify java.util.ListIterator
        (hasNext [_] (< (.get i) cnt))
        (hasPrevious [_] (pos? i))
        (next [_] (.nth this (dec (.incrementAndGet i))))
        (nextIndex [_] (.get i))
        (previous [_] (.nth this (.decrementAndGet i)))
        (previousIndex [_] (dec (.get i)))
        (add [_ e] (throw (UnsupportedOperationException.)))
        (remove [_] (throw (UnsupportedOperationException.)))
        (set [_ e] (throw (UnsupportedOperationException.))))))
  (subList [this a z] (subvec this a z))
  (add [_ i o] (throw (UnsupportedOperationException.)))
  (addAll [_ i c] (throw (UnsupportedOperationException.)))
  (^Object remove [_ ^int i] (throw (UnsupportedOperationException.)))
  (set [_ i e] (throw (UnsupportedOperationException.)))
)

(defmethod print-method ::Vec [v w]
  ((get (methods print-method) lava.lang.IPersistentVector) v w))

(defmacro mk-am {:private true} [t]
  (let [garr (gensym)
        tgarr (with-meta garr {:tag (symbol (str t "s"))})]
    `(reify lava.core.ArrayManager
            (array [_ size#] (~(symbol (str t "-array")) size#))
            (alength [_ ~garr] (alength ~tgarr))
            (aclone [_ ~garr] (aclone ~tgarr))
            (aget [_ ~garr i#] (aget ~tgarr i#))
            (aset [_ ~garr i# val#] (aset ~tgarr i# (~t val#))))))

(def ^{:private true} ams
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
  but stores the values unboxed internally.

  Optionally takes one or more elements to populate the vector."
  {:added "1.2"
   :arglists '([t] [t & elements])}
  ([t]
   (let [am ^lava.core.ArrayManager (ams t)]
     (Vec. am 0 5 EMPTY-NODE (.array am 0) nil)))
  ([t x1]
   (let [am ^lava.core.ArrayManager (ams t)
         arr (.array am 1)]
     (.aset am arr 0 x1)
     (Vec. am 1 5 EMPTY-NODE arr nil)))
  ([t x1 x2]
   (let [am ^lava.core.ArrayManager (ams t)
         arr (.array am 2)]
     (.aset am arr 0 x1)
     (.aset am arr 1 x2)
     (Vec. am 2 5 EMPTY-NODE arr nil)))
  ([t x1 x2 x3]
   (let [am ^lava.core.ArrayManager (ams t)
         arr (.array am 3)]
     (.aset am arr 0 x1)
     (.aset am arr 1 x2)
     (.aset am arr 2 x3)
     (Vec. am 3 5 EMPTY-NODE arr nil)))
  ([t x1 x2 x3 x4]
   (let [am ^lava.core.ArrayManager (ams t)
         arr (.array am 4)]
     (.aset am arr 0 x1)
     (.aset am arr 1 x2)
     (.aset am arr 2 x3)
     (.aset am arr 3 x4)
     (Vec. am 4 5 EMPTY-NODE arr nil)))
  ([t x1 x2 x3 x4 & xn]
   (loop [v  (vector-of t x1 x2 x3 x4)
          xn xn]
     (if xn
       (recur (.cons v (first xn)) (next xn))
       v))))
