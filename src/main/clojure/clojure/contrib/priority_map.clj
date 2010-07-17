;; A priority map is a map from items to priorities,
;; offering queue-like peek/pop as well as the map-like ability to
;; easily reassign priorities and other conveniences.
;; by Mark Engelberg (mark.engelberg@gmail.com)
;; July 16, 2010

(ns clojure.contrib.priority-map
  (:use clojure.test)
  (:import clojure.lang.MapEntry java.util.Map clojure.lang.PersistentTreeMap))

(comment
"A priority map is very similar to a sorted map, but whereas a sorted map produces a
sequence of the entries sorted by key, a priority map produces the entries sorted by value.
In addition to supporting all the functions a sorted map supports, a priority map
can also be thought of as a queue of [item priority] pairs.  To support usage as
a versatile priority queue, priority maps also support conj/peek/pop operations.

The standard way to construct a priority map is with priority-map:
user=> (def p (priority-map :a 2 :b 1 :c 3 :d 5 :e 4 :f 3))
#'user/p
user=> p
{:b 1, :a 2, :c 3, :f 3, :e 4, :d 5}

So :b has priority 1, :a has priority 2, and so on.
Notice how the priority map prints in an order sorted by its priorities (i.e., the map's values)

We can use assoc to assign a priority to a new item:
user=> (assoc p :g 1)
{:b 1, :g 1, :a 2, :c 3, :f 3, :e 4, :d 5}

or to assign a new priority to an extant item:
user=> (assoc p :c 4)
{:b 1, :a 2, :f 3, :c 4, :e 4, :d 5}

We can remove an item from the priority map:
user=> (dissoc p :e)
{:b 1, :a 2, :c 3, :f 3, :d 5}

An alternative way to add to the priority map is to conj a [item priority] pair:
user=> (conj p [:g 0])
{:g 0, :b 1, :a 2, :c 3, :f 3, :e 4, :d 5}

or use into:
user=> (into p [[:g 0] [:h 1] [:i 2]])
{:g 0, :b 1, :h 1, :a 2, :i 2, :c 3, :f 3, :e 4, :d 5}

Priority maps are countable:
user=> (count p)
6

Like other maps, equivalence is based not on type, but on contents.
In other words, just as a sorted-map can be equal to a hash-map,
so can a priority-map.
user=> (= p {:b 1, :a 2, :c 3, :f 3, :e 4, :d 5})
true

You can test them for emptiness:
user=> (empty? (priority-map))
true
user=> (empty? p)
false

You can test whether an item is in the priority map:
user=> (contains? p :a)
true
user=> (contains? p :g)
false

It is easy to look up the priority of a given item, using any of the standard map mechanisms:
user=> (get p :a)
2
user=> (get p :g 10)
10
user=> (p :a)
2
user=> (:a p)
2

Priority maps derive much of their utility by providing priority-based seq.
Note that no guarantees are made about the order in which items of the same priority appear.
user=> (seq p)
([:b 1] [:a 2] [:c 3] [:f 3] [:e 4] [:d 5])
Because no guarantees are made about the order of same-priority items, note that
rseq might not be an exact reverse of the seq.  It is only guaranteed to be in
descending order.
user=> (rseq p)
([:d 5] [:e 4] [:c 3] [:f 3] [:a 2] [:b 1])

This means first/rest/next/for/map/etc. all operate in priority order.
user=> (first p)
[:b 1]
user=> (rest p)
([:a 2] [:c 3] [:f 3] [:e 4] [:d 5])

Priority maps support metadata:
user=> (meta (with-meta p {:extra :info}))
{:extra :info}

But perhaps most importantly, priority maps can also function as priority queues.
peek, like first, gives you the first [item priority] pair in the collection.
pop removes the first [item priority] from the collection.
(Note that unlike rest, which returns a seq, pop returns a priority map).

user=> (peek p)
[:b 1]
user=> (pop p)
{:a 2, :c 3, :f 3, :e 4, :d 5}

It is also possible to use a custom comparator:
user=> (priority-map-by (comparator >) :a 1 :b 2 :c 3)
{:c 3, :b 2, :a 1}

All of these operations are efficient.  Generally speaking, most operations
are O(log n) where n is the number of distinct priorities.  Some operations
(for example, straightforward lookup of an item's priority, or testing
  whether a given item is in the priority map) are as efficient
as Clojure's built-in map.

The key to this efficiency is that internally, not only does the priority map store
an ordinary hash map of items to priority, but it also stores a sorted map that
maps priorities to sets of items with that priority.

A typical textbook priority queue data structure supports at the ability to add
a [item priority] pair to the queue, and to pop/peek the next [item priority] pair.
But many real-world applications of priority queues require more features, such
as the ability to test whether something is already in the queue, or to reassign
a priority.  For example, a standard formulation of Dijkstra's algorithm requires the
ability to reduce the priority number associated with a given item.  Once you
throw persistence into the mix with the desire to adjust priorities, the traditional
structures just don't work that well.

This particular blend of Clojure's built-in hash sets, hash maps, and sorted maps
proved to be a great way to implement an especially flexible persistent priority queue.

Connoisseurs of algorithms will note that this structure's peek operation is not O(1) as
it would be if based upon a heap data structure, but I feel this is a small concession for
the blend of persistence, priority reassignment, and priority-sorted seq, which can be
quite expensive to achieve with a heap (I did actually try this for comparison).  Furthermore,
this peek's logarithmic behavior is quite good (on my computer I can do a million
                                                 peeks at a priority map with a million items in 750ms).  Also, consider that peek and pop
usually follow one another, and even with a heap, pop is logarithmic.  So the net combination
of peek and pop is not much different between this versatile formulation of a priority map and
a more limited heap-based one.  In a nutshell, peek, although not O(1), is unlikely to be the
bottleneck in your program.

All in all, I hope you will find priority maps to be an easy-to-use and useful addition
to Clojure's assortment of built-in maps (hash-map and sorted-map).
")

; Note that the plan is to eventually support subseq, but this will require
; some changes to core:
;; user=> (subseq p < 3)
;; ([:b 1] [:a 2])
;; user=> (subseq p >= 3)
;; ([:c 3] [:f 3] [:e 4] [:d 5])

(declare pm-empty)

; A Priority Map is comprised of a sorted map that maps priorities to hash sets of items
; with that priority (priority->set-of-items),
; as well as a hash map that maps items to priorities (item->priority)
; Priority maps may also have metadata

(deftype PersistentPriorityMap [priority->set-of-items item->priority __meta]
  Object
  (toString [this] (str (.seq this)))

  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this item] (get item->priority item))
  (valAt [this item not-found] (get item->priority item not-found))

  clojure.lang.IPersistentMap
  (count [this] (count item->priority))

  (assoc [this item priority]
    (let [current-priority (get item->priority item nil)]
      (if current-priority
        ;Case 1 - item is already in priority map, so this is a reassignment
        (if (= current-priority priority)
          ;Subcase 1 - no change in priority, do nothing
          this
          (let [item-set (get priority->set-of-items current-priority)]
            (if (= (count item-set) 1)
              ;Subcase 2 - it was the only item of this priority
              ;so remove old priority entirely
              ;and conj item onto new priority's set
              (PersistentPriorityMap.
                (assoc (dissoc priority->set-of-items current-priority)
                  priority (conj (get priority->set-of-items priority #{}) item))
                (assoc item->priority item priority))
              ;Subcase 3 - there were many items associated with the item's original priority,
              ;so remove it from the old set and conj it onto the new one.
              (PersistentPriorityMap.
                (assoc priority->set-of-items
                  current-priority (disj (get priority->set-of-items current-priority) item)
                  priority (conj (get priority->set-of-items priority #{}) item))
                (assoc item->priority item priority)))))
        ; Case 2: Item is new to the priority map, so just add it.
        (PersistentPriorityMap.
          (assoc priority->set-of-items
            priority (conj (get priority->set-of-items priority #{}) item))
          (assoc item->priority item priority)))))

  (empty [this] pm-empty)

  ; cons defines conj behavior
  (cons [this e] (let [[item priority] e] (.assoc this item priority)))

  ;This implementation of equiv is a direct translation of the one found in APersistentMap
  (equiv [this o]
    (cond
      (not (instance? Map o)) false
      (not= (count this) (count o)) false
      :else (loop [s (.seq this)]
              (if (nil? s) true
                (let [^java.util.Map$Entry e (first s),
                      found (.containsKey ^Map o (.getKey e))]
                  (if (or (not found) (not (clojure.lang.Util/equiv (.getValue e) (.get ^Map o (.getKey e)))))
                    false
                    (recur (next s))))))))

  ;containsKey implements (contains? pm k) behavior
  (containsKey [this item] (contains? item->priority item))

  (entryAt [this k]
    (let [v (.valAt this k this)]
      (when-not (identical? v this)
        (MapEntry. k v))))

  (seq [this]
    (seq (for [[priority item-set] priority->set-of-items, item item-set]
           (MapEntry. item priority))))

  ;without implements (dissoc pm k) behavior
  (without
    [this item]
    (let [priority (item->priority item),
          item-set (priority->set-of-items priority),]
      (if (= (count item-set) 1)
        ;If it is the only item with this priority, remove that priority's set completely
        (PersistentPriorityMap. (dissoc priority->set-of-items priority) (dissoc item->priority item))
        ;Otherwise, just remove the item from the priority's set.
        (PersistentPriorityMap.
          (assoc priority->set-of-items priority (disj item-set item)),
          (dissoc item->priority item)))))

  java.io.Serializable  ;Serialization comes for free with the other things implemented
  Map ;Makes this compatible with java's map
  (size [this] (count item->priority))
  (isEmpty [this] (zero? (count item->priority)))
  (containsValue [this v] (contains? (priority->set-of-items this) v))
  (get [this k] (.valAt this k))
  (put [this k v] (throw (UnsupportedOperationException.)))
  (remove [this k] (throw (UnsupportedOperationException.)))
  (putAll [this m] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  (keySet [this] (set (keys this)))
  (values [this] (vals this))
  (entrySet [this] (set this))

  clojure.lang.IPersistentStack
  (peek [this]
    (when-not (.isEmpty this)
      (let [f (first priority->set-of-items)]
        (MapEntry. (first (val f)) (key f)))))

  (pop [this]
    (if (.isEmpty this) (throw (IllegalStateException. "Can't pop empty priority map"))
      (let [f (first priority->set-of-items),
            item-set (val f)
            item (first item-set),
            priority (key f)]
        (if (= (count item-set) 1)
          ;If the first item is the only item with its priority, remove that priority's set completely
          (PersistentPriorityMap.
            (dissoc priority->set-of-items priority)
            (dissoc item->priority item))
          ;Otherwise, just remove the item from the priority's set.
          (PersistentPriorityMap.
            (assoc priority->set-of-items priority (disj item-set item)),
            (dissoc item->priority item))))))

  clojure.lang.IFn
  ;makes priority map usable as a function
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))

  clojure.lang.IObj
  ;adds metadata support
  (meta [this] __meta)
  (withMeta [this m] (PersistentPriorityMap. priority->set-of-items item->priority m))

  clojure.lang.Reversible
  (rseq [this]
    (seq (for [[priority item-set] (rseq priority->set-of-items), item item-set]
           (MapEntry. item priority)))))

;; clojure.lang.Sorted
;; ; These methods provide support for subseq
;; (comparator [this] (.comparator ^PersistentTreeMap priority->set-of-items))
;; (entryKey [this entry] (val entry))
;; (seqFrom [this k ascending]
;;   (let [sets (if ascending (subseq priority->set-of-items >= k) (rsubseq priority->set-of-items <= k))]
;;     (seq (for [[priority item-set] sets, item item-set]
;;            (MapEntry. item priority)))))
;; (seq [this ascending]
;;   (if ascending (seq this) (rseq this))))

(def ^:private pm-empty (PersistentPriorityMap. (sorted-map) {} {}))
(defn- pm-empty-by [comparator] (PersistentPriorityMap. (sorted-map-by comparator) {}))

; The main way to build priority maps
(defn priority-map
  "keyval => key val
Returns a new priority map with supplied mappings"
  [& keyvals]
  (reduce conj pm-empty (partition 2 keyvals)))

(defn priority-map-by
  "keyval => key val
Returns a new priority map with supplied mappings"
  [comparator & keyvals]
  (reduce conj (pm-empty-by comparator) (partition 2 keyvals)))

(deftest test-priority-map
  (let [p (priority-map :a 2 :b 1 :c 3 :d 5 :e 4 :f 3)
        h {:a 2 :b 1 :c 3 :d 5 :e 4 :f 3}]
    (are [x y] (= x y)
      p {:a 2 :b 1 :c 3 :d 5 :e 4 :f 3}
      (assoc p :g 1) (assoc h :g 1)
      (assoc p :g 0) (assoc h :g 0)
      (assoc p :c 4) (assoc h :c 4)
      (assoc p :c 6) (assoc h :c 6)
      (assoc p :b 2) (assoc h :b 2)
      (assoc p :b 6) (assoc h :b 6)
      (dissoc p :e) (dissoc h :e)
      (dissoc p :g) (dissoc h :g)
      (dissoc p :c) (dissoc h :c)
      (conj p [:g 1]) (conj h [:g 1])
      (conj p [:g 0]) (conj h [:g 0])
      (conj p [:c 4]) (conj h [:c 4])
      (conj p [:c 6]) (conj h [:c 6])
      (conj p [:b 2]) (conj h [:b 2])
      (conj p [:b 6]) (conj h [:b 6])
      (into p [[:g 0] [:h 1] [:i 2]]) (into h [[:g 0] [:h 1] [:i 2]])
      (count p) (count h)
      (empty? p) false
      (empty? (priority-map)) true
      (contains? p :a) true
      (contains? p :g) false
      (get p :a) 2
      (get p :a 8) 2
      (get p :g) nil
      (get p :g 8) 8
      (p :a) 2
      (:a p) 2
      (seq p) '([:b 1] [:a 2] [:c 3] [:f 3] [:e 4] [:d 5])
      ;Note if implementation of hash-set changes, the :c and :f entries might be swapped
      (rseq p) '([:d 5] [:e 4] [:c 3] [:f 3] [:a 2] [:b 1])
      ;; (subseq p < 3) '([:b 1] [:a 2])
      ;; (subseq p <= 3) '([:b 1] [:a 2] [:c 3] [:f 3])
      ;; (subseq p > 3) '([:e 4] [:d 5])
      ;; (subseq p >= 3) '([:c 3] [:f 3] [:e 4] [:d 5])
      ;; (subseq p > 3 <= 4) '([:e 4])
      ;; (subseq p >= 3 <= 4) '([:c 3] [:f 3] [:e 4])
      ;; (subseq p >= 3 < 4) '([:c 3] [:f 3])
      ;; (subseq p > 3 < 4) nil
      ;; (subseq p > 2 < 3) nil
      ;; (subseq p > 2 <= 3) '([:c 3] [:f 3])
      ;; (subseq p >= 2 < 3) '([:a 2])
      ;; (subseq p >= 2 <= 3) '([:a 2] [:c 3] [:f 3])
      ;; (rsubseq p < 3) '([:a 2] [:b 1])
      ;; (rsubseq p <= 3) '([:c 3] [:f 3] [:a 2] [:b 1] )
      ;; (rsubseq p > 3) '([:d 5] [:e 4])
      ;; (rsubseq p >= 3) '([:d 5] [:e 4] [:c 3] [:f 3])
      ;; (rsubseq p > 3 <= 4) '([:e 4])
      ;; (rsubseq p >= 3 <= 4) '([:e 4] [:c 3] [:f 3] )
      ;; (rsubseq p >= 3 < 4) '([:c 3] [:f 3])
      ;; (rsubseq p > 3 < 4) nil
      ;; (rsubseq p > 2 < 3) nil
      ;; (rsubseq p > 2 <= 3) '([:c 3] [:f 3])
      ;; (rsubseq p >= 2 < 3) '([:a 2])
      ;; (rsubseq p >= 2 <= 3) '([:c 3] [:f 3] [:a 2] )
      (first p) [:b 1]
      (rest p) '([:a 2] [:c 3] [:f 3] [:e 4] [:d 5])
      (meta (with-meta p {:extra :info})) {:extra :info}
      (peek p) [:b 1]
      (pop p) {:a 2 :c 3 :f 3 :e 4 :d 5}
      (peek (priority-map)) nil
      (seq (priority-map-by (comparator >) :a 1 :b 2 :c 3)) [[:c 3] [:b 2] [:a 1]])))

