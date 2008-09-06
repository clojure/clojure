;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.set)

(defn union
  "Returns a set that is the union of the two sets."
  [xset yset]
    (reduce conj xset yset))

(defn difference
  "Returns a set that is xset without the elements of yset."
  [xset yset]
    (reduce disj xset yset))

(defn intersection
  "Returns a set of the elements present in both xset and yset."
  [xset yset]
    (difference xset (difference xset yset)))

(defn select
  "Returns a set of the elements for which pred is true"
  [pred xset]
    (reduce (fn [s k] (if (pred k) s (disj s k)))
            xset xset))

(defn project
  "Returns a rel of the elements of xrel with only the keys in ks"
  [xrel ks]
    (set (map #(select-keys % ks) xrel)))

(defn rename-keys
  "Returns the map with the keys in kmap renamed to the vals in kmap"
  [map kmap]
    (reduce 
     (fn [m [old new]]
       (if (not= old new)
         (-> m (assoc new (m old)) (dissoc old))
         m)) 
     map kmap))

(defn rename
  "Returns a rel of the maps in xrel with the keys in kmap renamed to the vals in kmap"
  [xrel kmap]
    (set (map #(rename-keys % kmap) xrel)))

(defn index
  "Returns a map of the distinct values of ks in the xrel mapped to a
  set of the maps in xrel with the corresponding values of ks."
  [xrel ks]
    (reduce
     (fn [m x]
       (let [ik (select-keys x ks)]
         (assoc m ik (conj (get m ik #{}) x))))
     {} xrel))
   
(defn map-invert
  "Returns the map with the vals mapped to the keys."
  [m] (reduce (fn [m [k v]] (assoc m v k)) {} m))

(defn join
  "When passed 2 rels, returns the rel corresponding to the natural
  join. When passed an additional keymap, joins on the corresponding
  keys."
  ([xrel yrel] ;natural join
   (if (and (seq xrel) (seq yrel))
     (let [ks (intersection (set (keys (first xrel))) (set (keys (first yrel))))
           [r s] (if (<= (count xrel) (count yrel))
                   [xrel yrel]
                   [yrel xrel])
           idx (index r ks)]
       (reduce (fn [ret x]
                 (let [found (idx (select-keys x ks))]
                   (if found
                     (reduce #(conj %1 (merge %2 x)) ret found)
                     ret)))
               #{} s))
     #{}))
  ([xrel yrel km] ;arbitrary key mapping
   (let [[r s k] (if (<= (count xrel) (count yrel))
                   [xrel yrel (map-invert km)]
                   [yrel xrel km])
         idx (index r (vals k))]
     (reduce (fn [ret x]
               (let [found (idx (rename-keys (select-keys x (keys k)) k))]
                 (if found
                   (reduce #(conj %1 (merge %2 x)) ret found)
                   ret)))
             #{} s))))

(comment
(refer 'set)
(def xs #{{:a 11 :b 1 :c 1 :d 4}
         {:a 2 :b 12 :c 2 :d 6}
         {:a 3 :b 3 :c 3 :d 8 :f 42}})

(def ys #{{:a 11 :b 11 :c 11 :e 5}
         {:a 12 :b 11 :c 12 :e 3}
         {:a 3 :b 3 :c 3 :e 7 }})

(join xs ys)
(join xs (rename ys {:b :yb :c :yc}) {:a :a})

(union #{:a :b :c} #{:c :d :e })
(difference #{:a :b :c} #{:c :d :e})
(intersection #{:a :b :c} #{:c :d :e})

(index ys [:b])
)

