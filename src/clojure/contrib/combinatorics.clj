;;; combinatorics.clj: efficient, functional algorithms for generating lazy
;;; sequences for common combinatorial functions.

;; by Mark Engelberg (mark.engelberg@gmail.com)
;; January 27, 2009

(comment
"  
(combinations items n) - A lazy sequence of all the unique
ways of taking n different elements from items.
Example: (combinations [1 2 3] 2) -> ((1 2) (1 3) (2 3))

(subsets items) - A lazy sequence of all the subsets of
items (but generalized to all sequences, not just sets).
Example: (subsets [1 2 3]) -> (() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))

(cartesian-product & seqs) - Takes any number of sequences
as arguments, and returns a lazy sequence of all the ways
to take one item from each seq.
Example: (cartesian-product [1 2] [3 4]) -> ((1 3) (1 4) (2 3) (2 4))
(cartesian-product seq1 seq2 seq3 ...) behaves like but is
faster than a nested for loop, such as:
(for [i1 seq1 i2 seq2 i3 seq3 ...] (list i1 i2 i3 ...))

(selections items n) - A lazy sequence of all the ways to
take n (possibly the same) items from the sequence of items.
Example: (selections [1 2] 3) -> ((1 1 1) (1 1 2) (1 2 1) (1 2 2) (2 1 1) (2 1 2) (2 2 1) (2 2 2))

(permutations items) - A lazy sequence of all the permutations
of items.
Example: (permutations [1 2 3]) -> ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

(lex-permutations items) - A lazy sequence of all distinct
permutations in lexicographic order
(this function returns the permutations as
vectors).  Only works on sequences of comparable
items.  (Note that the result will be quite different from
permutations when the sequence contains duplicate items.)  
Example: (lex-permutations [1 1 2]) -> ([1 1 2] [1 2 1] [2 1 1])

About permutations vs. lex-permutations:
lex-permutations is faster than permutations, but only works
on sequences of numbers.  They operate differently
on sequences with duplicate items (lex-permutations will only
give you back distinct permutations).  lex-permutations always
returns the permutations sorted lexicographically whereas
permutations will be in an order where the input sequence
comes first.  In general, I recommend using the regular
permutations function unless you have a specific
need for lex-permutations.

About this code:
These combinatorial functions can be written in an elegant way using recursion.  However, when dealing with combinations and permutations, you're usually generating large numbers of things, and speed counts.  My objective was to write the fastest possible code I could, restricting myself to Clojure's functional, persistent data structures (rather than using Java's arrays) so that this code could be safely leveraged within Clojure's transactional concurrency system.

I also restricted myself to algorithms that return results in a standard order.  For example, there are faster ways to generate cartesian-product, but I don't know of a faster way to generate the results in the standard nested-for-loop order.

Most of these algorithms are derived from algorithms found in Knuth's wonderful Art of Computer Programming books (specifically, the volume 4 fascicles), which present fast, iterative solutions to these common combinatorial problems.  Unfortunately, these iterative versions are somewhat inscrutable.  If you want to better understand these algorithms, the Knuth books are the place to start.

On my own computer, I use versions of all these algorithms that return sequences built with an uncached variation of lazy-seq.  Not only does this boost performance, but it's easier to use these rather large sequences more safely (from a memory consumption standpoint).  If some form of uncached sequences makes it into Clojure, I will update this accordingly.
"
)


(ns clojure.contrib.combinatorics)

(defn- index-combinations
  [n cnt]
  (lazy-seq
   (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
	 iter-comb
	 (fn iter-comb [c j]
	   (if (> j n) nil
	       (let [c (assoc c j (dec (c j)))]
		 (if (< (c j) j) [c (inc j)]
		     (loop [c c, j j]
		       (if (= j 1) [c j]
			   (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
	 step
	 (fn step [c j]
	   (cons (rseq (subvec c 1 (inc n)))
		 (lazy-seq (let [next-step (iter-comb c j)]
			     (when next-step (step (next-step 0) (next-step 1)))))))]
     (step c 1))))

(defn combinations
  "All the unique ways of taking n different elements from items"
  [items n]      
  (let [v-items (vec (reverse items))]
    (if (zero? n) (list ())
	(let [cnt (count items)]
	  (cond (> n cnt) nil
		(= n cnt) (list (seq items))
		:else
		(map #(map v-items %) (index-combinations n cnt)))))))

(defn subsets
  "All the subsets of items"
  [items]
  (mapcat (fn [n] (combinations items n))
	  (range (inc (count items)))))

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
	step
	(fn step [v-seqs]
	  (let [increment
		(fn [v-seqs]
		  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
		    (if (= i -1) nil
			(if-let [rst (next (v-seqs i))]
			  (assoc v-seqs i rst)
			  (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
	    (when v-seqs
	       (cons (map first v-seqs)
		     (lazy-seq (step (increment v-seqs)))))))]
    (when (every? first seqs)
      (lazy-seq (step v-original-seqs)))))


(defn selections
  "All the ways of taking n (possibly the same) elements from the sequence of items"
  [items n]
  (apply cartesian-product (take n (repeat items))))


(defn- iter-perm [v]
  (let [len (count v),
	j (loop [i (- len 2)]
	     (cond (= i -1) nil
		   (< (v i) (v (inc i))) i
		   :else (recur (dec i))))]
    (when j
      (let [vj (v j),
	    l (loop [i (dec len)]
		(if (< vj (v i)) i (recur (dec i))))]
	(loop [v (assoc v j (v l) l vj), k (inc j), l (dec len)]
	  (if (< k l)
	    (recur (assoc v k (v l) l (v k)) (inc k) (dec l))
	    v))))))

(defn- vec-lex-permutations [v]
  (when v (cons v (lazy-seq (vec-lex-permutations (iter-perm v))))))

(defn lex-permutations
  "Fast lexicographic permutation generator for a sequence of numbers"
  [c]
  (lazy-seq
   (let [vec-sorted (vec (sort c))]
     (if (zero? (count vec-sorted))
       (list [])
       (vec-lex-permutations (vec (sort c)))))))
  
(defn permutations
  "All the permutations of items, lexicographic by index"
  [items]
  (let [v (vec items)]
    (map #(map v %) (lex-permutations (range (count v))))))
