;/**
; *   Copyright (c) Rich Hickey. All rights reserved.
; *   The use and distribution terms for this software are covered by the
; *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
; *   which can be found in the file CPL.TXT at the root of this distribution.
; *   By using this software in any fashion, you are agreeing to be bound by
; * 	 the terms of this license.
; *   You must not remove this notice, or any other, from this software.
; **/

(in-module "clojure")
(import "org.clojure.runtime" '(Num RT IntegerNum Cons))
#+:JVM(import "java.lang" '(System))
#+:CLI(import "System" '(Console))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; data and control flow ;;;;;;;;;;;;;;;;;;;;

(defn apply (fn & args+)
  (.applyTo fn __tld (spread* args+)))

(defn complement (fn)
  (fn (& args)
    (not (apply fn args))))

(defn constantly (x)
  (fn (& args) x))

(defn identity (x) x)

(defn eq (x y)
  (RT.eq x y))

(defn eql (x y)
  (RT.eql x y))

(defn equal (x y)
  (RT.equal x y))

(defn equals (x y)
  #+:JVM (.equals x y)
  #+:CLI (.Equals x y))

(defn not (x)
  (if x nil t))

(defn null? (x)
      (if x nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn acons (key val alist)
  (cons (cons key val) alist))

(defn* adjoin 
       ((x list)
        (if (member x list)
            list
          (cons x list)))
       ((x list keys)
        (if (member x list keys)
            list
          (cons x list))))

(defn* append
       (() nil)
       ((first) first)
       ((first & rest)
        (nconc (copy-list first) (apply append rest))))

(defn* assoc
       ((item alist) (assoc item alist nil))
       ((item alist keys)
        (assoc-if (fn (y)
                    ((or (:test keys) eql) item y))
                  alist
                  keys)))

(defn* assoc-if
       ((fun alist) (assoc-if fun alist nil))
       ((fun alist keys)
        (cond ((atom? alist) nil)
              ((and (cons? (first alist))
                    (fun (if (:key keys)
                            ((:key keys) (ffirst alist))
                          (ffirst alist))))
               (first alist))
              (t (assoc-if fun (rest alist) keys)))))

(defn atom? (x) (not (cons? x)))

(defn* butlast
       ((list) (butlast list 1))
       ((list n)
        (nreverse (nthcdr n (reverse list)))))

(defn first (x)
  (when x
    (.first x)))

(defn rest (x)
  (when x
    (.rest x)))

(defn ffirst (x)
  (when x
    (first (first x))))

(defn frest (x)
  (when x
    (first (rest x))))

(defn rrest (x)
  (when x
    (rest (rest x))))

(defn cons (x y)
  (RT.cons x y))

(defn cons? (x)
  (instance? x Cons.))

(defn copy-list (list)
  (letfn ((cl (x)
              (if (atom? x)
                  x
                (cons (first x) 
                      (cl (rest x))))))
    (cons (first list) 
          (cl (rest list)))))

(defn copy-tree (tree)
  (if (atom? tree)
      tree
      (cons (copy-tree (first tree))
            (copy-tree (rest tree)))))

(defn* last
       ((list) (last list 1))
       ((list n)
        (do ((l list (rest l))
             (r list)
             (i 0 (1+ i)))
            ((null? l) r)
          (if (>= i n) (pop r)))))

(defn list (&rest args)
  args)

(defn spread* (args)
       (cond
        ((null? args) nil)
        ((null? (rest args)) (first args))
        (t (cons (first args) (rest (rest args))))))

(defn list* (& args)
       (spread* args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; numbers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn* +
       (() 0)
       ((x) x)
       ((x y)
        (Num.add x y))
       ((x y & nums)
        (Num.add (Num.add x y) (apply + nums))))

(defn* -
       ((x) (Num.negate x))
       ((x y)
        (Num.subtract x y))
       ((x y & nums)
        (apply - (Num.subtract x y) nums)))

(defn* *
       (() 1)
       ((x) x)
       ((x y)
        (Num.multiply x y))
       ((x y & nums)
        (Num.multiply (Num.multiply x y) (apply + nums))))

(defn 1+ (x)
  (.onePlus x))

(defn 1- (x)
  (.oneMinus x))



(defn integer? (x)
  (instance? x IntegerNum.))

(defn neg? (x)
  (.minusp x))

(defn num? (x)
  (instance? x Num.))

(defn pos? (x)
  (.plusp x))

(defn zerop (x)
  ;todo implement in Num
  (= x Num.ZERO))

(defn* =
       ((x) t)
       ((x y)
        (Num.equiv x y))
       ((x y & rest)
        (and (Num.equiv x y)
             (apply = y rest))))

(defcomparator < Num.lt)
(defcomparator <= Num.lte)
(defcomparator > Num.gt)
(defcomparator >= Num.gte)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; printer ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn prn (x)
      #+:JVM (.println System.out x)
      #+:CLI (Console.WriteLine x))



(defn fact (n)
      (if (= n 1)
          1
        (* n (fact (1- n)))))

(defn fmain (args)
      (prn (fact 50)))

(defmain fmain)