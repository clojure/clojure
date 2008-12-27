;;  Copyright (c) Frantisek Sodomka. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  clojure.contrib.test-clojure.sequences
;;
;;  Created 27 December 2008

(ns clojure.contrib.test-clojure.sequences
  (:use clojure.contrib.test-is))

;; first
;;
(deftest test-first
  (is (thrown? IllegalArgumentException (first)))
  (is (thrown? IllegalArgumentException (first true)))
  (is (thrown? IllegalArgumentException (first false)))
  (is (thrown? IllegalArgumentException (first 1)))
  (is (thrown? IllegalArgumentException (first 1 2)))
  (is (thrown? IllegalArgumentException (first \a)))
  (is (thrown? IllegalArgumentException (first 's)))
  (is (thrown? IllegalArgumentException (first :k)))
  (are (= _1 _2)
    (first nil) nil

    ; string
    (first "") nil
    (first "a") \a
    (first "abc") \a

    ; list
    (first ()) nil
    (first '(1)) 1
    (first '(1 2 3)) 1

    (first '(nil)) nil
    (first '(1 nil)) 1
    (first '(nil 2)) nil
    (first '(())) ()
    (first '(() nil)) ()
    (first '(() 2 nil)) ()

    ; vector
    (first []) nil
    (first [1]) 1
    (first [1 2 3]) 1

    (first [nil]) nil
    (first [1 nil]) 1
    (first [nil 2]) nil
    (first [[]]) []
    (first [[] nil]) []
    (first [[] 2 nil]) []

    ; set
    (first #{}) nil
    (first #{1}) 1
    ;(first #{1 2 3}) 1

    (first #{nil}) nil
    ;(first #{1 nil}) 1
    ;(first #{nil 2}) nil
    (first #{#{}}) #{}
    ;(first #{#{} nil}) #{}
    ;(first #{#{} 2 nil}) #{}

    ; map
    (first {}) nil
    (first (sorted-map :a 1)) '(:a 1)
    (first (sorted-map :a 1 :b 2 :c 3)) '(:a 1)

    ; array
    (first (into-array [])) nil
    (first (into-array [1])) 1
    (first (into-array [1 2 3])) 1
    (first (to-array [nil])) nil
    (first (to-array [1 nil])) 1
    (first (to-array [nil 2])) nil
  )
)

;; rest
;;
(deftest test-rest
  (is (thrown? IllegalArgumentException (rest)))
  (is (thrown? IllegalArgumentException (rest true)))
  (is (thrown? IllegalArgumentException (rest false)))
  (is (thrown? IllegalArgumentException (rest 1)))
  (is (thrown? IllegalArgumentException (rest 1 2)))
  (is (thrown? IllegalArgumentException (rest \a)))
  (is (thrown? IllegalArgumentException (rest 's)))
  (is (thrown? IllegalArgumentException (rest :k)))
  (are (= _1 _2)
    (rest nil) nil

    ; string
    (rest "") nil
    (rest "a") nil
    (rest "abc") '(\b \c)

    ; list
    (rest ()) nil
    (rest '(1)) nil
    (rest '(1 2 3)) '(2 3)

    (rest '(nil)) nil
    (rest '(1 nil)) '(nil)
    (rest '(1 ())) '(())
    (rest '(nil 2)) '(2)
    (rest '(())) nil
    (rest '(() nil)) '(nil)
    (rest '(() 2 nil)) '(2 nil)

    ; vector
    (rest []) nil
    (rest [1]) nil
    (rest [1 2 3]) [2 3]

    (rest [nil]) nil
    (rest [1 nil]) [nil]
    (rest [1 []]) [[]]
    (rest [nil 2]) [2]
    (rest [[]]) nil
    (rest [[] nil]) [nil]
    (rest [[] 2 nil]) [2 nil]

    ; set
    (rest #{}) nil
    (rest #{1}) nil
    ;(rest #{1 2 3}) 1

    (rest #{nil}) nil
    ;(rest #{1 nil}) 1
    ;(rest #{nil 2}) nil
    (rest #{#{}}) nil
    ;(rest #{#{} nil}) #{}
    ;(rest #{#{} 2 nil}) #{}

    ; map
    (rest {}) nil
    (rest (sorted-map :a 1)) nil
    (rest (sorted-map :a 1 :b 2 :c 3)) '((:b 2) (:c 3))

    ; array
    (rest (into-array [])) nil
    (rest (into-array [1])) nil
    (rest (into-array [1 2 3])) '(2 3)

    (rest (to-array [nil])) nil
    (rest (to-array [1 nil])) '(nil)
    ;(rest (to-array [1 (into-array [])])) (list (into-array []))
    (rest (to-array [nil 2])) '(2)
    (rest (to-array [(into-array [])])) nil
    (rest (to-array [(into-array []) nil])) '(nil)
    (rest (to-array [(into-array []) 2 nil])) '(2 nil)
  )
)

;; (ffirst coll) = (first (first coll))
;;
(deftest test-ffirst
  (is (thrown? IllegalArgumentException (ffirst)))
  (are (= _1 _2)
    (ffirst nil) nil

    (ffirst ()) nil
    (ffirst '((1 2) (3 4))) 1

    (ffirst []) nil
    (ffirst [[1 2] [3 4]]) 1

    (ffirst {}) nil
    (ffirst {:a 1}) :a

    (ffirst #{}) nil
    (ffirst #{[1 2]}) 1
  )
)

;; (frest coll) = (first (rest coll)) = (second coll)
;;
(deftest test-frest
  (is (thrown? IllegalArgumentException (frest)))
  (are (= _1 _2)
    (frest nil) nil

    (frest ()) nil
    (frest '(1)) nil
    (frest '(1 2 3 4)) 2

    (frest []) nil
    (frest [1]) nil
    (frest [1 2 3 4]) 2

    (frest {}) nil
    (frest (sorted-map :a 1)) nil
    (frest (sorted-map :a 1 :b 2)) [:b 2]

    (frest #{}) nil
    (frest #{1}) nil
    (frest (sorted-set 1 2 3 4)) 2
  )
)

;; (rfirst coll) = (rest (first coll))
;;
(deftest test-rfirst
  (is (thrown? IllegalArgumentException (rfirst)))
  (are (= _1 _2)
    (rfirst nil) nil

    (rfirst ()) nil
    (rfirst '((1 2 3) (4 5 6))) '(2 3)

    (rfirst []) nil
    (rfirst [[1 2 3] [4 5 6]]) '(2 3)

    (rfirst {}) nil
    (rfirst {:a 1}) '(1)

    (rfirst #{}) nil
    (rfirst #{[1 2]}) '(2)
  )
)

;; (rrest coll) = (rest (rest coll))
;;
(deftest test-rrest
  (is (thrown? IllegalArgumentException (rrest)))
  (are (= _1 _2)
    (rrest nil) nil

    (rrest ()) nil
    (rrest '(1)) nil
    (rrest '(1 2)) nil
    (rrest '(1 2 3 4)) '(3 4)

    (rrest []) nil
    (rrest [1]) nil
    (rrest [1 2]) nil
    (rrest [1 2 3 4]) '(3 4)

    (rrest {}) nil
    (rrest (sorted-map :a 1)) nil
    (rrest (sorted-map :a 1 :b 2)) nil
    (rrest (sorted-map :a 1 :b 2 :c 3 :d 4)) '([:c 3] [:d 4])

    (rrest #{}) nil
    (rrest #{1}) nil
    (rrest (sorted-set 1 2)) nil
    (rrest (sorted-set 1 2 3 4)) '(3 4)
  )
)


;; empty?
;;
(deftest test-empty?
  (are (empty? _)
    nil
    ()
    []
    {}
    #{}
    ""
    (into-array []))
  (are (not (empty? _))
    '(1 2)
    [1 2]
    {:a 1 :b 2}
    #{1 2}
    "abc"
    (into-array [1 2])))
