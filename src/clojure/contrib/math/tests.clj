(ns clojure.contrib.math.tests
  (:use clojure.contrib.test-is
	clojure.contrib.math))

(deftest test-expt
  (are (= _1 _2)
      (expt 2 3) 8
      (expt (expt 2 32) 2) (expt 2 64)
      (expt 4/3 2) 16/9
      (expt 2 -10) 1/1024
      (expt 0.5M 2) 0.25M
      (expt 5 4.2) (Math/pow 5 4.2)
      (expt 5.3 4) (Math/pow 5.3 4)))

(deftest test-abs
  (are (= _1 _2)  
      (abs -2) 2
      (abs 0) 0
      (abs 5) 5
      (abs 123456789123456789) 123456789123456789
      (abs -123456789123456789) 123456789123456789
      (abs 5/3) 5/3
      (abs -4/3) 4/3
      (abs 4.3M) 4.3M
      (abs -4.3M) 4.3M
      (abs 2.8) 2.8
      (abs -2.8) 2.8))

(deftest test-gcd
  (are (= _1 _2)
      (gcd 4 3) 1
      (gcd 24 12) 12
      (gcd 24 27) 3))

(deftest test-floor
  (are (= _1 _2)
      (floor 6) 6
      (floor -6) -6
      (floor 123456789123456789) 123456789123456789
      (floor -123456789123456789) -123456789123456789
      (floor 4/3) 1
      (floor -4/3) -2
      (floor 4.3M) 4
      (floor -4.3M) -5
      (floor 4.3) 4.0
      (floor -4.3) -5.0))

(deftest test-ceil
  (are (= _1 _2)
      (ceil 6) 6
      (ceil -6) -6
      (ceil 123456789123456789) 123456789123456789
      (ceil -123456789123456789) -123456789123456789
      (ceil 4/3) 2
      (ceil -4/3) -1
      (ceil 4.3M) 5
      (ceil -4.3M) -4
      (ceil 4.3) 5.0
      (ceil -4.3) -4.0))

(deftest test-round
  (are (= _1 _2)
      (round 6) 6
      (round -6) -6
      (round 123456789123456789) 123456789123456789
      (round -123456789123456789) -123456789123456789
      (round 4/3) 1
      (round 5/3) 2
      (round 5/2) 3
      (round -4/3) -1
      (round -5/3) -2
      (round -5/2) -2
      (round 4.3M) 4
      (round 4.7M) 5
      (round -4.3M) -4
      (round -4.7M) -5
      (round 4.5M) 5
      (round -4.5M) -4
      (round 4.3) 4
      (round 4.7) 5
      (round -4.3) -4
      (round -4.7) -5
      (round 4.5) 5
      (round -4.5) -4))

(deftest test-sqrt
  (are (= _1 _2)
      (sqrt 9) 3
      (sqrt 16/9) 4/3
      (sqrt 0.25M) 0.5M
      (sqrt 2) (Math/sqrt 2)))

(deftest test-exact-integer-sqrt
  (are (= _1 _2)
   (exact-integer-sqrt 15) [3 6]
   (exact-integer-sqrt (inc (expt 2 64))) [(expt 2 32) 1]))
