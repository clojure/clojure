(ns clojure.contrib.math.tests
  (:use clojure.test
	clojure.contrib.math))

(deftest test-expt
  (are [x y] (= x y)
      (expt 2 3) 8
      (expt (expt 2 32) 2) (expt 2 64)
      (expt 4/3 2) 16/9
      (expt 2 -10) 1/1024
      (expt 0.5M 2) 0.25M
      (expt 5 4.2) (Math/pow 5 4.2)
      (expt 5.3 4) (Math/pow 5.3 4)))

(deftest test-abs
  (are [x y] (= x y)
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
  (are [x y] (= x y)
      (gcd 4 3) 1
      (gcd 24 12) 12
      (gcd 24 27) 3
      (gcd 1 0) 1
      (gcd 0 1) 1
      (gcd 0 0) 0)
  (is (thrown? IllegalArgumentException (gcd nil 0)))
  (is (thrown? IllegalArgumentException (gcd 0 nil)))
  (is (thrown? IllegalArgumentException (gcd 7.0 0))))

(deftest test-lcm
  (are [x y] (= x y)
       (lcm 2 3) 6
       (lcm 3 2) 6
       (lcm -2 3) 6
       (lcm 2 -3) 6
       (lcm -2 -3) 6
       (lcm 4 10) 20
       (lcm 1 0) 0
       (lcm 0 1) 0
       (lcm 0 0))
  (is (thrown? IllegalArgumentException (lcm nil 0)))
  (is (thrown? IllegalArgumentException (lcm 0 nil)))
  (is (thrown? IllegalArgumentException (lcm 7.0 0))))

(deftest test-floor
  (are [x y] (= x y)
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
  (are [x y] (= x y)
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
  (are [x y] (= x y)
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
  (are [x y] (= x y)
      (sqrt 9) 3
      (sqrt 16/9) 4/3
      (sqrt 0.25M) 0.5M
      (sqrt 2) (Math/sqrt 2)))

(deftest test-exact-integer-sqrt
  (are [x y] (= x y)
   (exact-integer-sqrt 15) [3 6]
   (exact-integer-sqrt (inc (expt 2 64))) [(expt 2 32) 1]
   (exact-integer-sqrt 1000000000000) [1000000 0]))
