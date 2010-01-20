;;; profile.clj: simple code profiling & timing

;; by Stuart Sierra, http://stuartsierra.com/
;; May 9, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns #^{:author "Stuart Sierra"
       :doc "Simple code profiling & timing measurement.

Wrap any section of code in the prof macro, giving it a name, like this:

       (defn my-function [x y]
         (let [sum (prof :addition (+ x y))
               product (prof :multiplication (* x y))]
           [sum product]))

The run your code in the profile macro, like this:

       (profile (dotimes [i 10000] (my-function 3 4)))

Which prints a report for each named section of code:

          Name      mean       min       max     count       sum
      addition       265         0     37000     10000   2655000
multiplication       274         0     53000     10000   2747000

Times are measured in nanoseconds, to the maximum precision available
under the JVM.  See the function documentation for more details.
"}
  clojure.contrib.profile)

(def *profile-data* nil)

(def #^{:doc "Set this to false before loading/compiling to omit
profiling code."}  *enable-profiling* true)

(defmacro prof
  "If *enable-profiling* is true, wraps body in profiling code.
  Returns the result of body. Profile timings will be stored in
  *profile-data* using name, which must be a keyword, as the key.
  Timings are measured with System/nanoTime."
  [name & body]
  (assert (keyword? name))
  (if *enable-profiling*
    `(if *profile-data*
       (let [start-time# (System/nanoTime)
             value# (do ~@body)
             elapsed# (- (System/nanoTime) start-time#)]
         (swap! *profile-data* assoc ~name
                (conj (get @*profile-data* ~name) elapsed#))
         value#)
       ~@body)
    `(do ~@body)))

(defmacro with-profile-data
  "Executes body with *profile-data* bound to an atom of a new map.
  Returns the raw profile data as a map.  Keys in the map are profile
  names (keywords), and values are lists of elapsed time, in
  nanoseconds."
  [& body]
  `(binding [*profile-data* (atom {})]
     ~@body
     @*profile-data*))

(defn summarize
  "Takes the raw data returned by with-profile-data and returns a map
  from names to summary statistics.  Each value in the map will look
  like:

     {:mean ..., :min ..., :max ..., :count ..., :sum ...}

  :mean, :min, and :max are how long the profiled section took to run,
  in nanoseconds.  :count is the total number of times the profiled
  section was executed.  :sum is the total amount of time spent in the
  profiled section, in nanoseconds."
  [profile-data]
  (reduce (fn [m [k v]]
            (let [cnt (count v)
                  sum (reduce + v)]
              (assoc m k {:mean (int (/ sum cnt))
                          :min (apply min v)
                          :max (apply max v)
                          :count cnt
                          :sum sum})))
          {} profile-data))

(defn print-summary
  "Prints a table of the results returned by summarize."
  [profile-summary]
  (let [name-width (apply max (map (comp count name) (keys profile-summary)))
        fmt-string (str "%" name-width "s  %8d  %8d  %8d  %8d  %8d%n")]
    (printf (.replace fmt-string \d \s)
            "Name" "mean" "min" "max" "count" "sum")
    (doseq [k (sort (keys profile-summary))]
      (let [v (get profile-summary k)]
        (printf fmt-string (name k) (:mean v) (:min v) (:max v) (:count v) (:sum v))))))

(defmacro profile
  "Runs body with profiling enabled, then prints a summary of
  results.  Returns nil."
  [& body]
  `(print-summary (summarize (with-profile-data (do ~@body)))))
