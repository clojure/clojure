;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.instant
  (:import [java.util Calendar Date GregorianCalendar TimeZone]
           [java.sql Timestamp]))


;;; ------------------------------------------------------------------------
;;; convenience macros

(defmacro ^:private fail
  [msg]
  `(throw (RuntimeException. ~msg)))

(defmacro ^:private verify
  ([test msg] `(when-not ~test (fail ~msg)))
  ([test] `(verify ~test ~(str "failed: " (pr-str test)))))

(defmacro ^:private divisible?
  [num div]
  `(zero? (mod ~num ~div)))

(defmacro ^:private indivisible?
  [num div]
  `(not (divisible? ~num ~div)))


;;; ------------------------------------------------------------------------
;;; parser implementation

(defn- parse-int [^String s]
  (Long/parseLong s))

(defn- zero-fill-right [^String s width]
  (cond (= width (count s)) s
        (< width (count s)) (.substring s 0 width)
        :else (loop [b (StringBuilder. s)]
                (if (< (.length b) width)
                  (recur (.append b \0))
                  (.toString b)))))

(def parse-timestamp
     "Parse a string containing an RFC3339-like like timestamp.

The function new-instant is called with the following arguments.

                min  max           default
                ---  ------------  -------
  years          0          9'999      N/A (s must provide years)
  months         1             12        1
  days           1             31        1 (actual max days depends
  hours          0             23        0  on month and year)
  minutes        0             59        0
  seconds        0             60        0 (though 60 is only valid
  nanoseconds    0    999'999'999        0  when minutes is 59)
  offset-sign   -1              1        0
  offset-hours   0             23        0
  offset-minutes 0             59        0

These are all integers and will be non-nil. (The listed defaults
will be passed if the corresponding field is not present in s.)

Grammar (of s):

  date-fullyear   = 4DIGIT
  date-month      = 2DIGIT  ; 01-12
  date-mday       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on
                            ; month/year
  time-hour       = 2DIGIT  ; 00-23
  time-minute     = 2DIGIT  ; 00-59
  time-second     = 2DIGIT  ; 00-58, 00-59, 00-60 based on leap second
                            ; rules
  time-secfrac    = '.' 1*DIGIT
  time-numoffset  = ('+' / '-') time-hour ':' time-minute
  time-offset     = 'Z' / time-numoffset

  time-part       = time-hour [ ':' time-minute [ ':' time-second
                    [time-secfrac] [time-offset] ] ]

  timestamp       = date-year [ '-' date-month [ '-' date-mday
                    [ 'T' time-part ] ] ]

Unlike RFC3339:

  - we only consdier timestamp (was 'date-time')
    (removed: 'full-time', 'full-date')
  - timestamp can elide trailing components
  - time-offset is optional

Though time-offset is syntactically optional, a missing time-offset
will be treated as if the time-offset zero (+00:00) had been
specified.
"
     (let [timestamp #"(\d\d\d\d)(?:-(\d\d)(?:-(\d\d)(?:[T](\d\d)(?::(\d\d)(?::(\d\d)(?:[.](\d+))?)?)?)?)?)?(?:[Z]|([-+])(\d\d):(\d\d))?"]

       (fn [new-instant ^CharSequence cs]
         (if-let [[_ years months days hours minutes seconds fraction
                   offset-sign offset-hours offset-minutes]
                  (re-matches timestamp cs)]
           (new-instant
            (parse-int years)
            (if-not months   1 (parse-int months))
            (if-not days     1 (parse-int days))
            (if-not hours    0 (parse-int hours))
            (if-not minutes  0 (parse-int minutes))
            (if-not seconds  0 (parse-int seconds))
            (if-not fraction 0 (parse-int (zero-fill-right fraction 9)))
            (cond (= "-" offset-sign) -1
                  (= "+" offset-sign)  1
                  :else                0)
            (if-not offset-hours   0 (parse-int offset-hours))
            (if-not offset-minutes 0 (parse-int offset-minutes)))
           (fail (str "Unrecognized date/time syntax: " cs))))))


;;; ------------------------------------------------------------------------
;;; Verification of Extra-Grammatical Restrictions from RFC3339

(defn- leap-year?
  [year]
  (and (divisible? year 4)
       (or (indivisible? year 100)
           (divisible? year 400))))

(def ^:private days-in-month
     (let [dim-norm [nil 31 28 31 30 31 30 31 31 30 31 30 31]
           dim-leap [nil 31 29 31 30 31 30 31 31 30 31 30 31]]
       (fn [month leap-year?]
         ((if leap-year? dim-leap dim-norm) month))))

(defn validated
  "Return a function which constructs and instant by calling constructor
after first validting that those arguments are in range and otherwise
plausible. The resulting function will throw an exception if called
with invalid arguments."
  [new-instance]
  (fn [years months days hours minutes seconds nanoseconds
       offset-sign offset-hours offset-minutes]
    (verify (<= 1 months 12))
    (verify (<= 1 days (days-in-month months (leap-year? years))))
    (verify (<= 0 hours 23))
    (verify (<= 0 minutes 59))
    (verify (<= 0 seconds (if (= minutes 59) 60 59)))
    (verify (<= 0 nanoseconds 999999999))
    (verify (<= -1 offset-sign 1))
    (verify (<= 0 offset-hours 23))
    (verify (<= 0 offset-minutes 59))
    (new-instance years months days hours minutes seconds nanoseconds
                  offset-sign offset-hours offset-minutes)))


;;; ------------------------------------------------------------------------
;;; print integeration

(defn- fixup-offset
  [^String s]
  (let [x (- (count s) 2)]
    (str (.substring s 0 x) ":" (.substring s x))))

(defn- caldate->rfc3339
  "format java.util.Date or java.util.Calendar as RFC3339 timestamp."
  [d]
  (fixup-offset (format "#@%1$tFT%1$tT.%1$tL%1$tz" d)))

(defmethod print-method java.util.Date
  [^java.util.Date d, ^java.io.Writer w]
  (.write w (caldate->rfc3339 d)))

(defmethod print-dup java.util.Date
  [^java.util.Date d, ^java.io.Writer w]
  (.write w (caldate->rfc3339 d)))

(defmethod print-method java.util.Calendar
  [^java.util.Calendar c, ^java.io.Writer w]
  (.write w (caldate->rfc3339 c)))

(defmethod print-dup java.util.Calendar
  [^java.util.Calendar c, ^java.io.Writer w]
  (.write w (caldate->rfc3339 c)))

(defn- fixup-nanos                   ; 0123456789012345678901234567890123456
  [^long nanos ^String s]            ; #@2011-01-01T01:00:00.000000000+01:00
  (str (.substring s 0 22)
       (format "%09d" nanos)
       (.substring s 31)))

(defn- timestamp->rfc3339
  [^java.sql.Timestamp ts]
  (->> ts
       (format "#@%1$tFT%1$tT.%1$tN%1$tz") ; %1$tN prints 9 digits for frac.
       fixup-offset                        ; second, but last 6 are always
       (fixup-nanos (.getNanos ts))))      ; 0 though timestamp has getNanos

(defmethod print-method java.sql.Timestamp
  [^java.sql.Timestamp t, ^java.io.Writer w]
  (.write w (timestamp->rfc3339 t)))

(defmethod print-dup java.sql.Timestamp
  [^java.sql.Timestamp t, ^java.io.Writer w]
  (.write w (timestamp->rfc3339 t)))


;;; ------------------------------------------------------------------------
;;; reader integration

(defn- construct-calendar
  "Construct a java.util.Calendar, which preserves, preserving the timezone
offset, but truncating the subsecond fraction to milliseconds."
  ^GregorianCalendar
  [years months days hours minutes seconds nanoseconds
   offset-sign offset-hours offset-minutes]
  (doto (GregorianCalendar. years (dec months) days hours minutes seconds)
    (.set Calendar/MILLISECOND (/ nanoseconds 1000000))
    (.setTimeZone (TimeZone/getTimeZone
                   (format "GMT%s%02d:%02d"
                           (if (neg? offset-sign) "-" "+")
                           offset-hours offset-minutes)))))

(defn- construct-date
  "Construct a java.util.Date, which expresses the original instant as
milliseconds since the epoch, GMT."
  [years months days hours minutes seconds nanoseconds
   offset-sign offset-hours offset-minutes]
  (.getTime (construct-calendar years months days
                                hours minutes seconds nanoseconds
                                offset-sign offset-hours offset-minutes)))

(defn- construct-timestamp
  "Construct a java.sql.Timestamp, which has nanosecond precision."
  [years months days hours minutes seconds nanoseconds
   offset-sign offset-hours offset-minutes]
  (doto (Timestamp.
         (.getTimeInMillis
          (construct-calendar years months days
                              hours minutes seconds nanoseconds
                              offset-sign offset-hours offset-minutes)))
    (.setNanos nanoseconds)))

(def read-instant-date
     "Bind this to *instant-reader* to read instants as java.util.Date."
     (partial parse-timestamp (validated construct-date)))

(def read-instant-calendar
     "Bind this to *instant-reader* to read instants as java.util.Calendar.
Calendar preserves the timezone offset originally used in the date
literal as written."
     (partial parse-timestamp (validated construct-calendar)))

(def read-instant-timestamp
     "Bind this to *instant-reader* to read instants as
java.sql.Timestamp. Timestamp preserves fractional seconds with
nanosecond precision."
     (partial parse-timestamp (validated construct-timestamp)))

(alter-var-root #'clojure.core/*instant-reader*
                (constantly read-instant-date))
