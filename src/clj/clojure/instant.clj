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


(set! *warn-on-reflection* true)

;;; ------------------------------------------------------------------------
;;; convenience macros

(defmacro ^:private fail
  [msg]
  `(throw (RuntimeException. ~msg)))

(defmacro ^:private verify
  ([test msg] `(when-not ~test (fail ~msg)))
  ([test] `(verify ~test ~(str "failed: " (pr-str test)))))

(defn- divisible?
  [num div]
  (zero? (mod num div)))

(defn- indivisible?
  [num div]
  (not (divisible? num div)))


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
  years          0           9999      N/A (s must provide years)
  months         1             12        1
  days           1             31        1 (actual max days depends
  hours          0             23        0  on month and year)
  minutes        0             59        0
  seconds        0             60        0 (though 60 is only valid
  nanoseconds    0      999999999        0  when minutes is 59)
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

  - we only parse the timestamp format
  - timestamp can elide trailing components
  - time-offset is optional (defaults to +00:00)

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
after first validating that those arguments are in range and otherwise
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
;;; print integration

(def ^:private ^ThreadLocal thread-local-utc-date-format
  ;; SimpleDateFormat is not thread-safe, so we use a ThreadLocal proxy for access.
  ;; http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4228335
  (proxy [ThreadLocal] []
    (initialValue []
      (doto (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSS-00:00")
        ;; RFC3339 says to use -00:00 when the timezone is unknown (+00:00 implies a known GMT)
        (.setTimeZone (java.util.TimeZone/getTimeZone "GMT"))))))

(defn- print-date
  "Print a java.util.Date as RFC3339 timestamp, always in UTC."
  [^java.util.Date d, ^java.io.Writer w]
  (let [^java.text.DateFormat utc-format (.get thread-local-utc-date-format)]
    (.write w "#inst \"")
    (.write w (.format utc-format d))
    (.write w "\"")))

(defmethod print-method java.util.Date
  [^java.util.Date d, ^java.io.Writer w]
  (print-date d w))

(defmethod print-dup java.util.Date
  [^java.util.Date d, ^java.io.Writer w]
  (print-date d w))

(defn- print-calendar
  "Print a java.util.Calendar as RFC3339 timestamp, preserving timezone."
  [^java.util.Calendar c, ^java.io.Writer w]
  (let [calstr (format "%1$tFT%1$tT.%1$tL%1$tz" c)
        offset-minutes (- (.length calstr) 2)]
    ;; calstr is almost right, but is missing the colon in the offset
    (.write w "#inst \"")
    (.write w calstr 0 offset-minutes)
    (.write w ":")
    (.write w calstr offset-minutes 2)
    (.write w "\"")))

(defmethod print-method java.util.Calendar
  [^java.util.Calendar c, ^java.io.Writer w]
  (print-calendar c w))

(defmethod print-dup java.util.Calendar
  [^java.util.Calendar c, ^java.io.Writer w]
  (print-calendar c w))


(def ^:private ^ThreadLocal thread-local-utc-timestamp-format
  ;; SimpleDateFormat is not thread-safe, so we use a ThreadLocal proxy for access.
  ;; http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4228335
  (proxy [ThreadLocal] []
    (initialValue []
      (doto (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss")
        (.setTimeZone (java.util.TimeZone/getTimeZone "GMT"))))))

(defn- print-timestamp
  "Print a java.sql.Timestamp as RFC3339 timestamp, always in UTC."
  [^java.sql.Timestamp ts, ^java.io.Writer w]
  (let [^java.text.DateFormat utc-format (.get thread-local-utc-timestamp-format)]
    (.write w "#inst \"")
    (.write w (.format utc-format ts))
    ;; add on nanos and offset
    ;; RFC3339 says to use -00:00 when the timezone is unknown (+00:00 implies a known GMT)
    (.write w (format ".%09d-00:00" (.getNanos ts)))
    (.write w "\"")))

(defmethod print-method java.sql.Timestamp
  [^java.sql.Timestamp ts, ^java.io.Writer w]
  (print-timestamp ts w))

(defmethod print-dup java.sql.Timestamp
  [^java.sql.Timestamp ts, ^java.io.Writer w]
  (print-timestamp ts w))


;;; ------------------------------------------------------------------------
;;; reader integration

(defn- construct-calendar
  "Construct a java.util.Calendar, preserving the timezone
offset, but truncating the subsecond fraction to milliseconds."
  ^GregorianCalendar
  [years months days hours minutes seconds nanoseconds
   offset-sign offset-hours offset-minutes]
  (doto (GregorianCalendar. years (dec months) days hours minutes seconds)
    (.set Calendar/MILLISECOND (quot nanoseconds 1000000))
    (.setTimeZone (TimeZone/getTimeZone
                   (format "GMT%s%02d:%02d"
                           (if (neg? offset-sign) "-" "+")
                           offset-hours offset-minutes)))))

(defn- construct-date
  "Construct a java.util.Date, which expresses the original instant as
milliseconds since the epoch, UTC."
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
                              hours minutes seconds 0
                              offset-sign offset-hours offset-minutes)))
    ;; nanos must be set separately, pass 0 above for the base calendar
    (.setNanos nanoseconds)))

(def read-instant-date
  "To read an instant as a java.util.Date, bind *data-readers* to a map with
this var as the value for the 'inst key. The timezone offset will be used
to convert into UTC."
  (partial parse-timestamp (validated construct-date)))

(def read-instant-calendar
  "To read an instant as a java.util.Calendar, bind *data-readers* to a map with
this var as the value for the 'inst key.  Calendar preserves the timezone
offset."
  (partial parse-timestamp (validated construct-calendar)))

(def read-instant-timestamp
  "To read an instant as a java.sql.Timestamp, bind *data-readers* to a
map with this var as the value for the 'inst key. Timestamp preserves
fractional seconds with nanosecond precision. The timezone offset will
be used to convert into UTC."
  (partial parse-timestamp (validated construct-timestamp)))

