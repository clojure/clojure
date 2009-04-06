;;; cl-format.clj -- part of the pretty printer for Clojure

;; by Tom Faulhaber
;; April 3, 2009

;   Copyright (c) Tom Faulhaber, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; This module implements the Common Lisp compatible format function as documented
;; in "Common Lisp the Language, 2nd edition", Chapter 22 (available online at:
;; http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/clm/node200.html#SECTION002633000000000000000)

(in-ns 'clojure.contrib.pprint)

;;; Forward references
(declare compile-format)
(declare execute-format)
(declare init-navigator)
;;; End forward references

(defn cl-format 
  "An implementation of a Common Lisp compatible format function"
  [stream format-in & args]
  (let [compiled-format (if (string? format-in) (compile-format format-in) format-in)
        navigator (init-navigator args)]
    (execute-format stream compiled-format navigator)))

(def #^{:private true} *format-str* nil)

(defn- format-error [message offset] 
  (let [full-message (str message \newline *format-str* \newline 
                           (apply str (repeat offset \space)) "^" \newline)]
    (throw (RuntimeException. full-message))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Argument navigators manage the argument list
;;; as the format statement moves through the list
;;; (possibly going forwards and backwards as it does so)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct #^{:private true}
  arg-navigator :seq :rest :pos )

(defn init-navigator [s]
  "Create a new arg-navigator from the sequence with the position set to 0"
  (let [s (seq s)]
    (struct arg-navigator s s 0)))

;; TODO call format-error with offset
(defn- next-arg [ navigator ]
  (let [ rst (:rest navigator) ]
    (if rst
      [(first rst) (struct arg-navigator (:seq navigator ) (next rst) (inc (:pos navigator)))]
      (throw (new Exception  "Not enough arguments for format definition")))))

(defn- next-arg-or-nil [navigator]
  (let [rst (:rest navigator)]
    (if rst
      [(first rst) (struct arg-navigator (:seq navigator ) (next rst) (inc (:pos navigator)))]
      [nil navigator])))

;; Get an argument off the arg list and compile it if it's not already compiled
(defn- get-format-arg [navigator]
  (let [[raw-format navigator] (next-arg navigator)
        compiled-format (if (instance? String raw-format) 
                               (compile-format raw-format)
                               raw-format)]
    [compiled-format navigator]))

(declare relative-reposition)

(defn- absolute-reposition [navigator position]
  (if (>= position (:pos navigator))
    (relative-reposition navigator (- (:pos navigator) position))
    (struct arg-navigator (:seq navigator) (drop position (:seq navigator)) position)))

(defn- relative-reposition [navigator position]
  (let [newpos (+ (:pos navigator) position)]
    (if (neg? position)
      (absolute-reposition navigator newpos)
      (struct arg-navigator (:seq navigator) (drop position (:rest navigator)) newpos))))

(defstruct #^{:private true}
  compiled-directive :func :def :params :offset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; When looking at the parameter list, we may need to manipulate
;;; the argument list as well (for 'V' and '#' parameter types).
;;; We hide all of this behind a function, but clients need to
;;; manage changing arg navigator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: validate parameters when they come from arg list
(defn- realize-parameter [[param [raw-val offset]] navigator]
  (let [[real-param new-navigator]
        (cond 
         (contains? #{ :at :colon } param) ;pass flags through unchanged - this really isn't necessary
         [raw-val navigator]

         (= raw-val :parameter-from-args) 
         (next-arg navigator)

         (= raw-val :remaining-arg-count) 
         [(count (:rest navigator)) navigator]

         true 
         [raw-val navigator])]
    [[param [real-param offset]] new-navigator]))
         
(defn- realize-parameter-list [parameter-map navigator]
  (let [[pairs new-navigator] 
        (map-passing-context realize-parameter navigator parameter-map)]
    [(into {} pairs) new-navigator]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions that support individual directives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common handling code for ~A and ~S
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- format-ascii [print-func params arg-navigator offsets]
  (let [ [arg arg-navigator] (next-arg arg-navigator) 
         #^String base-output (print-func arg)
         base-width (.length base-output)
         min-width (+ base-width (:minpad params))
         width (if (>= min-width (:mincol params)) 
                 min-width
                 (+ min-width 
                    (* (+ (quot (- (:mincol params) min-width 1) 
                                (:colinc params) )
                          1)
                       (:colinc params))))
         chars (apply str (repeat (- width base-width) (:padchar params)))]
    (if (:at params)
      (print (str chars base-output))
      (print (str base-output chars)))
    arg-navigator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for the integer directives ~D, ~X, ~O, ~B and some
;;; of ~R
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- integral? [x]
  "returns true if a number is actually an integer (that is, has no fractional part)"
  (cond
   (integer? x) true
   (decimal? x) (>= (.ulp (.stripTrailingZeros (bigdec 0))) 1) ; true iff no fractional part
   (float? x)   (= x (Math/floor x))
   (ratio? x)   (let [#^clojure.lang.Ratio r x]
                  (= 0 (rem (.numerator r) (.denominator r))))
   :else        false))

(defn- remainders [base val]
  "Return the list of remainders (essentially the 'digits') of val in the given base"
  (reverse 
   (first 
    (consume #(if (pos? %) 
                [(rem % base) (quot % base)] 
                [nil nil]) 
             val))))

(defn- base-str [base val]
  "Return val as a string in the given base"
  (let [xlated-val (cond
                    (float? val) (bigdec val)
                    (ratio? val) (let [#^clojure.lang.Ratio r val] 
                                   (/ (.numerator r) (.denominator r)))
                    :else val)] 
    (apply str 
          (map 
           #(if (< % 10) (char (+ (int \0) %)) (char (+ (int \a) (- % 10)))) 
           (remainders base val)))))

(def #^{:private true}
     java-base-formats {8 "%o", 10 "%d", 16 "%x"})

(defn- opt-base-str [base val]
  "Return val as a string in the given base, using clojure.core/format if supported
for improved performance"
  (let [format-str (get java-base-formats base)]
    (if (and format-str (integer? val))
      (clojure.core/format format-str val)
      (base-str base val))))

(defn- group-by [unit lis]
  (reverse
   (first
    (consume (fn [x] [(seq (reverse (take unit x))) (seq (drop unit x))]) (reverse lis)))))

(defn- format-integer [base params arg-navigator offsets]
  (let [[arg arg-navigator] (next-arg arg-navigator)]
    (if (integral? arg)
      (let [neg (neg? arg)
            pos-arg (if neg (- arg) arg)
            raw-str (opt-base-str base pos-arg)
            group-str (if (:colon params)
                        (let [groups (map #(apply str %) (group-by (:commainterval params) raw-str))
                              commas (repeat (count groups) (:commachar params))]
                          (apply str (next (interleave commas groups))))
                        raw-str)
            #^String signed-str (cond
                                  neg (str "-" group-str)
                                  (:at params) (str "+" group-str)
                                  true group-str)
            padded-str (if (< (.length signed-str) (:mincol params))
                         (str (apply str (repeat (- (:mincol params) (.length signed-str)) 
                                                 (:padchar params)))
                              signed-str)
                         signed-str)]
        (print padded-str))
      (format-ascii print-str {:mincol (:mincol params) :colinc 1 :minpad 0 
                               :padchar (:padchar params) :at true} 
                    (init-navigator [arg]) nil))
    arg-navigator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for english formats (~R and ~:R)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def #^{:private true}
     english-cardinal-units 
     ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
      "ten" "eleven" "twelve" "thirteen" "fourteen"
      "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])

(def #^{:private true}
     english-ordinal-units 
     ["zeroth" "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth"
      "tenth" "eleventh" "twelfth" "thirteenth" "fourteenth"
      "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"])

(def #^{:private true}
     english-cardinal-tens
     ["" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])

(def #^{:private true}
     english-ordinal-tens
     ["" "" "twentieth" "thirtieth" "fortieth" "fiftieth"
      "sixtieth" "seventieth" "eightieth" "ninetieth"])

;; We use "short scale" for our units (see http://en.wikipedia.org/wiki/Long_and_short_scales)
;; Number names from http://www.jimloy.com/math/billion.htm
;; We follow the rules for writing numbers from the Blue Book
;; (http://www.grammarbook.com/numbers/numbers.asp)
(def #^{:private true}
     english-scale-numbers 
     ["" "thousand" "million" "billion" "trillion" "quadrillion" "quintillion" 
      "sextillion" "septillion" "octillion" "nonillion" "decillion" 
      "undecillion" "duodecillion" "tredecillion" "quattuordecillion" 
      "quindecillion" "sexdecillion" "septendecillion" 
      "octodecillion" "novemdecillion" "vigintillion"])

(defn- format-simple-cardinal [num]
  "Convert a number less than 1000 to a cardinal english string"
  (let [hundreds (quot num 100)
        tens (rem num 100)]
    (str
     (if (pos? hundreds) (str (nth english-cardinal-units hundreds) " hundred"))
     (if (and (pos? hundreds) (pos? tens)) " ")
     (if (pos? tens) 
       (if (< tens 20) 
         (nth english-cardinal-units tens)
         (let [ten-digit (quot tens 10)
               unit-digit (rem tens 10)]
           (str
            (if (pos? ten-digit) (nth english-cardinal-tens ten-digit))
            (if (and (pos? ten-digit) (pos? unit-digit)) "-")
            (if (pos? unit-digit) (nth english-cardinal-units unit-digit)))))))))

(defn- add-english-scales [parts offset]
  "Take a sequence of parts, add scale numbers (e.g., million) and combine into a string
offset is a factor of 10^3 to multiply by"
  (let [cnt (count parts)]
    (loop [acc []
           pos (dec cnt)
           this (first parts)
           remainder (next parts)]
      (if (nil? remainder)
        (str (apply str (interpose ", " acc))
             (if (and (not (empty? this)) (not (empty? acc))) ", ")
             this
             (if (and (not (empty? this)) (pos? (+ pos offset)))
               (str " " (nth english-scale-numbers (+ pos offset)))))
        (recur 
         (if (empty? this)
           acc
           (conj acc (str this " " (nth english-scale-numbers (+ pos offset)))))
         (dec pos)
         (first remainder)
         (next remainder))))))

(defn- format-cardinal-english [params navigator offsets]
  (let [[arg navigator] (next-arg navigator)]
    (if (= 0 arg)
      (print "zero")
      (let [abs-arg (if (neg? arg) (- arg) arg) ; some numbers are too big for Math/abs
            parts (remainders 1000 abs-arg)]
        (if (<= (count parts) (count english-scale-numbers))
          (let [parts-strs (map format-simple-cardinal parts)
                full-str (add-english-scales parts-strs 0)]
            (print (str (if (neg? arg) "minus ") full-str)))
          (format-integer ;; for numbers > 10^63, we fall back on ~D
           10
           { :mincol 0, :padchar \space, :commachar \, :commainterval 3, :colon true}
           (init-navigator [arg])
           { :mincol 0, :padchar 0, :commachar 0 :commainterval 0}))))
    navigator))

(defn- format-simple-ordinal [num]
  "Convert a number less than 1000 to a ordinal english string
Note this should only be used for the last one in the sequence"
  (let [hundreds (quot num 100)
        tens (rem num 100)]
    (str
     (if (pos? hundreds) (str (nth english-cardinal-units hundreds) " hundred"))
     (if (and (pos? hundreds) (pos? tens)) " ")
     (if (pos? tens) 
       (if (< tens 20) 
         (nth english-ordinal-units tens)
         (let [ten-digit (quot tens 10)
               unit-digit (rem tens 10)]
           (if (and (pos? ten-digit) (not (pos? unit-digit)))
             (nth english-ordinal-tens ten-digit)
             (str
              (if (pos? ten-digit) (nth english-cardinal-tens ten-digit))
              (if (and (pos? ten-digit) (pos? unit-digit)) "-")
              (if (pos? unit-digit) (nth english-ordinal-units unit-digit))))))
       (if (pos? hundreds) "th")))))

(defn- format-ordinal-english [params navigator offsets]
  (let [[arg navigator] (next-arg navigator)]
    (if (= 0 arg)
      (print "zeroth")
      (let [abs-arg (if (neg? arg) (- arg) arg) ; some numbers are too big for Math/abs
            parts (remainders 1000 abs-arg)]
        (if (<= (count parts) (count english-scale-numbers))
          (let [parts-strs (map format-simple-cardinal (drop-last parts))
                head-str (add-english-scales parts-strs 1)
                tail-str (format-simple-ordinal (last parts))]
            (print (str (if (neg? arg) "minus ") 
                        (cond 
                         (and (not (empty? head-str)) (not (empty? tail-str))) 
                         (str head-str ", " tail-str)
                         
                         (not (empty? head-str)) (str head-str "th")
                         :else tail-str))))
          (do (format-integer ;; for numbers > 10^63, we fall back on ~D
               10
               { :mincol 0, :padchar \space, :commachar \, :commainterval 3, :colon true}
               (init-navigator [arg])
               { :mincol 0, :padchar 0, :commachar 0 :commainterval 0})
              (let [low-two-digits (rem arg 100)
                    not-teens (or (< 11 low-two-digits) (> 19 low-two-digits))
                    low-digit (rem low-two-digits 10)]
                (print (cond 
                        (and (= low-digit 1) not-teens) "st"
                        (and (= low-digit 2) not-teens) "nd"
                        (and (= low-digit 3) not-teens) "rd"
                        :else "th")))))))
    navigator))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for roman numeral formats (~@R and ~@:R)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def #^{:private true}
     old-roman-table
     [[ "I" "II" "III" "IIII" "V" "VI" "VII" "VIII" "VIIII"]
      [ "X" "XX" "XXX" "XXXX" "L" "LX" "LXX" "LXXX" "LXXXX"]
      [ "C" "CC" "CCC" "CCCC" "D" "DC" "DCC" "DCCC" "DCCCC"]
      [ "M" "MM" "MMM"]])

(def #^{:private true}
     new-roman-table
     [[ "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
      [ "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
      [ "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
      [ "M" "MM" "MMM"]])

(defn- format-roman [table params navigator offsets]
  "Format a roman numeral using the specified look-up table"
  (let [[arg navigator] (next-arg navigator)]
    (if (and (number? arg) (> arg 0) (< arg 4000))
      (let [digits (remainders 10 arg)]
        (loop [acc []
               pos (dec (count digits))
               digits digits]
          (if (empty? digits)
            (print (apply str acc))
            (let [digit (first digits)]
              (recur (if (= 0 digit) 
                       acc 
                       (conj acc (nth (nth table pos) (dec digit))))
                     (dec pos)
                     (next digits))))))
      (format-integer ;; for anything <= 0 or > 3999, we fall back on ~D
           10
           { :mincol 0, :padchar \space, :commachar \, :commainterval 3, :colon true}
           (init-navigator [arg])
           { :mincol 0, :padchar 0, :commachar 0 :commainterval 0}))
    navigator))

(defn- format-old-roman [params navigator offsets]
  (format-roman old-roman-table params navigator offsets))

(defn- format-new-roman [params navigator offsets]
  (format-roman new-roman-table params navigator offsets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for character formats (~C)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def #^{:private true} 
     special-chars { 8 "Backspace", 9 "Tab",  10 "Newline", 13 "Return", 32 "Space"})

(defn- pretty-character [params navigator offsets]
  (let [[c navigator] (next-arg navigator)
        as-int (int c)
        base-char (bit-and as-int 127)
        meta (bit-and as-int 128)
        special (get special-chars base-char)]
    (if (> meta 0) (print "Meta-"))
    (print (cond
            special special
            (< base-char 32) (str "Control-" (char (+ base-char 64)))
            (= base-char 127) "Control-?"
            :else (char base-char)))
    navigator))

(defn- readable-character [params navigator offsets]
  (let [[c navigator] (next-arg navigator)]
    (condp = (:char-format params)
      \o (cl-format true "\\o~3,'0o" (int c))
      \u (cl-format true "\\u~4,'0x" (int c))
      nil (pr c))
    navigator))

(defn- plain-character [params navigator offsets]
  (let [[char navigator] (next-arg navigator)]
    (print char)
    navigator))

;; Check to see if a result is an abort (~^) construct
;; TODO: move these funcs somewhere more appropriate
(defn- abort? [context]
  (let [token (first context)]
    (or (= :up-arrow token) (= :colon-up-arrow token))))

;; Handle the execution of "sub-clauses" in bracket constructions
(defn- execute-sub-format [format args base-args]
  (second
   (map-passing-context 
    (fn [element context]
      (if (abort? context)
        [nil context] ; just keep passing it along
        (let [[params args] (realize-parameter-list (:params element) context)
              [params offsets] (unzip-map params)
              params (assoc params :base-args base-args)]
          [nil (apply (:func element) [params args offsets])])))
    args
    format)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for real number formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO - return exponent as int to eliminate double conversion
(defn- float-parts-base
  "Produce string parts for the mantissa (normalized 1-9) and exponent"
  [#^Object f]
  (let [#^String s (.toLowerCase (.toString f))
        exploc (.indexOf s (int \e))]
    (if (neg? exploc)
      (let [dotloc (.indexOf s (int \.))]
        (if (neg? dotloc)
          [s (str (dec (count s)))]
          [(str (subs s 0 dotloc) (subs s (inc dotloc))) (str (dec dotloc))]))
      [(str (subs s 0 1) (subs s 2 exploc)) (subs s (inc exploc))])))


(defn- float-parts [f]
  "Take care of leading and trailing zeros in decomposed floats"
  (let [[m #^String e] (float-parts-base f)
        m1 (rtrim m \0)
        m2 (ltrim m1 \0)
        delta (- (count m1) (count m2))
        #^String e (if (and (pos? (count e)) (= (nth e 0) \+)) (subs e 1) e)]
    (if (empty? m2)
      ["0" 0]
      [m2 (- (Integer/valueOf e) delta)])))

(defn- round-str [m e d w]
  (if (or d w)
    (let [len (count m)
          round-pos (if d (+ e d 1))
          round-pos (if (and w (< (inc e) (dec w)) 
                             (or (nil? round-pos) (< (dec w) round-pos)))
                      (dec w)
                      round-pos)
          [m1 e1 round-pos len] (if (= round-pos 0) 
                                  [(str "0" m) (inc e) 1 (inc len)]
                                  [m e round-pos len])]
      (if round-pos
        (if (> len round-pos)
          (let [round-char (nth m1 round-pos)
                #^String result (subs m1 0 round-pos)]
            (if (>= (int round-char) (int \5))
              (let [result-val (Integer/valueOf result)
                    leading-zeros (subs result 0 (min (prefix-count result \0) (- round-pos 1)))
                    round-up-result (str leading-zeros
                                         (String/valueOf (+ result-val 
                                                            (if (neg? result-val) -1 1))))
                    expanded (> (count round-up-result) (count result))]
                [round-up-result e1 expanded])
              [result e1 false]))
          [m e false])
        [m e false]))
    [m e false]))

(defn- expand-fixed [m e d]
  (let [m1 (if (neg? e) (str (apply str (repeat (dec (- e)) \0)) m) m)
        len (count m1)
        target-len (if d (+ e d 1) (inc e))]
    (if (< len target-len) 
      (str m1 (apply str (repeat (- target-len len) \0))) 
      m1)))

(defn- insert-decimal [m e]
  "Insert the decimal point at the right spot in the number to match an exponent"
  (if (neg? e)
    (str "." m)
    (let [loc (inc e)]
      (str (subs m 0 loc) "." (subs m loc)))))

(defn- get-fixed [m e d]
  (insert-decimal (expand-fixed m e d) e))

(defn- insert-scaled-decimal [m k]
  "Insert the decimal point at the right spot in the number to match an exponent"
  (if (neg? k)
    (str "." m)
    (str (subs m 0 k) "." (subs m k))))

;; the function to render ~F directives
;; TODO: support rationals. Back off to ~D/~A is the appropriate cases
(defn- fixed-float [params navigator offsets]
  (let [w (:w params)
        d (:d params)
        [arg navigator] (next-arg navigator)
        [mantissa exp] (float-parts arg)
        scaled-exp (+ exp (:k params))
        add-sign (and (:at params) (not (neg? arg)))
        prepend-zero (< -1.0 arg 1.0)
        append-zero (and (not d) (<= (dec (count mantissa)) scaled-exp))
        [rounded-mantissa scaled-exp] (round-str mantissa scaled-exp 
                                                 d (if w (- w (if add-sign 1 0))))
        fixed-repr (get-fixed rounded-mantissa scaled-exp d)]
    (if w
      (let [len (count fixed-repr)
            signed-len (if add-sign (inc len) len)
            prepend-zero (and prepend-zero (not (= signed-len w)))
            append-zero (and append-zero (not (= signed-len w)))
            full-len (if (or prepend-zero append-zero)
                       (inc signed-len) 
                       signed-len)]
        (if (and (> full-len w) (:overflowchar params))
          (print (apply str (repeat w (:overflowchar params))))
          (print (str
                  (apply str (repeat (- w full-len) (:padchar params)))
                  (if add-sign "+") 
                  (if prepend-zero "0")
                  fixed-repr
                  (if append-zero "0")))))
      (print (str
              (if add-sign "+") 
              (if prepend-zero "0")
              fixed-repr
              (if append-zero "0"))))
    navigator))


;; the function to render ~E directives
;; TODO: support rationals. Back off to ~D/~A is the appropriate cases
;; TODO: define ~E representation for Infinity
(defn- exponential-float [params navigator offsets]
  (let [[arg navigator] (next-arg navigator)]
    (loop [[mantissa exp] (float-parts (if (neg? arg) (- arg) arg))]
      (let [w (:w params)
            d (:d params)
            e (:e params)
            k (:k params)
            expchar (or (:exponentchar params) \E)
            add-sign (or (:at params) (neg? arg))
            prepend-zero (<= k 0)
            #^Integer scaled-exp (- exp (dec k))
            scaled-exp-str (str (Math/abs scaled-exp))
            scaled-exp-str (str expchar (if (neg? scaled-exp) \- \+) 
                                (if e (apply str 
                                             (repeat 
                                              (- e 
                                                 (count scaled-exp-str)) 
                                              \0))) 
                                scaled-exp-str)
            exp-width (count scaled-exp-str)
            base-mantissa-width (count mantissa)
            scaled-mantissa (str (apply str (repeat (- k) \0))
                                 mantissa
                                 (if d 
                                   (apply str 
                                          (repeat 
                                           (- d (dec base-mantissa-width)
                                              (if (neg? k) (- k) 0)) \0))))
            w-mantissa (if w (- w exp-width))
            [rounded-mantissa _ incr-exp] (round-str 
                                           scaled-mantissa 0
                                           (cond
                                            (= k 0) (dec d)
                                            (pos? k) d
                                            (neg? k) (dec d))
                                           (if w-mantissa 
                                             (- w-mantissa (if add-sign 1 0))))
            full-mantissa (insert-scaled-decimal rounded-mantissa k)
            append-zero (and (= k (count rounded-mantissa)) (nil? d))]
        (if (not incr-exp)
          (if w
            (let [len (+ (count full-mantissa) exp-width)
                  signed-len (if add-sign (inc len) len)
                  prepend-zero (and prepend-zero (not (= signed-len w)))
                  full-len (if prepend-zero (inc signed-len) signed-len)
                  append-zero (and append-zero (< full-len w))]
              (if (and (or (> full-len w) (and e (> (- exp-width 2) e)))
                       (:overflowchar params))
                (print (apply str (repeat w (:overflowchar params))))
                (print (str
                        (apply str 
                               (repeat 
                                (- w full-len (if append-zero 1 0) )
                                (:padchar params)))
                        (if add-sign (if (neg? arg) \- \+)) 
                        (if prepend-zero "0")
                        full-mantissa
                        (if append-zero "0")
                        scaled-exp-str))))
            (print (str
                    (if add-sign (if (neg? arg) \- \+)) 
                    (if prepend-zero "0")
                    full-mantissa
                    (if append-zero "0")
                    scaled-exp-str)))
          (recur [rounded-mantissa (inc exp)]))))
    navigator))

;; the function to render ~G directives
;; This just figures out whether to pass the request off to ~F or ~E based 
;; on the algorithm in CLtL.
;; TODO: support rationals. Back off to ~D/~A is the appropriate cases
;; TODO: refactor so that float-parts isn't called twice
(defn- general-float [params navigator offsets]
  (let [[arg _] (next-arg navigator)
        [mantissa exp] (float-parts (if (neg? arg) (- arg) arg))
        w (:w params)
        d (:d params)
        e (:e params)
        n (if (= arg 0.0) 0 (inc exp))
        ee (if e (+ e 2) 4)
        ww (if w (- w ee))
        d (if d d (max (count mantissa) (min n 7)))
        dd (- d n)]
    (if (<= 0 dd d)
      (let [navigator (fixed-float {:w ww, :d dd, :k 0, 
                                    :overflowchar (:overflowchar params),
                                    :padchar (:padchar params), :at (:at params)} 
                                   navigator offsets)]
        (print (apply str (repeat ee \space)))
        navigator)
      (exponential-float params navigator offsets))))

;; the function to render ~$ directives
;; TODO: support rationals. Back off to ~D/~A is the appropriate cases
(defn- dollar-float [params navigator offsets]
  (let [[#^Double arg navigator] (next-arg navigator)
        [mantissa exp] (float-parts (Math/abs arg))
        d (:d params) ; digits after the decimal
        n (:n params) ; minimum digits before the decimal
        w (:w params) ; minimum field width
        add-sign (and (:at params) (not (neg? arg)))
        [rounded-mantissa scaled-exp _] (round-str mantissa exp d nil)
        #^String fixed-repr (get-fixed rounded-mantissa scaled-exp d)
        full-repr (str (apply str (repeat (- n (.indexOf fixed-repr (int \.))) \0)) fixed-repr)
        full-len (+ (count full-repr) (if add-sign 1 0))]
    (print (str
            (if (and (:colon params) add-sign) (if (neg? arg) \- \+))
            (apply str (repeat (- w full-len) (:padchar params)))
            (if (and (not (:colon params)) add-sign) (if (neg? arg) \- \+))
            full-repr))
    navigator))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for the '~[...~]' conditional construct in its
;;; different flavors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ~[...~] without any modifiers chooses one of the clauses based on the param or 
;; next argument
;; TODO check arg is positive int
(defn- choice-conditional [params arg-navigator offsets]
  (let [arg (:selector params)
        [arg navigator] (if arg [arg arg-navigator] (next-arg arg-navigator))
        clauses (:clauses params)
        clause (if (or (neg? arg) (>= arg (count clauses)))
                 (first (:else params))
                 (nth clauses arg))]
    (if clause
      (execute-sub-format clause navigator (:base-args params))
      navigator)))

;; ~:[...~] with the colon reads the next argument treating it as a truth value
(defn- boolean-conditional [params arg-navigator offsets]
  (let [[arg navigator] (next-arg arg-navigator)
        clauses (:clauses params)
        clause (if arg
                 (second clauses)
                 (first clauses))]
    (if clause
      (execute-sub-format clause navigator (:base-args params))
      navigator)))

;; ~@[...~] with the at sign executes the conditional if the next arg is not
;; nil/false without consuming the arg
(defn- check-arg-conditional [params arg-navigator offsets]
  (let [[arg navigator] (next-arg arg-navigator)
        clauses (:clauses params)
        clause (if arg (first clauses))]
    (if arg
      (if clause
        (execute-sub-format clause arg-navigator (:base-args params))
        arg-navigator)
      navigator)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for the '~{...~}' iteration construct in its
;;; different flavors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ~{...~} without any modifiers uses the next argument as an argument list that 
;; is consumed by all the iterations
(defn- iterate-sublist [params navigator offsets]
  (let [max-count (:max-iterations params)
        param-clause (first (:clauses params))
        [clause navigator] (if (empty? param-clause) 
                             (get-format-arg navigator)
                             [param-clause navigator]) 
        [arg-list navigator] (next-arg navigator)
        args (init-navigator arg-list)]
    (loop [count 0
           args args
           last-pos -1]
      (if (and (not max-count) (= (:pos args) last-pos) (> count 1))
        ;; TODO get the offset in here and call format exception
        (throw (RuntimeException. "%{ construct not consuming any arguments: Infinite loop!")))
      (if (or (and (empty? (:rest args))
                   (or (not (:colon (:right-params params))) (> count 0)))
              (and max-count (>= count max-count)))
        navigator
        (let [iter-result (execute-sub-format clause args (:base-args params))] 
          (if (= :up-arrow (first iter-result))
            navigator
            (recur (inc count) iter-result (:pos args))))))))

;; ~:{...~} with the colon treats the next argument as a list of sublists. Each of the
;; sublists is used as the arglist for a single iteration.
(defn- iterate-list-of-sublists [params navigator offsets]
  (let [max-count (:max-iterations params)
        param-clause (first (:clauses params))
        [clause navigator] (if (empty? param-clause) 
                             (get-format-arg navigator)
                             [param-clause navigator]) 
        [arg-list navigator] (next-arg navigator)]
    (loop [count 0
           arg-list arg-list]
      (if (or (and (empty? arg-list)
                   (or (not (:colon (:right-params params))) (> count 0)))
              (and max-count (>= count max-count)))
        navigator
        (let [iter-result (execute-sub-format 
                           clause 
                           (init-navigator (first arg-list))
                           (init-navigator (next arg-list)))]
          (if (= :colon-up-arrow (first iter-result))
            navigator
            (recur (inc count) (next arg-list))))))))

;; ~@{...~} with the at sign uses the main argument list as the arguments to the iterations
;; is consumed by all the iterations
(defn- iterate-main-list [params navigator offsets]
  (let [max-count (:max-iterations params)
        param-clause (first (:clauses params))
        [clause navigator] (if (empty? param-clause) 
                             (get-format-arg navigator)
                             [param-clause navigator])]
    (loop [count 0
           navigator navigator
           last-pos -1]
      (if (and (not max-count) (= (:pos navigator) last-pos) (> count 1))
        ;; TODO get the offset in here and call format exception
        (throw (RuntimeException. "%@{ construct not consuming any arguments: Infinite loop!")))
      (if (or (and (empty? (:rest navigator))
                   (or (not (:colon (:right-params params))) (> count 0)))
              (and max-count (>= count max-count)))
        navigator
        (let [iter-result (execute-sub-format clause navigator (:base-args params))] 
          (if (= :up-arrow (first iter-result))
            (second iter-result)
            (recur 
             (inc count) iter-result (:pos navigator))))))))

;; ~@:{...~} with both colon and at sign uses the main argument list as a set of sublists, one
;; of which is consumed with each iteration
(defn- iterate-main-sublists [params navigator offsets]
  (let [max-count (:max-iterations params)
        param-clause (first (:clauses params))
        [clause navigator] (if (empty? param-clause) 
                             (get-format-arg navigator)
                             [param-clause navigator]) 
        ]
    (loop [count 0
           navigator navigator]
      (if (or (and (empty? (:rest navigator))
                   (or (not (:colon (:right-params params))) (> count 0)))
              (and max-count (>= count max-count)))
        navigator
        (let [[sublist navigator] (next-arg-or-nil navigator)
              iter-result (execute-sub-format clause (init-navigator sublist) navigator)]
          (if (= :colon-up-arrow (first iter-result))
            navigator
            (recur (inc count) navigator)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The '~< directive has two completely different meanings
;;; in the '~<...~>' form it does justification, but with
;;; ~<...~:>' it represents the logical block operation of the
;;; pretty printer.
;;; 
;;; Unfortunately, the current architecture decides what function
;;; to call at form parsing time before the sub-clauses have been
;;; folded, so it is left to run-time to make the decision.
;;; 
;;; TODO: make it possible to make these decisions at compile-time.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare format-logical-block)
(declare justify-clauses)

(defn- logical-block-or-justify [params navigator offsets]
  (if (:colon (:right-params params))
    (format-logical-block params navigator offsets)
    (justify-clauses params navigator offsets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for the '~<...~>' justification directive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- render-clauses [clauses navigator base-navigator]
  (loop [clauses clauses
         acc []
         navigator navigator]
    (if (empty? clauses)
      [acc navigator]
      (let [clause (first clauses)
            [iter-result result-str] (binding [*out* (java.io.StringWriter.)]
                                       [(execute-sub-format clause navigator base-navigator) 
                                        (.toString *out*)])]
        (if (= :up-arrow (first iter-result))
          [acc (second iter-result)]
          (recur (next clauses) (conj acc result-str) iter-result))))))

;; TODO support for ~:; constructions
(defn- justify-clauses [params navigator offsets]
  (let [[[eol-str] new-navigator] (when-let [else (:else params)]
                                    (render-clauses else navigator (:base-args params)))
        navigator (or new-navigator navigator)
        [else-params new-navigator] (when-let [p (:else-params params)]
                                      (realize-parameter-list p navigator))
        navigator (or new-navigator navigator)
        min-remaining (or (first (:min-remaining else-params)) 0)
        max-columns (or (first (:max-columns else-params))
                        (.getMaxColumn #^PrettyWriter *out*))
        clauses (:clauses params)
        [strs navigator] (render-clauses clauses navigator (:base-args params))
        slots (max 1
                   (+ (dec (count strs)) (if (:colon params) 1 0) (if (:at params) 1 0)))
        chars (reduce + (map count strs))
        mincol (:mincol params)
        minpad (:minpad params)
        colinc (:colinc params)
        minout (+ chars (* slots minpad))
        result-columns (if (<= minout mincol) 
                         mincol
                         (+ mincol (* colinc
                                      (+ 1 (quot (- minout mincol 1) colinc)))))
        total-pad (- result-columns chars)
        pad (max minpad (quot total-pad slots))
        extra-pad (- total-pad (* pad slots))
        pad-str (apply str (repeat pad (:padchar params)))]
    (if (and eol-str (> (+ (.getColumn #^PrettyWriter *out*) min-remaining result-columns) 
                        max-columns))
      (print eol-str))
    (loop [slots slots
           extra-pad extra-pad
           strs strs
           pad-only (or (:colon params)
                        (and (= (count strs) 1) (not (:at params))))]
      (if (seq strs)
        (do
          (print (str (if (not pad-only) (first strs))
                      (if (or pad-only (next strs) (:at params)) pad-str)
                      (if (pos? extra-pad) (:padchar params))))
          (recur 
           (dec slots)
           (dec extra-pad)
           (if pad-only strs (next strs))
           false))))
    navigator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for case modification with ~(...~).
;;; We do this by wrapping the underlying writer with
;;; a special writer to do the appropriate modification. This
;;; allows us to support arbitrary-sized output and sources
;;; that may block.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- downcase-writer 
  "Returns a proxy that wraps writer, converting all characters to lower case"
  [#^java.io.Writer writer]
  (proxy [java.io.Writer] []
    (close [] (.close writer))
    (flush [] (.flush writer))
    (write ([#^chars cbuf #^Integer off #^Integer len] 
              (.write writer cbuf off len))
           ([x]
              (condp = (class x)
		String 
		(let [s #^String x]
		  (.write writer (.toLowerCase s)))

		Integer
		(let [c #^Character x]
		  (.write writer (int (Character/toLowerCase (char c))))))))))

(defn- upcase-writer 
  "Returns a proxy that wraps writer, converting all characters to upper case"
  [#^java.io.Writer writer]
  (proxy [java.io.Writer] []
    (close [] (.close writer))
    (flush [] (.flush writer))
    (write ([#^chars cbuf #^Integer off #^Integer len] 
              (.write writer cbuf off len))
           ([x]
              (condp = (class x)
		String 
		(let [s #^String x]
		  (.write writer (.toUpperCase s)))

		Integer
		(let [c #^Character x]
		  (.write writer (int (Character/toUpperCase (char c))))))))))

(defn- capitalize-string
  "Capitalizes the words in a string. If first? is false, don't capitalize the 
                                      first character of the string even if it's a letter."
  [s first?]
  (let [#^Character f (first s) 
        s (if (and first? f (Character/isLetter f))
            (str (Character/toUpperCase f) (subs s 1))
            s)]
    (apply str 
           (first
            (consume
             (fn [s]
               (if (empty? s)
                 [nil nil]
                 (let [m (re-matcher #"\W\w" s)
                       match (re-find m)
                       offset (and match (inc (.start m)))]
                   (if offset
                     [(str (subs s 0 offset) 
                           (Character/toUpperCase #^Character (nth s offset)))
                      (subs s (inc offset))]
                     [s nil]))))
             s)))))

(defn- capitalize-word-writer
  "Returns a proxy that wraps writer, captializing all words"
  [#^java.io.Writer writer]
  (let [last-was-whitespace? (ref true)] 
    (proxy [java.io.Writer] []
      (close [] (.close writer))
      (flush [] (.flush writer))
      (write 
       ([#^chars cbuf #^Integer off #^Integer len] 
          (.write writer cbuf off len))
       ([x]
          (condp = (class x)
            String 
            (let [s #^String x]
              (.write writer 
                      #^String (capitalize-string (.toLowerCase s) @last-was-whitespace?))
              (dosync 
               (ref-set last-was-whitespace? 
                        (Character/isWhitespace 
                         #^Character (nth s (dec (count s)))))))

            Integer
            (let [c (char x)]
              (let [mod-c (if @last-was-whitespace? (Character/toUpperCase #^Character (char x)) c)] 
                (.write writer (int mod-c))
                (dosync (ref-set last-was-whitespace? (Character/isWhitespace #^Character (char x))))))))))))

(defn- init-cap-writer
  "Returns a proxy that wraps writer, capitalizing the first word"
  [#^java.io.Writer writer]
  (let [capped (ref false)] 
    (proxy [java.io.Writer] []
      (close [] (.close writer))
      (flush [] (.flush writer))
      (write ([#^chars cbuf #^Integer off #^Integer len] 
                (.write writer cbuf off len))
             ([x]
                (condp = (class x)
                 String 
                 (let [s (.toLowerCase #^String x)]
                   (if (not @capped) 
                     (let [m (re-matcher #"\S" s)
                           match (re-find m)
                           offset (and match (.start m))]
                       (if offset
                         (do (.write writer 
                                   (str (subs s 0 offset) 
                                        (Character/toUpperCase #^Character (nth s offset))
                                        (.toLowerCase #^String (subs s (inc offset)))))
                           (dosync (ref-set capped true)))
                         (.write writer s))) 
                     (.write writer (.toLowerCase s))))

                 Integer
                 (let [c #^Character (char x)]
                   (if (and (not @capped) (Character/isLetter c))
                     (do
                       (dosync (ref-set capped true))
                       (.write writer (int (Character/toUpperCase c))))
                     (.write writer (int (Character/toLowerCase c)))))))))))

(defn- modify-case [make-writer params navigator offsets]
  (let [clause (first (:clauses params))]
    (binding [*out* (make-writer *out*)] 
      (execute-sub-format clause navigator (:base-args params)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; If necessary, wrap the writer in a PrettyWriter object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pretty-writer [writer]
  (if (instance? PrettyWriter writer) 
    writer
    (PrettyWriter. writer *print-right-margin* *print-miser-width*)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for column-aware operations ~&, ~T
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: make an automatic newline for non-ColumnWriters
(defn fresh-line []
  "Make a newline if the Writer is not already at the beginning of the line.
N.B. Only works on ColumnWriters right now."
  (if (not (= 0 (.getColumn #^PrettyWriter *out*)))
    (prn)))

(defn- absolute-tabulation [params navigator offsets]
  (let [colnum (:colnum params) 
        colinc (:colinc params)
        current (.getColumn #^PrettyWriter *out*)
        space-count (cond
                     (< current colnum) (- colnum current)
                     (= colinc 0) 0
                     :else (- colinc (rem (- current colnum) colinc)))]
    (print (apply str (repeat space-count \space))))
  navigator)

(defn- relative-tabulation [params navigator offsets]
  (let [colrel (:colnum params) 
        colinc (:colinc params)
        start-col (+ colrel (.getColumn #^PrettyWriter *out*))
        offset (if (pos? colinc) (rem start-col colinc) 0)
        space-count (+ colrel (if (= 0 offset) 0 (- colinc offset)))]
    (print (apply str (repeat space-count \space))))
  navigator)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for accessing the pretty printer from a format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: support ~@; per-line-prefix separator
;; TODO: get the whole format wrapped so we can start the lb at any column
(defn- format-logical-block [params navigator offsets]
  (let [clauses (:clauses params)
        clause-count (count clauses)
        prefix (cond
                (> clause-count 1) (:string (:params (first (first clauses))))
                (:colon params) "(")
        body (nth clauses (if (> clause-count 1) 1 0))
        suffix (cond
                (> clause-count 2) (:string (:params (first (nth clauses 2))))
                (:colon params) ")")
        [arg navigator] (next-arg navigator)]
    (pprint-logical-block *out* :prefix prefix :suffix suffix
      (execute-sub-format 
       body 
       (init-navigator arg)
       (:base-args params)))
    navigator))

(defn- set-indent [params navigator offsets]
  (let [relative-to (if (:colon params) :current :block)]
    (pprint-indent relative-to (:n params))
    navigator))

;;; TODO: support ~:T section options for ~T

(defn- conditional-newline [params navigator offsets]
  (let [kind (if (:colon params) 
               (if (:at params) :mandatory :fill)
               (if (:at params) :miser :linear))]
    (pprint-newline kind)
    navigator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The table of directives we support, each with its params,
;;; properties, and the compilation function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We start with a couple of helpers
(defn- process-directive-table-element [ [ char params flags bracket-info & generator-fn ] ]
  [char, 
   {:directive char,
    :params `(array-map ~@params),
    :flags flags,
    :bracket-info bracket-info,
    :generator-fn (concat '(fn [ params offset]) generator-fn) }])

(defmacro #^{:private true}
  defdirectives 
  [ & directives ]
  `(def #^{:private true}
        directive-table (hash-map ~@(mapcat process-directive-table-element directives))))

(defdirectives 
  (\A 
   [ :mincol [0 Integer] :colinc [1 Integer] :minpad [0 Integer] :padchar [\space Character] ] 
   #{ :at :colon :both} {}
   #(format-ascii print-str %1 %2 %3))

  (\S 
   [ :mincol [0 Integer] :colinc [1 Integer] :minpad [0 Integer] :padchar [\space Character] ] 
   #{ :at :colon :both} {}
   #(format-ascii pr-str %1 %2 %3))

  (\D
   [ :mincol [0 Integer] :padchar [\space Character] :commachar [\, Character] 
    :commainterval [ 3 Integer]]
   #{ :at :colon :both } {}
   #(format-integer 10 %1 %2 %3))

  (\B
   [ :mincol [0 Integer] :padchar [\space Character] :commachar [\, Character] 
    :commainterval [ 3 Integer]]
   #{ :at :colon :both } {}
   #(format-integer 2 %1 %2 %3))

  (\O
   [ :mincol [0 Integer] :padchar [\space Character] :commachar [\, Character] 
    :commainterval [ 3 Integer]]
   #{ :at :colon :both } {}
   #(format-integer 8 %1 %2 %3))

  (\X
   [ :mincol [0 Integer] :padchar [\space Character] :commachar [\, Character] 
    :commainterval [ 3 Integer]]
   #{ :at :colon :both } {}
   #(format-integer 16 %1 %2 %3))

  (\R
   [:base [nil Integer] :mincol [0 Integer] :padchar [\space Character] :commachar [\, Character] 
    :commainterval [ 3 Integer]]
   #{ :at :colon :both } {}
   (do
     (cond                            ; ~R is overloaded with bizareness
      (first (:base params))     #(format-integer (:base %1) %1 %2 %3)
      (and (:at params) (:colon params))   #(format-old-roman %1 %2 %3)
      (:at params)               #(format-new-roman %1 %2 %3)
      (:colon params)            #(format-ordinal-english %1 %2 %3)
      true                       #(format-cardinal-english %1 %2 %3))))

  (\P
   [ ]
   #{ :at :colon :both } {}
   (fn [params navigator offsets]
     (let [navigator (if (:colon params) (relative-reposition navigator -1) navigator)
           strs (if (:at params) ["y" "ies"] ["" "s"])
           [arg navigator] (next-arg navigator)]
       (print (if (= arg 1) (first strs) (second strs)))
       navigator)))

  (\C
   [:char-format [nil Character]]
   #{ :at :colon :both } {}
   (cond
    (:colon params) pretty-character
    (:at params) readable-character
    :else plain-character))

  (\F
   [ :w [nil Integer] :d [nil Integer] :k [0 Integer] :overflowchar [nil Character] 
    :padchar [\space Character] ]
   #{ :at } {}
   fixed-float)

  (\E
   [ :w [nil Integer] :d [nil Integer] :e [nil Integer] :k [1 Integer] 
    :overflowchar [nil Character] :padchar [\space Character] 
    :exponentchar [nil Character] ]
   #{ :at } {}
   exponential-float)

  (\G
   [ :w [nil Integer] :d [nil Integer] :e [nil Integer] :k [1 Integer] 
    :overflowchar [nil Character] :padchar [\space Character] 
    :exponentchar [nil Character] ]
   #{ :at } {}
   general-float)

  (\$
   [ :d [2 Integer] :n [1 Integer] :w [0 Integer] :padchar [\space Character]]
   #{ :at :colon :both} {}
   dollar-float)

  (\% 
   [ :count [1 Integer] ] 
   #{ } {}
   (fn [params arg-navigator offsets]
     (dotimes [i (:count params)]
       (prn))
     arg-navigator))

  (\&
   [ :count [1 Integer] ] 
   #{ :pretty } {}
   (fn [params arg-navigator offsets]
     (let [cnt (:count params)]
       (if (pos? cnt) (fresh-line))
       (dotimes [i (dec cnt)]
         (prn)))
     arg-navigator))

  (\| 
   [ :count [1 Integer] ] 
   #{ } {}
   (fn [params arg-navigator offsets]
     (dotimes [i (:count params)]
       (print \formfeed))
     arg-navigator))

  (\~ 
   [ :n [1 Integer] ] 
   #{ } {}
   (fn [params arg-navigator offsets]
     (let [n (:n params)]
       (print (apply str (repeat n \~)))
       arg-navigator)))

  (\newline ;; Whitespace supression is handled in the compilation loop
   [ ] 
   #{:colon :at} {}
   (fn [params arg-navigator offsets]
     (if (:at params)
       (prn))
     arg-navigator))

  (\T
   [ :colnum [1 Integer] :colinc [1 Integer] ] 
   #{ :at :pretty } {}
   (if (:at params)
     #(relative-tabulation %1 %2 %3)
     #(absolute-tabulation %1 %2 %3)))

  (\* 
   [ :n [1 Integer] ] 
   #{ :colon :at } {}
   (fn [params navigator offsets]
     (let [n (:n params)]
       (if (:at params)
         (absolute-reposition navigator n)
         (relative-reposition navigator (if (:colon params) (- n) n)))
       )))

  (\? 
   [ ] 
   #{ :at } {}
   (if (:at params)
     (fn [params navigator offsets]        ; args from main arg list
       (let [[subformat navigator] (get-format-arg navigator)]
         (execute-sub-format subformat navigator  (:base-args params))))
     (fn [params navigator offsets]        ; args from sub-list
       (let [[subformat navigator] (get-format-arg navigator)
             [subargs navigator] (next-arg navigator)
             sub-navigator (init-navigator subargs)]
         (execute-sub-format subformat sub-navigator (:base-args params))
         navigator))))
       

  (\(
   [ ]
   #{ :colon :at :both} { :right \), :allows-separator nil, :else nil }
   (let [mod-case-writer (cond
                          (and (:at params) (:colon params))
                          upcase-writer

                          (:colon params)
                          capitalize-word-writer

                          (:at params)
                          init-cap-writer

                          :else
                          downcase-writer)]
     #(modify-case mod-case-writer %1 %2 %3)))

  (\) [] #{} {} nil) 

  (\[
   [ :selector [nil Integer] ]
   #{ :colon :at } { :right \], :allows-separator true, :else :last }
   (cond
    (:colon params)
    boolean-conditional

    (:at params)
    check-arg-conditional

    true
    choice-conditional))

  (\; [:min-remaining [nil Integer] :max-columns [nil Integer]] 
   #{ :colon } { :separator true } nil) 
   
  (\] [] #{} {} nil) 

  (\{
   [ :max-iterations [nil Integer] ]
   #{ :colon :at :both} { :right \}, :allows-separator false }
   (cond
    (and (:at params) (:colon params))
    iterate-main-sublists

    (:colon params)
    iterate-list-of-sublists

    (:at params)
    iterate-main-list

    true
    iterate-sublist))

   
  (\} [] #{:colon} {} nil) 

  (\<
   [:mincol [0 Integer] :colinc [1 Integer] :minpad [0 Integer] :padchar [\space Character]]
   #{:colon :at :both :pretty} { :right \>, :allows-separator true, :else :first }
   logical-block-or-justify)

  (\> [] #{:colon} {} nil) 

  ;; TODO: detect errors in cases where colon not allowed
  (\^ [:arg1 [nil Integer] :arg2 [nil Integer] :arg3 [nil Integer]] 
   #{:colon} {} 
   (fn [params navigator offsets]
     (let [arg1 (:arg1 params)
           arg2 (:arg2 params)
           arg3 (:arg3 params)
           exit (if (:colon params) :colon-up-arrow :up-arrow)]
       (cond
        (and arg1 arg2 arg3)
        (if (<= arg1 arg2 arg3) [exit navigator] navigator)

        (and arg1 arg2)
        (if (= arg1 arg2) [exit navigator] navigator)

        arg1
        (if (= arg1 0) [exit navigator] navigator)

        true          ; TODO: handle looking up the arglist stack for info
        (if (if (:colon params) 
              (empty? (:rest (:base-args params)))
              (empty? (:rest navigator)))
          [exit navigator] navigator))))) 

  (\W 
   [] 
   #{:at :colon :both} {}
   (let [bindings (concat
                   (if (:at params) [:level nil :length nil] [])
                   (if (:colon params) [:pretty true] []))]
     (fn [params navigator offsets]
       (let [[arg navigator] (next-arg navigator)]
         (if (apply write arg bindings)
           [:up-arrow navigator]
           navigator)))))

  (\_
   []
   #{:at :colon :both} {}
   conditional-newline)

  (\I
   [:n [0 Integer]]
   #{:colon} {}
   set-indent)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code to manage the parameters and flags accociated with each
;;; directive in the format string.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def #^{:private true}
     param-pattern #"^([vV]|#|('.)|([+-]?\d+)|(?=,))")
(def #^{:private true}
     special-params #{ :parameter-from-args :remaining-arg-count })

(defn- extract-param [[s offset saw-comma]]
  (let [m (re-matcher param-pattern s)
        param (re-find m)]
    (if param
      (let [token-str (first (re-groups m))
            remainder (subs s (.end m))
            new-offset (+ offset (.end m))]
        (if (not (= \, (nth remainder 0)))
          [ [token-str offset] [remainder new-offset false]]
          [ [token-str offset] [(subs remainder 1) (inc new-offset) true]]))
      (if saw-comma 
        (format-error "Badly formed parameters in format directive" offset)
        [ nil [s offset]]))))


(defn- extract-params [s offset] 
  (consume extract-param [s offset false]))

(defn- translate-param [[#^String p offset]]
  "Translate the string representation of a param to the internalized
                                      representation"
  [(cond 
    (= (.length p) 0) nil
    (and (= (.length p) 1) (contains? #{\v \V} (nth p 0))) :parameter-from-args
    (and (= (.length p) 1) (= \# (nth p 0))) :remaining-arg-count
    (and (= (.length p) 2) (= \' (nth p 0))) (nth p 1)
    true (new Integer p))
   offset])
 
(def #^{:private true}
     flag-defs { \: :colon, \@ :at })

(defn- extract-flags [s offset]
  (consume
   (fn [[s offset flags]]
     (if (empty? s)
       [nil [s offset flags]]
       (let [flag (get flag-defs (first s))]
         (if flag
           (if (contains? flags flag)
             (format-error 
              (str "Flag \"" (first s) "\" appears more than once in a directive")
              offset)
             [true [(subs s 1) (inc offset) (assoc flags flag [true offset])]])
           [nil [s offset flags]]))))
   [s offset {}]))

(defn- check-flags [def flags]
  (let [allowed (:flags def)]
    (if (and (not (:at allowed)) (:at flags))
      (format-error (str "\"@\" is an illegal flag for format directive \"" (:directive def) "\"")
                    (nth (:at flags) 1)))
    (if (and (not (:colon allowed)) (:colon flags))
      (format-error (str "\":\" is an illegal flag for format directive \"" (:directive def) "\"")
                    (nth (:colon flags) 1)))
    (if (and (not (:both allowed)) (:at flags) (:colon flags))
      (format-error (str "Cannot combine \"@\" and \":\" flags for format directive \"" 
                         (:directive def) "\"")
                    (min (nth (:colon flags) 1) (nth (:at flags) 1))))))

(defn- map-params [def params flags offset]
  "Takes a directive definition and the list of actual parameters and
a map of flags and returns a map of the parameters and flags with defaults
filled in. We check to make sure that there are the right types and number
of parameters as well."
  (check-flags def flags)
  (if (> (count params) (count (:params def)))
    (format-error 
     (cl-format 
      nil 
      "Too many parameters for directive \"~C\": ~D~:* ~[were~;was~:;were~] specified but only ~D~:* ~[are~;is~:;are~] allowed"
      (:directive def) (count params) (count (:params def)))
     (second (first params))))
  (doall
   (map #(let [val (first %1)]
           (if (not (or (nil? val) (contains? special-params val) 
                        (instance? (second (second %2)) val)))
             (format-error (str "Parameter " (name (first %2))
                                " has bad type in directive \"" (:directive def) "\": "
                                (class val))
                           (second %1))) )
        params (:params def)))
     
  (merge                                ; create the result map
   (into (array-map) ; start with the default values, make sure the order is right
         (reverse (for [[name [default]] (:params def)] [name [default offset]])))
   (reduce #(apply assoc %1 %2) {} (filter #(first (nth % 1)) (zipmap (keys (:params def)) params))) ; add the specified parameters, filtering out nils
   flags))                                ; and finally add the flags

(defn- compile-directive [s offset]
  (let [[raw-params [rest offset]] (extract-params s offset)
        [_ [rest offset flags]] (extract-flags rest offset)
        directive (first rest)
        def (get directive-table (Character/toUpperCase #^Character directive))
        params (if def (map-params def (map translate-param raw-params) flags offset))]
    (if (not directive)
      (format-error "Format string ended in the middle of a directive" offset))
    (if (not def)
      (format-error (str "Directive \"" directive "\" is undefined") offset))
    [(struct compiled-directive ((:generator-fn def) params offset) def params offset)
     (let [remainder (subs rest 1) 
           offset (inc offset)
           trim? (and (= \newline (:directive def))
                      (not (:colon params)))
           trim-count (if trim? (prefix-count remainder [\space \tab]) 0)
           remainder (subs remainder trim-count)
           offset (+ offset trim-count)]
       [remainder offset])]))
    
(defn- compile-raw-string [s offset]
  (struct compiled-directive (fn [_ a _] (print s) a) nil { :string s } offset))

(defn- right-bracket [this] (:right (:bracket-info (:def this))))
(defn- separator? [this] (:separator (:bracket-info (:def this))))
(defn- else-separator? [this] 
  (and (:separator (:bracket-info (:def this)))
       (:colon (:params this))))
  

(declare collect-clauses)

(defn- process-bracket [this remainder]
  (let [[subex remainder] (collect-clauses (:bracket-info (:def this))
                                           (:offset this) remainder)]
    [(struct compiled-directive 
             (:func this) (:def this) 
             (merge (:params this) (tuple-map subex (:offset this)))
             (:offset this))
     remainder]))

(defn- process-clause [bracket-info offset remainder]
  (consume 
   (fn [remainder]
     (if (empty? remainder)
       (format-error "No closing bracket found." offset)
       (let [this (first remainder)
             remainder (next remainder)]
         (cond
          (right-bracket this)
          (process-bracket this remainder)

          (= (:right bracket-info) (:directive (:def this)))
          [ nil [:right-bracket (:params this) nil remainder]]

          (else-separator? this)
          [nil [:else nil (:params this) remainder]]

          (separator? this)
          [nil [:separator nil nil remainder]] ;; TODO: check to make sure that there are no params on ~;

          true
          [this remainder]))))
   remainder))

(defn- collect-clauses [bracket-info offset remainder]
  (second
   (consume
    (fn [[clause-map saw-else remainder]]
      (let [[clause [type right-params else-params remainder]] 
            (process-clause bracket-info offset remainder)]
        (cond
         (= type :right-bracket)
         [nil [(merge-with concat clause-map 
                           {(if saw-else :else :clauses) [clause] 
                            :right-params right-params})
               remainder]]

         (= type :else)
         (cond
          (:else clause-map)
          (format-error "Two else clauses (\"~:;\") inside bracket construction." offset)
         
          (not (:else bracket-info))
          (format-error "An else clause (\"~:;\") is in a bracket type that doesn't support it." 
                        offset)

          (and (= :first (:else bracket-info)) (seq (:clauses clause-map)))
          (format-error
           "The else clause (\"~:;\") is only allowed in the first position for this directive." 
           offset)
         
          true         ; if the ~:; is in the last position, the else clause
                                        ; is next, this was a regular clause
          (if (= :first (:else bracket-info))
            [true [(merge-with concat clause-map { :else [clause] :else-params else-params})
                   false remainder]]
            [true [(merge-with concat clause-map { :clauses [clause] })
                   true remainder]]))

         (= type :separator)
         (cond
          saw-else
          (format-error "A plain clause (with \"~;\") follows an else clause (\"~:;\") inside bracket construction." offset)
         
          (not (:allows-separator bracket-info))
          (format-error "A separator (\"~;\") is in a bracket type that doesn't support it." 
                        offset)
         
          true
          [true [(merge-with concat clause-map { :clauses [clause] })
                 false remainder]]))))
    [{ :clauses [] } false remainder])))

(defn- process-nesting
  "Take a linearly compiled format and process the bracket directives to give it 
   the appropriate tree structure"
  [format]
  (first
   (consume 
    (fn [remainder]
      (let [this (first remainder)
            remainder (next remainder)
            bracket (:bracket-info (:def this))]
        (if (:right bracket)
          (process-bracket this remainder)
          [this remainder])))
    format)))

(defn compile-format 
  "Compiles format-str into a compiled format which can be used as an argument
to cl-format just like a plain format string. Use this function for improved 
performance when you're using the same format string repeatedly"
  [ format-str ]
;  (prlabel compiling format-str)
  (binding [*format-str* format-str]
    (process-nesting
     (first 
      (consume 
       (fn [[#^String s offset]]
         (if (empty? s)
           [nil s]
           (let [tilde (.indexOf s (int \~))]
             (cond
              (neg? tilde) [(compile-raw-string s offset) ["" (+ offset (.length s))]]
              (zero? tilde)  (compile-directive (subs s 1) (inc offset))
              true 
              [(compile-raw-string (subs s 0 tilde) offset) [(subs s tilde) (+ tilde offset)]]))))
       [format-str 0])))))

(defn- needs-pretty 
  "determine whether a given compiled format has any directives that depend on the
column number or pretty printing"
  [format]
  (loop [format format]
    (if (empty? format)
      false
      (if (or (:pretty (:flags (:def (first format))))
              (some needs-pretty (first (:clauses (:params (first format)))))
              (some needs-pretty (first (:else (:params (first format))))))
        true
        (recur (next format))))))

(defn execute-format [stream format args]
  (let [#^java.io.Writer real-stream (cond 
                                       (not stream) (java.io.StringWriter.)
                                       (true? stream) *out*
                                       :else stream)
        #^java.io.Writer wrapped-stream (if (and (needs-pretty format) 
                                                 (not (instance? PrettyWriter real-stream)))
                                          (pretty-writer real-stream)
                                          real-stream)]
    (binding [*out* wrapped-stream]
      (try
       (map-passing-context 
        (fn [element context]
          (if (abort? context)
            [nil context]
            (let [[params args] (realize-parameter-list 
                                 (:params element) context)
                  [params offsets] (unzip-map params)
                  params (assoc params :base-args args)]
              [nil (apply (:func element) [params args offsets])])))
        args
        format)
       (finally
        (if-not (identical? real-stream wrapped-stream)
          (.flush wrapped-stream))))
      (if (not stream) (.toString real-stream)))))


(defmacro formatter
  "Makes a function which can directly run format-in. The function is
fn [stream & args] ... and returns nil unless the stream is nil (meaning 
output to a string) in which case it returns the resulting string.

format-in can be either a control string or a previously compiled format."
  [format-in]
  (let [cf (gensym "compiled-format")]
    `(let [format-in# ~format-in]
       (do (defonce test-format# format-in#)
           (defonce ~cf (if (string? format-in#) (compile-format format-in#) format-in#))
           (fn [stream# & args#]
             (let [navigator# (init-navigator args#)]
               (execute-format stream# ~cf navigator#)))))))
