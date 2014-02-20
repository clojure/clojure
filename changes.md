<!-- -*- mode: markdown ; mode: visual-line ; coding: utf-8 -*- -->

# Changes to Clojure in Version 1.6

## CONTENTS

## 1 Compatibility and Dependencies

## 1.1 JDK Version Update

Clojure now builds with Java SE 1.6 and emits bytecode requiring Java
SE 1.6 instead of Java SE 1.5. [CLJ-1268]

## 1.2 ASM Library Update

The embedded version of the ASM bytecode library has been upgraded to
ASM 4.1. [CLJ-713]

## 1.3 Promoted "Alpha" Features

The following features are no longer marked Alpha in Clojure:

* Watches - add-watch, remove-watch
* Transients - transient, persistent!, conj!, assoc!, dissoc!, pop!, disj!
* Exception data - ex-info, ex-data
* Promises - promise, deliver
* Records - defrecord
* Types - deftype
* Pretty-print tables - print-table

## 2 New and Improved Features

### 2.1 Java API

The clojure.java.api package provides a minimal interface to bootstrap
Clojure access from other JVM languages. It does this by providing:
1. The ability to use Clojure's namespaces to locate an arbitrary var,
   returning the var's clojure.lang.IFn interface.
2. A convenience method read for reading data using Clojure's edn
   reader.

IFns provide complete access to Clojure's APIs. You can also access
any other library written in Clojure, after adding either its source
or compiled form to the classpath.

The public Java API for Clojure consists of the following classes and interfaces:

* clojure.java.api.Clojure
* clojure.lang.IFn

All other Java classes should be treated as implementation details,
and applications should avoid relying on them.

To look up and call a Clojure function:

    IFn plus = Clojure.var("clojure.core", "+");
    plus.invoke(1, 2);

Functions in clojure.core are automatically loaded. Other namespaces
can be loaded via require:

    IFn require = Clojure.var("clojure.core", "require");
    require.invoke(Clojure.read("clojure.set"));
   
IFns can be passed to higher order functions, e.g. the example below
passes plus to read:

    IFn map = Clojure.var("clojure.core", "map");
    IFn inc = Clojure.var("clojure.core", "inc");
    map.invoke(inc, Clojure.read("[1 2 3]"));

Most IFns in Clojure refer to functions. A few, however, refer to
non-function data values. To access these, use deref instead of fn:

    IFn printLength = Clojure.var("clojure.core", "*print-length*");
    Clojure.var("clojure.core", "deref").invoke(printLength);

### 2.2 Map destructuring extended to support namespaced keys

* [CLJ-1318](http://dev.clojure.org/jira/browse/CLJ-1318)

In the past, map destructuring with :keys and :syms would not work
with maps containing namespaced keys or symbols. The :keys and :syms
forms have been updated to allow them to match namespaced keys and
bind to a local variable based on the name.

Examples:

    (let [m {:x/a 1, :y/b 2}
          {:keys [x/a y/b]} m]
      (+ a b))

    (let [m {'x/a 1, 'y/b 2}
          {:syms [x/a y/b]} m]
      (+ a b))

Additionally, the :keys form can now take keywords instead of symbols.
This provides support specifically for auto-resolved keywords:

    (let [m {:x/a 1, :y/b 2}
          {:keys [:x/a :y/b]} m]
      (+ a b))

    (let [m {::x 1}
          {:keys [::x]} m]
      x)

### 2.3 New "some" operations

Many conditional functions rely on logical truth (where "falsey"
values are nil or false). Sometimes it is useful to have functions
that rely on "not nilness" instead. These functions have been added to
support these cases [CLJ-1343]:

* some? - same as (not (nil? x))
* if-some - like if-let, but checks (some? test) instead of test
* when-some - like when-let, but checks (some? test) instead of test

### 2.4 Hashing

Clojure 1.6 provides new hashing algorithms for primitives and
collections, accessible via IHashEq/hasheq (in Java) or the
clojure.core/hash function (in Clojure). In general, these changes
should be transparent to users, except hash codes used inside hashed
collections like maps and sets will have better properties.

Hash codes returned by the Java .hashCode() method are unchanged and
continue to match Java behavior or conform to the Java specification
as appropriate.

Any collections implementing IHashEq or wishing to interoperate with
Clojure collections should conform to the hashing algorithms specified
in http://clojure.org/data_structures#hash and use the new function
`mix-collection-hash` for the final mixing operation. Any details of
the current hashing algorithm not specified on that page should be
considered subject to future change.

### 2.5 bitops

* [CLJ-827](http://dev.clojure.org/jira/browse/CLJ-827) - unsigned-bit-shift-right

A new unsigned-bit-shift-right (Java's >>>) has been added to the core
library. The shift distance is truncated to the least 6 bits (per the
Java specification for long >>>).

Examples:
  (unsigned-bit-shift-right 2r100 1) ;; 2r010
  (unsigned-bit-shift-right 2r100 2) ;; 2r001
  (unsigned-bit-shift-right 2r100 3) ;; 2r000

### 2.6 clojure.test

* [CLJ-866](http://dev.clojure.org/jira/browse/CLJ-866) - test-vars

Added a new clojure.test/test-vars function that takes a list of vars, groups them by namespace, and
runs them *with their fixtures*.

## 3 Enhancements

### 3.1 Printing

* [CLJ-908](http://dev.clojure.org/jira/browse/CLJ-908)
  Print metadata for functions when *print-meta* is true and remove errant space at beginning.
* [CLJ-937](http://dev.clojure.org/jira/browse/CLJ-937)
  pprint cl-format now supports E, F, and G formats for ratios.

### 3.2 Error messages

* [CLJ-1099](http://dev.clojure.org/jira/browse/CLJ-1099)
  If non-seq passed where seq is needed, error message now is an ExceptionInfo with the instance value, retrievable via ex-data.
* [CLJ-1083](http://dev.clojure.org/jira/browse/CLJ-1083)
  Fix error message reporting for "munged" function names (like a->b).
* [CLJ-1056](http://dev.clojure.org/jira/browse/CLJ-1056)
  Handle more cases and improve error message for errors in defprotocol definitions.
* [CLJ-1102](http://dev.clojure.org/jira/browse/CLJ-1102)
  Better handling of exceptions with empty stack traces.
* [CLJ-939](http://dev.clojure.org/jira/browse/CLJ-939)
  Exceptions thrown in the top level ns form are reported without file or line number.

### 3.3 Documentation strings

* [CLJ-1164](http://dev.clojure.org/jira/browse/CLJ-1164)
  Fix typos in clojure.instant/validated and other internal instant functions.
* [CLJ-1143](http://dev.clojure.org/jira/browse/CLJ-1143)
  Correct doc string for ns macro.
* [CLJ-196](http://dev.clojure.org/jira/browse/CLJ-196)
  Clarify value of *file* is undefined in the REPL.
* [CLJ-1228](http://dev.clojure.org/jira/browse/CLJ-1228)
  Fix a number of spelling errors in namespace and doc strings.
* [CLJ-835](http://dev.clojure.org/jira/browse/CLJ-835)
  Update defmulti doc to clarify expectations for hierarchy argument.
* [CLJ-1304](http://dev.clojure.org/jira/browse/CLJ-1304)
  Fix minor typos in documentation and comments
* [CLJ-1302](http://dev.clojure.org/jira/browse/CLJ-1302)
  Mention that keys and vals order are consistent with seq order

### 3.4 Performance

* [CLJ-858](http://dev.clojure.org/jira/browse/CLJ-858)
  Improve speed of STM by removing System.currentTimeMillis.
* [CLJ-669](http://dev.clojure.org/jira/browse/CLJ-669)
  clojure.java.io/do-copy: use java.nio for Files
* [commit](https://github.com/clojure/clojure/commit/0b73494c3c855e54b1da591eeb687f24f608f346)
  Reduce overhead of protocol callsites by removing unneeded generated
  cache fields.

### 3.5 Other enhancements

* [CLJ-908](http://dev.clojure.org/jira/browse/CLJ-908)
  Make *default-data-reader-fn* set!-able in REPL, similar to *data-readers*.
* [CLJ-783](http://dev.clojure.org/jira/browse/CLJ-783)
  Make clojure.inspector/inspect-tree work on sets.
* [CLJ-896](http://dev.clojure.org/jira/browse/CLJ-896)
  Make browse-url aware of xdg-open.
* [CLJ-1160](http://dev.clojure.org/jira/browse/CLJ-1160)
  Fix clojure.core.reducers/mapcat does not stop on reduced? values.
* [CLJ-1121](http://dev.clojure.org/jira/browse/CLJ-1121)
  -> and ->> have been rewritten to work with a broader set of macros.
* [CLJ-1105](http://dev.clojure.org/jira/browse/CLJ-1105)
  clojure.walk now supports records.
* [CLJ-949](http://dev.clojure.org/jira/browse/CLJ-949)
  Removed all unnecessary cases of sneakyThrow.
* [CLJ-1238](http://dev.clojure.org/jira/browse/CLJ-1238)
  Allow EdnReader to read foo// (matches LispReader behavior).
* [CLJ-1264](http://dev.clojure.org/jira/browse/CLJ-1264)
  Remove uses of _ as a var in the Java code (causes warning in Java 8).
* [CLJ-394](http://dev.clojure.org/jira/browse/CLJ-394)
  Add record? predicate.
* [CLJ-1200](http://dev.clojure.org/jira/browse/CLJ-1200)
  ArraySeq dead code cleanup, ArraySeq_short support added.
* [CLJ-1331](http://dev.clojure.org/jira/browse/CLJ-1331)
  Primitive vectors should implement hasheq and use new hash algorithm

## 4 Bug Fixes

* [CLJ-1018](http://dev.clojure.org/jira/browse/CLJ-1018)
  Make range consistently return infinite sequence of start with a step of 0.
* [CLJ-863](http://dev.clojure.org/jira/browse/CLJ-863)
  Make interleave return () on 0 args and identity on 1 args.
* [CLJ-1072](http://dev.clojure.org/jira/browse/CLJ-1072)
  Update internal usages of the old metadata reader syntax to new syntax.
* [CLJ-1193](http://dev.clojure.org/jira/browse/CLJ-1193)
  Make bigint and biginteger functions work on double values outside long range.
* [CLJ-1154](http://dev.clojure.org/jira/browse/CLJ-1154)
  Make Compile.java flush but not close stdout so errors can be reported.
* [CLJ-1161](http://dev.clojure.org/jira/browse/CLJ-1161)
  Remove bad version.properties from sources jar.
* [CLJ-1175](http://dev.clojure.org/jira/browse/CLJ-1175)
  Fix invalid behavior of Delay/deref if an exception is thrown - exception will
  now be rethrown on subsequent calls and not enter a corrupted state.
* [CLJ-1171](http://dev.clojure.org/jira/browse/CLJ-1171)
  Fix several issues with instance? to make it consistent when used with apply.
* [CLJ-1202](http://dev.clojure.org/jira/browse/CLJ-1202)
  Protocol fns with dashes may get incorrectly compiled into field accesses.
* [CLJ-850](http://dev.clojure.org/jira/browse/CLJ-850)
  Add check to emit invokePrim with return type of double or long if type-hinted.
* [CLJ-1177](http://dev.clojure.org/jira/browse/CLJ-1177)
  clojure.java.io URL to File coercion corrupts path containing UTF-8 characters.
* [CLJ-1234](http://dev.clojure.org/jira/browse/CLJ-1234)
  Accept whitespace in Record and Type reader forms (similar to data literals).
* [CLJ-1233](http://dev.clojure.org/jira/browse/CLJ-1233)
  Allow ** as a valid symbol name without triggering dynamic warnings.
* [CLJ-1246](http://dev.clojure.org/jira/browse/CLJ-1246)
  Add support to clojure.reflect for classes with annotations.
  * [CLJ-1184](http://dev.clojure.org/jira/browse/CLJ-1184)
  Evaling #{do ...} or [do ...] is treated as do special form.
* [CLJ-1090](http://dev.clojure.org/jira/browse/CLJ-1090)
  Indirect function calls through Var instances fail to clear locals.
* [CLJ-1076](http://dev.clojure.org/jira/browse/CLJ-1076)
  pprint tests fail on Windows, expecting \n.
* [CLJ-766](http://dev.clojure.org/jira/browse/CLJ-766)
  Make into-array work consistently with short-array and byte-array on
  bigger types.
* [CLJ-1285](http://dev.clojure.org/jira/browse/CLJ-1285)
  Data structure invariants are violated after persistent operations when
  collision node created by transients.
* [CLJ-1222](http://dev.clojure.org/jira/browse/CLJ-1222)
  Multiplication overflow issues around Long/MIN_VALUE
* [CLJ-1118](http://dev.clojure.org/jira/browse/CLJ-1118)
  Inconsistent numeric comparison semantics between BigDecimals and other numerics
* [CLJ-1125](http://dev.clojure.org/jira/browse/CLJ-1125)
  Clojure can leak memory in a servlet container when using dynamic
  bindings or STM transactions.
* [CLJ-1082](http://dev.clojure.org/jira/browse/CLJ-1082)
  Subvecs of primitve vectors cannot be reduced
* [CLJ-1301](http://dev.clojure.org/jira/browse/CLJ-1301)
  Case expressions use a mixture of hashCode and hasheq, potentially
  leading to missed case matches when these differ.
* [CLJ-983](http://dev.clojure.org/jira/browse/CLJ-983)
  proxy-super does not restore original binding if call throws exception
* [CLJ-1176](http://dev.clojure.org/jira/browse/CLJ-1176)
  clojure.repl/source errors when *read-eval* bound to :unknown
* [CLJ-935](http://dev.clojure.org/jira/browse/CLJ-935)
  clojure.string/trim uses different definition of whitespace than
  triml and trimr
* [CLJ-1058](http://dev.clojure.org/jira/browse/CLJ-1058)
  StackOverflowError on exception in reducef for PersistentHashMap
  fold
* [CLJ-1328](http://dev.clojure.org/jira/browse/CLJ-1328)
  Fix some tests in the Clojure test suite to make their names unique
  and independent of hashing order
* [CLJ-1339](http://dev.clojure.org/jira/browse/CLJ-1339)
  Empty primitive vectors throw NPE on .equals with non-vector
  sequential types

# Changes to Clojure in Version 1.5.1

* fix for leak caused by ddc65a96fdb1163b

# Changes to Clojure in Version 1.5

## CONTENTS

<pre>
 1 Deprecated and Removed Features
    1.1 Clojure 1.5 reducers library requires Java 6 or later
 2 New and Improved Features
    2.1 Reducers
    2.2 Reader Literals improved
    2.3 clojure.core/set-agent-send-executor!, set-agent-send-off-executor!, and send-via
    2.4 New threading macros
    2.5 Column metadata captured by reader
    2.6 gen-class improvements
    2.7 Support added for marker protocols
    2.8 clojure.pprint/print-table output compatible with Emacs Org mode
    2.9 clojure.string/replace and replace-first handle special characters more predictably
    2.10 Set and map constructor functions allow duplicates
    2.11 More functions preserve metadata
    2.12 New edn reader, improvements to *read-eval*
 3 Performance Enhancements
 4 Improved error messages
 5 Improved documentation strings
 6 Bug Fixes
 7 Binary Compatibility Notes
</pre>

## 1 Deprecated and Removed Features

### 1.1 Clojure 1.5 reducers library requires Java 6 or later

The new reducers library (see below) requires Java 6 plus a ForkJoin
library, or Java 7 or later.  Clojure 1.5 can still be compiled and
run with Java 5.  The only limitations with Java 5 are that the new
reducers library will not work, and building Clojure requires skipping
the test suite (e.g. by using the command "ant jar").


## 2 New and Improved Features

### 2.1 Reducers

Reducers provide a set of high performance functions for working with collections. The actual fold/reduce algorithms are specified via the collection being reduced. This allows each collection to define the most efficient way to reduce its contents.

The implementation details of reducers are available at the  [Clojure blog](http://clojure.com/blog/2012/05/08/reducers-a-library-and-model-for-collection-processing.html) and therefore won't be repeated in these change notes. However, as a summary:

* There is a new namespace: clojure.core.reducers
* It contains new versions of map, filter etc based upon transforming reducing functions - reducers
* It contains a new function, fold, which is a parallel reduce+combine
fold uses fork/join when working with (the existing!) Clojure vectors and maps
* Your new parallel code has exactly the same shape as your existing seq-based code
* The reducers are composable
* Reducer implementations are primarily functional - no iterators
* The model uses regular data structures, not 'parallel collections' or other OO malarkey
* It's fast, and can become faster still
* This is work-in-progress

Examples:

	user=> (require '[clojure.core.reducers :as r])
	user=> (reduce + (r/filter even? (r/map inc [1 1 1 2])))
	;=> 6


	;;red is a reducer awaiting a collection
	user=> (def red (comp (r/filter even?) (r/map inc)))
	user=> (reduce + (red [1 1 1 2]))
	;=> 6

	user=> (into #{} (r/filter even? (r/map inc [1 1 1 2])))
	;=> #{2}

### 2.2 Reader Literals improved

* [CLJ-1034](http://dev.clojure.org/jira/browse/CLJ-1034)
  "Conflicting data-reader mapping" should no longer be thrown where there really isn't a conflict. Until this patch, having data_readers.clj on the classpath twice would cause the above exception.

* [CLJ-927](http://dev.clojure.org/jira/browse/CLJ-927)
  Added `*default-data-reader-fn*` to clojure.core. When no data reader is found for a tag and `*default-data-reader-fn*`is non-nil, it will be called with two arguments, the tag and the value.  If `*default-data-reader-fn*` is nil (the default), an exception will be thrown for the unknown tag.

### 2.3 clojure.core/set-agent-send-executor!, set-agent-send-off-executor!, and send-via

Added two new functions:

* clojure.core/set-agent-send-executor!

  Allows the user to set the `java.util.concurrent.Executor` used when calling `clojure.core/send`. Defaults to a fixed thread pool of size: (numCores + 2)

* clojure.core/set-agent-send-off-executor!

 	Allows the user to set the `java.util.concurrent.Executor` used when calling `clojure.core/send-off`. Defaults to a cached thread pool.

* clojure.core/send-via

	Like `send`, and `send-off`, except the first argument to this function is an executor to use when sending.




### 2.4 New threading macros

* clojure.core/cond-> [expr & clauses]

	Takes an expression and a set of test/form pairs. Threads the expression (via ->) through each form for which the corresponding test expression (not threaded) is true.

Example:

	user=> (cond-> 1
				   true inc
	               false (* 42)
	               (= 2 2) (* 3))
	6

* clojure.core/cond->> [expr & clauses]

	Takes an expression and a set of test/form pairs. Threads expr (via ->>)
  through each form for which the corresponding test expression (not threaded) is true.

Example:

	user=> (def d [0 1 2 3])
	#'user/d
	user=> (cond->> d
				    true (map inc)
					(seq? d) (map dec)
					(= (count d) 4) (reduce +)) ;; no threading in the test expr
					                            ;; so d must be passed in explicitly
	10


* clojure.core/as-> [expr name & forms]

Binds name to expr, evaluates the first form in the lexical context of that binding, then binds name to that result, repeating for each successive form

Note: this form does not actually perform any threading. Instead it allows the user to assign a name and lexical context to a value created by a parent threading form.

Example:

	user=> (-> 84
	    	   (/ 4)
	    	   (as-> twenty-one          ;; uses the value from ->
	           		  (* 2 twenty-one)))  ;; no threading here
	42

* clojure.core/some-> [expr & forms]


When expr is not nil, threads it into the first form (via ->),
 and when that result is not nil, through the next etc.

Example:

	user=> (defn die [x] (assert false))
	#'user/die
	user=> (-> 1 inc range next next next die)
	AssertionError Assert failed: false  user/die (NO_SOURCE_FILE:65)
	user=> (some-> 1 inc range next next next die)
	nil



* clojure.core/some->> [expr & forms]

  When expr is not nil, threads it into the first form (via ->>),
  and when that result is not nil, through the next etc.

  Same as some-> except the value is threaded as the last argument in each form.

### 2.5 Column metadata captured by reader

* [CLJ-960](http://dev.clojure.org/jira/browse/CLJ-960)
  Data read by the clojure reader is now tagged with :column in addition to :line.


### 2.6 gen-class improvements

* [CLJ-745](http://dev.clojure.org/jira/browse/CLJ-745)
  It is now possible to expose protected final methods via `:exposes-methods` in `gen-class`. This allows Clojure classes created via gen-class to access protected methods of its parent class.

Example:

	(gen-class :name clojure.test_clojure.genclass.examples.ProtectedFinalTester
    	       :extends java.lang.ClassLoader
        	   :main false
           	   :prefix "pf-"
           	   :exposes-methods {findSystemClass superFindSystemClass})

* [CLJ-948](http://dev.clojure.org/jira/browse/CLJ-948)
  It is now possible to annotate constructors via `gen-class`.

Example:

	(gen-class :name foo.Bar
    	       :extends clojure.lang.Box
        	   :constructors {^{Deprecated true} [Object] [Object]}
           	   :init init
           	   :prefix "foo")

### 2.7 Support added for marker protocols

* [CLJ-966](http://dev.clojure.org/jira/browse/CLJ-966)
  `defprotocol` no longer requires that at least one method be given in the definition of the protocol. This allows for marker protocols, whose sole reason of existence is to allow `satisfies?` to be true for a given type.


Example:

	user=> (defprotocol P (hi [_]))
    P
    user=> (defprotocol M) ; marker protocol
    M
    user=> (deftype T [a] M P (hi [_] "hi there"))
    user.T
    user=> (satisfies? P (T. 1))
    true
    user=> (satisfies? M (T. 1))
    true
    user=> (hi (T. 1))
    "hi there"
    user=> (defprotocol M2 "marker for 2") ; marker protocol again
    M2
    user=> (extend-type T M2)
    nil
    user=> (satisfies? M2 (T. 1))
    true


### 2.8 clojure.pprint/print-table output compatible with Emacs Org mode

For the convenience of those that use Emacs Org mode,
`clojure.pprint/print-table` now prints tables in the form used by
that mode.  Emacs Org mode has features to make it easy to edit such
tables, and even to do spreadsheet-like calculations on their
contents.  See the [Org mode documentation on
tables](http://orgmode.org/manual/Tables.html) for details.

    user=> (clojure.pprint/print-table [:name :initial-impression]
               [{:name "Rich" :initial-impression "rock star"}
                {:name "Andy" :initial-impression "engineer"}])
    | :name | :initial-impression |
    |-------+---------------------|
    |  Rich |           rock star |
    |  Andy |            engineer |


### 2.9 clojure.string/replace and replace-first handle special characters more predictably

`clojure.string/replace` and `clojure.string/replace-first` are now
consistent in the way that they handle the replacement strings: all
characters in the replacement strings are treated literally, including
backslash and dollar sign characters.

    user=> (require '[clojure.string :as s])

    user=> (s/replace-first "munge.this" "." "$")
    ;=> "munge$this"

    user=> (s/replace "/my/home/dir" #"/" (fn [s] "\\"))
    ;=> "\\my\\home\\dir"

There is one exception, which is described in the doc strings.  If you
call these functions with a regex to search for and a string as the
replacement, then dollar sign and backslash characters in the
replacement string are treated specially.  Occurrences of `$1` in the
replacement string are replaced with the string that matched the first
parenthesized subexpression of the regex, occurrences of `$2` are
replaced with the match of the second parenthesized subexpression,
etc.

    user=> (s/replace "x12, b4" #"([a-z]+)([0-9]+)" "$1 <- $2")
    ;=> "x <- 12, b <- 4"

Individual occurrences of `$` or `\` in the replacement string that
you wish to be treated literally can be escaped by prefixing them with
a `\`.  If you wish your replacement string to be treated literally
and its contents are unknown to you at compile time (or you don't wish
to tarnish your constant string with lots of backslashes), you can use
the new function `clojure.string/re-quote-replacement` to do the
necessary escaping of special characters for you.

    user=> (s/replace "x12, b4" #"([a-z]+)([0-9]+)"
                         (s/re-quote-replacement "$1 <- $2"))
    ;=> "$1 <- $2, $1 <- $2"


### 2.10 Set and map constructor functions allow duplicates

All of the functions that construct sets such as `set` and
`sorted-set` allow duplicate elements to appear in their arguments,
and they are documented to treat this case as if by repeated uses of
`conj`.

Similarly, all map constructor functions such as `hash-map`,
`array-map`, and `sorted-map` allow duplicate keys, and are documented
to treat this case as if by repeated uses of `assoc`.

As before, literal sets, e.g. `#{1 2 3}`, do not allow duplicate
elements, and while elements can be expressions evaluated at run time
such as `#{(inc x) (dec y)}`, this leads to a check for duplicates at
run time whenever the set needs to be constructed, throwing an
exception if any duplicates are found.

Similarly, literal maps do not allow duplicate keys.  New to Clojure
1.5 is a performance optimization: if all keys are compile time
constants but one or more values are expressions requiring evaluation
at run time, duplicate keys are checked for once at compile time only,
not each time a map is constructed at run time.

* [CLJ-1065](http://dev.clojure.org/jira/browse/CLJ-1065)
  Allow duplicate set elements and map keys for all set and map constructors


### 2.11 More functions preserve metadata

Most functions that take a collection and return a "modified" version
of that collection preserve the metadata that was on the input
collection, e.g. `conj`, `assoc`, `dissoc`, etc.  One notable
exception was `into`, which would return a collection with metadata
`nil` for several common types of input collections.

Now the functions `into`, `select-keys`, `clojure.set/project`, and
`clojure.set/rename` return collections with the same metadata as
their input collections.

### 2.12 New edn reader, improvements to `*read-eval*`

The new `clojure.edn` namespace reads edn (http://edn-format.org) data,
and should be used for reading data from untrusted sources.

Clojure's core read* functions can evaluate code, and should not be
used to read data from untrusted sources. As of 1.5, `*read-eval*`
supports a documented set of thread-local bindings, see the doc string
for details.

`*read-eval*`'s default can be set to false by setting a system property:

    -Dclojure.read.eval=false

## 3 Performance Enhancements

* [CLJ-988](http://dev.clojure.org/jira/browse/CLJ-988)
  Multimethod tables are now protected by a read/write lock instead of a synchronized method. This should result in a performance boost for multithreaded code using multimethods.
* [CLJ-1061](http://dev.clojure.org/jira/browse/CLJ-1061)
  `when-first` now evaluates its expression only once.
* [CLJ-1084](http://dev.clojure.org/jira/browse/CLJ-1084)
  `PersistentVector$ChunkedSeq` now implements `Counted` interface, to avoid some cases where vector elements were being counted by iterating over their elements.
* [CLJ-867](http://dev.clojure.org/jira/browse/CLJ-867)
  Records with same fields and field values, but different types, now usually hash to different values.
* [CLJ-1000](http://dev.clojure.org/jira/browse/CLJ-1000)
  Cache hasheq() for seqs, sets, vectors, maps and queues
* (no ticket) array-map perf tweaks
* [CLJ-1111](http://dev.clojure.org/jira/browse/CLJ-1111)
  Allows loop to evaluate to primitive values


## 4 Improved error messages

* [CLJ-103](http://dev.clojure.org/jira/browse/CLJ-103)
  Improved if-let error message when form has a improperly defined body.
* [CLJ-897](http://dev.clojure.org/jira/browse/CLJ-897)
  Don't use destructuring in defrecord/deftype arglists to get a slightly better error message when forgetting to specify the fields vector
* [CLJ-788](http://dev.clojure.org/jira/browse/CLJ-788)
  Add source and line members and getters to CompilerException
* [CLJ-157](http://dev.clojure.org/jira/browse/CLJ-157)
  Better error messages for syntax errors w/ defn and fn
* [CLJ-940](http://dev.clojure.org/jira/browse/CLJ-940)
  Passing a non-sequence to refer :only results in uninformative exception
* [CLJ-1052](http://dev.clojure.org/jira/browse/CLJ-1052)
  `assoc` now throws an exception if the last key argument is missing a value.


## 5 Improved documentation strings

* [CLJ-893](http://dev.clojure.org/jira/browse/CLJ-893)
  Document that vec will alias Java arrays
* [CLJ-892](http://dev.clojure.org/jira/browse/CLJ-892)
  Clarify doc strings of sort and sort-by: they will modify Java array arguments
* [CLJ-1019](http://dev.clojure.org/jira/browse/CLJ-1019)
  ns-resolve doc has a typo
* [CLJ-1038](http://dev.clojure.org/jira/browse/CLJ-1038)
  Docstring for deliver doesn't match behavior
* [CLJ-1055](http://dev.clojure.org/jira/browse/CLJ-1055)
  "be come" should be "become"
* [CLJ-917](http://dev.clojure.org/jira/browse/CLJ-917)
  clojure.core/definterface is not included in the API docs
* (no ticket) clojure.core/read, read-string, and *read-eval* all have more extensive documentation.


## 6 Bug Fixes

* [CLJ-962](http://dev.clojure.org/jira/browse/CLJ-962)
  Vectors returned by subvec allow access at negative indices
* [CLJ-952](http://dev.clojure.org/jira/browse/CLJ-952)
  bigdec does not properly convert a clojure.lang.BigInt
* [CLJ-975](http://dev.clojure.org/jira/browse/CLJ-975)
  inconsistent destructuring behaviour when using nested maps
* [CLJ-954](http://dev.clojure.org/jira/browse/CLJ-954)
  TAP support in clojure.test.tap Needs Updating
* [CLJ-881](http://dev.clojure.org/jira/browse/CLJ-881)
  exception when cl-format is given some ~f directive/value combinations
* [CLJ-763](http://dev.clojure.org/jira/browse/CLJ-763)
  Do not check for duplicates in destructuring map creation
* [CLJ-667](http://dev.clojure.org/jira/browse/CLJ-667)
  Allow loops fully nested in catch/finally
* [CLJ-768](http://dev.clojure.org/jira/browse/CLJ-768)
  cl-format bug in ~f formatting
* [CLJ-844](http://dev.clojure.org/jira/browse/CLJ-844)
  NPE calling keyword on map from bean
* [CLJ-934](http://dev.clojure.org/jira/browse/CLJ-934)
  disj! Throws exception when attempting to remove multiple items in one call
* [CLJ-943](http://dev.clojure.org/jira/browse/CLJ-943)
  When load-lib fails, a namespace is still created
* [CLJ-981](http://dev.clojure.org/jira/browse/CLJ-981)
  clojure.set/rename-keys deletes keys when there's a collision
* [CLJ-961](http://dev.clojure.org/jira/browse/CLJ-961)
  with-redefs loses a Var's root binding if the Var is thread-bound
* [CLJ-1032](http://dev.clojure.org/jira/browse/CLJ-1032)
  seque leaks threads from the send-off pool
* [CLJ-1041](http://dev.clojure.org/jira/browse/CLJ-1041)
  reduce-kv on sorted maps should stop on seeing a Reduced value
* [CLJ-1011](http://dev.clojure.org/jira/browse/CLJ-1011)
  clojure.data/diff should cope with null and false values in maps
* [CLJ-977](http://dev.clojure.org/jira/browse/CLJ-977)
  (int \a) returns a value, (long \a) throws an exception
* [CLJ-964](http://dev.clojure.org/jira/browse/CLJ-964)
  test-clojure/rt.clj has undeclared dependency on clojure.set
* [CLJ-923](http://dev.clojure.org/jira/browse/CLJ-923)
  Reading ratios prefixed by + is not working
* [CLJ-1012](http://dev.clojure.org/jira/browse/CLJ-1012)
  partial function should also accept 1 arg (just f)
* [CLJ-932](http://dev.clojure.org/jira/browse/CLJ-932)
  contains? Should throw exception on non-keyed collections
* [CLJ-730](http://dev.clojure.org/jira/browse/CLJ-730) Create test suite for functional fns (e.g. juxt, comp, partial, etc.)
* [CLJ-757](http://dev.clojure.org/jira/browse/CLJ-757)
  Empty transient maps/sets return wrong value for .contains
* [CLJ-828](http://dev.clojure.org/jira/browse/CLJ-828)
  clojure.core/bases returns a cons when passed a class and a Java array when passed an interface
* [CLJ-1062](http://dev.clojure.org/jira/browse/CLJ-1062)
  CLJ-940 breaks compilation of namespaces that don't have any public functions
* [CLJ-1070](http://dev.clojure.org/jira/browse/CLJ-1070)
  PersistentQueue's hash function does not match its equality
* [CLJ-987](http://dev.clojure.org/jira/browse/CLJ-987)
  pprint doesn't flush the underlying stream
* [CLJ-963](http://dev.clojure.org/jira/browse/CLJ-963)
  Support pretty printing namespace declarations under code-dispatch
* [CLJ-902](http://dev.clojure.org/jira/browse/CLJ-902)
  doc macro broken for namespaces
* [CLJ-909](http://dev.clojure.org/jira/browse/CLJ-909) Make LineNumberingPushbackReader's buffer size configurable
* [CLJ-910](http://dev.clojure.org/jira/browse/CLJ-910) Allow for type-hinting the method receiver in memfn
* [CLJ-1048](http://dev.clojure.org/jira/browse/CLJ-1048) add test.generative to Clojure's tests
* [CLJ-1071](http://dev.clojure.org/jira/browse/CLJ-1071) ExceptionInfo does no abstraction
* [CLJ-1085](http://dev.clojure.org/jira/browse/CLJ-1085) clojure.main/repl unconditionally refers REPL utilities into `*ns*`
* (no ticket) Rich Hickey fix: syntax-quote was walking records, returning maps
* [CLJ-1116](http://dev.clojure.org/jira/browse/CLJ-1116) More REPL-friendly 'ns macro
* (no ticket) Rich Hickey fix: deref any j.u.c.Future
* [CLJ-1092](http://dev.clojure.org/jira/browse/CLJ-1092) New function re-quote-replacement has incorrect :added metadata
* [CLJ-1098](http://dev.clojure.org/jira/browse/CLJ-1098) Implement IKVReduce and CollFold for nil
* (no ticket) Rich Hickey fix: impose once semantics on fabricated closures for e.g. loops
* [CLJ-1140](http://dev.clojure.org/jira/browse/CLJ-1140) Restore {:as x} destructuring for empty lists
* [CLJ-1150](http://dev.clojure.org/jira/browse/CLJ-1150) Make some PersistentVector's and APersistentVector.SubVector's internals public
* (no ticket) Rich Hickey fix: use non-loading classForName
* [CLJ-1106](http://dev.clojure.org/jira/browse/CLJ-1106) Fixing set equality

## 7 Binary Compatibility Notes

* `public static inner class LispReader.ReaderException(int line, Throwable cause)`
  Constructor changed to `ReaderException(int line, int column, Throwable cause)`
* `public Object clojure.lang.Agent.dispatch(IFn fn, ISeq args, boolean solo)`
  Replaced with `dispatch(IFn fn, ISeq args, Executor exec)`

# Changes to Clojure in Version 1.4

## CONTENTS

<pre>
 1 Deprecated and Removed Features
    1.1 Fields that Start With a Dash Can No Longer Be Accessed Using Dot Syntax
 2 New/Improved Features
    2.1 Reader Literals
    2.2 clojure.core/mapv
    2.3 clojure.core/filterv
    2.4 clojure.core/ex-info and clojure.core/ex-data
    2.5 clojure.core/reduce-kv
    2.6 clojure.core/contains? Improved
    2.7 clojure.core/min and clojure.core/max prefer NaN
    2.8 clojure.java.io/as-file and clojure.java.io/as-url Handle URL-Escaping Better
    2.9 New Dot Syntax for Record and Type Field Access
    2.10 Record Factory Methods Available Inside defrecord
    2.11 assert-args Displays Namespace and Line Number on Errors
    2.12 File and Line Number Added to Earmuff Dynamic Warning
    2.13 require Can Take a :refer Option
    2.14 *compiler-options* Var
    2.15 Improved Reporting of Invalid Characters in Unicode String Literals
    2.16 clojure.core/hash No Longer Relies on .hashCode
    2.17 Java 7 Documentation
    2.18 loadLibrary Loads Library Using System ClassLoader
    2.19 Java int is boxed as java.lang.Integer
 3 Performance Enhancements
 4 Bug Fixes
</pre>

## 1 Deprecated and Removed Features

### 1.1 Record and Type Fields that Start With a Dash Can No Longer Be Accessed Using Dot Syntax

Clojure 1.4 introduces a field accessor syntax for the dot special form that aligns Clojure field lookup syntax with ClojureScript's.

For example, in Clojure 1.3, one can declare a record with a field starting with dash and access it like this:

    (defrecord Bar [-a]) ;=> user.Bar
    (.-a (Bar. 10)) ;=> 10

In 1.4, the above code results in `IllegalArgumentException No matching field found: a for class user.Bar`

However, the field may still be accessed as a keyword:

    (:-a (Bar. 10)) ;=> 10

## 2 New and Improved Features

### 2.1 Reader Literals

Clojure 1.4 supports reader literals, which are data structures tagged
by a symbol to denote how they will be read.

When Clojure starts, it searches for files named `data_readers.clj`
at the root of the classpath. Each such file must contain a Clojure
map of symbols, like this:

    {foo/bar my.project.foo/bar
     foo/baz my.project/baz}

The key in each pair is a tag that will be recognized by
the Clojure reader. The value in the pair is the
fully-qualified name of a Var which will be invoked by the reader to
parse the form following the tag. For example, given the
data_readers.clj file above, the Clojure reader would parse this
form:

    #foo/bar [1 2 3]

by invoking the Var `#'my.project.foo/bar` on the vector `[1 2 3]`. The
data reader function is invoked on the form AFTER it has been read
as a normal Clojure data structure by the reader.

Reader tags without namespace qualifiers are reserved for Clojure. Default
reader tags are defined in `clojure.core/default-data-readers` but may be
overridden in `data_readers.clj` or by rebinding `*data-readers*`.

#### 2.1.1 Instant Literals

Clojure supports literals for instants in the form
`#inst "yyyy-mm-ddThh:mm:ss.fff+hh:mm"`. These literals are parsed as `java.util.Date`s
by default. They can be parsed as `java.util.Calendar`s or `java.util.Timestamp`s
by binding `*data-readers*` to use `clojure.instant/read-instant-calendar` or
`clojure.instant/read-instant-timestamp`.

    (def instant "#inst \"@2010-11-12T13:14:15.666\"")

    ; Instants are read as java.util.Date by default
    (= java.util.Date (class (read-string instant)))
    ;=> true

    ; Instants can be read as java.util.Calendar or java.util.Timestamp

    (binding [*data-readers* {'inst read-instant-calendar}]
      (= java.util.Calendar (class (read-string instant))))
    ;=> true

    (binding [*data-readers* {'inst read-instant-timestamp}]
      (= java.util.Timestamp (class (read-string instant))))
    ;=> true

#### 2.1.2 UUID Literals

Clojure supports literals for UUIDs in the form `#uuid "uuid-string"`. These
literals are parsed as `java.util.UUID`s.

### 2.2 clojure.core/mapv

`mapv` takes a function `f` and one or more collections and returns a
vector consisting of the result of applying `f` to the set of first items of
each collection, followed by applying `f` to the set of second items in each
collection, until any one of the collections is exhausted. Any remaining
items in other collections are ignored. `f` should accept a number of arguments
equal to the number of collections.

    (= [1 2 3] (mapv + [1 2 3]))
    ;=> true

    (= [2 3 4] (mapv + [1 2 3] (repeat 1)))
    ;=> true

### 2.3 clojure.core/filterv

`filterv` takes a predicate `pred` and a collection and returns a vector
of the items in the collection for which `(pred item)` returns true. `pred`
must be free of side-effects.

    (= [] (filterv even? [1 3 5]))
    ;=> true

    (= [2 4] (filter even? [1 2 3 4 5]))
    ;=> true

### 2.4 clojure.core/ex-info and clojure.core/ex-data

`ex-info` creates an instance of `ExceptionInfo`. `ExceptionInfo` is a
`RuntimeException` subclass that takes a string `msg` and a map of data.

    (ex-info "Invalid use of robots" {:robots false})
    ;=> #<ExceptionInfo clojure.lang.ExceptionInfo: Invalid use of robots {:robots false}>

`ex-data` is called with an exception and will retrieve that map of data
if the exception is an instance of `ExceptionInfo`.

    (ex-data (ex-info "Invalid use of robots" {:robots false}))
    ;=> {:robots false}

### 2.5 clojure.core/reduce-kv

`reduce-kv` reduces an associative collection. It takes a function `f`,
an initial value `init` and an associative collection `coll`. `f` should
be a function of 3 arguments. Returns the result of applying `f` to `init`,
the first key and the first value in `coll`, then applying `f` to that result
and the 2nd key and value, etc. If `coll` contains no entries, returns `init`
and f is not called. Note that `reduce-kv` is supported on vectors,
where the keys will be the ordinals.

    (reduce-kv str "Hello " {:w \o :r \l :d \!})
    ;=> "Hello :rl:d!:wo"
    (reduce-kv str "Hello " [\w \o \r \l \d \!])
    ;=> "Hello 0w1o2r3l4d5!"

### 2.6 clojure.core/contains? Improved

`contains?` now works with `java.util.Set`.

### 2.7 clojure.core/min and clojure.core/max prefer NaN

`min` and `max` now give preference to returning NaN if either of their
arguments is NaN.

### 2.8 clojure.java.io/as-file and clojure.java.io/as-url Handle URL-Escaping Better

`as-file` and `as-url` now handle URL-escaping in both directions.

### 2.9 New Dot Syntax for Record and Type Field Access

Clojure 1.4 introduces a field accessor syntax for the dot special
form that aligns Clojure field lookup syntax with ClojureScript's.

In 1.4, to declare a record type and access its property `x`, one can
write:

    (defrecord Foo [x]) ;=> user.Foo
    (.-x (Foo. 10)) ;=> 10

This addition makes it easier to write code that will run as expected
in both Clojure and ClojureScript.

### 2.10 Record Factory Methods Available Inside defrecord

Prior to 1.4, you could not use the factory functions (`->RecordClass`
and `map->RecordClass`) to construct a new record from inside a
`defrecord` definition.

The following example did not work prior to 1.4, but is now
valid. This example makes use of `->Mean` which would have not yet
been available.

    (defrecord Mean [last-winner]
      Player
      (choose [_] (if last-winner last-winner (random-choice)))
      (update-strategy [_ me you] (->Mean (when (iwon? me you) me))))

### 2.11 assert-args Displays Namespace and Line Number on Errors

`assert-args` now uses &form to report the namespace and line number where
macro syntax errors occur.

### 2.12 File and Line Number Added to Earmuff Dynamic Warning

When a variable is defined using earmuffs but is not declared dynamic,
Clojure emits a warning. That warning now includes the file and line
number.

### 2.13 require Can Take a :refer Option

`require` can now take a `:refer` option. `:refer` takes a list of symbols
to refer from the namespace or `:all` to bring in all public vars.

### 2.14 \*compiler-options\* Var

The dynamic var `*compiler-options*` contains a map of options to send
to the Clojure compiler.

Supported options:

* `:elide-meta`: Have certain metadata elided during compilation. This
should be set to a collection of keywords.
* `:disable-locals-clearing`: Set to true to disable clearing. Useful for
using a debugger.

The main function of the Clojure compiler sets the
`*compiler-options*` from properties prefixed by `clojure.compiler`,
e.g.

    java -Dclojure.compiler.elide-meta='[:doc :file :line]'

### 2.15 Improved Reporting of Invalid Characters in Unicode String Literals

When the reader finds an invalid character in a Unicode string literal, it
now reports the character instead of its numerical representation.

### 2.16 clojure.core/hash No Longer Relies on .hashCode

`hash` no longer directly uses .hashCode() to return the hash of a Clojure
data structure. It calls `clojure.lang.Util.hasheq`, which has its own implementation
for Integer, Short, Byte, and Clojure collections. This ensures that the hash code
returned is consistent with `=`.

### 2.17 Java 7 Documentation

`*core-java-api*` will now return the URL for the Java 7 Javadoc when you are
running Java 7.

### 2.18 loadLibrary Loads Library Using System ClassLoader

A static method, `loadLibrary`, was added to `clojure.lang.RT` to load a
library using the system ClassLoader instead of Clojure's class loader.

### 2.19 Java int is Boxed As java.lang.Integer

Java `int`s are now boxed as `java.lang.Integer`s. See
[the discussion on clojure-dev](https://groups.google.com/forum/#!msg/clojure/7-hARL5c1lI/ntnnOweEGfUJ)
for more information.

## 3 Performance Enhancements

* `(= char char)` is now optimized
* `equiv` is inlined in variadic =
* `toString` cached on keywords and symbols

## 4 Bug Fixes

* [CLJ-829](http://dev.clojure.org/jira/browse/CLJ-829)
  Transient hashmaps mishandle hash collisions
* [CLJ-773](http://dev.clojure.org/jira/browse/CLJ-773)
  Macros that are expanded away still have their vars referenced in the emitted byte code
* [CLJ-837](http://dev.clojure.org/jira/browse/CLJ-837)
  java.lang.VerifyError when compiling deftype or defrecord with argument name starting with double underscore characters
* [CLJ-369](http://dev.clojure.org/jira/browse/CLJ-369)
  Check for invalid interface method names
* [CLJ-845](http://dev.clojure.org/jira/browse/CLJ-845)
  Unexpected interaction between protocol extension and namespaced method keyword/symbols
  * Ignoring namespace portion of symbols used to name methods in extend-type and extend-protocol
* [CLJ-852](http://dev.clojure.org/jira/browse/CLJ-852)
  IllegalArgumentException thrown when defining a var whose value is calculated with a primitive fn
* [CLJ-855](http://dev.clojure.org/jira/browse/CLJ-855)
  catch receives a RuntimeException rather than the expected checked exception
* [CLJ-876](http://dev.clojure.org/jira/browse/CLJ-876)
  #^:dynamic vars declared in a nested form are not immediately dynamic
* [CLJ-886](http://dev.clojure.org/jira/browse/CLJ-886)
  java.io/do-copy can garble multibyte characters
* [CLJ-895](http://dev.clojure.org/jira/browse/CLJ-895)
  Collection.toArray implementations do not conform to Java API docs
  * obey contract for toArray return type
* [CLJ-898](http://dev.clojure.org/jira/browse/CLJ-898)
  Agent sends consume heap
  * Only capture a shallow copy of the current Frame in binding-conveyor-fn, so that sends in agent actions don't build infinite Frame stacks
* [CLJ-928](http://dev.clojure.org/jira/browse/CLJ-928)
  Instant literal for Date and Timestamp should print in UTC
* [CLJ-931](http://dev.clojure.org/jira/browse/CLJ-933)
  Syntactically broken clojure.test/are tests succeed
* [CLJ-933](http://dev.clojure.org/jira/browse/CLJ-933)
  Compiler warning on clojure.test-clojure.require-scratch

# Changes to Clojure in Version 1.3

## CONTENTS
<pre>
 1 Deprecated and Removed Features
    1.1 Earmuffed Vars are No Longer Automatically Considered Dynamic
    1.2 ISeq No Longer Inherits from Sequential
    1.3 Removed Bit Operation Support for Boxed Numbers
    1.4 Ancillary Namespaces No Longer Auto-Load on Startup
    1.5 Replicate Deprecated
 2 New/Improved Features
    2.1 Enhanced Primitive Support
    2.2 defrecord and deftype Improvements
    2.3 Better Exception Reporting
    2.4 clojure.reflect/reflect
    2.5 clojure.data/diff
    2.6 clojure.core/every-pred and clojure.core/some-fn Combinators
    2.7 clojure.core/realized?
    2.8 clojure.core/with-redefs-fn & with-redefs
    2.9 clojure.core/find-keyword
    2.10 clojure.repl/pst
    2.11 clojure.pprint/print-table
    2.12 pprint respects *print-length*
    2.13 compilation and deployment via Maven
    2.14 internal keyword map uses weak refs
    2.15 ^:const defs
    2.16 Message Bearing Assert
    2.17 Error Checking for defmulti Options
    2.18 Removed Checked Exceptions
    2.19 vector-of Takes Multiple Arguments
    2.20 deref with timeout
    2.21 Walk Support for sorted-by Collections
    2.22 string.join Enhanced to Work with Sets
    2.23 clojure.test-helper
    2.24 Newline outputs platform-specific newline sequence
    2.25 init-proxy and update-proxy return proxy
    2.26 doc & find-doc moved to REPL
    2.27 clojure.java.shell/sh accepts as input anything that clojure.java.io/copy does
    2.28 InterruptedHandler Promoted to clojure.repl
    2.29 Add support for running -main namespaces from clojure.main
    2.30 Set thread names on agent thread pools
    2.31 Add docstring support to def
    2.32 Comp function returns identity when called with zero arity
    2.33 Type hints can be applied to arg vectors
    2.34 Binding Conveyance
 3 Performance Enhancements
 4 Bug Fixes
 5 Modular Contrib
</pre>

## 1 Deprecated and Removed Features

### 1.1 Earmuffed Vars Are No Longer Automatically Considered Dynamic.

    (def *fred*)
    => Warning: *fred* not declared dynamic and thus is not dynamically rebindable, but its name suggests otherwise. Please either indicate ^:dynamic ** or change the name.

### 1.2 ISeq No Longer Inherits From Sequential

This allows ISeq implementers to be in the map or set equality partition.

### 1.3 Removed Bit Operation Support for Boxed Numbers

Bit Operations map directly to primitive operations

### 1.4 Ancillary Namespaces No Longer Auto-Load on Startup

The following namespaces are no longer loaded on startup: clojure.set, clojure.xml, clojure.zip

### 1.5 Replicate Deprecated

Use repeat instead.

## 2 New/Improved Features

### 2.1 Enhanced Primitive Support

Full details here:

 - [Enhanced Primitive Support][EPS]
 - [Documentation for 1.3 Numerics][NUM]

[EPS]: http://dev.clojure.org/display/doc/Enhanced+Primitive+Support
[NUM]: http://dev.clojure.org/display/doc/Documentation+for+1.3+Numerics

### 2.2 defrecord and deftype Improvements

Details here: [Defrecord Improvements](http://dev.clojure.org/display/design/defrecord+improvements)

### 2.3 Better Exception Reporting

Details here: [Error Handling](http://dev.clojure.org/display/design/Error+Handling)

Additionally:

Better error messages:

 * When calling macros with arity
 * For Invalid Map Literals
 * For alias function if using unknown namespace
 * In the REPL
 * Add "starting at <line>" to EOF while reading exceptions
 * Better compilation error reporting

### 2.4 clojure.reflect/reflect

Full details here: [Reflection API](http://dev.clojure.org/display/design/Reflection+API)

### 2.5 clojure.data/diff

Recursively compares a and b, returning a tuple of [things-only-in-a things-only-in-b things-in-both]

    (diff {:a 1 :b 2} {:a 1 :b 22 :c 3})
    => ({:b 2} {:c 3, :b 22} {:a 1})

### 2.6 clojure.core/every-pred and clojure.core/some-fn Combinators

every-pred takes a set of predicates and returns a function f that returns true if all of its composing predicates return a logical true value against all of its arguments, else it returns false.

    ((every-pred even?) 2 4 6)
    => true

    ((every-pred even?) 2 4 5)
    =>false

some-fn takes a set of predicates and returns a function f that returns the first logical true value  returned by one of its composing predicates against any of its arguments, else it returns logical false.

    ((some-fn even?) 2 4 5)
    => true
    ((some-fn odd?) 2 4 6)
    => false

### 2.7 clojure.core/realized?

Returns true if a value has been produced for a promise, delay, future or lazy sequence.

    (let [x (range 5)]
      (println (realized? x))
      (first x)
      (println (realized? x)))
    => false
    => true

### 2.8 clojure.core/with-redefs-fn & clojure.core/with-redefs

with-redefs-fn temporarily redefines Vars during a call to func. with-redefs temporarily redefines Vars while executing the body.

    (with-redefs [nil? :temp] (println nil?))
    => :temp

### 2.9 clojure.core/find-keyword

Returns a Keyword with the given namespace and name if one already exists.

    (find-keyword "def")
    => :def
    (find-keyword "fred")
    => nil

### 2.10 clojure.repl/pst

Prints a stack trace of the exception


    (pst (IllegalArgumentException.))

    IllegalArgumentException
        user/eval27 (NO_SOURCE_FILE:18)
        clojure.lang.Compiler.eval (Compiler.java:6355)
        clojure.lang.Compiler.eval (Compiler.java:6322)
        clojure.core/eval (core.clj:2699)
        clojure.main/repl/read-eval-print--5906 (main.clj:244)
        clojure.main/repl/fn--5911 (main.clj:265)
        clojure.main/repl (main.clj:265)
        clojure.main/repl-opt (main.clj:331)
        clojure.main/main (main.clj:427)
        clojure.lang.Var.invoke (Var.java:397)
        clojure.lang.Var.applyTo (Var.java:518)
        clojure.main.main (main.java:37)

### 2.11 clojure.pprint/print-table

Prints a collection of maps in a textual table.

    (print-table [:fred :barney]
                 [{:fred "ethel"}
                  {:fred "wilma" :barney "betty"}])

    ===============
    :fred | :barney
    ===============
    ethel |
    wilma | betty
    ===============

### 2.12 pprint respects \*print-length\*

Assigning \*print-length\* now affects output of pprint

### 2.13 compilation and deployment via Maven

See the following pages for more information:

 - [Maven Settings and Repositories][MSR]
 - [Why Maven?][WM]
 - [Common Contrib Build][CCB]
 - [How to Make Releases][HMR]

 [MSR]: http://dev.clojure.org/display/doc/Maven+Settings+and+Repositories
 [WM]: http://dev.clojure.org/pages/viewpage.action?pageId=950842
 [CCB]: http://dev.clojure.org/display/design/Common+Contrib+Build
 [HMR]:http://dev.clojure.org/display/design/How+to+Make+Releases

### 2.14 internal keyword map uses weak refs

### 2.15 ^:const defs

^:const lets you name primitive values with speedier reference.

    (def constants
     {:pi 3.14
      :e 2.71})

    (def ^:const pi (:pi constants))
    (def ^:const e (:e constants))

The overhead of looking up :e and :pi in the map happens at compile time, as (:pi constants) and (:e constants) are evaluated when their parent def forms are evaluated.

### 2.16 Message Bearing Assert

Assert can take a second argument which will be printed when the assert fails

    (assert (= 1 2) "1 is not equal to 2")
    => AssertionError Assert failed: 1 is not equal to 2

### 2.17 Error Checking for defmulti Options

defmulti will check to verify that its options are valid. For example, the following code will throw an exception:

    (defmulti fred :ethel :lucy :ricky)
    => IllegalArgumentException

### 2.18 Removed Checked Exceptions

Clojure does not throw checked exceptions

### 2.19 vector-of Takes Multiple Args

vector-of takes multiple args used to populate the array

    (vector-of :int 1 2 3)
    => [1 2 3]

### 2.20 deref with timeout

deref now takes a timeout option - when given with a blocking reference, will return the timeout-val if the timeout (in milliseconds) is reached before value is available.

    (deref (promise) 10 :ethel)
    => :ethel

### 2.21 Walk Support for sorted-by Collections

Walk modified to work on sorted-by collections

    let [x (sorted-set-by > 1 2 3)] (walk inc reverse x))
    => (2 3 4)

### 2.22 string.join Enhanced to Work with Sets

Just like join works on other collections

    (join " and " #{:fred :ethel :lucy})
    => ":lucy and :fred and :ethel"

### 2.23 clojure.test-helper

All test helpers moved into clojure.test-helper

### 2.24 Newline outputs platform-specific newline sequence

Newline sequence is output as \r\n on Windows now.

### 2.25 init-proxy and update-proxy return proxy

Now you can chain calls on the proxy

### 2.26 doc & find-doc moved to REPL

Adds special form docs to the REPL

### 2.27 clojure.java.shell/sh accepts as input anything that clojure.java.io/copy does

This adds InputStream, Reader, File, byte[] to the list of inputs for clojure.java.shell/sh

### 2.28 Interrupt Handler Promoted to clojure.repl

Promoting this library eliminates the need for a dependency on old contrib.

### 2.29 Add support for running -main namespaces from clojure.main

This patch allows clojure.main to accept an argument pointing to a namespace to look for a -main function in. This allows users to write -main functions that will work the same whether the code is AOT-compiled for use in an executable jar or just run from source.

### 2.30 Set thread names on agent thread pools

It's a best practice to name the threads in an executor thread pool with a custom ThreadFactory so that the purpose of these threads is clear in thread dumps and other runtime operational tools.

Patch causes thread names like:

    clojure-agent-send-pool-%d     (should be fixed # of threads)
    clojure-agent-send-off-pool-%d (will be added and removed over time)

### 2.31 Add docstring support to def

A def can now have a docstring between name and value.

    (def foo "a foo" :foo)

### 2.32 Comp function returns identity when called with zero arity

    (= (comp) identity)
    => true

### 2.33 Type hints can be applied to arg vectors

You can hint different arities separately:

    (defn hinted
      (^String [])
      (^Integer [a])
      (^java.util.List [a & args]))

This is preferred over hinting the function name. Hinting the function name is still allowed for backward compatibility, but will likely be deprecated in a future release.

### 2.34 Binding Conveyance

Clojure APIs that pass work off to other threads (e.g. send, send-off, pmap, future) now convey the dynamic bindings of the calling thread:

    (def ^:dynamic *num* 1)
    (binding [*num* 2] (future (println *num*)))
    ;; prints "2", not "1"

## 3 Performance Enhancements

  * Code path for using vars is now much faster for the common case
  * Improved startup time
  * Fix performance on some numeric overloads
    See [CLJ-380](http://dev.clojure.org/jira/browse/CLJ-5) for more information
  * Promises are lock free
  * Functions only get metadata support code when metadata explicitly supplied
  * definterface/gen-interface accepts array type hints
  * inline nil?
  * inline bit-functions & math ops
  * inline n-ary min & max
  * PersistentQueue count is now O(1)
  * Intrinsics: unchecked math operators now emit bytecodes directly where possible

## 4 Bug Fixes

[Complete list of Tickets for 1.3 Release][ISSUES].

[ISSUES]: http://dev.clojure.org/jira/secure/IssueNavigator.jspa?mode=hide&requestId=10052

 * [CLJ-8](http://dev.clojure.org/jira/browse/CLJ-8)
   detect and report cyclic load dependencies
    * Patch restore detection of cyclic load dependencies

 * [CLJ-31](http://dev.clojure.org/jira/browse/CLJ-31)
   compiler now correctly rejects attempts to recur across try
    (fn [x] (try (recur 1)))
    => CompilerException

 * [CLJ-286](http://dev.clojure.org/jira/browse/CLJ-286)
   \*out\* being used as java.io.PrintWriter
    * Patch fixes using Writer instead of PrintWriter
    * fix clojure.main to not assume that *err* is a PrintWriter

 * [CLJ-292](http://dev.clojure.org/jira/browse/CLJ-292)
   LazySeq.sval() nests RuntimeExceptions
    * Patch causes only the original RuntimeException to be thrown

 * [CLJ-390](http://dev.clojure.org/jira/browse/CLJ-390)
   sends from agent error-handlers should be allowed
    * Patch allows agent error-handler to send successfully

 * [CLJ-426](http://dev.clojure.org/jira/browse/CLJ-426)
   case should handle hash collision
    * There were situations where a hash collision would occur with case and an exception would be thrown. See [discussion](https://groups.google.com/d/topic/clojure/m4ZDWKSfmfo/discussion) for more details

 * [CLJ-430](http://dev.clojure.org/jira/browse/CLJ-430)
   clojure.java.io URL Coercion throws java.lang.ClassCastException
    * Patch correct exception to be thrown

 * [CLJ-432](http://dev.clojure.org/jira/browse/CLJ-432)
   deftype does not work if containing ns contains dashes
    * Patch munges namespaces with dashes properly

 * [CLJ-433](http://dev.clojure.org/jira/browse/CLJ-433)
   munge should not munge $ (which isJavaIdentifierPart), should munge ' (which is not)

 * [CLJ-435](http://dev.clojure.org/jira/browse/CLJ-435)
   stackoverflow exception in printing meta with :type
    * Patch fixes exception being thrown on certain type metadata
      (with-meta {:value 2} {:type Object})
      => No message. [Thrown class java.lang.StackOverflowError]

 * [CLJ-437](http://dev.clojure.org/jira/browse/CLJ-437)
   Bugs in clojure.set/subset? and superset? for sets with false/nil elements
    * Patch fixes failing on subset? and superset? for sets with false/nil elements

 * [CLJ-439](http://dev.clojure.org/jira/browse/CLJ-439)
   Automatic type translation from Integer to Long
    * Patch fixes increase coercion from Integer to Long

 * [CLJ-444](http://dev.clojure.org/jira/browse/CLJ-444)
   Infinite recursion in Keyword.intern leads to stack overflow
    * No more infinite recursion with patch

 * [CLJ-673](http://dev.clojure.org/jira/browse/CLJ-673)
   use system class loader when base loader is null
    * facilitates placing Clojure on bootclasspath

 * [CLJ-678](http://dev.clojure.org/jira/browse/CLJ-678)
   into-array should work with all primitive types

 * [CLJ-680](http://dev.clojure.org/jira/browse/CLJ-680)
   printing promises should not block
    * Patch allows printing of promises without blocking

 * [CLJ-682](http://dev.clojure.org/jira/browse/CLJ-682)
   cl-format: ~w throws an exception when not wrapped in a pretty-writer
    * Patch fixes the following bug in cl-format with ~w:

 * [CLJ-693](http://dev.clojure.org/jira/browse/CLJ-693)
   VerifyError with symbol metadata, macros, and defrecord

 * [CLJ-702](http://dev.clojure.org/jira/browse/CLJ-702)
   case gives NPE when used with nil
    * Patch allows nil to be used with case

 * [CLJ-734](http://dev.clojure.org/jira/browse/CLJ-734)
   starting scope of let bindings seems incorrect from jdi perspective
    * Patch fixes local variables table to have the correct code index for let bindings.

 * [CLJ-739](http://dev.clojure.org/jira/browse/CLJ-739)
   version.properties file is not closed
    * Patch properly closes version.properties file

 * [CLJ-751](http://dev.clojure.org/jira/browse/CLJ-751)
   cl-format: ~( throws an exception with an empty string
    * Patch fixes the following bug in cl-format when format is nil
    (cl-format nil "~:(~a~)" "")
    => NullPointerException

 * [CLJ-780](http://dev.clojure.org/jira/browse/CLJ-780)
   race condition in reference cache on Java 5
    * Map.Entry instances can have null values prior to Java 6. This patch provides a workaround.

 * floats were being boxed as Doubles, now they are boxed as Floats

 * several "holding onto head" fixes
    * Stop top-level defs from hanging onto the head of an expression that uses a lazy seq
    * Stop multimethods from holding onto heads of their arguments

## 5 Modular Contrib

In 1.3, the monolithic clojure-contrib.jar has been replaced by a modular system of contrib libraries, so that production systems can include only the code they actually need. This also allows individual contribs to have their own release cycles. Many contribs have moved forward by several point versions already. Documentation for updating applications to use the new contrib libraries is at http://dev.clojure.org/display/design/Where+Did+Clojure.Contrib+Go

Important Note: Many of the new modular contribs are compatible with both 1.2 and 1.3. This offers an incremental migration path: First, upgrade your contrib libraries while holding Clojure at 1.2, Then, in a separate step, upgrade to Clojure 1.3.
