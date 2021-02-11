<!-- -*- mode: markdown ; mode: visual-line ; coding: utf-8 -*- -->

# Changes to Clojure in Version 1.10.3

## 1 Changes reverted

* [CLJ-2564](https://clojure.atlassian.net/browse/CLJ-2564)
  Improve error message for case

## 2 Fixes

* [CLJ-2453](https://clojure.atlassian.net/browse/CLJ-2453)
  Enable reader conditionals in Clojure prepl

# Changes to Clojure in Version 1.10.2

## 1 Dependencies

Updated dependencies:

* spec.alpha dependency to 0.2.194 - [changes](https://github.com/clojure/spec.alpha/blob/master/CHANGES.md)
* core.specs.alpha dependency to 0.2.56 - [changes](https://github.com/clojure/core.specs.alpha/blob/master/CHANGES.md)

## 2 Fixes

## 2.1 Interop / JVM

* [CLJ-1472](https://clojure.atlassian.net/browse/CLJ-1472)
  Ensure monitor object is on stack, for verifiers
* [CLJ-2517](https://clojure.atlassian.net/browse/CLJ-2517)
  More fixes for invocation of static interface methods with primitive args
* [CLJ-2492](https://clojure.atlassian.net/browse/CLJ-2492)
  Remove uses of deprecated Class.newInstance()
* [CLJ-2534](https://clojure.atlassian.net/browse/CLJ-2534)
  Fix javadoc urls for JDK 11+
* [CLJ-2571](https://clojure.atlassian.net/browse/CLJ-2571)
  Add Throwable return type hint to ex-cause
* [CLJ-2572](https://clojure.atlassian.net/browse/CLJ-2572)
  Avoid reflection in clojure.data
* [CLJ-2502](https://clojure.atlassian.net/browse/CLJ-2502)
  Fix reflection warnings in clojure.stacktrace/print-stack-trace
* [CLJ-2597](https://clojure.atlassian.net/browse/CLJ-2597)
  proxy should emit Java 1.8 bytecode

## 2.2 Core

* [CLJ-2580](https://clojure.atlassian.net/browse/CLJ-2580)
  Fix case expression branch analysis that resulted in compilation error
* [CLJ-2564](https://clojure.atlassian.net/browse/CLJ-2564)
  Improve error message for case
* [CLJ-2585](https://clojure.atlassian.net/browse/CLJ-2585)
  nth with not-found on regex matcher returns not-found on last group index
* [CLJ-1364](https://clojure.atlassian.net/browse/CLJ-1364)
  vector-of does not implement equals or hashing methods
* [CLJ-2549](https://clojure.atlassian.net/browse/CLJ-2549)
  vector-of does not implement IObj for metadata
* [CLJ-1187](https://clojure.atlassian.net/browse/CLJ-1187)
  quoted metadata on empty literal colls is lost
* [CLJ-2459](https://clojure.atlassian.net/browse/CLJ-2459)
  ExceptionInInitializerError if jars executed with java -jar

## 2.3 Printing

* [CLJ-2469](https://clojure.atlassian.net/browse/CLJ-2469)
  Fix errors in printing some maps with namespace syntax
* [CLJ-1445](https://clojure.atlassian.net/browse/CLJ-1445)
  pprint doesn't print collection metadata when `*print-meta*` is true

## 2.4 Docstrings

* [CLJ-2295](https://clojure.atlassian.net/browse/CLJ-2295)
  Eliminate duplicate doc string printing for special forms
* [CLJ-2495](https://clojure.atlassian.net/browse/CLJ-2495)
  prepl docstring is incorrect
* [CLJ-2169](https://clojure.atlassian.net/browse/CLJ-2169)
  conj has out-of-date :arglists

## 3 Performance

* [CLJ-1005](https://clojure.atlassian.net/browse/CLJ-1005)
  Use transient map in zipmap

# Changes to Clojure in Version 1.10.1

## 1 Features and Major Changes

### 1.1 Workaround Java Performance Regression When Loading user.clj

Recent builds of Java 8 (u202), 11 (11.0.2), 12, and 13 included
some changes that [drastically affect](https://bugs.openjdk.java.net/browse/JDK-8219233)
optimization performance of calls from static initializers to static fields.
Clojure provides support for loading code on startup from a user.clj file and this
occurred in the static initializer of the Clojure runtime (RT) class and was thus
affected.

This issue may eventually be resolved in Java, but in Clojure we have
modified runtime initialization to avoid loading user.clj in a static
initializer, which mitigates the case where this caused a performance
degradation.

* [CLJ-2484](https://clojure.atlassian.net/browse/CLJ-2484)
  Significant performance regression of code loaded in user.clj in Java 8u202/11.0.

### 1.2 clojure.main Error Reporting

clojure.main is frequently used as a Clojure program launcher by external tools.
Previously, uncaught exceptions would be automatically printed by the JVM, which
would also print the stack trace.

This release will now catch exceptions and use the same error triage and printing
functionality as the Clojure repl. The full stack trace, ex-info, and other
information will be printed to a target specified by the configuration.

The three available error targets are:

* file - write to a temp file (default, falls back to stderr)
* stderr - write to stderr stream
* none - don't write

These error targets can be specified either as options to clojure.main, or as
Java system properties (flags take precedence). When invoking clojure.main
(or using the clj tool), use `--report <target>`. For Java system property,
use `-Dclojure.main.report=<target>`.

* [CLJ-2463](https://clojure.atlassian.net/browse/CLJ-2463)
  Improve error printing in clojure.main with -m, -e, etc
* [CLJ-2497](https://clojure.atlassian.net/browse/CLJ-2497)
  Put error report location on its own line
* [CLJ-2504](https://clojure.atlassian.net/browse/CLJ-2504)
  Provide more options for error reporting in clojure.main

## 2 Fixes

* [CLJ-2499](http://dev.clojure.org/jira/browse/CLJ-2499)
  Some compiler expr evals report as wrong error phase
* [CLJ-2491](https://clojure.atlassian.net/browse/CLJ-2491)
  Updated fragile tests so Clojure test suite runs on Java 12

# Changes to Clojure in Version 1.10

## 1 Compatibility and Dependencies

### 1.1 Java

Clojure 1.10 now requires Java 8 or above. There were a number of updates related to this change and/or Java compatibility fixes for Java 8, 9, 10, and 11:

* [CLJ-2363](http://dev.clojure.org/jira/browse/CLJ-2363)
  Bump to Java 8 as minimum requirement, update embedded ASM to 6.2,
  remove reliance on jsr166 jar, update javadoc links, and remove
  conditional logic.
* [CLJ-2367](http://dev.clojure.org/jira/browse/CLJ-2367)
  ASM regression fix
* [CLJ-2284](http://dev.clojure.org/jira/browse/CLJ-2284)
  Fix invalid bytecode generation for static interface method calls in Java 9+
* [CLJ-2066](http://dev.clojure.org/jira/browse/CLJ-2066)
  Add reflection fallback for --illegal-access warnings in Java 9+
* [CLJ-2330](http://dev.clojure.org/jira/browse/CLJ-2330)
  Fix brittle test that fails on Java 10 build due to serialization drift
* [CLJ-2374](http://dev.clojure.org/jira/browse/CLJ-2374)
  Add type hint to address reflection ambiguity in JDK 11
* [CLJ-2375](http://dev.clojure.org/jira/browse/CLJ-2375)
  Fix usage of deprecated JDK apis
* [CLJ-2414](http://dev.clojure.org/jira/browse/CLJ-2414)
  Regression in reflectively finding default methods

### 1.2 Dependencies

Updated dependencies:

* spec.alpha dependency to 0.2.176 - [changes](https://github.com/clojure/spec.alpha/blob/master/CHANGES.md)
* core.specs.alpha dependency to 0.2.44 - [changes](https://github.com/clojure/core.specs.alpha/blob/master/CHANGES.md)

## 2 Features and major changes

### 2.1 Error messages

Clojure errors can occur in several distinct "phases" - reading source, macroexpansion, compilation, execution, and result printing. Clojure (and the REPL) now identify these phases in the exception and the message.

The read/macroexpand/compile phases produce a CompilerException and indicate the location in the caller source code where the problem occurred (previously macroexpansion reported the error in the macroexpansion stack). CompilerException now implements IExceptionInfo and ex-data will report exception data including the following (optional) keys:

* :clojure.error/phase - phase (:read-source, :macro-syntax-check, :macroexpansion, :compile-syntax-check, :compilation, :execution, :read-eval-result, :print-eval-result)
* :clojure.error/source - source file
* :clojure.error/line - line in source file
* :clojure.error/column - column of line in source file
* :clojure.error/symbol - symbol being macroexpanded or compiled
* :clojure.error/class - cause exception class symbol
* :clojure.error/cause - cause exception message
* :clojure.error/spec - explain-data for spec errors

clojure.main also contains two new functions: `ex-triage` and `ex-str` that can be used by external tools to mimic some or all of the Clojure repl reporting. `ex-triage` takes the output of `Throwable->map` and produces a concise analysis of the error phase, cause, etc (same keys as above). `ex-str` takes that analysis data and produces a message to print at the repl.

* [CLJ-2373](http://dev.clojure.org/jira/browse/CLJ-2373)
  Detect phase and overhaul exception message and printing
* [CLJ-2415](http://dev.clojure.org/jira/browse/CLJ-2415)
  Error cause should always be on 2nd line of error message
* [CLJ-2420](http://dev.clojure.org/jira/browse/CLJ-2420)
  Refinement of error phases, `ex-triage`, execution error line reporting
* [CLJ-2427](http://dev.clojure.org/jira/browse/CLJ-2427)
  CompilerException.toString() can throw if making message during initialization
* [CLJ-2430](http://dev.clojure.org/jira/browse/CLJ-2430)
  Elevate phase in throwable data and conveyance for prepl
* [CLJ-2435](http://dev.clojure.org/jira/browse/CLJ-2435)
  Include root cause class name in compilation and macroexpansion error phases
* [CLJ-2438](http://dev.clojure.org/jira/browse/CLJ-2438)
  Demunge source symbol in execution error messages


### 2.2 Protocol extension by metadata

`defprotocol` has a new option `:extend-via-metadata`. When :extend-via-metadata is true, values can extend protocols by adding metadata where keys are fully-qualified protocol function symbols and values are function implementations. Protocol implementations are checked first for direct definitions (defrecord, deftype, reify), then metadata definitions, then external extensions (extend, extend-type, extend-protocol).

### 2.3 tap

tap is a shared, globally accessible system for distributing a series of informational or diagnostic values to a set of (presumably effectful) handler functions. It can be used as a better debug prn, or for facilities like logging etc.

`tap>` sends a value to the set of taps. Taps can be added with `add-tap` and will be called with any value sent to `tap>`. The tap function may (briefly) block (e.g. for streams) and will never impede calls to `tap>`, but blocking indefinitely may cause tap values to be dropped. If no taps are registered, `tap>` discards. Remove taps with `remove-tap`.

### 2.4 Read string capture mode

`read+string` is a new function that mimics `read` but also captures the string that is read and returns both the read value and the (whitespace-trimmed) read string. `read+string` requires a LineNumberingPushbackReader.

### 2.5 prepl (alpha)

prepl is a new stream-based REPL with structured output (suitable for programmatic use). Forms are read from the reader, evaluated, and return data maps for the return value (if successful), output to `*out*` (possibly many), output to `*err*` (possibly many), or tap> values (possibly many).

New functions in clojure.core.server:

* `prepl` - the repl
* `io-prepl` - a prepl bound to `*in*` and `*out*` suitable for use with the Clojure socket server
* `remote-prepl` - a prepl that can be connected to a remote prepl over a socket

prepl is alpha and subject to change.

### 2.6 datafy and nav

clojure.datafy is a facility for object to data transformation. The `datafy` and `nav` functions can be used used to transform and (lazily) navigate through object graphs. The data transformation process can be influenced by consumers using protocols or metadata.

datafy is alpha and subject to change.

* [CLJ-2429](http://dev.clojure.org/jira/browse/CLJ-2429)
  Datafy JavaReflector


### 2.6 Other new functions in core

These functions have been added to match existing functions in ClojureScript to increase the portability of error-handling code:

* `ex-cause` - extract the cause exception
* `ex-message` - extract the cause message

This function has been added to construct a PrintWriter implementation whose behavior on flush and close is provided as functions:

* `PrintWriter-on` - create a PrintWriter from flush-fn and close-fn

The following function has been added, extending `resolve`:

* `requiring-resolve` - resolve or, if needed, require symbol's namespace, then resolve
* `serialized-require` - like `require` but for use in asynchronous load uses

## 3 Enhancements

### 3.1 Error messages

* [CLJ-1279](http://dev.clojure.org/jira/browse/CLJ-1279)
  Report correct arity count for function arity errors inside macros
* [CLJ-2386](http://dev.clojure.org/jira/browse/CLJ-2386)
  Omit ex-info construction frames
* [CLJ-2394](http://dev.clojure.org/jira/browse/CLJ-2394)
  Warn in pst that stack trace for syntax error failed before execution
* [CLJ-2396](http://dev.clojure.org/jira/browse/CLJ-2396)
  Omit :in clauses when printing spec function errors if using default explain printer
* [CLJ-1797](http://dev.clojure.org/jira/browse/CLJ-1797)
  Mention cljc in error when require fails
* [CLJ-1130](http://dev.clojure.org/jira/browse/CLJ-1130)
  Improve error message when unable to match static method

### 3.2 Documentation

* [CLJ-2044](http://dev.clojure.org/jira/browse/CLJ-2044)
  clojure.instant - add arglist meta for functions
* [CLJ-2257](http://dev.clojure.org/jira/browse/CLJ-2257)
  `proxy` - fix typo
* [CLJ-2332](http://dev.clojure.org/jira/browse/CLJ-2332)
  `remove-tap` - fix repetition
* [CLJ-2122](http://dev.clojure.org/jira/browse/CLJ-2122)
  `flatten` - describe result as lazy

### 3.3 Performance

* [CLJ-1654](http://dev.clojure.org/jira/browse/CLJ-1654)
  Reuse seq in `some`
* [CLJ-1366](http://dev.clojure.org/jira/browse/CLJ-1366)
  The empty map literal is read as a different map each time
* [CLJ-2362](http://dev.clojure.org/jira/browse/CLJ-2362)
  `with-meta` should return identity when new meta is identical to prior

### 3.4 Other enhancements

* `symbol` can now take a var or a keyword argument
* [CLJ-1209](http://dev.clojure.org/jira/browse/CLJ-1209)
  Print ex-data in clojure.test error reports
* [CLJ-2163](http://dev.clojure.org/jira/browse/CLJ-2163)
  Add test for var serialization
* [CLJ-2417](http://dev.clojure.org/jira/browse/CLJ-2417)
  `sort` and `sort-by` should retain meta

## 4 Fixes

### 4.1 Collections

* [CLJ-2297](http://dev.clojure.org/jira/browse/CLJ-2297)
  PersistentHashMap leaks memory when keys are removed with `without`
* [CLJ-1587](http://dev.clojure.org/jira/browse/CLJ-1587)
  PersistentArrayMap’s assoc doesn’t respect HASHTABLE_THRESHOLD
* [CLJ-2050](http://dev.clojure.org/jira/browse/CLJ-2050)
  Remove redundant key comparisons in HashCollisionNode
* [CLJ-2089](http://dev.clojure.org/jira/browse/CLJ-2089)
  Sorted colls with default comparator don’t check that first element is Comparable

### 4.2 API

* [CLJ-2031](http://dev.clojure.org/jira/browse/CLJ-2031)
  clojure.walk/postwalk does not preserve MapEntry type objects
* [CLJ-2349](http://dev.clojure.org/jira/browse/CLJ-2349)
  Report correct line number for uncaught ExceptionInfo in clojure.test
* [CLJ-1764](http://dev.clojure.org/jira/browse/CLJ-1764)
  partition-by runs infinite loop when one element of infinite partition is accessed
* [CLJ-1832](http://dev.clojure.org/jira/browse/CLJ-1832)
  unchecked-* functions have different behavior on primitive longs vs boxed Longs

### 4.3 Other

* [CLJ-1403](http://dev.clojure.org/jira/browse/CLJ-1403)
  ns-resolve might throw ClassNotFoundException but should return nil
* [CLJ-2407](http://dev.clojure.org/jira/browse/CLJ-2407)
  Fix bugs in Clojure unit tests
* [CLJ-1079](http://dev.clojure.org/jira/browse/CLJ-1079)
  In reader, don't ignore explicit :line :col meta

# Changes to Clojure in Version 1.9

## 1 New and Improved Features

### 1.1 spec

spec is a new core library for describing, validating, and testing the structure of data and functions.

For more information, see:

* [About spec](https://clojure.org/about/spec)
* [spec Guide](https://clojure.org/guides/spec)

Note that spec is in alpha state and API compatibility is not guaranteed. Also, spec and the specs for the Clojure core API are distributed as external libraries that must be included to use Clojure.

### 1.2 Support for working with maps with qualified keys

Several enhancements have been made to add support for working with maps with qualified keys:

* Map namespace syntax - specify the default namespace context for the keys (or symbols) in a map once - `#:car{:make "Jeep" :model "Wrangler"}`. For more information see https://clojure.org/reference/reader#_maps ([CLJ-1910](http://dev.clojure.org/jira/browse/CLJ-1910))
* Destructuring support - namespaced map keys can now specified once as a namespace for :keys or :syms. For more information see https://clojure.org/reference/special_forms#_map_binding_destructuring ([CLJ-1919](http://dev.clojure.org/jira/browse/CLJ-1919))
* `*print-namespace-maps*` - by default maps will not print with the map namespace syntax except in the clojure.main repl. This dynamic var is a flag to allow you to control whether the namespace map syntax is used.

### 1.3 New predicates

Specs rely heavily on predicates and many new type and value oriented predicates have been added to clojure.core:

* `boolean?`
* `int?` `pos-int?` `neg-int?` `nat-int?`
* `double?`
* `ident?` `simple-ident?` `qualified-ident?`
* `simple-symbol?` `qualified-symbol?`
* `simple-keyword?` `qualified-keyword?`
* `bytes?` (for `byte[]`)
* `indexed?`
* `uuid?` `uri?`
* `seqable?`
* `any?`

### 1.4 More support for instants

More support has been added for the notion of instants in time:

* Added a new protocol `Inst` for instant types
* `Inst` is extended for `java.util.Date`
* `Inst` is optionally extended for `java.time.Instant` in Java 1.8+
* New functions that work for instants: `inst?`, `inst-ms`

### 1.5 Other new core functions

These are some other new functions in clojure.core:

* `bounded-count` - a count that avoids realizing the entire collection beyond a bound
* `swap-vals!` and `reset-vals!` - new atom functions that return both the old and new values ([CLJ-1454](http://dev.clojure.org/jira/browse/CLJ-1454))
* `halt-when` - new transducer that ends transduction when pred is satisfied

### 1.6 Other reader enhancements

* Can now bind `*reader-resolver*` to an impl of LispReader$Resolver to control the reader’s use of namespace interactions when resolving autoresolved keywords and maps.
* Add new ## reader macro for symbolic values, and read/print support for double vals ##Inf, ##-Inf, ##NaN ([CLJ-1074](http://dev.clojure.org/jira/browse/CLJ-1074))

## 2 Enhancements

### 2.1 Spec syntax checking

If a macro has a spec defined via fdef, that spec will be checked at compile time. Specs have been defined for many clojure.core macros and errors will be reported for these based on the specs at compile time.

### 2.2 Documentation

* `doc` will now report specs for functions with specs defined using `fdef`
* `doc` can now be invoked with a fully-qualified keyword representing a spec name

### 2.3 Performance

* Improved update-in performance
* Optimized seq & destructuring
* [CLJ-2210](http://dev.clojure.org/jira/browse/CLJ-2210)
  Cache class derivation in compiler to improve compiler performance
* [CLJ-2188](http://dev.clojure.org/jira/browse/CLJ-2188)
  `slurp` - mark return type as String
* [CLJ-2070](http://dev.clojure.org/jira/browse/CLJ-2070)
  `clojure.core/delay` - improve performance
* [CLJ-1917](http://dev.clojure.org/jira/browse/CLJ-1917)
  Reducing seq over string should call String/length outside of loop
* [CLJ-1901](http://dev.clojure.org/jira/browse/CLJ-1901)
  `amap` - should call alength only once
* [CLJ-1224](http://dev.clojure.org/jira/browse/CLJ-1935)
  Record instances now cache hasheq and hashCode like maps
* [CLJ-99](http://dev.clojure.org/jira/browse/CLJ-99)
  `min-key` and `max-key` - evaluate k on each arg at most once

### 2.4 Other enhancements

* Added Var serialization for identity, not value
* `into` now has a 0-arity (returns `[]`) and 1-arity (returns the coll that's passed)
* [CLJ-2184](http://dev.clojure.org/jira/browse/CLJ-2184)
  Propagate meta in doto forms to improve error reporting
* [CLJ-1744](http://dev.clojure.org/jira/browse/CLJ-1744)
  Clear unused locals, which can prevent memory leaks in some cases
* [CLJ-1673](http://dev.clojure.org/jira/browse/CLJ-1673)
  `clojure.repl/dir-fn` now works on namespace aliases
* [CLJ-1423](http://dev.clojure.org/jira/browse/CLJ-1423)
  Allow vars to be invoked with infinite arglists (also, faster)

## 3 Fixes

### 3.1 Security

* [CLJ-2204](http://dev.clojure.org/jira/browse/CLJ-2204)
  Disable serialization of proxy classes to avoid potential issue when deserializing

### 3.2 Docs

* [CLJ-2170](http://dev.clojure.org/jira/browse/CLJ-2170)
  fix improperly located docstrings
* [CLJ-2156](http://dev.clojure.org/jira/browse/CLJ-2156)
  `clojure.java.io/copy` - doc char[] support
* [CLJ-2104](http://dev.clojure.org/jira/browse/CLJ-2104)
  `clojure.pprint` docstring - fix typo
* [CLJ-2051](http://dev.clojure.org/jira/browse/CLJ-2051)
  `clojure.instant/validated` docstring - fix typo
* [CLJ-2039](http://dev.clojure.org/jira/browse/CLJ-2039)
  `deftype` - fix typo in docstring
* [CLJ-2028](http://dev.clojure.org/jira/browse/CLJ-2028)
  `filter`, `filterv`, `remove`, `take-while` - fix docstrings
* [CLJ-1918](http://dev.clojure.org/jira/browse/CLJ-1918)
  `await` - improve docstring re `shutdown-agents`
* [CLJ-1873](http://dev.clojure.org/jira/browse/CLJ-1873)
  `require`, `*data-readers*` - add .cljc files to docstrings
* [CLJ-1859](http://dev.clojure.org/jira/browse/CLJ-1859)
  `zero?`, `pos?`, `neg?` - fix docstrings
* [CLJ-1837](http://dev.clojure.org/jira/browse/CLJ-1837)
  `index-of`, `last-index-of` - clarify docstrings
* [CLJ-1826](http://dev.clojure.org/jira/browse/CLJ-1826)
  `drop-last` - fix docstring
* [CLJ-1159](http://dev.clojure.org/jira/browse/CLJ-1159)
  `clojure.java.io/delete-file` - improve docstring

### 3.3 Other fixes

* `clojure.core/Throwable->map` formerly returned `StackTraceElement`s which were later handled by the printer. Now the StackTraceElements are converted to data such that the return value is pure Clojure data, as intended.
* [CLJ-2091](http://dev.clojure.org/jira/browse/CLJ-2091)
  `clojure.lang.APersistentVector#hashCode` is not thread-safe
* [CLJ-2077](http://dev.clojure.org/jira/browse/CLJ-2077)
  Clojure can't be loaded from the boot classpath under java 9
* [CLJ-2048](http://dev.clojure.org/jira/browse/CLJ-2048)
  Specify type to avoid ClassCastException when stack trace is elided by JVM
* [CLJ-1914](http://dev.clojure.org/jira/browse/CLJ-1914)
  Fixed race condition in concurrent `range` realization
* [CLJ-1887](http://dev.clojure.org/jira/browse/CLJ-1887)
  `IPersistentVector.length()` - implement missing method
* [CLJ-1870](http://dev.clojure.org/jira/browse/CLJ-1870)
  Fixed reloading a `defmulti` removes metadata on the var
* [CLJ-1860](http://dev.clojure.org/jira/browse/CLJ-1860)
  Make -0.0 hash consistent with 0.0
* [CLJ-1841](http://dev.clojure.org/jira/browse/CLJ-1841)
  `bean` - iterator was broken
* [CLJ-1793](http://dev.clojure.org/jira/browse/CLJ-1793)
  Clear 'this' before calls in tail position
* [CLJ-1790](http://dev.clojure.org/jira/browse/CLJ-1790)
  Fixed error extending protocols to Java arrays
* [CLJ-1714](http://dev.clojure.org/jira/browse/CLJ-1714)
  using a class in a type hint shouldn’t load the class
* [CLJ-1705](http://dev.clojure.org/jira/browse/CLJ-1705)
  `vector-of` - fix NullPointerException if given unrecognized type
* [CLJ-1398](http://dev.clojure.org/jira/browse/CLJ-1398)
  `clojure.java.javadoc/javadoc` - update doc urls
* [CLJ-1371](http://dev.clojure.org/jira/browse/CLJ-1371)
  `Numbers.divide(Object, Object)` - add checks for NaN
* [CLJ-1358](http://dev.clojure.org/jira/browse/CLJ-1358)
  `doc` - does not expand special cases properly (try, catch)
* [CLJ-1242](http://dev.clojure.org/jira/browse/CLJ-1242)
  equals doesn't throw on sorted collections
* [CLJ-700](http://dev.clojure.org/jira/browse/CLJ-700)
  `contains?`, `get`, and `find` broken for transient collections

# Changes to Clojure in Version 1.8

## 1 New and Improved Features

### 1.1 Direct Linking

Direct linking can be enabled with `-Dclojure.compiler.direct-linking=true`

Direct linking allows functions compiled with direct linking on to make direct
static method calls to most other functions, instead of going through the var
and the Fn object. This can enable further optimization by the jit, at a cost
in dynamism. In particular, directly-linked calls will not see redefinitions.

With this change, clojure.core itself is compiled with direct linking
and therefore other namespaces cannot redefine core fns and have those
redefinitions seen by core code.

A new metadata key ^:redef is provided. A function declared with this key can
be redefined and will never be direct linked. Also, functions declared as
^:dynamic will never be direct linked.

* [CLJ-1809](http://dev.clojure.org/jira/browse/CLJ-1809)
* [CLJ-1805](http://dev.clojure.org/jira/browse/CLJ-1805)
* [CLJ-1854](http://dev.clojure.org/jira/browse/CLJ-1854)
* [CLJ-1856](http://dev.clojure.org/jira/browse/CLJ-1856)

### 1.2 String Functions

Several new string functions were added to clojure.string to increase
portability and reduce the need for Java interop calls:

* index-of - search for the index of a char or string in a string
* last-index-of - search for the index of a char or string backwards in a string
* starts-with? - true if string starts with a substring
* ends-with? - true if string ends with a substring
* includes? - true if string includes a substring

* [CLJ-1449](http://dev.clojure.org/jira/browse/CLJ-1449)

### 1.3 Socket Server and REPL

The Clojure runtime now has the ability to start a socket server at initialization
based on system properties. One expected use for this is serving a socket-based
REPL, but it also has many other potential uses for dynamically adding server
capability to existing programs without code changes.

A socket server will be started for each JVM system property like
`clojure.server.<server-name>`. The value for this property is an edn map
representing the configuration of the socket server with the following properties:

* address - host or address, defaults to loopback
* port - positive integer, required
* accept - namespaced symbol of function to invoke on socket accept, required
* args - sequential collection of args to pass to accept
* bind-err - defaults to true, binds `*err*` to socket out stream
* server-daemon - defaults to true, socket server thread doesn't block exit
* client-daemon - defaults to true, socket client thread doesn't block exit

Additionally, there is a repl function provided that is slightly customized for
use with the socket server in `clojure.core.server/repl`.

Following is an example of starting a socket server with a repl listener.
This can be added to any existing Clojure program to allow it to accept
external REPL clients.

```
-Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl}"
```

An example client you can use to connect to this socket repl is telnet:

```
$ telnet 127.0.0.1 5555
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
user=> (println "hello")
hello
```

See:

* [CLJ-1671](http://dev.clojure.org/jira/browse/CLJ-1671)
* [CLJ-1853](http://dev.clojure.org/jira/browse/CLJ-1853)
* [Socket REPL design page](http://dev.clojure.org/display/design/Socket+Server+REPL)
* [CLJ-1829](http://dev.clojure.org/jira/browse/CLJ-1829)

## 2 Enhancements

### 2.1 Error handling

* [CLJ-1778](http://dev.clojure.org/jira/browse/CLJ-1778)
  let-bound namespace-qualified bindings should throw (if not map destructuring)
* [CLJ-1456](http://dev.clojure.org/jira/browse/CLJ-1456)
  Compiler now errors if too few or too many arguments to throw
* [CLJ-1282](http://dev.clojure.org/jira/browse/CLJ-1282)
  quote now throws if passed more or less than one arg
* [CLJ-1210](http://dev.clojure.org/jira/browse/CLJ-1210)
  Improved error message for (clojure.java.io/reader nil)

### 2.2 Documentation strings

* [CLJ-1060](http://dev.clojure.org/jira/browse/CLJ-1060)
  'list*' returns not a list
* [CLJ-1722](http://dev.clojure.org/jira/browse/CLJ-1722)
  Typo in the docstring of 'with-bindings'
* [CLJ-1769](http://dev.clojure.org/jira/browse/CLJ-1769)
  Docstrings for *' and +' refer to * and +
* [CLJ-1414](http://dev.clojure.org/jira/browse/CLJ-1414)
  sort and sort-by now indicate sort is stable in docstring

### 2.3 Performance

* [CLJ-703](http://dev.clojure.org/jira/browse/CLJ-703)
  Improve writeClassFile performance
* [CLJ-1765](http://dev.clojure.org/jira/browse/CLJ-1765)
  areduce performance improvements
* [CLJ-1724](http://dev.clojure.org/jira/browse/CLJ-1724)
  Remove unnecessary call to seq() in LazySeq.hashCode()
* [CLJ-1295](http://dev.clojure.org/jira/browse/CLJ-1295)
  Improved array-map dissoc performance
* [CLJ-1277](http://dev.clojure.org/jira/browse/CLJ-1277)
  Speed up printing of time instants with type hints
* [CLJ-1259](http://dev.clojure.org/jira/browse/CLJ-1259)
  Speed up pprint and cl-format with type hints
* [CLJ-668](http://dev.clojure.org/jira/browse/CLJ-668)
  Improve slurp performance by using StringWriter and jio/copy

### 2.4 Other enhancements

* [CLJ-1208](http://dev.clojure.org/jira/browse/CLJ-1208)
  Optionally require namespace on defrecord class init
* [CLJ-1823](http://dev.clojure.org/jira/browse/CLJ-1823)
  Document new :load-ns option to defrecord/deftype
* [CLJ-1810](http://dev.clojure.org/jira/browse/CLJ-1810)
  ATransientMap now marked public
* [CLJ-1653](http://dev.clojure.org/jira/browse/CLJ-1653)
  str of an empty list should be "()"
* [CLJ-1567](http://dev.clojure.org/jira/browse/CLJ-1567)
  Removed unused local in condp implementation
* [CLJ-1351](http://dev.clojure.org/jira/browse/CLJ-1351)
  Unused swapThunk method was being emitted for fns with keyword callsites
* [CLJ-1329](http://dev.clojure.org/jira/browse/CLJ-1329)
  Removed unused local in PersistentVector.cons()
* [CLJ-1831](http://dev.clojure.org/jira/browse/CLJ-1831)
  Add clojure.core/map-entry? predicate
* [CLJ-1845](http://dev.clojure.org/jira/browse/CLJ-1845)
  Make clojure.core/load dynamic so it can be redef'ed even with direct linking

## 3 Bug Fixes

* [CLJ-130](http://dev.clojure.org/jira/browse/CLJ-130)
  Namespace metadata lost in AOT compile
* [CLJ-1134](http://dev.clojure.org/jira/browse/CLJ-1134)
  star-directive in clojure.pprint/cl-format with at-prefix ("~n@*") does
  not obey its specification
* [CLJ-1137](http://dev.clojure.org/jira/browse/CLJ-1137)
  Metadata on a def gets evaluated twice
* [CLJ-1157](http://dev.clojure.org/jira/browse/CLJ-1157)
  Classes generated by gen-class aren't loadable from remote codebase
* [CLJ-1225](http://dev.clojure.org/jira/browse/CLJ-1225)
  quot overflow issues around Long/MIN_VALUE for BigInt
* [CLJ-1313](http://dev.clojure.org/jira/browse/CLJ-1313)
  Correct a few unit tests
* [CLJ-1319](http://dev.clojure.org/jira/browse/CLJ-1319)
  array-map fails lazily if passed an odd number of arguments
* [CLJ-1361](http://dev.clojure.org/jira/browse/CLJ-1361)
  pprint with code-dispatch incorrectly prints a simple ns macro call
* [CLJ-1390](http://dev.clojure.org/jira/browse/CLJ-1390)
  pprint a GregorianCalendar results in Arity exception
* [CLJ-1399](http://dev.clojure.org/jira/browse/CLJ-1399)
  field name unmunged when recreating deftypes serialized into bytecode
* [CLJ-1485](http://dev.clojure.org/jira/browse/CLJ-1485)
  clojure.test.junit/with-junit-output doesn't handle multiple expressions
* [CLJ-1528](http://dev.clojure.org/jira/browse/CLJ-1528)
  clojure.test/inc-report-counter is not thread-safe
* [CLJ-1533](http://dev.clojure.org/jira/browse/CLJ-1533)
  invokePrim path does not take into account var or form meta
* [CLJ-1562](http://dev.clojure.org/jira/browse/CLJ-1562)
  some->,some->>,cond->,cond->> and as-> doesn't work with (recur)
* [CLJ-1565](http://dev.clojure.org/jira/browse/CLJ-1565)
  pprint produces infinite output for a protocol
* [CLJ-1588](http://dev.clojure.org/jira/browse/CLJ-1588)
  StackOverflow in clojure.test macroexpand with `are` and anon `fn`
* [CLJ-1644](http://dev.clojure.org/jira/browse/CLJ-1644)
  into-array fails for sequences starting with nil
* [CLJ-1645](http://dev.clojure.org/jira/browse/CLJ-1645)
  protocol class does not set the source file
* [CLJ-1657](http://dev.clojure.org/jira/browse/CLJ-1657)
  proxy bytecode calls super methods of abstract classes
* [CLJ-1659](http://dev.clojure.org/jira/browse/CLJ-1659)
  compile leaks files
* [CLJ-1761](http://dev.clojure.org/jira/browse/CLJ-1761)
  clojure.core/run! does not always return nil per docstring
* [CLJ-1782](http://dev.clojure.org/jira/browse/CLJ-1782)
  Spelling mistake in clojure.test/use-fixtures
* [CLJ-1785](http://dev.clojure.org/jira/browse/CLJ-1785)
  Reader conditionals throw when returning nil
* [CLJ-1766](http://dev.clojure.org/jira/browse/CLJ-1766)
  Serializing+deserializing lists breaks their hash
* [CLJ-1609](http://dev.clojure.org/jira/browse/CLJ-1609)
  Edge case in Reflector's search for a public method declaration
* [CLJ-1586](http://dev.clojure.org/jira/browse/CLJ-1586)
  Compiler doesn't preserve metadata for LazySeq literals
* [CLJ-1232](http://dev.clojure.org/jira/browse/CLJ-1232)
  Functions with non-qualified return type hints will now work without
  import from other namespace
* [CLJ-1812](http://dev.clojure.org/jira/browse/CLJ-1812)
  Fix test failure on windows due to line endings
* [CLJ-1380](http://dev.clojure.org/jira/browse/CLJ-1380)
  3-arg ExceptionInfo constructor permitted nil data
* [CLJ-1226](http://dev.clojure.org/jira/browse/CLJ-1226)
  set! of a deftype field using field-access syntax caused ClassCastException
* Records and types without fields eval to empty map
* [CLJ-1827](http://dev.clojure.org/jira/browse/CLJ-1827)
  Fix reflection warning introduced in CLJ-1259
* [CLJ-1453](http://dev.clojure.org/jira/browse/CLJ-1453)
  Ensure that all Iterator implementations throw NoSuchElementException
  on next() when exhausted
* [CLJ-1868](http://dev.clojure.org/jira/browse/CLJ-1868)
  Avoid compiler NPE when checking class return type

# Changes to Clojure in Version 1.7

## 1 Compatibility Notes

Please be aware of the following issues when upgrading to Clojure 1.7.

### Seqs on Java iterators that return the same mutating object

Seqs are fundamentally incompatible with Java iterators that return
the same mutating object on every call to next().  Some Clojure
libraries incorrectly rely on calling seq on such iterators.

In 1.7, iterator-seqs are chunked, which will cause many of these
incorrect usages to return incorrect results immediately.

The `seq` and `iterator-seq` docstrings have been updated to include
an explicit warning. Libraries that incorrectly use `seq` and
`iterator-seq` will need to be fixed before running against 1.7.

* [CLJ-1669](http://dev.clojure.org/jira/browse/CLJ-1669)
* [CLJ-1738](http://dev.clojure.org/jira/browse/CLJ-1738)

### Thread owner check removed on transients

Prior to Clojure 1.7, transients would allow modification only from the
thread that created the transient. This check has been removed. It is
still a requirement that transients should be updated by only a single
thread at a time.

This constraint was relaxed to allow transients to be used in cases where
code is multiplexed across multiple threads in a pool (such as go blocks
in core.async).

### keys/vals require custom map type to implement Iterable

Invoking `keys` or `vals` on a custom map type that implements IPersistentMap
will now use the Iterable iterator() method instead of accessing entries
via the seq of the map. There have been no changes in the type hierarchy
(IPersistentMap has always extended Iterable) but former map-like instances
may have skipped implementing this method in the past.

* [CLJ-1602](http://dev.clojure.org/jira/browse/CLJ-1602)

## 2 New and Improved Features

### 2.1 Transducers

Transducers is a new way to decouple algorithmic transformations from their
application in different contexts. Transducers are functions that transform
reducing functions to build up a "recipe" for transformation.

Also see: http://clojure.org/transducers

Many existing sequence functions now have a new arity (one fewer argument
than before). This arity will return a transducer that represents the same
logic but is independent of lazy sequence processing. Functions included are:

* map
* mapcat
* filter
* remove
* take
* take-while
* drop
* drop-while
* take-nth
* replace
* partition-by
* partition-all
* keep
* keep-indexed
* map-indexed
* distinct
* interpose

Additionally some new transducer functions have been added:

* cat - concatenates the contents of each input
* dedupe - removes consecutive duplicated values
* random-sample - returns items from coll with random probability

And this function can be used to make completing transforms:

* completing

There are also several new or modified functions that can be used to apply
transducers in different ways:

* sequence - takes a transformation and a coll and produces a lazy seq
* transduce - reduce with a transformation (eager)
* eduction - returns a reducible/iterable of applications of the transducer to items in coll. Applications are re-performed with every reduce/iterator.

There have been a number of internal changes to support transducers:

* volatiles - there are a new set of functions (volatile!, vswap!, vreset!, volatile?) to create and use volatile "boxes" to hold state in stateful transducers. Volatiles are faster than atoms but give up atomicity guarantees so should only be used with thread isolation.
* array iterators - added support for iterators over arrays
* conj can be used as a reducing function and will conj to []

Some related issues addressed during development:
* [CLJ-1511](http://dev.clojure.org/jira/browse/CLJ-1511)
* [CLJ-1497](http://dev.clojure.org/jira/browse/CLJ-1497)
* [CLJ-1549](http://dev.clojure.org/jira/browse/CLJ-1549)
* [CLJ-1537](http://dev.clojure.org/jira/browse/CLJ-1537)
* [CLJ-1554](http://dev.clojure.org/jira/browse/CLJ-1554)
* [CLJ-1601](http://dev.clojure.org/jira/browse/CLJ-1601)
* [CLJ-1606](http://dev.clojure.org/jira/browse/CLJ-1606)
* [CLJ-1621](http://dev.clojure.org/jira/browse/CLJ-1621)
* [CLJ-1600](http://dev.clojure.org/jira/browse/CLJ-1600)
* [CLJ-1635](http://dev.clojure.org/jira/browse/CLJ-1635)
* [CLJ-1683](http://dev.clojure.org/jira/browse/CLJ-1683)
* [CLJ-1669](http://dev.clojure.org/jira/browse/CLJ-1669)
* [CLJ-1723](http://dev.clojure.org/jira/browse/CLJ-1723)

### 2.2 Reader Conditionals

Reader Conditionals are a new capability to support portable code that
can run on multiple Clojure platforms with only small changes. In
particular, this feature aims to support the increasingly common case
of libraries targeting both Clojure and ClojureScript.

Code intended to be common across multiple platforms should use a new
supported file extension: ".cljc". When requested to load a namespace,
the platform-specific file extension (.clj, .cljs) will be checked
prior to .cljc.

A new reader form can be used to specify "reader conditional" code in
cljc files (and *only* cljc files). Each platform defines a feature
identifying the platform (:clj, :cljs, :cljr). The reader conditional
specifies code that is read conditionally based on the feature. The
REPL also allows reader conditionals.

Form #? takes a list of alternating feature and expression. These are
checked like cond and the selected expression is read and returned. Other
branches are read but skipped. If no branch is selected, the reader reads
nothing (not nil, but literally as if reading no form). An optional
`:default` branch can be used as a fallthrough.

Reader conditional with 2 features and a default:

	#?(:clj     Double/NaN
	   :cljs    js/NaN
	   :default nil)

There is also a reader conditional splicing form. The evaluated expression
should be sequential and will be spliced into the surrounded code, similar
to unquote-splicing.

For example:

   [1 2 #?@(:clj [3 4] :cljs [5 6])]

This form would read as [1 2 3 4] on Clojure, [1 2 5 6] on ClojureScript,
and [1 2] on any other platform. Splicing is not allowed at the top level.

Additionally, the reader can now be invoked with options for the features
to use and how to interpret reader conditionals. By default, reader conditionals
are not allowed, but that can be turned on, or a "preserve" mode can be used to
preserve all branches (most likely useful for tooling or source transforms).

In the preserve mode, the reader conditional itself and any tagged literals
within the unselected branches are returned as tagged literal data.

For more information, see:
http://dev.clojure.org/display/design/Reader+Conditionals

* [CLJ-1424](http://dev.clojure.org/jira/browse/CLJ-1424)
* [CLJ-1685](http://dev.clojure.org/jira/browse/CLJ-1685)
* [CLJ-1698](http://dev.clojure.org/jira/browse/CLJ-1698)
* [CLJ-1699](http://dev.clojure.org/jira/browse/CLJ-1699)
* [CLJ-1700](http://dev.clojure.org/jira/browse/CLJ-1700)
* [CLJ-1728](http://dev.clojure.org/jira/browse/CLJ-1728)
* [CLJ-1706](http://dev.clojure.org/jira/browse/CLJ-1706)

### 2.3 Keyword and Symbol Construction

In response to issues raised in [CLJ-1439](http://dev.clojure.org/jira/browse/CLJ-1439),
several changes have been made in symbol and keyword construction:

1) The main bottleneck in construction of symbols (which also occurs inside keywords) was
interning of the name and namespace strings. This interning has been removed, resulting
in a performance increase.

2) Keywords are cached and keyword construction includes a cache check. A change was made
to only clear the cache reference queue when there is a cache miss.

### 2.4 Warn on Boxed Math

One source of performance issues is the (unintended) use of arithmetic operations on
boxed numbers. To make detecting the presence of boxed math easier, a warning will now
be emitted about boxed math if \*unchecked-math* is set to :warn-on-boxed (any truthy
value will enable unchecked-math, only this specific value enables the warning).

Example use:

    user> (defn plus-2 [x] (+ x 2))  ;; no warning, but boxed
	#'user/plus-2
    user> (set! *unchecked-math* :warn-on-boxed)
	true
    user> (defn plus-2 [x] (+ x 2)) ;; now we see a warning
    Boxed math warning, NO_SOURCE_PATH:10:18 - call: public static java.lang.Number
	clojure.lang.Numbers.unchecked_add(java.lang.Object,long).
    #'user/plus-2
	user> (defn plus-2 [^long x] (+ x 2)) ;; use a hint to avoid boxing
	#'user/plus-2

* [CLJ-1325](http://dev.clojure.org/jira/browse/CLJ-1325)
* [CLJ-1535](http://dev.clojure.org/jira/browse/CLJ-1535)
* [CLJ-1642](http://dev.clojure.org/jira/browse/CLJ-1642)

### 2.5 update - like update-in for first level

`update` is a new function that is like update-in specifically for first-level keys:

    (update m k f args...)

Example use:

    user> (update {:a 1} :a inc)
	{:a 2}
	user> (update {:a 1} :a + 2)
	{:a 3}
	user> (update {} :a identity)  ;; missing returns nil
	{:a nil}

* [CLJ-1251](http://dev.clojure.org/jira/browse/CLJ-1251)

### 2.6 Faster reduce and iterator paths

Several important Clojure functions now return sequences that also
contain fast reduce() (or in some cases iterator()) paths. In many
cases, the new implementations are also faster for lazy sequences

* repeat - now implements IReduce
* cycle - implements IReduceInit
* iterate - implements IReduceInit
* range - implements IReduce, specialized case handles common case of all longs
* keys - iterates directly over the keys of a map, without seq or MapEntry allocation
* vals - iterates directly over the vals of a map, without seq or MapEntry allocation
* iterator-seq - creates a chunked sequence when previously it was unchunked

Additionally, hash-maps and hash-sets now provide iterators that walk
the data structure directly rather than via a sequence.

A new interface (IMapIterable) for direct key and val iterators on maps
was added. External data structures can use this interface to provide
direct key and val iterators via keys and vals.

These enhancements are particularly effective when used
in tandem with transducers via transduce, sequence, into, and
eduction.

* [CLJ-1603](http://dev.clojure.org/jira/browse/CLJ-1603)
* [CLJ-1515](http://dev.clojure.org/jira/browse/CLJ-1515)
* [CLJ-1602](http://dev.clojure.org/jira/browse/CLJ-1602)
* [CLJ-1669](http://dev.clojure.org/jira/browse/CLJ-1669)
* [CLJ-1692](http://dev.clojure.org/jira/browse/CLJ-1692)
* [CLJ-1694](http://dev.clojure.org/jira/browse/CLJ-1694)
* [CLJ-1711](http://dev.clojure.org/jira/browse/CLJ-1711)
* [CLJ-1709](http://dev.clojure.org/jira/browse/CLJ-1709)
* [CLJ-1713](http://dev.clojure.org/jira/browse/CLJ-1713)
* [CLJ-1726](http://dev.clojure.org/jira/browse/CLJ-1726)
* [CLJ-1727](http://dev.clojure.org/jira/browse/CLJ-1727)

### 2.7 Printing as data

There have been enhancements in how the REPL prints values without a
print-method, specifically Throwable and the fallthrough Object case.
Both cases now print in a tagged literal data form that can be read
by the reader.

Unhandled objects print with the class, hash code, and toString:

	user=> *ns*
	#object[clojure.lang.Namespace 0x55aa628 "user"]

Thrown exceptions will still be printed in the normal way by the default
REPL but printing them to a stream will show a different form:

	user=> (/ 1 0)
	ArithmeticException Divide by zero  clojure.lang.Numbers.divide (Numbers.java:158)
	user=> (println *e)
	#error {
	 :cause Divide by zero
	 :via
	 [{:type java.lang.ArithmeticException
	   :message Divide by zero
	   :at [clojure.lang.Numbers divide Numbers.java 158]}]
	 :trace
	 [[clojure.lang.Numbers divide Numbers.java 158]
	  [clojure.lang.Numbers divide Numbers.java 3808]
	  ;; ... elided frames
	  ]}

Additionally, there is a new function available to obtain a Throwable as 
map data: `Throwable->map`.

* [CLJ-1703](http://dev.clojure.org/jira/browse/CLJ-1703)
* [CLJ-1716](http://dev.clojure.org/jira/browse/CLJ-1716)
* [CLJ-1735](http://dev.clojure.org/jira/browse/CLJ-1735)

### 2.8 run!

run! is a new function that takes a side effect reducing function and runs
it for all items in a collection via reduce. The accumulator is ignored and
nil is returned.

    (run! println (range 10))

## 3 Enhancements

### 3.1 Error messages

* [CLJ-1261](http://dev.clojure.org/jira/browse/CLJ-1261)
  Invalid defrecord results in exception attributed to consuming ns instead of defrecord ns
* [CLJ-1297](http://dev.clojure.org/jira/browse/CLJ-1297)
  Give more specific hint if namespace with "-" not found to check file uses "_"

### 3.2 Documentation strings

* [CLJ-1417](http://dev.clojure.org/jira/browse/CLJ-1417)
  clojure.java.io/input-stream has incorrect docstring
* [CLJ-1357](http://dev.clojure.org/jira/browse/CLJ-1357)
  Fix typo in gen-class doc-string
* [CLJ-1479](http://dev.clojure.org/jira/browse/CLJ-1479)
  Fix typo in filterv example
* [CLJ-1480](http://dev.clojure.org/jira/browse/CLJ-1480)
  Fix typo in defmulti docstring
* [CLJ-1477](http://dev.clojure.org/jira/browse/CLJ-1477)
  Fix typo in deftype docstring
* [CLJ-1478](http://dev.clojure.org/jira/browse/CLJ-1378)
  Fix typo in clojure.main usage
* [CLJ-1738](http://dev.clojure.org/jira/browse/CLJ-1738)
  Clarify usage on Java iterators in seq and iterator-seq

### 3.3 Performance

* [CLJ-1430](http://dev.clojure.org/jira/browse/CLJ-1430)
  Improve performance of partial with more unrolling
* [CLJ-1384](http://dev.clojure.org/jira/browse/CLJ-1384)
  clojure.core/set should use transients for better performance
* [CLJ-1429](http://dev.clojure.org/jira/browse/CLJ-1429)
  Cache unknown multimethod value default dispatch
* [CLJ-1529](http://dev.clojure.org/jira/browse/CLJ-1529)
  Reduce compile times by avoiding unnecessary calls to Class.forName()
* [CLJ-1546](http://dev.clojure.org/jira/browse/CLJ-1546)
  vec is now faster on almost all inputs
* [CLJ-1618](http://dev.clojure.org/jira/browse/CLJ-1618)
  set is now faster on almost all inputs
* [CLJ-1695](http://dev.clojure.org/jira/browse/CLJ-1695)
  Fixed reflection call in variadic vector-of constructor

### 3.4 Other enhancements

* [CLJ-1191](http://dev.clojure.org/jira/browse/CLJ-1191)
  Improve apropos to show some indication of namespace of symbols found
* [CLJ-1378](http://dev.clojure.org/jira/browse/CLJ-1378)
  Hints don't work with #() form of function
* [CLJ-1498](http://dev.clojure.org/jira/browse/CLJ-1498)
  Removes owner-thread check from transients - this check was preventing some valid usage of transients in core.async where a transient is created on one thread and then used again in another pooled thread (while still maintaining thread isolation).
* [CLJ-803](http://dev.clojure.org/jira/browse/CLJ-803)
  Extracted IAtom interface implemented by Atom.
* [CLJ-1315](http://dev.clojure.org/jira/browse/CLJ-1315)
  Don't initialize classes when importing them
* [CLJ-1330](http://dev.clojure.org/jira/browse/CLJ-1330)
  Class name clash between top-level functions and defn'ed ones
* [CLJ-1349](http://dev.clojure.org/jira/browse/CLJ-1349)
  Update to latest test.generative and add dependency on test.check
* [CLJ-1546](http://dev.clojure.org/jira/browse/CLJ-1546)
  vec now works with things that only implement Iterable or IReduceInit
* [CLJ-1618](http://dev.clojure.org/jira/browse/CLJ-1618)
  set now works with things that only implement Iterable or IReduceInit
* [CLJ-1633](http://dev.clojure.org/jira/browse/CLJ-1633)
  PersistentList/creator doesn't handle ArraySeqs correctly
* [CLJ-1589](http://dev.clojure.org/jira/browse/CLJ-1589)
  Clean up unused paths in InternalReduce
* [CLJ-1677](http://dev.clojure.org/jira/browse/CLJ-1677)
  Add setLineNumber() to LineNumberingPushbackReader
* [CLJ-1667](http://dev.clojure.org/jira/browse/CLJ-1667)
  Change test to avoid using hard-coded socket port
* [CLJ-1683](http://dev.clojure.org/jira/browse/CLJ-1683)
  Change reduce tests to better catch reduce without init bugs

## 4 Bug Fixes

* [CLJ-1362](http://dev.clojure.org/jira/browse/CLJ-1362)
  Reduce broken on some primitive vectors
* [CLJ-1388](http://dev.clojure.org/jira/browse/CLJ-1388)
  Equality bug on records created with nested calls to map->record
* [CLJ-1274](http://dev.clojure.org/jira/browse/CLJ-1274)
  Unable to set compiler options via system properties except for AOT compilation
* [CLJ-1241](http://dev.clojure.org/jira/browse/CLJ-1241)
  NPE when AOTing overrided clojure.core functions
* [CLJ-1185](http://dev.clojure.org/jira/browse/CLJ-1185)
  reductions does not check for reduced value
* [CLJ-1039](http://dev.clojure.org/jira/browse/CLJ-1039)
  Using def with metadata {:type :anything} throws ClassCastException during printing
* [CLJ-887](http://dev.clojure.org/jira/browse/CLJ-887)
  Error when calling primitive functions with destructuring in the arg vector
* [CLJ-823](http://dev.clojure.org/jira/browse/CLJ-823)
  Piping seque into seque can deadlock
* [CLJ-738](http://dev.clojure.org/jira/browse/CLJ-738)
  <= is incorrect when args include Double/NaN
* [CLJ-1408](http://dev.clojure.org/jira/browse/CLJ-1408)
  Make cached string value of Keyword and Symbol transient
* [CLJ-1466](http://dev.clojure.org/jira/browse/CLJ-1466)
  clojure.core/bean should implement Iterable
* [CLJ-1578](http://dev.clojure.org/jira/browse/CLJ-1578)
  Make refer of Clojure core function not throw exception on reload
* [CLJ-1501](http://dev.clojure.org/jira/browse/CLJ-1501)
  LazySeq equals() should not use equiv() logic
* [CLJ-1572](http://dev.clojure.org/jira/browse/CLJ-1572)
  into (and other fns that rely on reduce) require only IReduceInit
* [CLJ-1619](http://dev.clojure.org/jira/browse/CLJ-1619)
  PersistentVector now directly implements reduce without init
* [CLJ-1580](http://dev.clojure.org/jira/browse/CLJ-1580)
  Transient collections should guarantee thread visibility
* [CLJ-1590](http://dev.clojure.org/jira/browse/CLJ-1590)
  Some IReduce/IReduceInit implementors don't respect reduced
* [CLJ-979](http://dev.clojure.org/jira/browse/CLJ-979)
  Clojure resolves to wrong deftype classes when AOT compiling or reloading
* [CLJ-1636](http://dev.clojure.org/jira/browse/CLJ-1636)
  Fix intermittent SeqIterator problem by removing use of this as a sentinel
* [CLJ-1637](http://dev.clojure.org/jira/browse/CLJ-1636)
  Fix regression from CLJ-1546 that broke vec on MapEntry
* [CLJ-1663](http://dev.clojure.org/jira/browse/CLJ-1663)
  Fix regression from CLJ-979 for DynamicClassLoader classloader delegation
* [CLJ-1604](http://dev.clojure.org/jira/browse/CLJ-1604)
  Fix error from AOT'ed code defining a var with a clojure.core symbol name
* [CLJ-1561](http://dev.clojure.org/jira/browse/CLJ-1561)
  Fix incorrect line number reporting for error locations
* [CLJ-1568](http://dev.clojure.org/jira/browse/CLJ-1568)
  Fix incorrect line number reporting for error locations
* [CLJ-1638](http://dev.clojure.org/jira/browse/CLJ-1638)
  Fix regression from CLJ-1546 removed PersistentVector.create(List) method
* [CLJ-1681](http://dev.clojure.org/jira/browse/CLJ-1681)
  Fix regression from CLJ-1248 (1.6) in reflection warning with literal nil argument
* [CLJ-1648](http://dev.clojure.org/jira/browse/CLJ-1648)
  Use equals() instead of == when resolving Symbol
* [CLJ-1195](http://dev.clojure.org/jira/browse/CLJ-1195)
  emit-hinted-impl expands to ns-qualified invocation of fn
* [CLJ-1237](http://dev.clojure.org/jira/browse/CLJ-1237)
  reduce of sequence that switches between chunked and unchunked many times throws StackOverflow

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
`mix-collection-hash` for the final mixing operation. Alternatively,
you may call the helper functions `hash-ordered-coll` and
`hash-unordered-coll`.

Any details of the current hashing algorithm not specified on that
page should be considered subject to future change.

Related tickets for dev and regressions:

* [CLJ-1328](http://dev.clojure.org/jira/browse/CLJ-1328)
  Make several Clojure tests independent of ordering
* [CLJ-1331](http://dev.clojure.org/jira/browse/CLJ-1331)
  Update primitive vectors to use Murmur3 hash
* [CLJ-1335](http://dev.clojure.org/jira/browse/CLJ-1335)
  Update hash for empty PersistentList and LazySeq
* [CLJ-1336](http://dev.clojure.org/jira/browse/CLJ-1336)
  Make hashing mixing functions available in Clojure
* [CLJ-1338](http://dev.clojure.org/jira/browse/CLJ-1338)
  Make Murmur3 class public
* [CLJ-1344](http://dev.clojure.org/jira/browse/CLJ-1344)
  Update mapHasheq to call Murmur3 algorithm
* [CLJ-1348](http://dev.clojure.org/jira/browse/CLJ-1348)
  Add hash-ordered-coll and hash-unordered-coll
* [CLJ-1355](http://dev.clojure.org/jira/browse/CLJ-1355)
  Restore cached hashCode for Symbol and (uncached) hashCode for Keyword
* [CLJ-1365](http://dev.clojure.org/jira/browse/CLJ-1365)
  Add type hints for new collection hash functions

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
* [CLJ-1352](http://dev.clojure.org/jira/browse/CLJ-1352) - fix
  regression in CLJ-866

Added a new clojure.test/test-vars function that takes a list of vars, groups them by namespace, and
runs them *with their fixtures*.

## 3 Enhancements

### 3.1 Printing

* [CLJ-908](http://dev.clojure.org/jira/browse/CLJ-908)
  Print metadata for functions when *print-meta* is true and remove errant space at beginning.
* [CLJ-937](http://dev.clojure.org/jira/browse/CLJ-937)
  pprint cl-format now supports E, F, and G formats for ratios.

### 3.2 Error messages

* [CLJ-1248](http://dev.clojure.org/jira/browse/CLJ-1248)
  Include type information in reflection warning messages
* [CLJ-1099](http://dev.clojure.org/jira/browse/CLJ-1099)
  If non-seq passed where seq is needed, error message now is an
  ExceptionInfo with the instance value, retrievable via ex-data.
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
* [CLJ-1354](http://dev.clojure.org/jira/browse/CLJ-1354)
  Make APersistentVector.SubVector public so other collections can access
* [CLJ-1353](http://dev.clojure.org/jira/browse/CLJ-1353)
  Make awt run headless during the build process

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
* [CLJ-1363](http://dev.clojure.org/jira/browse/CLJ-1363)
  Field access via .- in reflective case does not work
* [CLJ-944](http://dev.clojure.org/jira/browse/CLJ-944)
  Compiler gives constant collections types which mismatch their
  runtime values
* [CLJ-1387](http://dev.clojure.org/jira/browse/CLJ-1387)
  reduce-kv on large hash maps ignores reduced result

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

## 3 Performance and Memory Enhancements

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
* (no ticket) Move loop locals into same clearing context as loop body


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

    (= [2 4] (filterv even? [1 2 3 4 5]))
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
