<!-- -*- mode: markdown ; mode: visual-line ; coding: utf-8 -*- -->

# Changes to Clojure in Version 1.4

## CONTENTS

<pre>
 1 Deprecated and Removed Features
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
 5 Modular Contrib
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
an initial value `init` and an association collection `coll`. `f` should 
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
  java.lang.VerifyError when compiling deftype or defrecord with argument name starting with double underscore characters.
* [CLJ-369](http://dev.clojure.org/jira/browse/CLJ-369)
  Check for invalid interface method names
* [CLJ-845](http://dev.clojure.org/jira/browse/CLJ-845)
  Unexpected interaction between protocol extension and namespaced method keyword/symbols
  * Ignoring namespace portion of symbols used to name methods in extend-type and extend-protocol
* [CLJ-852](http://dev.clojure.org/jira/browse/CLJ-852)
  IllegalArgumentException thrown when defining a var whose value is calculated with a primitive fn.
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
  * Only capture a shallow copy of the current Frame in binding-conveyor-fn, so that sends in agent actions don't built infinite Frame stacks.
* [CLJ-928](http://dev.clojure.org/jira/browse/CLJ-928)
  Instant literal for Date and Timestamp should print in UTC
* [CLJ-931](http://dev.clojure.org/jira/browse/CLJ-933)
  Syntactically broken clojure.test/are tests succeed
* [CLJ-933](http://dev.clojure.org/jira/browse/CLJ-933)
  Compiler warning on clojure.test-clojure.require-scratch
  
